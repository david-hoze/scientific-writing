{-# LANGUAGE DeriveDataTypeable #-}
module Test.Integration.WebSocket (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (SomeException, try, catch)
import Data.Aeson ((.=))
import Data.Data (Typeable)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import Network.HTTP.Types (status404)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (openFreePort, runSettingsSocket, defaultSettings)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Socket (close)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options (IsOption(..))
-- DependencyType(AllFinish) re-exported from Test.Tasty
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Eval (classifyCell, CellKind(..), buildResultMsg, buildDeclOkMsg, buildErrorMsg)
import Protocol (ClientMsg(..), EvalReq(..), ReadyMsg(..), ServerMsg(..), ErrorType(..))
import Render (renderViaGHCi, cleanTypeStr)
import Session (Session, initSession, evalInSession, evalExprInSession, closeSession)

------------------------------------------------------------------------
-- Tasty option: --long
------------------------------------------------------------------------

newtype LongTests = LongTests Bool
  deriving (Eq, Ord, Typeable)

instance IsOption LongTests where
  defaultValue = LongTests False
  parseValue s = case s of
    "true"  -> Just (LongTests True)
    "false" -> Just (LongTests False)
    "True"  -> Just (LongTests True)
    "False" -> Just (LongTests False)
    _       -> Nothing
  optionName = pure "long"
  optionHelp = pure "Run long-running sweep tests (10-30s)"

------------------------------------------------------------------------
-- Server wiring (mirrors Main.hs without importing it)
------------------------------------------------------------------------

testApp :: Session -> Application
testApp session =
  websocketsOr wsOpts (testWsApp session) fallbackApp
  where
    wsOpts = WS.defaultConnectionOptions
      { WS.connectionCompressionOptions = WS.NoCompression }
    fallbackApp _ respond = respond $ responseLBS status404 [] "Not found"

testWsApp :: Session -> WS.ServerApp
testWsApp session pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    WS.sendTextData conn (Aeson.encode $ SM_ready (ReadyMsg "0.1.0" ["QEC.Notebook"]))
    testLoop session conn `catch` handleDisconnect
  where
    handleDisconnect :: WS.ConnectionException -> IO ()
    handleDisconnect _ = return ()

testLoop :: Session -> WS.Connection -> IO ()
testLoop session conn = do
  raw <- WS.receiveData conn :: IO BL.ByteString
  case Aeson.eitherDecode raw of
    Left _ -> testLoop session conn  -- ignore malformed
    Right msg -> do
      testHandleClient session conn msg
      testLoop session conn

testHandleClient :: Session -> WS.Connection -> ClientMsg -> IO ()
testHandleClient session conn (CM_eval req) = do
  start <- getCurrentTime
  let src    = T.unpack (evalSource req)
      cellId = evalCellId req
  case classifyCell src of
    Declaration -> do
      _ <- evalInSession session src
      ms <- elapsedMs start
      sendJSON conn (SM_result (buildDeclOkMsg cellId ms))

    GHCiCommand -> do
      _ <- evalInSession session src
      ms <- elapsedMs start
      sendJSON conn (SM_result (buildDeclOkMsg cellId ms))

    Expression -> do
      result <- try (evalExprInSession session src)
        :: IO (Either SomeException (String, String))
      case result of
        Left err -> do
          ms <- elapsedMs start
          sendJSON conn (SM_result (buildErrorMsg cellId RuntimeException (show err) ms))
        Right (typeStr, _) -> do
          let cleanType = cleanTypeStr typeStr
          if cleanType == "()"
            then do
              ms <- elapsedMs start
              sendJSON conn (SM_result (buildErrorMsg cellId TypeError
                ("Expression failed to evaluate: " ++ src) ms))
            else do
              ro <- renderViaGHCi session typeStr
              ms <- elapsedMs start
              sendJSON conn (SM_result (buildResultMsg cellId ms ro))

testHandleClient _ _ CM_reset       = return ()  -- no-op (matches production)
testHandleClient _ _ (CM_cancel _)  = return ()
testHandleClient _ _ (CM_save _ _)  = return ()
testHandleClient _ _ (CM_load _)    = return ()

sendJSON :: Aeson.ToJSON a => WS.Connection -> a -> IO ()
sendJSON conn val = WS.sendTextData conn (Aeson.encode val)

elapsedMs :: UTCTime -> IO Int
elapsedMs start = do
  end <- getCurrentTime
  return (round (nominalDiffTimeToSeconds (end `diffUTCTime` start) * 1000))

------------------------------------------------------------------------
-- Test entry point
------------------------------------------------------------------------

tests :: IO TestTree
tests = do
  result <- try startServer :: IO (Either SomeException (Int, IO ()))
  case result of
    Left err -> pure $ testGroup "Integration (SKIPPED)"
      [ testCase "server startup failed" $
          assertFailure ("GHCi/server failed to start: " ++ show err)
      ]
    Right (port, cleanup) ->
      pure $ withResource (pure port) (const cleanup) $ \getPort ->
        sequentialTestGroup "Integration" AllFinish
          [ connectionTests getPort
          , expressionTests getPort
          , declarationTests getPort
          , errorTests getPort
          , sequenceTests getPort
          , resetTests getPort
          , sweepTests getPort
          ]

-- | Start the server on an ephemeral port. Returns (port, cleanup).
startServer :: IO (Int, IO ())
startServer = do
  (port, sock) <- openFreePort
  session <- initSession
  serverThread <- async $
    runSettingsSocket defaultSettings sock (testApp session)
  waitForServer port
  let cleanup = do
        cancel serverThread
        closeSession session
        close sock
  return (port, cleanup)

-- | Poll until the server accepts a WebSocket connection.
waitForServer :: Int -> IO ()
waitForServer port = go (60 :: Int)  -- 60 * 500ms = 30s
  where
    go 0 = error "Server did not start within 30 seconds"
    go n = do
      result <- try probe :: IO (Either SomeException ())
      case result of
        Right () -> return ()
        Left _   -> do
          threadDelay 500000
          go (n - 1)
    probe = WS.runClient "127.0.0.1" port "/" $ \conn -> do
      _ <- WS.receiveData conn :: IO BL.ByteString
      return ()

------------------------------------------------------------------------
-- WS client helpers
------------------------------------------------------------------------

-- | Run a test action with a fresh WebSocket connection.
-- Receives and verifies the ready message before calling the action.
withConn :: IO Int -> (WS.Connection -> IO ()) -> Assertion
withConn getPort action = do
  port <- getPort
  WS.runClient "127.0.0.1" port "/" $ \conn -> do
    ready <- recvMsgOrFail conn
    assertTextField ready "type" "ready"
    action conn

-- | Send an eval message.
sendEval :: WS.Connection -> Text -> Text -> IO ()
sendEval conn cellId source =
  WS.sendTextData conn $ Aeson.encode $ Aeson.object
    [ "type"    .= ("eval" :: Text)
    , "cell_id" .= cellId
    , "source"  .= source
    ]

-- | Send a reset message.
sendReset :: WS.Connection -> IO ()
sendReset conn =
  WS.sendTextData conn $ Aeson.encode $ Aeson.object
    [ "type" .= ("reset" :: Text) ]

-- | Receive a server message with a 60-second timeout.
recvMsg :: WS.Connection -> IO (Maybe Aeson.Value)
recvMsg conn = timeout (60 * 1000000) $ do
  raw <- WS.receiveData conn :: IO BL.ByteString
  case Aeson.eitherDecode raw of
    Left err  -> error ("Failed to decode server message: " ++ err)
    Right val -> return val

-- | Receive a server message; fail the test on timeout.
recvMsgOrFail :: WS.Connection -> IO Aeson.Value
recvMsgOrFail conn = do
  mval <- recvMsg conn
  case mval of
    Nothing  -> assertFailure "Timed out waiting for server message" >> error "unreachable"
    Just val -> return val

-- | Send an eval and collect the result, skipping progress messages.
-- Returns (resultMsg, [progressMsgs]).
evalAndWait :: WS.Connection -> Text -> Text -> IO (Aeson.Value, [Aeson.Value])
evalAndWait conn cellId source = do
  sendEval conn cellId source
  collectResult cellId []
  where
    collectResult cid progs = do
      msg <- recvMsgOrFail conn
      let ty  = getTextField "type"    msg
          mid = getTextField "cell_id" msg
      case ty of
        Just "result" | mid == Just cid -> return (msg, reverse progs)
        Just "progress" -> collectResult cid (msg : progs)
        _ -> collectResult cid progs  -- skip unexpected

-- | Receive a result with a 120-second timeout (for long-running evals).
recvMsgLong :: WS.Connection -> IO (Maybe Aeson.Value)
recvMsgLong conn = timeout (120 * 1000000) $ do
  raw <- WS.receiveData conn :: IO BL.ByteString
  case Aeson.eitherDecode raw of
    Left err  -> error ("Failed to decode server message: " ++ err)
    Right val -> return val

-- | Like evalAndWait but with a 120-second timeout per message.
evalAndWaitLong :: WS.Connection -> Text -> Text -> IO (Aeson.Value, [Aeson.Value])
evalAndWaitLong conn cellId source = do
  sendEval conn cellId source
  collectResult cellId []
  where
    collectResult cid progs = do
      mval <- recvMsgLong conn
      case mval of
        Nothing -> assertFailure "Timed out (120s) waiting for result" >> error "unreachable"
        Just msg -> do
          let ty  = getTextField "type"    msg
              mid = getTextField "cell_id" msg
          case ty of
            Just "result" | mid == Just cid -> return (msg, reverse progs)
            Just "progress" -> collectResult cid (msg : progs)
            _ -> collectResult cid progs

------------------------------------------------------------------------
-- JSON assertion helpers
------------------------------------------------------------------------

getField :: Text -> Aeson.Value -> Maybe Aeson.Value
getField k (Aeson.Object o) = KM.lookup (Key.fromText k) o
getField _ _                = Nothing

getTextField :: Text -> Aeson.Value -> Maybe Text
getTextField k v = case getField k v of
  Just (Aeson.String t) -> Just t
  _                     -> Nothing

assertTextField :: Aeson.Value -> Text -> Text -> Assertion
assertTextField obj field expected =
  case getTextField field obj of
    Nothing  -> assertFailure ("Missing text field: " ++ T.unpack field)
    Just val -> val @?= expected

assertFieldNull :: Aeson.Value -> Text -> Assertion
assertFieldNull obj field =
  case getField field obj of
    Nothing        -> return ()
    Just Aeson.Null -> return ()
    Just val       -> assertFailure
      ("Expected null for '" ++ T.unpack field ++ "', got: " ++ show val)

------------------------------------------------------------------------
-- § 4.3  Connection tests
------------------------------------------------------------------------

connectionTests :: IO Int -> TestTree
connectionTests getPort = sequentialTestGroup "Connection" AllFinish
  [ testCase "ready_on_connect" $ do
      port <- getPort
      WS.runClient "127.0.0.1" port "/" $ \conn -> do
        ready <- recvMsgOrFail conn
        assertTextField ready "type"    "ready"
        assertTextField ready "version" "0.1.0"
        case getField "prelude" ready of
          Just (Aeson.Array arr) ->
            assertBool "prelude contains QEC.Notebook"
              (Aeson.String "QEC.Notebook" `elem` arr)
          _ -> assertFailure "Expected prelude to be an array"

  , testCase "malformed_json" $ withConn getPort $ \conn -> do
      WS.sendTextData conn ("{not json}" :: BL.ByteString)
      -- Server should not crash; subsequent eval still works
      (result, _) <- evalAndWait conn "mj-1" "42 :: Int"
      assertTextField result "status" "ok"

  , testCase "unknown_message_type" $ withConn getPort $ \conn -> do
      WS.sendTextData conn (Aeson.encode $ Aeson.object
        [ "type" .= ("bogus" :: Text) ])
      -- Server should not crash; subsequent eval still works
      (result, _) <- evalAndWait conn "um-1" "42 :: Int"
      assertTextField result "status" "ok"

  , testCase "concurrent_connects" $ do
      -- Both connections open simultaneously, proving the server handles
      -- multiple clients.  Evals are sequential because they share one
      -- GHCi session whose sentinel protocol is not thread-safe.
      port <- getPort
      WS.runClient "127.0.0.1" port "/" $ \conn1 -> do
        ready1 <- recvMsgOrFail conn1
        assertTextField ready1 "type" "ready"
        WS.runClient "127.0.0.1" port "/" $ \conn2 -> do
          ready2 <- recvMsgOrFail conn2
          assertTextField ready2 "type" "ready"
          -- Eval on each connection (sequentially)
          (r1, _) <- evalAndWait conn1 "cc-1" "42 :: Int"
          assertTextField r1 "status" "ok"
          (r2, _) <- evalAndWait conn2 "cc-2" "42 :: Int"
          assertTextField r2 "status" "ok"
  ]

------------------------------------------------------------------------
-- § 4.4  Expression evaluation tests
------------------------------------------------------------------------

expressionTests :: IO Int -> TestTree
expressionTests getPort = sequentialTestGroup "Expression evaluation" AllFinish
  [ testCase "eval_int_literal" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "ei-1" "42 :: Int"
      assertTextField result "status" "ok"
      case getTextField "show_text" result of
        Just st -> assertBool "show_text contains 42" ("42" `T.isInfixOf` st)
        Nothing -> assertFailure "Missing show_text"
      assertFieldNull result "render_as"

  , testCase "eval_css_code" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "ec-1" "repetitionCode 3"
      assertTextField result "status" "ok"
      assertTextField result "render_as" "css_code"
      case getField "data" result of
        Just (Aeson.Object d) -> do
          case KM.lookup (Key.fromText "n") d of
            Just (Aeson.Number n) -> realToFrac n @?= (3 :: Double)
            _ -> assertFailure "data.n missing or not a number"
          case KM.lookup (Key.fromText "k") d of
            Just (Aeson.Number k) -> realToFrac k @?= (1 :: Double)
            _ -> assertFailure "data.k missing or not a number"
        _ -> assertFailure "Expected data to be an object"

  , testCase "eval_cat_params" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "ecp-1" "defaultCatParams"
      assertTextField result "status" "ok"
      assertTextField result "render_as" "cat_qubit_params"
      case getField "data" result of
        Just (Aeson.Object d) ->
          case KM.lookup (Key.fromText "alpha_sq") d of
            Just (Aeson.Number _) -> return ()
            _ -> assertFailure "data.alpha_sq missing or not a number"
        _ -> assertFailure "Expected data to be an object"

  , testCase "eval_pauli_channel" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "epc-1" "catNoise defaultCatParams"
      assertTextField result "status" "ok"
      assertTextField result "render_as" "pauli_channel"
      case getField "data" result of
        Just (Aeson.Object d) ->
          case KM.lookup (Key.fromText "p_z") d of
            Just (Aeson.Number _) -> return ()
            _ -> assertFailure "data.p_z missing or not a number"
        _ -> assertFailure "Expected data to be an object"

  , testCase "eval_bin_matrix" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "ebm-1" "cssHZ (repetitionCode 3)"
      assertTextField result "status" "ok"
      assertTextField result "render_as" "bin_matrix"
      case getField "data" result of
        Just (Aeson.Object d) -> do
          assertBool "data.rows present" (KM.member (Key.fromText "rows") d)
          assertBool "data.cols present" (KM.member (Key.fromText "cols") d)
        _ -> assertFailure "Expected data to be an object"

  , testCase "eval_sim_result" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "esr-1"
        "runSimulation defaultSimConfig (repetitionCode 3) 0.01 42"
      assertTextField result "status" "ok"
      assertTextField result "render_as" "sim_result"
      case getField "data" result of
        Just (Aeson.Object d) ->
          case KM.lookup (Key.fromText "logical_rate") d of
            Just (Aeson.Number _) -> return ()
            _ -> assertFailure "data.logical_rate missing or not a number"
        _ -> assertFailure "Expected data to be an object"

  , testCase "eval_resource_estimate" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "ere-1"
        "estimateResources shorRSA2048 defaultCatParams RepetitionCat defaultFactory"
      assertTextField result "status" "ok"
      assertTextField result "render_as" "resource_estimate"
      case getField "data" result of
        Just (Aeson.Object d) -> do
          assertBool "data.total_qubits present"
            (KM.member (Key.fromText "total_qubits") d)
          case KM.lookup (Key.fromText "total_qubits") d of
            Just (Aeson.Number n) ->
              assertBool "total_qubits > 0" (realToFrac n > (0 :: Double))
            _ -> assertFailure "data.total_qubits not a number"
          assertBool "data.code_distance present"
            (KM.member (Key.fromText "code_distance") d)
          assertBool "data.runtime_seconds present"
            (KM.member (Key.fromText "runtime_seconds") d)
          assertBool "data.code_family present"
            (KM.member (Key.fromText "code_family") d)
        _ -> assertFailure "Expected data to be an object"

  , testCase "eval_resource_comparison" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "erc-1"
        "compareArchitectures shorRSA2048 defaultCatParams [RepetitionCat, SurfaceCode, LDPCCat]"
      assertTextField result "status" "ok"
      assertTextField result "render_as" "resource_comparison"
      case getField "data" result of
        Just (Aeson.Object d) -> do
          assertBool "data.columns present"
            (KM.member (Key.fromText "columns") d)
          case KM.lookup (Key.fromText "rows") d of
            Just (Aeson.Array rows) ->
              assertBool "rows has 3 entries (one per code family)"
                (length rows == 3)
            _ -> assertFailure "data.rows missing or not an array"
        _ -> assertFailure "Expected data to be an object"

  , testCase "eval_code_construction_valid" $ withConn getPort $ \conn -> do
      -- mkCSSCode with matching matrices from repetitionCode 3 → Right CSSCode
      (result, _) <- evalAndWait conn "ecc-1"
        "mkCSSCode (cssHX (repetitionCode 3)) (cssHZ (repetitionCode 3))"
      assertTextField result "status" "ok"
      assertTextField result "render_as" "code_construction"
      case getField "data" result of
        Just dv@(Aeson.Object d) -> do
          assertTextField dv "status" "valid"
          assertBool "data.code present"
            (KM.member (Key.fromText "code") d)
        _ -> assertFailure "Expected data to be an object"

  , testCase "eval_code_construction_invalid" $ withConn getPort $ \conn -> do
      -- mkCSSCode with mismatched dimensions → Left (DimensionMismatch ...)
      (result, _) <- evalAndWait conn "ecc-2"
        "mkCSSCode (cssHX (repetitionCode 3)) (cssHZ (repetitionCode 5))"
      assertTextField result "status" "ok"
      assertTextField result "render_as" "code_construction"
      case getField "data" result of
        Just dv@(Aeson.Object _) -> do
          assertTextField dv "status" "invalid"
          case getField "error" dv of
            Just ev@(Aeson.Object _) ->
              assertTextField ev "kind" "dimension_mismatch"
            _ -> assertFailure "data.error missing or not an object"
        _ -> assertFailure "Expected data to be an object"

  , testCase "eval_string" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "es-1" "show 42"
      assertTextField result "status" "ok"
      case getTextField "show_text" result of
        Just st -> assertBool "show_text is non-empty" (not (T.null st))
        Nothing -> assertFailure "Missing show_text"

  , testCase "elapsed_ms_present" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "em-1" "42 :: Int"
      case getField "elapsed_ms" result of
        Just (Aeson.Number n) ->
          assertBool "elapsed_ms >= 0" (realToFrac n >= (0 :: Double))
        _ -> assertFailure "elapsed_ms missing or not a number"

  , testCase "haskell_type_present" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "ht-1" "repetitionCode 3"
      case getTextField "haskell_type" result of
        Just ht -> assertBool "haskell_type contains CSSCode"
          ("CSSCode" `T.isInfixOf` ht)
        Nothing -> assertFailure "Missing haskell_type"
  ]

------------------------------------------------------------------------
-- § 4.5  Declaration tests
------------------------------------------------------------------------

declarationTests :: IO Int -> TestTree
declarationTests getPort = sequentialTestGroup "Declarations" AllFinish
  [ testCase "let_binding" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "lb-1" "let x = 42 :: Int"
      assertTextField result "status" "decl_ok"
      assertFieldNull result "render_as"

  , testCase "let_then_use" $ withConn getPort $ \conn -> do
      (r1, _) <- evalAndWait conn "ltu-1" "let y = repetitionCode 5"
      assertTextField r1 "status" "decl_ok"
      (r2, _) <- evalAndWait conn "ltu-2" "y"
      assertTextField r2 "status" "ok"
      assertTextField r2 "render_as" "css_code"
      case getField "data" r2 of
        Just (Aeson.Object d) ->
          case KM.lookup (Key.fromText "n") d of
            Just (Aeson.Number n) -> realToFrac n @?= (5 :: Double)
            _ -> assertFailure "data.n missing or not a number"
        _ -> assertFailure "Expected data to be an object"

  , testCase "import_module" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "im-1" "import Data.List (nub)"
      assertTextField result "status" "decl_ok"

  , testCase "ghci_command_set" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "gc-1" ":set -XScopedTypeVariables"
      assertTextField result "status" "decl_ok"
  ]

------------------------------------------------------------------------
-- § 4.6  Error tests
------------------------------------------------------------------------

errorTests :: IO Int -> TestTree
errorTests getPort = sequentialTestGroup "Errors" AllFinish
  [ testCase "undefined_variable" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "uv-1" "notAFunction 3"
      assertTextField result "status" "error"
      assertTextField result "render_as" "error"

  , testCase "type_error" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "te-1" "repetitionCode True"
      assertTextField result "status" "error"

  , testCase "failed_eval_unit" $ withConn getPort $ \conn -> do
      (result, _) <- evalAndWait conn "feu-1" "undefined :: ()"
      assertTextField result "status" "error"

  , testCase "empty_source" $ withConn getPort $ \conn -> do
      -- Should not crash the server
      sendEval conn "es0-1" ""
      result <- recvMsgOrFail conn
      let status = getTextField "status" result
      assertBool "status is decl_ok or error"
        (status == Just "decl_ok" || status == Just "error")
  ]

------------------------------------------------------------------------
-- § 4.7  Sequence and state tests
------------------------------------------------------------------------

sequenceTests :: IO Int -> TestTree
sequenceTests getPort = sequentialTestGroup "Sequence & state" AllFinish
  [ testCase "binding_persists" $ withConn getPort $ \conn -> do
      (r1, _) <- evalAndWait conn "bp-1" "let z = 99 :: Int"
      assertTextField r1 "status" "decl_ok"
      (r2, _) <- evalAndWait conn "bp-2" "z"
      assertTextField r2 "status" "ok"
      case getTextField "show_text" r2 of
        Just st -> assertBool "show_text contains 99" ("99" `T.isInfixOf` st)
        Nothing -> assertFailure "Missing show_text"

  , testCase "binding_chain" $ withConn getPort $ \conn -> do
      (r1, _) <- evalAndWait conn "bc-1" "let a = repetitionCode 3"
      assertTextField r1 "status" "decl_ok"
      (r2, _) <- evalAndWait conn "bc-2" "let b = cssHZ a"
      assertTextField r2 "status" "decl_ok"
      (r3, _) <- evalAndWait conn "bc-3" "b"
      assertTextField r3 "status" "ok"
      assertTextField r3 "render_as" "bin_matrix"
  ]

resetTests :: IO Int -> TestTree
resetTests getPort = sequentialTestGroup "Reset" AllFinish
  [ testCase "reset_clears_bindings (XFAIL: reset is no-op)" $ withConn getPort $ \conn -> do
      -- NOTE: This test will fail until CM_reset is implemented in the
      -- server (currently a no-op). The failure is expected and documents
      -- the missing feature.
      (r1, _) <- evalAndWait conn "rc-1" "let w___reset_test = 1 :: Int"
      assertTextField r1 "status" "decl_ok"
      sendReset conn
      -- Small delay to let reset process
      threadDelay 100000
      (r2, _) <- evalAndWait conn "rc-2" "w___reset_test"
      assertTextField r2 "status" "error"

  , testCase "reset_reimports" $ withConn getPort $ \conn -> do
      sendReset conn
      threadDelay 100000
      (result, _) <- evalAndWait conn "rr-1" "repetitionCode 3"
      assertTextField result "status" "ok"
  ]

------------------------------------------------------------------------
-- § 4.8  Sweep / progress tests (long-running)
------------------------------------------------------------------------

sweepTests :: IO Int -> TestTree
sweepTests getPort = askOption $ \(LongTests long) ->
  if not long
    then testGroup "Sweep (skipped -- use --long to enable)" []
    else sequentialTestGroup "Sweep" AllFinish
      [ testCase "sweep_produces_result" $ withConn getPort $ \conn -> do
          -- Set up the config
          (r1, _) <- evalAndWait conn "sw-1"
            "let qc = SimConfig 500 defaultBPConfig 1"
          assertTextField r1 "status" "decl_ok"
          -- Run the sweep
          (result, _progs) <- evalAndWaitLong conn "sw-2"
            "sweep (sweepCodes \"rep\" repetitionCode [3,5]) (noiseRange 0.02 0.1 3) qc"
          assertTextField result "status" "ok"
          assertTextField result "render_as" "threshold_plot"
          case getField "data" result of
            Just (Aeson.Object d) ->
              assertBool "data.results present"
                (KM.member (Key.fromText "results") d)
            _ -> assertFailure "Expected data to be an object"

      , testCase "sweep_sends_progress" $ withConn getPort $ \conn -> do
          (r1, _) <- evalAndWait conn "sp-1"
            "let qc = SimConfig 500 defaultBPConfig 1"
          assertTextField r1 "status" "decl_ok"
          (_, progs) <- evalAndWaitLong conn "sp-2"
            "sweep (sweepCodes \"rep\" repetitionCode [3,5]) (noiseRange 0.02 0.1 3) qc"
          assertBool "at least one progress message" (not (null progs))
      ]
