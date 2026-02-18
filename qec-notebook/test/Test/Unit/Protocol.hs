module Test.Unit.Protocol (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (encode, eitherDecode, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Protocol

tests :: TestTree
tests = testGroup "Protocol"
  [ clientMsgParsingTests
  , serverMsgEncodingTests
  , malformedJsonTests
  ]

------------------------------------------------------------------------
-- § 3.3  ClientMsg parsing (FromJSON)
------------------------------------------------------------------------

clientMsgParsingTests :: TestTree
clientMsgParsingTests = testGroup "ClientMsg FromJSON"
  [ testCase "CM_eval round-trips" $ do
      let json = Aeson.object
            [ "type"    .= ("eval" :: Text)
            , "cell_id" .= ("c-1" :: Text)
            , "source"  .= ("ldpcCatCode 0" :: Text)
            ]
      case Aeson.fromJSON json :: Aeson.Result ClientMsg of
        Aeson.Error err  -> assertFailure ("Parse failed: " ++ err)
        Aeson.Success cm -> case cm of
          CM_eval req -> do
            evalCellId req @?= "c-1"
            evalSource req @?= "ldpcCatCode 0"
          other -> assertFailure ("Expected CM_eval, got: " ++ show other)

  , testCase "CM_cancel round-trips" $ do
      let json = Aeson.object
            [ "type"    .= ("cancel" :: Text)
            , "cell_id" .= ("c-1" :: Text)
            ]
      case Aeson.fromJSON json :: Aeson.Result ClientMsg of
        Aeson.Error err  -> assertFailure ("Parse failed: " ++ err)
        Aeson.Success cm -> case cm of
          CM_cancel cid -> cid @?= "c-1"
          other -> assertFailure ("Expected CM_cancel, got: " ++ show other)

  , testCase "CM_reset round-trips" $ do
      let json = Aeson.object [ "type" .= ("reset" :: Text) ]
      case Aeson.fromJSON json :: Aeson.Result ClientMsg of
        Aeson.Error err  -> assertFailure ("Parse failed: " ++ err)
        Aeson.Success cm -> case cm of
          CM_reset -> return ()
          other    -> assertFailure ("Expected CM_reset, got: " ++ show other)
  ]

------------------------------------------------------------------------
-- § 3.3  ServerMsg encoding (ToJSON)
------------------------------------------------------------------------

serverMsgEncodingTests :: TestTree
serverMsgEncodingTests = testGroup "ServerMsg ToJSON"
  [ testCase "SM_ready encodes type=ready" $ do
      let msg = SM_ready (ReadyMsg "0.1.0" ["QEC.Notebook"])
          val = Aeson.toJSON msg
      assertJsonField val "type" (Aeson.String "ready")
      assertJsonField val "version" (Aeson.String "0.1.0")
      case getJsonField "prelude" val of
        Just (Aeson.Array arr) ->
          assertBool "prelude contains QEC.Notebook"
            (Aeson.String "QEC.Notebook" `elem` arr)
        _ -> assertFailure "Expected prelude to be an array"

  , testCase "SM_result Ok encodes status=ok" $ do
      let msg = SM_result ResultMsg
            { resCellId      = "c-1"
            , resStatus      = Ok
            , resHaskellType = Just "Int"
            , resRenderAs    = Nothing
            , resData        = Nothing
            , resShowText    = "42"
            , resElapsedMs   = 5
            , resErrorType   = Nothing
            , resErrorMessage = Nothing
            }
          val = Aeson.toJSON msg
      assertJsonField val "type"   (Aeson.String "result")
      assertJsonField val "status" (Aeson.String "ok")

  , testCase "SM_result DeclOk encodes status=decl_ok" $ do
      let msg = SM_result ResultMsg
            { resCellId      = "c-2"
            , resStatus      = DeclOk
            , resHaskellType = Nothing
            , resRenderAs    = Nothing
            , resData        = Nothing
            , resShowText    = ""
            , resElapsedMs   = 1
            , resErrorType   = Nothing
            , resErrorMessage = Nothing
            }
          val = Aeson.toJSON msg
      assertJsonField val "status" (Aeson.String "decl_ok")

  , testCase "SM_result Error encodes status=error" $ do
      let msg = SM_result ResultMsg
            { resCellId      = "c-3"
            , resStatus      = Error
            , resHaskellType = Nothing
            , resRenderAs    = Just "error"
            , resData        = Just (Aeson.object
                [ Key.fromString "error_type" .= ("type_error" :: Text)
                , Key.fromString "message"    .= ("bad type" :: Text)
                ])
            , resShowText    = ""
            , resElapsedMs   = 2
            , resErrorType   = Just TypeError
            , resErrorMessage = Just "bad type"
            }
          val = Aeson.toJSON msg
      assertJsonField val "status" (Aeson.String "error")

  , testCase "SM_result JSON round-trips through encode/decode" $ do
      let msg = SM_result ResultMsg
            { resCellId      = "c-4"
            , resStatus      = Ok
            , resHaskellType = Just "CSSCode"
            , resRenderAs    = Just "css_code"
            , resData        = Just (Aeson.object [Key.fromString "n" .= (3 :: Int)])
            , resShowText    = "code"
            , resElapsedMs   = 100
            , resErrorType   = Nothing
            , resErrorMessage = Nothing
            }
          encoded = encode msg
      case eitherDecode encoded :: Either String Aeson.Value of
        Left err  -> assertFailure ("Decode failed: " ++ err)
        Right val -> do
          assertJsonField val "type"         (Aeson.String "result")
          assertJsonField val "cell_id"      (Aeson.String "c-4")
          assertJsonField val "status"       (Aeson.String "ok")
          assertJsonField val "haskell_type" (Aeson.String "CSSCode")
          assertJsonField val "render_as"    (Aeson.String "css_code")
  ]

------------------------------------------------------------------------
-- § 3.3  Malformed JSON
------------------------------------------------------------------------

malformedJsonTests :: TestTree
malformedJsonTests = testGroup "malformed JSON"
  [ testCase "missing type field fails" $ do
      let json = Aeson.object [ "cell_id" .= ("c-1" :: Text) ]
      case Aeson.fromJSON json :: Aeson.Result ClientMsg of
        Aeson.Error _  -> return ()  -- expected
        Aeson.Success _ -> assertFailure "Should have failed"

  , testCase "unknown type value fails" $ do
      let json = Aeson.object [ "type" .= ("bogus" :: Text) ]
      case Aeson.fromJSON json :: Aeson.Result ClientMsg of
        Aeson.Error _  -> return ()  -- expected
        Aeson.Success _ -> assertFailure "Should have failed"
  ]

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

getJsonField :: Text -> Aeson.Value -> Maybe Aeson.Value
getJsonField k (Aeson.Object o) = KM.lookup (Key.fromText k) o
getJsonField _ _                = Nothing

assertJsonField :: Aeson.Value -> Text -> Aeson.Value -> Assertion
assertJsonField obj field expected =
  case getJsonField field obj of
    Nothing  -> assertFailure ("Missing field: " ++ T.unpack field)
    Just val -> val @?= expected
