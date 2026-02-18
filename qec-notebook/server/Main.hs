-- | qec-notebook: WebSocket + static file server entry point.
module Main where

import Control.Exception (try, catch, SomeException, finally)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds)
import Network.HTTP.Types (status200, status404, Header)
import Network.Wai (Application, pathInfo, responseFile, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Network.WebSockets (ConnectionException)
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import System.FilePath ((</>), normalise)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Eval (classifyCell, buildResultMsg, buildDeclOkMsg, buildErrorMsg, CellKind(..))
import Protocol (ClientMsg(..), EvalReq(..), ReadyMsg(..), ServerMsg(..), ErrorType(..))
import Render (renderViaGHCi, cleanTypeStr)
import Session (Session, initSession, evalInSession, evalExprInSession, closeSession)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "qec-notebook v0.1.0"
  putStrLn "Starting GHCi session..."
  session <- initSession
  staticDir <- canonicalizePath "static"
  port <- getPort
  putStrLn ("GHCi ready. Open http://localhost:" ++ show port ++ " in your browser.")
  run port (app session staticDir) `finally` closeSession session

getPort :: IO Int
getPort = do
  args <- getArgs
  case args of
    ["--port", p] -> return (read p)
    _             -> return 8170

app :: Session -> FilePath -> Application
app session staticDir =
  websocketsOr noCompression (wsApp session) (staticApp staticDir)
  where
    noCompression = WS.defaultConnectionOptions
      { WS.connectionCompressionOptions = WS.NoCompression }

-- | Serve static files from the static/ directory.
staticApp :: FilePath -> Application
staticApp staticDir req respond = do
  let path = pathInfo req
  let relPath = case path of
        []   -> "index.html"
        segs -> foldr (\seg acc -> T.unpack seg </> acc) "" segs
  let fullPath = normalise (staticDir </> relPath)
  if staticDir `isPrefixOf` fullPath
    then respond (responseFile status200 (contentHeaders fullPath) fullPath Nothing)
    else respond (responseLBS status404 [] "Not found")

-- | Guess Content-Type from file extension.
contentHeaders :: FilePath -> [Header]
contentHeaders fp
  | ".html" `isSuffixOf` fp = [("Content-Type", "text/html; charset=utf-8")]
  | ".css"  `isSuffixOf` fp = [("Content-Type", "text/css; charset=utf-8")]
  | ".js"   `isSuffixOf` fp = [("Content-Type", "application/javascript; charset=utf-8")]
  | ".json" `isSuffixOf` fp = [("Content-Type", "application/json; charset=utf-8")]
  | ".svg"  `isSuffixOf` fp = [("Content-Type", "image/svg+xml")]
  | ".png"  `isSuffixOf` fp = [("Content-Type", "image/png")]
  | otherwise                = []

-- | WebSocket handler.
wsApp :: Session -> WS.ServerApp
wsApp session pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    sendJSON conn (SM_ready (ReadyMsg (T.pack "0.1.0") [T.pack "QEC.Notebook"]))
    loop session conn `catch` handleDisconnect
  where
    handleDisconnect :: ConnectionException -> IO ()
    handleDisconnect _ = putStrLn "[ws] client disconnected"

-- | Send a ToJSON value over the WebSocket.
sendJSON :: Aeson.ToJSON a => WS.Connection -> a -> IO ()
sendJSON conn val = WS.sendTextData conn (Aeson.encode val)

-- | Main receive loop.
loop :: Session -> WS.Connection -> IO ()
loop session conn = do
  raw <- WS.receiveData conn :: IO BL.ByteString
  case Aeson.eitherDecode raw of
    Left _err -> loop session conn
    Right clientMsg -> do
      handleClient session conn clientMsg
      loop session conn

-- | Handle a single client message.
handleClient :: Session -> WS.Connection -> ClientMsg -> IO ()
handleClient session conn (CM_eval req) = do
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
      putStrLn ("[eval] expression: " ++ take 60 src)
      result <- try (evalExprInSession session src)
        :: IO (Either SomeException (String, String))
      putStrLn "[eval] evalExprInSession done"
      case result of
        Left err -> do
          putStrLn ("[eval] ERROR: " ++ take 80 (show err))
          ms <- elapsedMs start
          sendJSON conn (SM_result (buildErrorMsg cellId RuntimeException (show err) ms))
        Right (typeStr, _) -> do
          let cleanType = cleanTypeStr typeStr
          putStrLn ("[eval] type: " ++ take 60 cleanType)
          if cleanType == "()"
            then do
              ms <- elapsedMs start
              sendJSON conn (SM_result (buildErrorMsg cellId TypeError
                ("Expression failed to evaluate: " ++ src) ms))
            else do
              ro <- renderViaGHCi session typeStr
              putStrLn "[eval] render done"
              ms <- elapsedMs start
              sendJSON conn (SM_result (buildResultMsg cellId ms ro))
              putStrLn ("[eval] sent result, " ++ show ms ++ "ms")

handleClient _ _ CM_reset     = return ()
handleClient _ _ (CM_cancel _) = return ()
handleClient _ _ (CM_save _ _) = return ()
handleClient _ _ (CM_load _)   = return ()

-- | Compute elapsed milliseconds since a start time.
elapsedMs :: UTCTime -> IO Int
elapsedMs start = do
  end <- getCurrentTime
  return (round (nominalDiffTimeToSeconds (end `diffUTCTime` start) * 1000))

-- | Strip leading/trailing whitespace.
strip :: String -> String
strip = f . f
  where f = reverse . dropWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')
