-- | GHCi session management (subprocess-based).
--
-- Uses unique sentinel markers to delimit output, avoiding fragile
-- prompt-detection heuristics.
module Session
  ( Session
  , initSession
  , evalInSession
  , evalInSessionWithProgress
  , evalExprInSession
  , evalIOExprInSession
  , resetSession
  , closeSession
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import System.IO (Handle, hGetLine, hPutStrLn, hClose, hFlush, hSetBuffering, hSetEncoding, BufferMode(..), utf8)
import System.Process (createProcess, CreateProcess(..), StdStream(..), proc, ProcessHandle, terminateProcess)

-- | A running GHCi subprocess.
data Session = Session
  { sIn      :: !Handle
  , sOut     :: !Handle
  , sErr     :: !Handle
  , sProc    :: !ProcessHandle
  , sCounter :: !(IORef Int)
  }

-- | Unique sentinel that marks the end of a GHCi response.
sentinel :: Int -> String
sentinel n = "___QEC_SENTINEL_" ++ show n ++ "___"

-- | Start GHCi as a subprocess with cabal exec so qec-cat is in scope.
initSession :: IO Session
initSession = do
  let ghciArgs = [ "exec", "ghci", "--"
                 , "-ignore-dot-ghci"
                 , "-v0"
                 , "-fdiagnostics-color=never"
                 , "-fobject-code"   -- compile modules to native code (much faster than bytecode)
                 , "-O1"             -- enable optimizations for loaded code
                 ]
  (Just inh, Just outh, Just errh, ph) <-
    createProcess (proc "cabal" ghciArgs)
      { std_in  = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      , cwd     = Nothing
      }
  hSetBuffering inh  LineBuffering
  hSetBuffering outh LineBuffering
  hSetEncoding inh  utf8
  hSetEncoding outh utf8
  hSetEncoding errh utf8

  -- Drain stderr in background to prevent deadlocks
  _ <- forkIO $ drainHandle errh

  counter <- newIORef (0 :: Int)
  let s = Session inh outh errh ph counter

  -- Consume the startup banner
  _ <- evalInSession s "putStrLn \"\""

  -- Set UTF-8 encoding inside GHCi so Unicode JSON output works
  _ <- evalInSession s "import System.IO (hSetEncoding, stdout, stderr, utf8)"
  _ <- evalInSession s "hSetEncoding stdout utf8"
  _ <- evalInSession s "hSetEncoding stderr utf8"

  -- Auto-import the notebook prelude
  _ <- evalInSession s ":set -XOverloadedStrings"
  _ <- evalInSession s "import Prelude"
  _ <- evalInSession s "import QEC.Notebook"
  _ <- evalInSession s "import QEC.Notebook.Renderable"
  _ <- evalInSession s "import Data.Word (Word64)"

  putStrLn "  [session] QEC.Notebook imported"
  return s

-- | Drain a handle line-by-line, logging to server stdout.
drainHandle :: Handle -> IO ()
drainHandle h = go
  where
    go = do
      r <- try (hGetLine h) :: IO (Either SomeException String)
      case r of
        Left  _ -> return ()
        Right line -> do
          putStrLn ("[ghci:stderr] " ++ line)
          go

-- | Get the next sentinel and increment the counter.
nextSentinel :: Session -> IO String
nextSentinel (Session _ _ _ _ ref) = do
  n <- readIORef ref
  modifyIORef' ref (+1)
  return (sentinel n)

-- | Send a raw line to GHCi stdin.
sendLine :: Session -> String -> IO ()
sendLine (Session inh _ _ _ _) s = hPutStrLn inh s

-- | Flush GHCi stdin.
flushIn :: Session -> IO ()
flushIn (Session inh _ _ _ _) = hFlush inh

-- | Send a statement to GHCi and collect all output lines until the sentinel.
--
-- WARNING: This clobbers GHCi's @it@ binding because the sentinel uses
-- @putStrLn@. For expressions where you need @it@, use 'evalExprInSession'.
evalInSession :: Session -> String -> IO String
evalInSession s stmt = do
  sent <- nextSentinel s
  sendLine s stmt
  sendLine s ("putStrLn " ++ show sent)
  flushIn s
  collectUntil sent (sOut s)

-- | Like 'evalInSession' but calls a callback for each non-sentinel line.
-- Used to intercept progress markers printed by sweepIO.
evalInSessionWithProgress :: Session -> String -> (String -> IO ()) -> IO String
evalInSessionWithProgress s stmt onLine = do
  sent <- nextSentinel s
  sendLine s stmt
  sendLine s ("putStrLn " ++ show sent)
  flushIn s
  collectUntilWithProgress sent (sOut s) onLine

-- | Evaluate an expression and return (typeString, errorString).
--
-- Uses @let@ binding to suppress GHCi's auto-print, which avoids
-- enormous show output flooding the pipe for large values.
-- Resets @___qecIt@ before each eval so stale bindings don't leak
-- through on error.
evalExprInSession :: Session -> String -> IO (String, String)
evalExprInSession s expr = do
  -- Step 1: reset ___qecIt so stale values are detectable
  sent0 <- nextSentinel s
  sendLine s "let ___qecIt = ()"
  sendLine s ("putStrLn " ++ show sent0)
  flushIn s
  _ <- collectUntil sent0 (sOut s)

  -- Step 2: bind the expression (no auto-print)
  sent1 <- nextSentinel s
  sendLine s ("let ___qecIt = (" ++ expr ++ ")")
  sendLine s ("putStrLn " ++ show sent1)
  flushIn s
  _ <- collectUntil sent1 (sOut s)

  -- Step 3: get the type
  sent2 <- nextSentinel s
  sendLine s ":type ___qecIt"
  sendLine s ("putStrLn " ++ show sent2)
  flushIn s
  typeOut <- collectUntil sent2 (sOut s)

  return (typeOut, "")

-- | Read lines from a handle until we see one that equals the sentinel.
collectUntil :: String -> Handle -> IO String
collectUntil sent h = go []
  where
    go acc = do
      line <- hGetLine h
      if line == sent
        then return (unlines (reverse acc))
        else go (line : acc)

-- | Evaluate an IO expression using monadic bind (@<-@) and collect progress.
-- Returns (typeString, errorString). The callback receives each line of
-- output during evaluation (for intercepting progress markers).
evalIOExprInSession :: Session -> String -> (String -> IO ()) -> IO (String, String)
evalIOExprInSession s expr onLine = do
  -- Step 1: reset ___qecIt
  sent0 <- nextSentinel s
  sendLine s "let ___qecIt = ()"
  sendLine s ("putStrLn " ++ show sent0)
  flushIn s
  _ <- collectUntil sent0 (sOut s)

  -- Step 2: bind the IO expression using <- (monadic bind, executes the IO action)
  sent1 <- nextSentinel s
  sendLine s ("___qecIt <- (" ++ expr ++ ")")
  sendLine s ("putStrLn " ++ show sent1)
  flushIn s
  _ <- collectUntilWithProgress sent1 (sOut s) onLine

  -- Step 3: get the type
  sent2 <- nextSentinel s
  sendLine s ":type ___qecIt"
  sendLine s ("putStrLn " ++ show sent2)
  flushIn s
  typeOut <- collectUntil sent2 (sOut s)

  return (typeOut, "")

-- | Like 'collectUntil' but calls a callback for each non-sentinel line.
collectUntilWithProgress :: String -> Handle -> (String -> IO ()) -> IO String
collectUntilWithProgress sent h onLine = go []
  where
    go acc = do
      line <- hGetLine h
      if line == sent
        then return (unlines (reverse acc))
        else do
          onLine line
          go (line : acc)

-- | Reset the GHCi session: clear all user bindings and re-import the prelude.
-- Uses GHCi's :load command with no arguments to clear the context, then
-- re-runs the same imports as initSession.
resetSession :: Session -> IO ()
resetSession s = do
  -- :load with no args clears all loaded modules and user bindings
  _ <- evalInSession s ":load"
  -- Re-import the prelude (same sequence as initSession)
  _ <- evalInSession s "import System.IO (hSetEncoding, stdout, stderr, utf8)"
  _ <- evalInSession s "hSetEncoding stdout utf8"
  _ <- evalInSession s "hSetEncoding stderr utf8"
  _ <- evalInSession s ":set -XOverloadedStrings"
  _ <- evalInSession s "import Prelude"
  _ <- evalInSession s "import QEC.Notebook"
  _ <- evalInSession s "import QEC.Notebook.Renderable"
  _ <- evalInSession s "import Data.Word (Word64)"
  putStrLn "  [session] reset complete"

-- | Close the GHCi session.
closeSession :: Session -> IO ()
closeSession (Session inh _ _ ph _) = do
  _ <- try (hPutStrLn inh ":quit") :: IO (Either SomeException ())
  _ <- try (hFlush inh) :: IO (Either SomeException ())
  _ <- try (hClose inh) :: IO (Either SomeException ())
  terminateProcess ph
