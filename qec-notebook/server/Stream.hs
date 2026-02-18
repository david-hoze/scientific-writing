-- | TVar-based streaming for long computations (e.g. sweep progress).
-- The server can poll a TVar and send progress messages.
module Stream
  ( SweepAccumulator
  , newSweepAccumulator
  , readSweepResults
  , appendSweepResult
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')

import QEC.Notebook (SweepResult)

-- | Shared accumulator for streaming sweep results.
type SweepAccumulator = TVar [SweepResult]

newSweepAccumulator :: IO SweepAccumulator
newSweepAccumulator = newTVarIO []

readSweepResults :: SweepAccumulator -> IO [SweepResult]
readSweepResults = readTVarIO

appendSweepResult :: SweepAccumulator -> SweepResult -> IO ()
appendSweepResult var r = atomically $ modifyTVar' var (r :)
