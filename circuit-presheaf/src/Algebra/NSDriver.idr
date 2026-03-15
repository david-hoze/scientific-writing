module Algebra.NSDriver

import Algebra.M2Gen
import Algebra.M2Parse
import System as Sys
import System.File
import Data.String

%default covering

||| Write an M2 script to a file.
export
writeM2Script : String -> String -> IO (Either FileError ())
writeM2Script filename content = writeFile filename content

||| Run M2 on a script file if available, return output.
export
runM2 : String -> IO (Either String String)
runM2 scriptFile = do
  let outFile = scriptFile ++ ".out"
  ret <- system ("bash -c 'M2 --script " ++ scriptFile ++ " > " ++ outFile ++ " 2>&1'")
  if ret /= 0
    then pure (Left ("M2 exited with code " ++ show ret ++ ". Check " ++ outFile))
    else do
      Right content <- readFile outFile
        | Left err => pure (Left ("Could not read M2 output: " ++ show err))
      pure (Right content)

||| Full pipeline: write script, run M2 (if available), parse output.
export
runPipeline : String -> String -> IO ()
runPipeline filename scriptContent = do
  Right () <- writeM2Script filename scriptContent
    | Left err => putStrLn ("Error writing M2 script: " ++ show err)
  putStrLn ("M2 script written to: " ++ filename)
  Right output <- runM2 filename
    | Left msg => putStrLn msg
  let results = parseM2Output output
  putStrLn ("M2 results: " ++ show results)
  if isUnsat results
    then putStrLn "System is UNSATISFIABLE"
    else putStrLn "System is SATISFIABLE (or inconclusive)"
