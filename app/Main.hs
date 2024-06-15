module Main (main) where

import Lib (task01, task02)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["1"] = task01
parse ["2"] = task02
parse _ = do
  hPutStrLn stderr "Unimplemented"
  exitFailure
