module Utils where

import           System.Environment
import           System.IO

getInput :: IO String
getInput = do
  [filename] <- getArgs
  handle <- openFile filename ReadMode
  input <- hGetContents handle
  return input
