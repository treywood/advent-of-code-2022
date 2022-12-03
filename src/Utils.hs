module Utils where

import           System.Environment
import           System.IO

getInput :: IO String
getInput = do
  [filename] <- getArgs
  handle <- openFile filename ReadMode
  input <- hGetContents handle
  return input

run :: Show a => IO a -> IO ()
run prog = do
  result <- prog
  putStrLn (show result)
