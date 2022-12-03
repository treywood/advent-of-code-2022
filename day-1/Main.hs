module Main where

import           Data.List          (sort)
import           System.Environment
import           System.IO

main :: IO ()
main = do
  [fileName] <- getArgs
  handle <- openFile fileName ReadMode
  input <- hGetContents handle
  let (elfCals, _) = foldl calc ([], 0) (lines input)
  let ans = sum $ take 3 ((reverse . sort) elfCals)
  putStrLn (show ans)
  where
    calc :: ([Int],Int) -> String -> ([Int],Int)
    calc (m, acc) ""   = (acc : m, 0)
    calc (m, acc) line = (m, acc + (read line))
