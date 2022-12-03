module Main where

import           Data.List (sort)
import           Utils     (getInput)

main :: IO ()
main = do
  input <- getInput
  let (elfCals, _) = foldl calc ([], 0) (lines input)
  let ans = sum $ take 3 ((reverse . sort) elfCals)
  putStrLn (show ans)
  where
    calc :: ([Int],Int) -> String -> ([Int],Int)
    calc (m, acc) ""   = (acc : m, 0)
    calc (m, acc) line = (m, acc + (read line))
