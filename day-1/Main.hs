module Main where

import Data.List (sort)
import Utils (getInput, run)

main :: IO ()
main = run $ do
    input <- getInput
    let (elfCals, _) = foldl calc ([], 0) (lines input)
    return $ sum (take 3 ((reverse . sort) elfCals))
  where
    calc :: ([Int], Int) -> String -> ([Int], Int)
    calc (m, acc) "" = (acc : m, 0)
    calc (m, acc) line = (m, acc + (read line))
