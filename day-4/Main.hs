module Main where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Text.Regex (matchRegex, mkRegex)
import Utils (getInput, run)

data Range = Range Int Int deriving (Show)

count :: (a -> Bool) -> [a] -> Int
count p = sum . (map (\el -> if (p el) then 1 else 0))

main :: IO ()
main = run $ do
    input <- getInput
    let pairs = getPairs (lines input)
    return (count doesOverlap pairs)
  where
    getPairs :: [String] -> [(Range, Range)]
    getPairs = catMaybes . (map parsePair) . (map $ splitOn ",")

    parsePair :: [String] -> Maybe (Range, Range)
    parsePair [p1, p2] = (,) <$> parseRange p1 <*> parseRange p2
    parsePair _ = Nothing

    parseRange :: String -> Maybe Range
    parseRange str = case (matchRegex pairRegex str) of
        Just [start, end] -> Just $ Range (read start) (read end)
        _ -> Nothing

    pairRegex = mkRegex "([0-9]+)-([0-9]+)"

    doesOverlap :: (Range, Range) -> Bool
    doesOverlap (Range s1 e1, Range s2 e2) = (s1 <= e2 && s1 >= s2) || (s2 <= e1 && s2 >= s1)
