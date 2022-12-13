module Main where

import Data.List (elemIndex, intersect)
import Data.List.Split (chunksOf)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

priorities :: [Char]
priorities = ['a' .. 'z'] ++ ['A' .. 'Z']

main :: IO ()
main =
    run $
        Config
            { parser = sepEndBy (some alphaNumChar) newline
            , run1 = putShowLn . part1
            , run2 = putShowLn . part2
            }

part1 :: [String] -> Int
part1 = sum . (map grade)
  where
    grade :: String -> Int
    grade pack = ((maybe 0 (+ 1)) . (`elemIndex` priorities)) $ head (intersect h1 h2)
      where
        mid = (length pack) `div` 2
        (h1, h2) = (take mid pack, drop mid pack)

part2 :: [String] -> Int
part2 = sum . (map grade) . (chunksOf 3)
  where
    grade :: [String] -> Int
    grade (g1 : gs) = (maybe 0 (+ 1) . (`elemIndex` priorities)) badge
      where
        badge = (last . map head) $ scanl intersect g1 gs
    grade _ = 0
