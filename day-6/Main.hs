module Main where

import Text.Megaparsec (some)
import Text.Megaparsec.Char (alphaNumChar)
import Utils (Config (..), run)

main :: IO ()
main =
    run $
        Config
            { parser = some alphaNumChar
            , run1 = startOfPacketMarker 4 0
            , run2 = startOfPacketMarker 14 0
            }

startOfPacketMarker :: Int -> Int -> String -> Int
startOfPacketMarker len i str
    | allDiff (take len str) = i + len
    | otherwise = startOfPacketMarker len (i + 1) (tail str)
  where
    allDiff :: (Eq a) => [a] -> Bool
    allDiff [] = True
    allDiff [_] = True
    allDiff (x : xs)
        | x `elem` xs = False
        | otherwise = allDiff xs
