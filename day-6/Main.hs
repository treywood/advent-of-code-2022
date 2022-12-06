module Main where

import Utils

main :: IO ()
main = run $ do
    input <- getInput
    return (startOfPacketMarker input 14 0)
  where
    startOfPacketMarker :: String -> Int -> Int -> Int
    startOfPacketMarker str len i
        | allDiff (take len str) = i + len
        | otherwise = startOfPacketMarker (tail str) len (i + 1)

    allDiff :: (Eq a) => [a] -> Bool
    allDiff [] = True
    allDiff [_] = True
    allDiff (x : xs)
        | x `elem` xs = False
        | otherwise = allDiff xs
