module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Utils (Config (..), run)

main :: IO ()
main =
    run $
        Config
            { parser = sepEndBy pairs newline
            , run1 = part1
            , run2 = part2
            }
  where
    pairs = do
        a <- alphaNumChar
        space
        b <- alphaNumChar
        return (a, b)

{--
 A: Rock
 B: Paper
 C: Scissors

 X: Rock
 Y: Paper
 Z: Scissors
 --}
part1 :: [(Char, Char)] -> Int
part1 = sum . (map grade)
  where
    grade :: (Char, Char) -> Int
    grade ('A', 'X') = 1 + 3
    grade ('A', 'Y') = 2 + 6
    grade ('A', 'Z') = 3 + 0
    grade ('B', 'X') = 1 + 0
    grade ('B', 'Y') = 2 + 3
    grade ('B', 'Z') = 3 + 6
    grade ('C', 'X') = 1 + 6
    grade ('C', 'Y') = 2 + 0
    grade ('C', 'Z') = 3 + 3
    grade _ = 0

{--
 A: Rock
 B: Paper
 C: Scissors

 X: Lose
 Y: Draw
 Z: Win
 --}
part2 :: [(Char, Char)] -> Int
part2 = sum . (map grade)
  where
    grade :: (Char, Char) -> Int
    grade ('A', 'X') = 3 + 0
    grade ('A', 'Y') = 1 + 3
    grade ('A', 'Z') = 2 + 6
    grade ('B', 'X') = 1 + 0
    grade ('B', 'Y') = 2 + 3
    grade ('B', 'Z') = 3 + 6
    grade ('C', 'X') = 2 + 0
    grade ('C', 'Y') = 3 + 3
    grade ('C', 'Z') = 1 + 6
    grade _ = 0
