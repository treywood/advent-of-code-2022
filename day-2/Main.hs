module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

main :: IO ()
main =
    run $
        Config
            { parser = sepEndBy pairs newline
            , run1 = putShowLn . sum . map grade1
            , run2 = putShowLn . sum . map grade2
            }
  where
    pairs = (,) <$> (alphaNumChar <* space) <*> alphaNumChar

{--
 A: Rock
 B: Paper
 C: Scissors

 X: Rock
 Y: Paper
 Z: Scissors
 --}
grade1 :: (Char, Char) -> Int
grade1 ('A', 'X') = 1 + 3
grade1 ('A', 'Y') = 2 + 6
grade1 ('A', 'Z') = 3 + 0
grade1 ('B', 'X') = 1 + 0
grade1 ('B', 'Y') = 2 + 3
grade1 ('B', 'Z') = 3 + 6
grade1 ('C', 'X') = 1 + 6
grade1 ('C', 'Y') = 2 + 0
grade1 ('C', 'Z') = 3 + 3
grade1 _ = 0

{--
 A: Rock
 B: Paper
 C: Scissors

 X: Lose
 Y: Draw
 Z: Win
 --}
grade2 :: (Char, Char) -> Int
grade2 ('A', 'X') = 3 + 0
grade2 ('A', 'Y') = 1 + 3
grade2 ('A', 'Z') = 2 + 6
grade2 ('B', 'X') = 1 + 0
grade2 ('B', 'Y') = 2 + 3
grade2 ('B', 'Z') = 3 + 6
grade2 ('C', 'X') = 2 + 0
grade2 ('C', 'Y') = 3 + 3
grade2 ('C', 'Z') = 1 + 6
grade2 _ = 0
