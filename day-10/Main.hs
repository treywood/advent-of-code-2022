module Main where

import Data.List (singleton)
import Data.List.Split (chunksOf)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

main :: IO ()
main =
    run $
        Config
            { parser = concat <$> sepBy (addx <|> noop) newline
            , run1 = putShowLn . part1
            , run2 = mapM_ putStrLn . part2
            }
  where
    addx = try $ ([id] ++) . singleton . (+) <$> (string "addx " *> integer)
    noop = try $ string "noop" *> return [id]

part1 :: [Int -> Int] -> Int
part1 = foldl signalStrength 0 . zip [1 ..] . scanl (\x f -> f x) 1
  where
    signalStrength :: Int -> (Int, Int) -> Int
    signalStrength acc (i, x)
        | (i - 20) `mod` 40 == 0 = acc + (i * x)
        | otherwise = acc

part2 :: [Int -> Int] -> [String]
part2 =
    chunksOf 40
        . map third
        . drop 1
        . scanl (\x f -> f x) (1, 0, '#')
        . map draw
  where
    draw :: (Int -> Int) -> (Int, Int, Char) -> (Int, Int, Char)
    draw f (x, c, _) = (f x, c + 1, ch)
      where
        p = c `mod` 40
        sprite = [p - 1 .. p + 1]
        ch =
            if x `elem` sprite
                then '#'
                else '.'

    third (_, _, c) = c
