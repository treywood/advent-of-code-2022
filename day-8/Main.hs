module Main where

import Data.List (singleton, transpose)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils 

type Grid = [([Int],[Int])]

main :: IO ()
main =
    run $
        Config
            { parser = do
                rows :: [[Int]] <- sepEndBy ((map (read . singleton) <$> (some digitChar))) newline
                let cols = transpose rows
                return [(r, c) | r <- rows, c <- cols]
            , run1 = putShowLn . part1
            , run2 = putShowLn . part2
            }

part1 :: Grid -> Int
part1 = length . (filter visible) . (zip [0 ..])
  where
    visible :: (Int, ([Int], [Int])) -> Bool
    visible (i, (row, col)) = vis_n || vis_e || vis_s || vis_w
      where
        (col_i, row_i) = i `divMod` (length row)
        me = row !! row_i
        vis_n = all (< me) $ reverse (take col_i col)
        vis_e = all (< me) $ drop (row_i + 1) row
        vis_s = all (< me) $ drop (col_i + 1) col
        vis_w = all (< me) $ reverse (take row_i row)

part2 :: Grid -> Int
part2 = maximum . (map scenicScore) . (zip [0..])
    where
        scenicScore :: (Int, ([Int], [Int])) -> Int
        scenicScore (i, (row, col)) = n * e * s * w
          where
            (col_i, row_i) = i `divMod` (length row)
            me = row !! row_i
            n = score $ reverse (take col_i col)
            s = score $ drop (col_i + 1) col
            w = score $ reverse (take row_i row)
            e = score $ (drop (row_i + 1) row)

            score :: [Int] -> Int
            score xs = case (break (>= me) xs) of
                (seen, []) -> length seen
                (seen, _) -> (length seen) + 1
