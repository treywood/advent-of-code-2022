module Main where

import Data.List (transpose, singleton)
import Utils

main :: IO ()
main = run $ do
    input <- getInput
    let rows :: [[Int]] = map (map (read . singleton)) (lines input)
    let cols = transpose rows
    let grid = [(r,c) | r <- rows, c <- cols]
    return $ maximum (map checkTree (zip grid [0..]))
  where
    checkTree :: (([Int],[Int]),Int) -> Int
    checkTree ((row,col),i) = n * e * s * w
      where
        row_i = i `mod` (length row)
        col_i = i `div` (length row)
        me = row !! row_i
        n = count $ reverse (take col_i col)
        s = count $ drop (col_i + 1) col
        w = count $ reverse (take row_i row)
        e = count $ (drop (row_i + 1) row)

        count :: [Int] -> Int
        count xs = case (break (>= me) xs) of
                    (seen, []) -> length seen
                    (seen, _) -> (length seen) + 1
