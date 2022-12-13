module Main where

import Data.Function (on)
import Data.List.Extra (firstJust)
import Data.List.HT (sieve)
import Data.List (transpose, minimumBy, sortOn)
import Data.Maybe (fromJust, mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Grid = [([Char], [Char])]
type Point = (Int, Int)

data Input = Input Grid Point Point deriving (Show)

main :: IO ()
main =
    run $
        Config
            { parser = inputFromRows <$> sepEndBy (some alphaNumChar) newline
            , run1 = showGrid
            , run2 = \(Input grid start end) -> seek grid end start >>= putShowLn
            }
  where
    inputFromRows :: [String] -> Input
    inputFromRows rows = Input grid start end
      where
        grid = [(r,c) | r <- rows, c <- transpose rows]
        start = findP 'S' grid
        end = findP 'E' grid

        findP :: Char -> Grid -> Point
        findP c = fromJust . firstJust (findP' c) . zip [0..]

        findP' :: Char -> (Int, ([Char],[Char])) -> Maybe Point
        findP' c (i, (row, _)) = if row !! row_i == c
                                    then Just (col_i, row_i)
                                    else Nothing
          where
            (col_i, row_i) = i `divMod` (length row)

seek :: Grid -> Point -> Point -> IO [Point]
seek grid end start = head <$> seek' [] start
  where
    seek' :: [Point] -> Point -> IO [[Point]]
    seek' path p
        | start == end = return [path']
        | otherwise = do
            putStrLn $ (show path')
            paths <- concat <$> mapM (seek' path') ns
            return paths
      where
        path' = path ++ [p]
        ns = neighbors grid p path'

get :: Point -> Grid -> Char
get (x,y) grid = case (!!x) . snd . (!!y) $ grid of
              'S' -> 'a'
              'E' -> 'z'
              c -> c

neighbors :: Grid -> Point -> [Point] -> [Point]
neighbors grid p@(x,y) path = 
    sortOn (negate . fromEnum . (flip get) grid) $
        filter check [(x,y+1),(x-1,y),(x+1,y),(x,y-1)]
  where
    check n = not (n `elem` path) && canGo n
    canGo n = inside n && notTooHigh n

    notTooHigh n = (fromEnum (get n grid) - fromEnum (get p grid)) <= 1

    inside :: Point -> Bool
    inside (x',y') = x' >= 0 && y' >= 0 && x' < (rowsN grid) && y' < (colsN grid)

    colsN :: Grid -> Int
    colsN = length . fst . (!! 0)

    rowsN :: Grid -> Int
    rowsN = length . snd . (!! 0)

showGrid :: Input -> IO ()
showGrid (Input grid@((row, _) : _) _ _) = mapM_ putStrLn (sieve n $ map fst grid)
    where n = length row
showGrid _ = undefined
