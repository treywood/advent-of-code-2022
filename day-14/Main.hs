module Main where

import Data.Function ((&))
import Data.List (transpose)
import Data.List.Extra (groupOn, maximumOn, minimumOn, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Point = (Int, Int)
type Grid = Map Point Char

main :: IO ()
main =
    run $
        Config
            { parser =
                (foldl updateGrid (M.singleton (500, 0) '+'))
                    <$> sepEndBy (sepBy point (string " -> ")) newline
            , run1 = putShowLn . countSand . part1
            , run2 = putShowLn . countSand . part2
            }
  where
    point :: Parser Point
    point = (,) <$> integer <* char ',' <*> integer

    updateGrid :: Grid -> [Point] -> Grid
    updateGrid grid pts = foldl addLine grid pairs
      where
        pairs = zip pts (drop 1 pts)

        addLine :: Grid -> (Point, Point) -> Grid
        addLine grid' (p1, p2) =
            foldl (\g p -> M.insert p '#' g) grid' (path p1 p2)
          where
            path :: Point -> Point -> [Point]
            path (x1, y1) (x2, y2)
                | x1 == x2 && y1 < y2 = map (x1,) [y1 .. y2]
                | x1 == x2 && y1 > y2 = map (x1,) [y2 .. y1]
                | y1 == y2 && x1 < x2 = map (,y1) [x1 .. x2]
                | y1 == y2 && x1 > x2 = map (,y1) [x2 .. x1]
                | otherwise = []

part1 :: Grid -> Grid
part1 = sandDrop abyss
  where
    abyss = seq

part2 :: Grid -> Grid
part2 grid = sandDrop infiniteFloor grid
  where
    pts = M.keys grid
    fy = (snd (maximumOn snd pts)) + 2

    infiniteFloor :: Int -> Grid -> Grid
    infiniteFloor x g =
        (foldr (\p g' -> M.insert p '#' g') g floorPts)
            & M.insert (x, fy - 1) 'o'
            & sandDrop infiniteFloor
      where
        floorPts = [(x', fy) | x' <- [x - 2 .. x + 2]]

countSand :: Grid -> Int
countSand = length . M.filter (== 'o')

sandDrop :: (Int -> Grid -> Grid) -> Grid -> Grid
sandDrop floorFn = sandDrop' [(500, 0)]
  where
    sandDrop' :: [Point] -> Grid -> Grid
    sandDrop' [] grid = grid
    sandDrop' path@(p@(px, py) : ps) grid = maybe (floorFn px grid) handleLanding landing
      where
        pts = M.keys grid
        landing = listToMaybe $ sortOn snd $ filter findLanding pts
        findLanding (x, y) = x == px && y > py
        handleLanding (lx, ly)
            | M.notMember left grid = sandDrop' (left : path) grid
            | M.notMember right grid = sandDrop' (right : path) grid
            | p == p' = sandDrop' ps $ M.insert p' 'o' grid
            | otherwise = sandDrop' path $ M.insert p' 'o' grid
          where
            left = (lx - 1, ly)
            right = (lx + 1, ly)
            p' = (lx, ly - 1)

printGrid :: Grid -> IO ()
printGrid grid = mapM_ putShowLn $ (map $ map (\p -> M.findWithDefault ' ' p grid)) rows
  where
    pts = M.keys grid
    (x1, x2) = (fst $ minimumOn fst pts, fst $ maximumOn fst pts)
    (y1, y2) = (snd $ minimumOn snd pts, snd $ maximumOn snd pts)
    rows = transpose $ groupOn fst [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
