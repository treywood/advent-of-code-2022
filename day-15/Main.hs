module Main where

import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

data Sensor = Sensor Point Int deriving (Show)
data Box = Box Point Point deriving (Show)

type Point = (Int, Int)

main :: IO ()
main =
    run $
        Config
            { parser = unzip <$> sepEndBy sensorAndBeacon newline
            , run1 = putShowLn . part1 2_000_000
            , run2 = putShowLn . part2 (Box (0, 0) (4_000_000, 4_000_000))
            }
  where
    sensorAndBeacon = try $ do
        sx <- string "Sensor at x=" *> integer
        sy <- string ", y=" *> integer <* string ": "

        bx <- string "closest beacon is at x=" *> integer
        by <- string ", y=" *> integer

        let dist = abs (sx - bx) + abs (sy - by)
        return (Sensor (sx, sy) dist, (bx, by))

part1 :: Int -> ([Sensor], [Point]) -> Int
part1 ty (sensors, beacons) = fst $ foldl' fn (0, S.fromList bxs) sensors
  where
    bxs = map fst $ filter ((== ty) . snd) beacons
    fn :: (Int, Set Int) -> Sensor -> (Int, Set Int)
    fn (acc, set) (Sensor (x, y) d)
        | abs (ty - y) > d = (acc, set)
        | otherwise = (acc + length xs, set')
      where
        set' = foldr S.insert set xs
        xs = [x' | x' <- [x - dx .. x + dx], S.notMember x' set]
        dx = d - (abs (ty - y))

part2 :: Box -> ([Sensor], [Point]) -> Maybe Int
part2 box (sensors, _) = tuningFrequency <$> search box
  where
    tuningFrequency (x, y) = x * 4_000_000 + y

    search :: Box -> Maybe Point
    search b@(Box p1 p2)
        | any (inRange b) sensors = Nothing
        | p1 == p2 = Just p1
        | otherwise = listToMaybe $ catMaybes $ map search (divideBox b)

    inRange :: Box -> Sensor -> Bool
    inRange (Box (x1, y1) (x2, y2)) (Sensor (sx, sy) d) =
        abs (sx - x1) + abs (sy - y1) <= d
            && abs (sx - x2) + abs (sy - y2) <= d
            && abs (sx - x1) + abs (sy - y2) <= d
            && abs (sx - x2) + abs (sy - y1) <= d

    divideBox :: Box -> [Box]
    divideBox (Box (minx, miny) (maxx, maxy)) =
        [ Box (x1, y1) (x2, y2)
        | (x1, x2) <- [(minx, midx), (midx + 1, maxx)]
        , (y1, y2) <- [(miny, midy), (midy + 1, maxy)]
        , y1 <= y2 && x1 <= x2
        ]
      where
        midx = (minx + maxx) `div` 2
        midy = (miny + maxy) `div` 2
