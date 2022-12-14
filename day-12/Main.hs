module Main where

import Data.List (sortOn)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Grid = Map Point Int
type Point = (Int, Int)
type PathMap = Map Point Int
type HeightTest = Point -> Point -> Bool

data Input = Input Grid Point Point deriving (Show)

main :: IO ()
main =
    run $
        Config
            { parser = input <$> sepEndBy (some alphaNumChar) newline
            , run1 = part1
            , run2 = part2
            }
  where
    input :: [[Char]] -> Input
    input rows = Input (M.map (fromEnum . fix) grid) start end
      where
        grid =
            M.fromList
                [ (p, c)
                | (c, i) <- zip (concat rows) [0 ..]
                , let p = i `divMod` n
                ]

        fix 'S' = 'a'
        fix 'E' = 'z'
        fix c = c

        start = find 'S'
        end = find 'E'

        n = (length . head) rows
        find v = fst . head $ filter ((== v) . snd) $ M.assocs grid

part1 :: Input -> IO ()
part1 (Input grid start end) = putShowLn $ (! end) $ shortestPaths grid ascend start
  where
    ascend p n = (grid ! n - grid ! p) <= 1

part2 :: Input -> IO ()
part2 (Input grid _ end) = putShowLn $ minimum $ candidates
  where
    candidates = M.elems $ M.filterWithKey isA distances

    distances = shortestPaths grid descend end
    descend p n = (grid ! p - grid ! n) <= 1

    isA :: Point -> Int -> Bool
    isA p _ = grid ! p == fromEnum 'a'

shortestPaths :: Grid -> HeightTest -> Point -> PathMap
shortestPaths grid testHeight = shortestPaths' 0 mempty
  where
    shortestPaths' :: Int -> PathMap -> Point -> PathMap
    shortestPaths' len paths p = foldl (shortestPaths' (len + 1)) paths' ns
      where
        ns = neighbors grid testHeight p (len + 1) paths'
        paths' = M.insertWith min p len paths

neighbors :: Grid -> HeightTest -> Point -> Int -> PathMap -> [Point]
neighbors grid testHeight p@(x, y) len paths =
    sortOn (negate . (grid !)) $
        filter check [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
  where
    check n =
        M.member n grid
            && (maybe True (> len) $ M.lookup n paths)
            && testHeight p n
