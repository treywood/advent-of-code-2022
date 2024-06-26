module Main where

import Control.Applicative
import Data.List (nub)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Point = (Int, Int)

origin :: Point
origin = (0, 0)

main :: IO ()
main =
    run $
        Config
            { parser = concat <$> sepBy moves newline
            , run1 = putShowLn . solution [origin, origin]
            , run2 = putShowLn . solution (replicate 10 origin)
            }
  where
    moves = (oneOf ['U', 'D', 'L', 'R'] <* space) <**> (replicate <$> integer)

solution :: [Point] -> [Char] -> Int
solution rope = length . nub . snd . runMoves rope

runMoves :: [Point] -> [Char] -> ([Point], [Point])
runMoves rope = foldl move (rope, [last rope])
  where
    move (h : ts, ps) m = (rope', last rope' : ps)
      where
        h' = moveHead h m
        rope' = scanl moveTail h' ts
    move _ _ = undefined

    moveHead :: Point -> Char -> Point
    moveHead (hx, hy) 'U' = (hx, hy + 1)
    moveHead (hx, hy) 'D' = (hx, hy - 1)
    moveHead (hx, hy) 'R' = (hx + 1, hy)
    moveHead (hx, hy) 'L' = (hx - 1, hy)
    moveHead _ _ = undefined

    moveTail :: Point -> Point -> Point
    moveTail (hx, hy) (tx, ty)
        -- up
        | dx == 0 && dy > 1 = (tx, ty + 1)
        -- down
        | dx == 0 && dy < -1 = (tx, ty - 1)
        -- right
        | dy == 0 && dx > 1 = (tx + 1, ty)
        -- left
        | dy == 0 && dx < -1 = (tx - 1, ty)
        -- up right
        | dy > 0 && dx > 1 = (tx + 1, ty + 1)
        | dx > 0 && dy > 1 = (tx + 1, ty + 1)
        -- up left
        | dy > 0 && dx < -1 = (tx - 1, ty + 1)
        | dx < 0 && dy > 1 = (tx - 1, ty + 1)
        -- down right
        | dy < 0 && dx > 1 = (tx + 1, ty - 1)
        | dx > 0 && dy < -1 = (tx + 1, ty - 1)
        -- down left
        | dy < 0 && dx < -1 = (tx - 1, ty - 1)
        | dx < 0 && dy < -1 = (tx - 1, ty - 1)
        | otherwise = (tx, ty)
      where
        dx = hx - tx
        dy = hy - ty
