module Main where

import Control.Monad.State
import Data.Set (Set, insert, singleton)
import Utils

type Point = (Int,Int)
type ParserState = ([Point], Set Point, [Move])

data Move = Move Char Int deriving (Show)
instance Read Move where
    readsPrec _ (c:' ':n) = [(Move c (read n), "")]
    readsPrec _ _ = undefined

origin :: Point
origin = (0,0)

main :: IO ()
main = run $ do
    input <- getInput
    let rope = take 10 (repeat origin)
    let initState = (rope, singleton origin, fmap read $ lines input)
    let ((), (_,ps,_)) = runState runMoves initState
    -- _ <- mapM putStrLn $ take 100 (map show ps)
    return $ length ps
  where
    runMoves :: State ParserState ()
    runMoves = state $ runMoves'

    runMoves' :: ParserState -> ((), ParserState)
    runMoves' s@(_,_,[]) = ((), s)
    runMoves' ([],_,_) = undefined
    runMoves' ((h:ts), ps, move:rest) = runState runMoves newState
      where 
        (h',rest') = moveHead h move rest
        ts' = moveTails h' ts
        newState = ((h':ts'), insert (last ts') ps, rest')

    moveHead :: Point -> Move -> [Move] -> (Point,[Move])
    moveHead h (Move _ 0) rest = (h, rest)
    moveHead (hx,hy) (Move 'U' n) rest = ((hx, hy + 1), (Move 'U' (n-1)) : rest)
    moveHead (hx,hy) (Move 'D' n) rest = ((hx, hy - 1), (Move 'D' (n-1)) : rest)
    moveHead (hx,hy) (Move 'R' n) rest = ((hx + 1, hy), (Move 'R' (n-1)) : rest)
    moveHead (hx,hy) (Move 'L' n) rest = ((hx - 1, hy), (Move 'L' (n-1)) : rest)
    moveHead _ _ _ = undefined

    moveTails :: Point -> [Point] -> [Point]
    moveTails h ts = moveTails' h ts []

    moveTails' :: Point -> [Point] -> [Point] -> [Point]
    moveTails' _ [] ts = ts
    moveTails' h (t:ts) ts' = moveTails' t' ts (ts' ++ [t'])
      where t' = moveTail h t

    moveTail :: Point -> Point -> Point
    moveTail (hx,hy) (tx,ty)
        -- up
        | dx == 0 && dy > 1 = (tx,ty+1)

        -- down
        | dx == 0 && dy < -1 = (tx,ty-1)

        -- right
        | dy == 0 && dx > 1 = (tx+1,ty)

        -- left
        | dy == 0 && dx < -1 = (tx-1,ty)

        -- up right
        | dy > 0 && dx > 1 = (tx+1,ty+1)
        | dx > 0 && dy > 1 = (tx+1,ty+1)
        
        -- up left
        | dy > 0 && dx < -1 = (tx-1,ty+1)
        | dx < 0 && dy > 1 = (tx-1,ty+1)

        -- down right
        | dy < 0 && dx > 1 = (tx+1,ty-1)
        | dx > 0 && dy < -1 = (tx+1,ty-1)
        
        -- down left
        | dy < 0 && dx < -1 = (tx-1,ty-1)
        | dx < 0 && dy < -1 = (tx-1,ty-1)

        | otherwise = (tx,ty)
      where
        dx = hx - tx
        dy = hy - ty


