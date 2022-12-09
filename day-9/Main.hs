module Main where

import Control.Monad.State
import Data.List
import Utils

type Point = (Int,Int)
type ParserState = (Point, Point, [(Move,Point,Point)], [Move])

data Move = Move Char Int deriving (Show)
instance Read Move where
    readsPrec _ (c:' ':n) = [(Move c (read n), "")]
    readsPrec _ _ = undefined

origin :: Point
origin = (0,0)

main :: IO ()
main = run $ do
    input <- getInput
    let initState = (origin, origin, [], map read $ lines input)
    let ((), (_,_,ps,_)) = runState process initState
    _ <- mapM putStrLn $ take 100 (map show ps)
    return ()
  where
    process :: State ParserState ()
    process = state $ process'

    process' :: ParserState -> ((), ParserState)
    process' s@(_,_,_,[]) = ((), s)
    process' (h,t,ps,move:rest) =
        let 
            (h',rest') = processLine h move rest
            t' = moveTail h' t
            newState = (h', t', ps ++ [(move,h',t')], rest')
        in
            runState process newState

    processLine :: Point -> Move -> [Move] -> (Point,[Move])
    processLine h (Move _ 0) rest = (h, rest)
    processLine (hx,hy) (Move 'U' 1) rest = ((hx, hy + 1), rest)
    processLine (hx,hy) (Move 'U' n) rest = ((hx, hy + 1), (Move 'U' (n-1)) : rest)
    processLine (hx,hy) (Move 'D' 1) rest = ((hx, hy - 1), rest)
    processLine (hx,hy) (Move 'D' n) rest = ((hx, hy - 1), (Move 'D' (n-1)) : rest)
    processLine (hx,hy) (Move 'R' 1) rest = ((hx + 1, hy), rest)
    processLine (hx,hy) (Move 'R' n) rest = ((hx + 1, hy), (Move 'R' (n-1)) : rest)
    processLine (hx,hy) (Move 'L' 1) rest = ((hx - 1, hy), rest)
    processLine (hx,hy) (Move 'L' n) rest = ((hx - 1, hy), (Move 'L' (n-1)) : rest)
    processLine _ _ _ = undefined

    moveTail :: Point -> Point -> Point
    moveTail (hx,hy) (tx,ty)
        -- up
        | hx == tx && dy > 1 = (tx,ty+1)
        -- down
        | hx == tx && dy < -1 = (tx,ty-1)
        -- right
        | hy == ty && dx > 1 = (tx+1,ty)
        -- left
        | hy == ty && dx < -1 = (tx-1,ty)
        -- up right
        | dy == 1 && dx > 1 = (tx+1,ty+1)
        | dx == 1 && dy > 1 = (tx+1,ty+1)
        -- up left
        | dy == 1 && dx < -1 = (tx-1,ty+1)
        | dx == -1 && dy > 1 = (tx-1,ty+1)
        -- down right
        | dy == -1 && dx > 1 = (tx+1,ty-1)
        | dx == 1 && dy > 1 = (tx+1,ty-1)
        -- down left
        | dy == -1 && dx < -1 = (tx-1,ty-1)
        | dx == -1 && dy > 1 = (tx-1,ty-1)
        | otherwise = (tx,ty)
      where
        dx = hx - tx
        dy = hy - ty


