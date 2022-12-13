module Main where

import Data.List.Split
import Utils

data Instr = Noop | Addx Int deriving (Show)

instance Read Instr where
    readsPrec _ str = case (words str) of
        "noop" : _ -> [(Noop, "")]
        ("addx" : x : _) -> [(Addx (read x), "")]
        _ -> undefined

main :: IO ()
main = run $ do
    input :: [Instr] <- ((map read) . lines) <$> getInput
    let xs = scanl (\x f -> f x) (1, 0, '#') (input >>= toFn)
    let crt = map (\(_, _, c) -> c) xs
    mapM putStrLn (chunksOf 40 crt)
  where
    toFn :: Instr -> [(Int, Int, Char) -> (Int, Int, Char)]
    toFn Noop = [draw id]
    toFn (Addx x) = [draw id, draw (+ x)]

    draw :: (Int -> Int) -> (Int, Int, Char) -> (Int, Int, Char)
    draw f (x, c, _) = (f x, c + 1, char)
      where
        p = c `mod` 40
        sprite = [p - 1 .. p + 1]
        char =
            if x `elem` sprite
                then '#'
                else '.'
