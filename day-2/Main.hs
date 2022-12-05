module Main where

import Data.Maybe (mapMaybe)
import Utils (getInput, run)

data Move = Rock | Paper | Scissors
data Outcome = Lose | Draw | Win

instance Read Move where
    readsPrec _ "A" = [(Rock, "")]
    readsPrec _ "B" = [(Paper, "")]
    readsPrec _ "C" = [(Scissors, "")]
    readsPrec _ _ = undefined

instance Read Outcome where
    readsPrec _ "X" = [(Lose, "")]
    readsPrec _ "Y" = [(Draw, "")]
    readsPrec _ "Z" = [(Win, "")]
    readsPrec _ _ = undefined

main :: IO ()
main = run $ do
    input <- getInput
    return $ tabulate (lines input)
  where
    tabulate :: [String] -> Int
    tabulate = sum . (map grade) . (mapMaybe parse)

    parse :: String -> Maybe (Move, Outcome)
    parse line = case (words line) of
        [w1, w2] -> Just (read w1, read w2)
        _ -> Nothing

    grade :: (Move, Outcome) -> Int
    grade (Rock, Lose) = 3 + 0
    grade (Paper, Lose) = 1 + 0
    grade (Scissors, Lose) = 2 + 0
    grade (Rock, Draw) = 1 + 3
    grade (Paper, Draw) = 2 + 3
    grade (Scissors, Draw) = 3 + 3
    grade (Rock, Win) = 2 + 6
    grade (Paper, Win) = 3 + 6
    grade (Scissors, Win) = 1 + 6
