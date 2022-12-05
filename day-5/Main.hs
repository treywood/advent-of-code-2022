module Main where

import Data.Char (isAlpha, isSpace)
import Data.List (transpose)
import Data.Maybe (mapMaybe)
import Text.Regex (matchRegex, mkRegex)
import Utils

data Move = Move Int Int Int deriving (Show)

filterNot :: (t -> Bool) -> [t] -> [t]
filterNot p = filter (not . p)

main :: IO ()
main = run $ do
    input <- getInput
    let (stackStrs, moveStrs) = break (all isSpace) (lines input)
    let stacks = parseStacks stackStrs
    let moves = mapMaybe parseMove moveStrs
    let resultStacks = foldl runMove stacks moves
    return $ (map head resultStacks)
  where
    parseStacks :: [String] -> [String]
    parseStacks = (filterNot (all isSpace)) . (map $ (filter isAlpha) . init) . transpose

    moveRegex = mkRegex "move ([0-9]+) from ([0-9]+) to ([0-9]+)"

    parseMove :: String -> Maybe Move
    parseMove line = case (matchRegex moveRegex line) of
        Just [n1, n2, n3] -> Just $ Move (read n1) (read n2) (read n3)
        _ -> Nothing

    runMove :: [String] -> Move -> [String]
    runMove stacks (Move x from to) = map doMove (zip stacks [1 ..])
      where
        fromStack = stacks !! (from - 1)
        blocks = take x fromStack

        doMove :: (String, Int) -> String
        doMove (stack, i)
            | i == from = drop x stack
            | i == to = blocks ++ stack
            | otherwise = stack
