module Main where

import Data.Char (isSpace)
import Data.List (transpose)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

data Move = Move Int Int Int deriving (Show)
data Input = Input [String] [Move] deriving (Show)

main :: IO ()
main =
    run $
        Config
            { parser = input
            , run1 = putShowLn . part1
            , run2 = putShowLn . part2
            }
  where
    input :: Parser Input
    input = do
        (stackStrs, moveStrs) <- (break (all isSpace) . lines) <$> getInput

        setInput $ (unlines . transpose . init) stackStrs
        stacks <- space >> sepEndBy stack space

        setInput $ (unlines . tail) moveStrs
        moves <- sepEndBy move newline

        return $ Input stacks moves

    stack :: Parser String
    stack =
        between
            (space <* some (char '[') <* newline <* space)
            (space <* some (char ']'))
            (some alphaNumChar)

    move :: Parser Move
    move =
        Move
            <$> (string "move " *> integer)
            <*> (string " from " *> integer)
            <*> (string " to " *> integer)

part1 :: Input -> String
part1 (Input stacks moves) = map head $ foldl (runMove reverse) stacks moves

part2 :: Input -> String
part2 (Input stacks moves) = map head $ foldl (runMove id) stacks moves

runMove :: (String -> String) -> [String] -> Move -> [String]
runMove orderFn stacks (Move x from to) = map doMove (zip stacks [1 ..])
  where
    fromStack = stacks !! (from - 1)
    blocks = take x fromStack

    doMove :: (String, Int) -> String
    doMove (stack, i)
        | i == from = drop x stack
        | i == to = (orderFn blocks) ++ stack
        | otherwise = stack
