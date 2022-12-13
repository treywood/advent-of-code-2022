module Main where

import Data.Char (isSpace)
import Data.List (transpose)
import Text.Megaparsec (between, getInput, sepEndBy, setInput, some)
import Text.Megaparsec.Char (alphaNumChar, char, newline, space, string)
import Utils

data Move = Move Int Int Int deriving (Show)
data Input = Input [String] [Move] deriving (Show)

main :: IO ()
main =
    run $
        Config
            { parser = parseInput
            , run1 = putShowLn . part1
            , run2 = putShowLn . part2
            }
  where
    parseInput :: Parser Input
    parseInput = do
        input <- Text.Megaparsec.getInput
        let (stackStrs, moveStrs) = break (all isSpace) (lines input)

        setInput $ (unlines . transpose . init) stackStrs
        stacks <- space >> sepEndBy parseStack space

        setInput $ (unlines . tail) moveStrs
        moves <- sepEndBy parseMove newline

        return $ Input stacks moves

    parseStack :: Parser String
    parseStack =
        between
            (space >> some (char '[') >> newline >> space)
            (space >> some (char ']'))
            (some alphaNumChar)

    parseMove :: Parser Move
    parseMove = do
        _ <- string "move "
        n1 <- integer
        _ <- string " from "
        n2 <- integer
        _ <- string " to "
        n3 <- integer
        return $ Move n1 n2 n3

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
