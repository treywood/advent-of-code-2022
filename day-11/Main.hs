{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Function ((&))
import Data.List (sortOn)
import Data.Map (Map, elems)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

type Monkeys = Map Int Monkey

data Monkey = Monkey
    { index :: Int
    , items :: [Int]
    , operation :: Expr
    , divisor :: Int
    , nexts :: (Int, Int)
    , activity :: Int
    }
    deriving (Show)

data Expr = Old | Num Int | Bin Expr Char Expr
    deriving (Show)

runExpr :: Expr -> Int -> Int
runExpr Old x = x
runExpr (Num y) _ = y
runExpr (Bin e1 op e2) x = case op of
    '*' -> ev1 * ev2
    '+' -> ev1 + ev2
    '-' -> ev1 + ev2
    _ -> undefined
  where
    ev1 = (runExpr e1) x
    ev2 = (runExpr e2) x

main :: IO ()
main =
    run $
        Config
            { parser = (M.fromList . zip [0 ..]) <$> sepBy parseMonkey space
            , run1 = putShowLn . monkeyBusiness 20 (`div` 3)
            , run2 = \ms -> putShowLn $ monkeyBusiness 10_000 (crt ms) ms
            }
  where
    parseMonkey :: Parsec Void String Monkey
    parseMonkey = do
        _ <- string "Monkey "
        index <- read <$> (some digitChar)
        string ":" >> space
        items <- parseItems
        operation <- parseExpr
        divisor <- parseDivisor
        nexts <- parseNexts
        return $ Monkey index items operation divisor nexts 0
      where
        parseItems :: Parsec Void String [Int]
        parseItems = do
            space <* string "Starting items:" <* space
            items <- sepBy (read <$> (some digitChar)) (string "," >> space)
            return items

        parseExpr :: Parsec Void String Expr
        parseExpr = do
            space <* string "Operation: new =" <* space
            w1 <- wordToExpr <$> (some alphaNumChar) <* space
            op <- oneOf ['*', '+', '-'] <* space
            w2 <- wordToExpr <$> (some alphaNumChar)
            return $ Bin w1 op w2
          where
            wordToExpr "old" = Old
            wordToExpr x = Num (read x)

        parseDivisor :: Parsec Void String Int
        parseDivisor = do
            space <* string "Test: divisible by" <* space
            den <- read <$> (some digitChar)
            return den

        parseNexts :: Parsec Void String (Int, Int)
        parseNexts = do
            space <* string "If true: throw to monkey" <* space
            ifTrue <- read <$> (some digitChar)
            space <* string "If false: throw to monkey" <* space
            ifFalse <- read <$> (some digitChar)
            return (ifTrue, ifFalse)

    divisors :: Monkeys -> [Int]
    divisors = (map divisor) . elems

    crt monkeys x = x `mod` (product (divisors monkeys))

monkeyBusiness :: Int -> (Int -> Int) -> Monkeys -> Int
monkeyBusiness n fn monkeys = product . (take 2) . (sortOn negate) $ (map activity monkeys')
  where
    monkeys' = elems $ runRounds fn n monkeys

runRounds :: (Int -> Int) -> Int -> Monkeys -> Monkeys
runRounds f n monkeys = (iterate (runRound f) monkeys) !! n

runRound :: (Int -> Int) -> Monkeys -> Monkeys
runRound f ms = foldl runMonkey ms (M.keys ms)
  where
    runMonkey :: Monkeys -> Int -> Monkeys
    runMonkey monkeys i =
        fromMaybe monkeys (update <$> M.lookup i monkeys)
      where
        update m =
            (foldl (processItem m) monkeys m.items)
                & (M.insert m.index updatedMonkey)
          where
            updatedMonkey = m {items = [], activity = m.activity + (length m.items)}

    processItem :: Monkey -> Monkeys -> Int -> Monkeys
    processItem monkey monkeys item = M.update updateTarget tnum monkeys
      where
        newItem = f (runExpr monkey.operation item)
        tnum =
            if newItem `mod` monkey.divisor == 0
                then fst monkey.nexts
                else snd monkey.nexts

        updateTarget :: Monkey -> Maybe Monkey
        updateTarget m = Just $ m {items = m.items ++ [newItem]}
