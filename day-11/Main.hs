{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Char (isSpace)
import Data.List.Split (endBy, splitOn, splitWhen)
import Data.Map (Map, fromList)
import Data.Maybe (mapMaybe)
import Data.Void
import Text.Megaparsec (Parsec, oneOf, optional, runParser, some)
import Text.Megaparsec.Char (alphaNumChar, space, string)
import Utils

data Monkey = Monkey
    { items :: [Int]
    , operation :: Expr
    , test :: (Int, Int, Int)
    }
    deriving (Show)

data Expr = Old | Num Int | Bin Expr Char Expr
    deriving (Show)

parseExpr :: Parsec Void String Expr
parseExpr = do
    _ <- optional $ string "Operation: new = "
    w1 <- fmap wordToExpr (some alphaNumChar)
    space
    op <- oneOf ['*', '+', '-']
    space
    w2 <- fmap wordToExpr (some alphaNumChar)
    return $ Bin w1 op w2
  where
    wordToExpr "old" = Old
    wordToExpr x = Num (read x)

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

type Monkeys = Map Int Monkey

parseMonkey :: [String] -> Maybe Monkey
parseMonkey [itemsStr, opStr, testStr, trueStr, falseStr] = Just $ Monkey items expr test
  where
    items = (map read) . (endBy ", ") . last . (splitOn ": ") $ itemsStr
    test = (den, t, f)
    den = read . last . (splitOn "divisible by ") $ testStr
    t = read . last . (splitOn "throw to monkey ") $ trueStr
    f = read . last . (splitOn "throw to monkey ") $ falseStr
    expr = case (runParser parseExpr "" opStr) of
        Right e -> e
        Left _ -> undefined

-- operation =
parseMonkey _ = Nothing

parseMonkeys :: [String] -> Monkeys
parseMonkeys ls = fromList (zip [0 ..] monkeys)
  where
    monkeys =
        (mapMaybe parseMonkey)
            . (map $ (map $ dropWhile isSpace) . (drop 1))
            . (splitWhen (all isSpace))
            $ ls

main :: IO ()
main = run $ do
    input <- getInput
    return $ parseMonkeys (lines input)
