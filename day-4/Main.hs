module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

data Range = Range Int Int deriving (Show)

count :: (a -> Bool) -> [a] -> Int
count p = sum . (map (\el -> if (p el) then 1 else 0))

main :: IO ()
main =
    run $
        Config
            { parser = (sepEndBy parseRanges newline)
            , run1 = putShowLn . length . (filter overlaps)
            , run2 = putShowLn . length . (filter covers)
            }
  where
    parseRanges = do
        r1 <- parseRange
        _ <- char ','
        r2 <- parseRange
        return (r1, r2)
    parseRange = do
        n1 <- integer
        _ <- char '-'
        n2 <- integer
        return $ Range n1 n2

overlaps :: (Range, Range) -> Bool
overlaps (Range s1 e1, Range s2 e2) = (s1 >= s2 && e1 <= e2) || (s2 >= s1 && e2 <= e1)

covers :: (Range, Range) -> Bool
covers (Range s1 e1, Range s2 e2) = (s1 <= e2 && s1 >= s2) || (s2 <= e1 && s2 >= s1)
