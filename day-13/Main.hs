module Main where

import Data.List (sort)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

data Elem = I Int | L [Elem] | End deriving (Show, Eq)

instance {-# OVERLAPPING #-} Ord [Elem] where
    (<=) = isSorted

data Sig = Break Bool | Continue

main :: IO ()
main =
    run $
        Config
            { parser = concat <$> sepEndBy (endBy packet space) newline
            , run1 = putShowLn . part1
            , run2 = putShowLn . part2
            }
  where
    packet :: Parser [Elem]
    packet = do
        el <- list
        case el of
            L xs -> return xs
            _ -> fail "expected a list"

    list :: Parser Elem
    list = L <$> between (char '[') (char ']') (sepBy (int <|> list) (char ','))

    int :: Parser Elem
    int = I <$> integer

part1 :: [[Elem]] -> Int
part1 = sum . map fst . filter (\(_, (p1, p2)) -> isSorted p1 p2) . zip [1 ..] . pairs
  where
    pairs :: [[Elem]] -> [([Elem], [Elem])]
    pairs (x : y : xs) = (x, y) : pairs xs
    pairs _ = []

part2 :: [[Elem]] -> Int
part2 = product . map fst . findDistressPackets . zip [1 ..] . sort . (++ distressPackets)
  where
    distressPackets = [[I 2], [I 6]]
    findDistressPackets = (filter (\(_, p) -> p `elem` distressPackets))

isSorted :: [Elem] -> [Elem] -> Bool
isSorted ls rs = case sig of
    Break b -> b
    Continue -> True
  where
    sig = orderedL ls rs

orderedL :: [Elem] -> [Elem] -> Sig
orderedL ls rs = foldl orderedL' Continue $ zip (ls ++ [End]) (rs ++ [End])
  where
    orderedL' :: Sig -> (Elem, Elem) -> Sig
    orderedL' Continue (End, End) = Continue
    orderedL' Continue (_, End) = Break False
    orderedL' Continue (End, _) = Break True
    orderedL' sig@(Break _) _ = sig
    orderedL' Continue pair = orderedP pair

    orderedP :: (Elem, Elem) -> Sig
    orderedP (I left, I right)
        | left == right = Continue
        | otherwise = Break (left < right)
    orderedP (left@(I _), right@(L _)) = orderedP ((L [left]), right)
    orderedP (left@(L _), right@(I _)) = orderedP (left, (L [right]))
    orderedP (L ls', L rs') = orderedL ls' rs'
    orderedP (End, End) = Continue
    orderedP (_, End) = Break False
    orderedP (End, _) = Break True
