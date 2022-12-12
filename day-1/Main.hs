module Main where

import Data.List (sort)
import Text.Megaparsec (endBy, sepEndBy, some)
import Text.Megaparsec.Char (newline)
import Utils (Config (..), integer, run)

main :: IO ()
main =
    run $
        Config
            { parser = fmap (map sum) (sepEndBy (endBy integer newline) (some newline))
            , run1 = maximum
            , run2 = part2
            }

part2 :: [Int] -> Int
part2 cals = sum $ take 3 $ (reverse . sort) cals
