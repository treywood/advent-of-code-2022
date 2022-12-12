module Main where

import Data.List (sortOn)
import Text.Megaparsec (endBy, sepEndBy)
import Text.Megaparsec.Char (newline)
import Utils (Config (..), integer, run)

main :: IO ()
main =
    run $
        Config
            { parser = fmap (map sum) (sepEndBy (endBy integer newline) newline)
            , run1 = maximum
            , run2 = part2
            }

part2 :: [Int] -> Int
part2 = sum . take 3 . (sortOn negate)
