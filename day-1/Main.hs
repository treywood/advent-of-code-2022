module Main where

import Data.List (sortOn)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

main :: IO ()
main =
    run $
        Config
            { parser = map sum <$> sepEndBy (endBy integer newline) newline
            , run1 = putShowLn . maximum
            , run2 = putShowLn . sum . take 3 . sortOn negate
            }
