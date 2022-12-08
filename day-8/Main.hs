module Main where

import Utils

main :: IO ()
main = run $ do
    input <- getInput
    return input
