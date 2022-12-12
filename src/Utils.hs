{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Utils where

import Data.Void
import System.Environment
import System.IO
import Text.Megaparsec (Parsec, ParsecT, errorBundlePretty, runParser, some)
import Text.Megaparsec.Char (digitChar)

type InputParser = Parsec Void String

data Config a o1 o2 = Config
    { parser :: InputParser a
    , run1 :: (a -> o1)
    , run2 :: (a -> o2)
    }

integer :: ParsecT Void String m Int
integer = fmap read (some digitChar)

notImplemented :: a -> String
notImplemented _ = "Not Implemented"

getInput :: IO String
getInput = do
    [filename] <- getArgs
    handle <- openFile filename ReadMode
    input <- hGetContents handle
    return input

run :: (Show o1, Show o2) => Config a o1 o2 -> IO ()
run config = do
    input <- getInput
    case runParser config.parser "" input of
        Right parsedInput -> do
            putStrLn "Part 1:"
            putStrLn $ show $ config.run1 parsedInput
            putStrLn "\nPart 2:"
            putStrLn $ show $ config.run2 parsedInput
        Left err -> do
            putStrLn "Parse Error:"
            putStrLn (errorBundlePretty err)
