{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Utils where

import Data.Void
import System.Environment
import System.IO
import Text.Megaparsec (Parsec, ParsecT, eof, errorBundlePretty, optional, runParser, some, someTill)
import Text.Megaparsec.Char (char, digitChar, printChar)

type Parser = Parsec Void String

data Config a = Config
    { parser :: Parser a
    , run1 :: (a -> IO ())
    , run2 :: (a -> IO ())
    }

integer :: ParsecT Void String m Int
integer = do
    sign <- optional (char '-')
    num <- read <$> some digitChar
    return $ case sign of
        Just '-' -> negate num
        Nothing -> num

notImplemented :: a -> String
notImplemented _ = "Not Implemented"

putShowLn :: (Show a) => a -> IO ()
putShowLn = putStrLn . show

run :: Config a -> IO ()
run config = do
    [filename] <- getArgs
    handle <- openFile filename ReadMode
    input <- hGetContents handle
    case runParser config.parser "" input of
        Right parsedInput -> do
            putStrLn "Part 1:"
            config.run1 parsedInput
            putStrLn "\nPart 2:"
            config.run2 parsedInput
        Left err -> do
            putStrLn "Parse Error:"
            putStrLn (errorBundlePretty err)
