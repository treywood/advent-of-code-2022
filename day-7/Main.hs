module Main where

import Control.Monad.State
import Data.List (intercalate, isPrefixOf)
import Data.Map.Lazy as M
import Data.Maybe (fromMaybe, fromJust)
import Utils

data Entry = Dir String | File String Int deriving (Show)

type FileSystem = Map String [Entry]
type ParserState = ([String], FileSystem, [String])

processLines :: State ParserState ()
processLines = state $ processLines'
  where
    processLines' :: ParserState -> ((), ParserState)
    processLines' s@(_, _, []) = ((), s)
    processLines' (cwd, fs, line : rest) =
        let
            cwdStr = intercalate "/" (reverse cwd)
            newState = case (words line) of
                ("$" : "cd" : ".." : _) -> (tail cwd, fs, rest)
                ("$" : "cd" : dir : _) ->
                    let
                        path = cwdStr ++ "/" ++ dir
                    in
                        (dir : cwd, insertWith (\_ v -> v) path [] fs, rest)
                ("$" : "ls" : _) ->
                    let
                        (entries, (_, _, rest')) = runState parseEntries (cwd, fs, rest)
                    in
                        (cwd, insert cwdStr entries fs, rest')
                _ -> undefined
        in
            runState processLines newState

    parseEntries :: State ParserState [Entry]
    parseEntries = state $ \(cwd, fs, input) ->
        let
            (entryStrs, rest) = break (isPrefixOf "$") input
            entries = fmap (toEntry cwd) entryStrs
        in
            (entries, (cwd, fs, rest))
      where
        toEntry :: [String] -> String -> Entry
        toEntry cwd str = case (words str) of
            ["dir", dir] -> let
                path = dir : cwd
                fullPath = intercalate "/" (reverse path)
              in
                Dir fullPath
            [sizeStr, file] -> File file (read sizeStr)
            _ -> undefined

main :: IO ()
main = run $ do
    input <- getInput
    let ((), (_, fs, _)) = runState processLines ([], mempty, (lines input))
    let dirSizes = calcSizes fs
    let totalSize = fromJust $ M.lookup "/" dirSizes
    let unusedSize = 70000000 - totalSize
    let sizeToFree = 30000000 - unusedSize
    return (minimum $ M.filter (>= sizeToFree) dirSizes)
  where
    calcSizes :: FileSystem -> Map String Int
    calcSizes fs = M.map (dirSize fs) fs 

    dirSize :: FileSystem -> [Entry] -> Int
    dirSize fs = sum . fmap (entrySize fs)

    entrySize :: FileSystem -> Entry -> Int
    entrySize _ (File _ s) = s
    entrySize fs (Dir name) = fromMaybe 0 (fmap (dirSize fs) (M.lookup name fs))
