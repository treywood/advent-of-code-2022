module Main where

import Data.List (intercalate)
import Data.Map.Lazy as M
import Data.Maybe (fromJust, fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils (Config (..), InputParser, integer, run)

data Entry = Dir String | File String Int deriving (Show)

type FileSystem = Map String [Entry]
type Path = [String]

main :: IO ()
main =
    run $
        Config
            { parser = filesystemParser
            , run1 = part1
            , run2 = part2
            }
  where
    filesystemParser :: InputParser FileSystem
    filesystemParser = do
        files <- some $ parseCommands []
        return $
            Prelude.foldl
                (\m (k, v) -> M.insertWith (++) k v m)
                M.empty
                (concat files)

    parseCommands :: Path -> InputParser [(String, [Entry])]
    parseCommands cwd = do
        entries <- someTill ((cd cwd) <|> (ls cwd)) eof
        return (concat entries)

    cd :: Path -> InputParser [(String, [Entry])]
    cd cwd = try $ do
        _ <- string "$ cd "
        subpath <- some (alphaNumChar <|> oneOf ['/', '.']) <* newline
        let newpath = case subpath of
                ".." -> init cwd
                _ -> cwd ++ [subpath]
        entries <- parseCommands newpath
        return entries

    ls :: Path -> InputParser [(String, [Entry])]
    ls cwd = try $ do
        _ <- string "$ ls" <* newline
        entries <- some (parseFile cwd <|> parseDir cwd)
        return entries

    parseFile :: Path -> InputParser (String, [Entry])
    parseFile cwd = try $ do
        filesize <- integer
        _ <- space
        filename <- some (alphaNumChar <|> char '.') <* newline
        let cwdStr = (intercalate "/" cwd)
        let fullpath = cwdStr ++ "/" ++ filename
        return (cwdStr, [File fullpath filesize])

    parseDir :: Path -> InputParser (String, [Entry])
    parseDir cwd = try $ do
        _ <- string "dir "
        dirname <- some alphaNumChar <* newline
        let cwdStr = (intercalate "/" cwd)
        let fullpath = cwdStr ++ "/" ++ dirname
        return (cwdStr, [Dir fullpath])

part1 :: FileSystem -> Int
part1 = sum . (M.filter (<= 100_000)) . calcSizes

part2 :: FileSystem -> Int
part2 fs = minimum $ (M.filter (>= sizeToFree)) dirSizes
  where
    dirSizes = calcSizes fs
    totalSize = fromJust $ M.lookup "/" dirSizes
    unusedSize = 70_000_000 - totalSize
    sizeToFree = 30_000_000 - unusedSize

calcSizes :: FileSystem -> Map String Int
calcSizes fs = M.map (dirSize fs) fs

dirSize :: FileSystem -> [Entry] -> Int
dirSize fs = sum . fmap (entrySize fs)

entrySize :: FileSystem -> Entry -> Int
entrySize _ (File _ s) = s
entrySize fs (Dir name) = fromMaybe 0 (fmap (dirSize fs) (M.lookup name fs))
