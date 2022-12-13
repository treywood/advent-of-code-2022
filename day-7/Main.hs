module Main where

import Data.List (intercalate)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

data Entry = Dir String | File String Int deriving (Show)

type FileSystem = Map String [Entry]
type Path = [String]

main :: IO ()
main =
    run $
        Config
            { parser = filesystemParser
            , run1 = putShowLn . part1
            , run2 = putShowLn . part2
            }
  where
    filesystemParser :: Parser FileSystem
    filesystemParser = do
        files <- some $ cmds []
        return $
            foldl
                (\m (k, v) -> M.insertWith (++) k v m)
                M.empty
                (concat files)

    cmds :: Path -> Parser [(String, [Entry])]
    cmds cwd = do
        entries <- some ((cd cwd) <|> (ls cwd))
        return (concat entries)

    cd :: Path -> Parser [(String, [Entry])]
    cd cwd = try $ do
        _ <- string "$ cd "
        subpath <- some (alphaNumChar <|> oneOf ['/', '.']) <* newline
        let newpath = case subpath of
                ".." -> init cwd
                _ -> cwd ++ [subpath]
        entries <- cmds newpath
        return entries

    ls :: Path -> Parser [(String, [Entry])]
    ls cwd = try $ do
        _ <- string "$ ls" <* newline
        entries <- some (file cwd <|> dir cwd)
        return entries

    file :: Path -> Parser (String, [Entry])
    file cwd = try $ do
        filesize <- integer <* space
        filename <- some (alphaNumChar <|> char '.') <* newline
        let cwdStr = (intercalate "/" cwd)
        let fullpath = cwdStr ++ "/" ++ filename
        return (cwdStr, [File fullpath filesize])

    dir :: Path -> Parser (String, [Entry])
    dir cwd = try $ do
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
entrySize fs (Dir name) = fromMaybe 0 ((dirSize fs) <$> (M.lookup name fs))
