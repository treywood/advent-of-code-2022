module Main where

import           Data.List          (elemIndex, intersect)
import           Data.List.Split    (chunksOf)
import           System.Environment
import           System.IO

priorities :: [Char]
priorities = ['a' .. 'z'] ++ ['A' .. 'Z']

main :: IO ()
main = do
  [filename] <- getArgs
  handle <- openFile filename ReadMode
  input <- hGetContents handle
  let groups = chunksOf 3 (lines input)
  let ans = sum $ (map prioritize groups)
  putStrLn (show ans)
  where
    prioritize :: [String] -> Int
    prioritize [g1,g2,g3] = score badge
      where
        badge : _ = intersect (intersect g1 g2) g3
        score :: Char -> Int
        score c = maybe 0 (+1) (c `elemIndex` priorities)
