{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Function ((&))
import Data.List.Extra
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Map.Internal.Debug
import Data.Maybe (mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

data Valve = Valve
    { flow :: Int
    , links :: Map String Int
    }
    deriving (Show, Eq)
type Valves = Map String Valve

main :: IO ()
main =
    run $
        Config
            { parser = (mapWithDistances . M.fromList) <$> sepEndBy valve newline
            , run1 = putShowLn . scorePath . part1
            , run2 = putStrLn . showTree
            }
  where
    valve = do
        name <- string "Valve " *> some alphaNumChar <* space
        flow <- string "has flow rate=" *> integer <* char ';' <* space
        links <- (M.fromList . (`zip` repeat 1)) <$> (manyNexts <|> singleNext)
        return $ (name, Valve flow links)

    singleNext =
        try $
            singleton <$> (string "tunnel leads to valve " *> some alphaNumChar)

    manyNexts =
        try $
            string "tunnels lead to valves "
                *> sepBy (some alphaNumChar) (char ',' <* space)

    mapWithDistances vs =
        (iterate (\vs' -> M.foldlWithKey' mapDistances vs' vs') vs !! M.size vs)
            & filterNoFlow "AA"
      where
        mapDistances valves name (v@Valve {links}) = valves'
          where
            valves' = M.insert name (v {links = links'}) valves
            links' =
                (map (\k -> M.map (\v' -> (M.findWithDefault 0 k links) + v') (llinks ! k)) lnames)
                    & M.unionsWith min
                    & M.delete name
                    & M.union links

            llinks = M.map (.links) valves
            lnames = M.keys links
        filterNoFlow keep valves = foldr M.delete valves' names
          where
            names = M.keys (M.filterWithKey (\n v -> n /= keep && v.flow == 0) valves)
            valves' = M.map (\v -> v {links = foldr M.delete v.links names}) valves

data Path = Path
    { name :: String
    , score :: Int
    , maxScore :: Int
    , time :: Int
    , tail :: [Path]
    }
    deriving (Show)

countPaths :: Path -> Int
countPaths (Path {tail = []}) = 1
countPaths (Path {tail}) = sum $ map countPaths tail

scorePath :: Path -> Int
scorePath (Path {score, tail = []}) = score
scorePath (Path {tail}) = maximum (map scorePath tail)

part1 :: Valves -> Path
part1 = bestPath (Path "AA" 0 0 30 [])
  where
    bestPath :: Path -> Valves -> Path
    bestPath p@Path {name, score, maxScore, time} valves = p'
      where
        tail'
            | time == 0 = []
            | otherwise = mapMaybe neighborPath $ M.assocs validLinks
        Valve {links} = valves ! name
        validLinks = M.filterWithKey (\k _ -> M.findWithDefault False k $ M.map ((> 0) . (.flow)) valves) links
        maxScore' = maximum $ map (.score) tail'
        p' = p {tail = map (\t -> t {maxScore = maxScore'}) tail'}
        neighborPath (nName, nDist)
            | newScore >= maxScore = Just $ bestPath (Path nName newScore newScore nTime []) valves'
            | otherwise = Nothing
          where
            Valve {flow = nFlow} = valves ! nName
            valves' = M.adjust (\v -> v {flow = 0}) nName valves
            newScore = score + nScore
            (nScore, nTime)
                | nFlow == 0 = (0, nDist)
                | otherwise = (nFlow * (time - nDist - 1), time - nDist - 1)
