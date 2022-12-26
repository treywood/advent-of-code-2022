{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Function ((&))
import Data.List.Extra
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Map.Internal.Debug
import Data.Set (Set)
import qualified Data.Set as S
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
            , run1 = putStrLn . showTree . distinctPaths . part1
            , run2 = putStrLn . showTree . distinctPaths . part2
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
    , ptail :: [Path]
    }
    deriving (Show)

scorePath :: Path -> Int
scorePath (Path {score, ptail = []}) = score
scorePath (Path {ptail, score, maxScore})
    | score >= maxScore = maximum (map scorePath ptail)
    | otherwise = 0

distinctPaths :: Path -> Map (Set String) Int
distinctPaths = collectPaths mempty mempty
  where
    collectPaths :: Set String -> Map (Set String) Int -> Path -> Map (Set String) Int
    collectPaths path scores (Path {name, score, ptail = []}) = M.insert (S.insert name path) score scores
    collectPaths path scores (Path {name, ptail}) = foldl' (collectPaths (S.insert name path)) scores ptail

part1 :: Valves -> Path
part1 = allPaths (Path "AA" 0 0 30 [])

part2 :: Valves -> Path
part2 = allPaths (Path "AA" 0 0 26 [])

allPaths :: Path -> Valves -> Path
allPaths p@Path {name, score, maxScore, time} valves = p'
  where
    p' = p {ptail = map (\t -> t {maxScore = maxScore'}) ptail'}
    maxScore' = maximum $ map (.score) ptail'
    ptail'
        | time == 0 = []
        | otherwise = map neighborPath $ M.assocs nonZeroLinks

    Valve {links} = valves ! name
    nonZeroLinks = M.filterWithKey (\k _ -> M.findWithDefault False k $ M.map ((> 0) . (.flow)) valves) links

    neighborPath :: (String, Int) -> Path
    neighborPath (nName, nDist) = allPaths (Path nName newScore (max maxScore newScore) nTime []) valves'
      where
        Valve {flow} = valves ! nName
        valves' = M.adjust (\v -> v {flow = 0}) nName valves
        newScore = score + nScore
        (nScore, nTime)
            | flow == 0 = (0, nDist)
            | otherwise = (flow * (time - nDist - 1), time - nDist - 1)
