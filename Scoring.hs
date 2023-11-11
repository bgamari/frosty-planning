{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Scoring
    ( Points(..)
    , DroppedOut(..)
    , getScored
    , scoreSeries
    ) where

import Data.Foldable
import Data.Maybe
import Data.List (sort)
import Data.Tuple (swap)
import Data.Coerce
import Data.Bifunctor

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.FilePath

import Control.Monad.Trans.State

import MultiSet
import qualified MultiSet as MS
import Results

newtype Points = Points Float
    deriving (Show, Eq, Ord)

instance Monoid Points where
    mempty = Points 0
instance Semigroup Points where
    Points a <> Points b = Points (a+b)

scoreSeries :: M.Map String Races
            -> M.Map String [M.Map Sailor (DroppedOut Points)]
scoreSeries series = fmap scoreDay series
  where
    allSailors = foldMap (foldMap raceParticipants . races) series
    -- Build complete record of race scores, including DNFs and RC duty.
    scoreDay :: Races -> [M.Map Sailor (DroppedOut Points)]
    scoreDay rs = computeDropouts (dropouts rs) $ fmap scoreRace (races rs)
      where
        scoreRace :: Race -> M.Map Sailor Points
        scoreRace race = M.fromList
            [ (sailor, score)
            | let ranks :: M.Map Sailor Int
                  ranks = raceRanks race
            , sailor <- S.toList allSailors
            , let score
                    | Just rank <- M.lookup sailor ranks
                    = Points $ realToFrac rank
                    | sailor `elem` raceRCs race
                    , Just score <- M.lookup sailor avgScores
                    = score
                    | otherwise
                    = dnfRank
            ]

        scores :: M.Map Sailor [Points]
        scores = M.fromListWith (++)
            [ (sailor, [Points $ realToFrac n])
            | race <- races rs
            , (sailor, n) <- M.toList $ raceRanks race
            ]

        avgScores :: M.Map Sailor Points
        avgScores = fmap meanPoints scores

        dnfRank :: Points
        dnfRank = Points $ realToFrac $ succ $ maximum $ map (length . raceFinishers) (races rs)

computeDropouts :: Int -> [M.Map Sailor Points] -> [M.Map Sailor (DroppedOut Points)]
computeDropouts nDropouts results =
    evalState (traverse (M.traverseWithKey dropoutRace) results) sailorDropouts
  where
    sailorScores :: M.Map Sailor (MultiSet Points)
    sailorScores = M.fromListWith (<>)
        [ (sailor, MultiSet.singleton pts)
        | race <- results
        , (sailor, pts) <- M.toList race
        ]

    sailorDropouts :: M.Map Sailor (MultiSet Points)
    sailorDropouts = fmap (MultiSet.fromList . take nDropouts . reverse . sort . MultiSet.toList) sailorScores

    dropoutRace
        :: Sailor -> Points
        -> State (M.Map Sailor (MultiSet Points)) (DroppedOut Points)
    dropoutRace sailor points = do
        dropoutsEnv <- get
        let dropouts :: MultiSet Points
            Just dropouts = M.lookup sailor dropoutsEnv
        case MultiSet.delete points dropouts of
          Just ms -> do
              put $ M.insert sailor ms dropoutsEnv
              return $ DroppedOut points
          Nothing -> return $ Scored points

data DroppedOut a = Scored a | DroppedOut a
    deriving (Show)

testComputeDropouts :: IO ()
testComputeDropouts = print $ computeDropouts dropouts scores
  where
    dropouts = 1
    scores = map (M.fromList . map (bimap mkSailor Points))
        [ [ ("bob", 1), ("joe", 2), ("frd", 3) ]
        , [ ("bob", 1), ("joe", 3), ("frd", 2) ]
        , [ ("bob", 1), ("joe", 3), ("frd", 2) ]
        ]

getScored :: DroppedOut Points -> Points
getScored = getScored' (Points 0)

getScored' :: a -> DroppedOut a -> a
getScored' z (DroppedOut _) = z
getScored' _ (Scored s) = s

meanPoints :: [Points] -> Points
meanPoints = coerce (mean @Float)

mean :: RealFrac a => [a] -> a
mean xs = sum xs / realToFrac (length xs)

main :: IO ()
main = do
    let season = "2023-2024"
    series <- readSeries ("results" </> season)
    print series
    print $ scoreSeries series
