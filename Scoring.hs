{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternGuards #-}

module Scoring
    ( Points(..)
    , scoreSeries
    ) where

import Data.Foldable
import Data.Maybe
import Data.List (sort)
import Data.Tuple (swap)
import Data.Coerce

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Results

newtype Points = Points Float
    deriving (Show, Eq, Ord)

instance Monoid Points where
    mempty = Points 0
instance Semigroup Points where
    Points a <> Points b = Points (a+b)

scoreSeries :: M.Map String [Race]
            -> M.Map String [M.Map Sailor Points]
scoreSeries series = fmap scoreDay series
  where
    allSailors = foldMap (foldMap raceParticipants) series
    -- Build complete record of race scores, including DNFs and RC duty.
    scoreDay :: [Race] -> [M.Map Sailor Points]
    scoreDay races = fmap scoreRace races
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
            | race <- races
            , (sailor, n) <- M.toList $ raceRanks race
            ]

        avgScores :: M.Map Sailor Points
        avgScores = fmap meanPoints scores

        dnfRank :: Points
        dnfRank = Points $ realToFrac $ succ $ maximum $ map (length . raceFinishers) races

meanPoints :: [Points] -> Points
meanPoints = coerce (mean @Float)

mean :: RealFrac a => [a] -> a
mean xs = sum xs / realToFrac (length xs)

main :: IO ()
main = do
    races <- readSeries "results"
    print $ races
    print $ scoreSeries races
