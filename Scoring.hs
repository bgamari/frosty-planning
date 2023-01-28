{-# LANGUAGE TypeApplications #-}

module Scoring where

import Data.Foldable
import Data.Maybe
import Data.List (sort)
import Data.Tuple (swap)
import Data.Coerce

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Results

newtype Points = Points Float
    deriving (Show)

instance Monoid Points where
    mempty = Points 0
instance Semigroup Points where
    Points a <> Points b = Points (a+b)

newtype Scores = Scores { getScores :: M.Map Sailor Points }
    deriving (Show)

instance Monoid Scores where
    mempty = Scores mempty

instance Semigroup Scores where
    Scores a <> Scores b = Scores (M.unionWith (<>) a b)

scoreSeries :: M.Map String [Race] -> Scores
scoreSeries series = series'
  where
    allSailors = foldMap (foldMap raceParticipants) series
    series' :: Scores
    series' = foldMap scoreDay series
      where
        -- Build complete record of race scores, including DNFs and RC duty.
        scoreDay :: [Race] -> Scores
        scoreDay races = Scores $ M.fromList
            [ (sailor, score)
            | race <- races
            , sailor <- S.toList allSailors
            , let ranks :: M.Map Sailor Int
                  ranks = raceRanks race
            , let score :: Points
                  score
                    | Just rank <- M.lookup sailor ranks
                    = Points $ realToFrac rank
                    | sailor `elem` raceRCs race
                    , Just score <- M.lookup sailor avgScores
                    = score
                    | otherwise
                    = dnfRank
            ]
          where
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
