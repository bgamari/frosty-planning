module Scoring where

import Results

newtype Scores = Scores { getScores :: M.Map Sailor Float }
    deriving (Show)

instance Monoid Scores where
    mempty = Scores mempty

instance Semigroup Scores where
    Scores a <> Scores b = Scores (M.unionWith (+) a b)

scoreRace :: S.Set Sailor -> Race -> Scores
scoreRace allSailors race = Scores scores
  where
    n = 1 + length (raceFinishers race)
    dnfs = allSailors `S.difference` raceParticipants race
    scores = 
        M.fromList (zip (raceFinishers race) [1..])
        <> M.fromList (zip (S.toList dnfs) (repeat n))

scoreDay :: FilePath -> IO Scores
scoreDay dir = do
    races <- readRaces dir
    let allSailors = foldMap raceParticipants races
    let scores :: [Scores]
        scores = map (scoreRace allSailors) races
    return $ fold scores

scoreSeries :: M.Map String [Race] -> Scores
scoreSeries series = scores
  where
    allSailors = foldMap (foldMap raceParticipants) series
    series' :: M.Map String [Race] -> Scores
    series' = fmap scoreDay series
      where
        -- Build complete record of race scores, including DNFs and RC duty.
        scoreDay :: [Race] -> Scores
        scoreDay races = M.fromList
            [ (sailor, score)
            | race <- races
            , sailor <- allSailors
            , let finishers :: M.Map Sailor Int
                  finishers = raceRanks race
            , let score
                    | Just rank <- M.lookup sailor raceRanks
                    = rank
                    | sailor `elem` raceRCs race
                    = fromMaybe (error "fixDay") $
                      M.lookup avgScores sailor
                    | otherwise
                    = dnfRank
            ]
          where
            scores :: M.Map Sailor [Int]
            scores = M.fromList (++)
                [ (sailor, n)
                | race <- races
                , (sailor, n) <- zip (raceFinishers race) [1..]
                ]
            avgScores :: M.Map Sailor Float
            avgScores = fmap (mean . fmap realToFrac) scores

            dnfRank = succ $ maximum $ map (length . raceFinishers) races



mean :: RealFrac a => [a] -> a
mean xs = sum xs / realToFrac (length xs)

main :: IO ()
main = do
    readSeries
    print $ sort $ map swap $ M.toList $ getScores $ fold races
