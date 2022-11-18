{-# LANGUAGE OverloadedStrings #-}

module Results where

import Data.List
import Data.Tuple
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory
import System.FilePath

newtype Sailor = Sailor T.Text
    deriving (Eq, Ord, Show)

mkSailor :: T.Text -> Sailor
mkSailor = Sailor . T.toLower . T.strip

data Race = Race { raceFinishers :: [Sailor]
                 , raceRCs :: [Sailor]
                 }

raceParticipants :: Race -> S.Set Sailor
raceParticipants r = S.fromList $ raceFinishers r <> raceRCs r

racesParticipants :: [Race] -> S.Set Sailor
racesParticipants = foldMap raceParticipants

parseRace :: T.Text -> Race
parseRace t
  | Just rest <- "RC:" `T.stripPrefix` rcLine
  = Race { raceRCs = map mkSailor $ T.splitOn "," rest
         , raceFinishers = map mkSailor $ filter (not . T.null) $ map T.strip ls
         }
  | otherwise
  = error "parseRace: invalid"
  where rcLine:ls = T.lines t

readRace :: FilePath -> IO Race
readRace fname = parseRace <$> T.readFile fname

newtype Scores = Scores { getScores :: M.Map Sailor Int }
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

readScoreDay :: FilePath -> IO Scores
readScoreDay dir = do
    raceFiles <- listDirectory dir
    races <- mapM (readRace . (dir </>)) raceFiles
    let allSailors = foldMap raceParticipants races
    let scores :: [Scores]
        scores = map (scoreRace allSailors) races
    return $ fold scores

main :: IO ()
main = do
    let resultsDir = "results"
    days <- listDirectory resultsDir
    races <- mapM (readScoreDay . (resultsDir </>)) days
    print $ sort $ map swap $ M.toList $ getScores $ fold races
