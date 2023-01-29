{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternGuards #-}

module Results
    ( Sailor
    , mkSailor
    , sailorName
    , Ranking(..)
    , Race(..)
    , raceFinishers
    , raceRanks
    , raceParticipants
    , racesParticipants
    , readRace
    , readRaces
    , readSeries
    ) where

import Control.Applicative
import Data.List
import Data.Tuple
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory
import System.FilePath

newtype Sailor = Sailor { sailorName :: T.Text }
    deriving (Eq, Ord, Show)

mkSailor :: T.Text -> Sailor
mkSailor = Sailor . T.toLower . T.strip

newtype Ranking a = Ranking { getRanking :: [a] }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via ZipList

data Race = Race { raceRanking :: Ranking Sailor
                 , raceRCs :: S.Set Sailor
                 , raceDNFs :: S.Set Sailor
                 }
    deriving (Show)

raceFinishers :: Race -> S.Set Sailor
raceFinishers = S.fromList . getRanking . raceRanking

raceRanks :: Race -> M.Map Sailor Int
raceRanks race =
    M.fromList $ zip (getRanking $ raceRanking race) [1..]

raceParticipants :: Race -> S.Set Sailor
raceParticipants r = raceFinishers r <> raceRCs r <> raceDNFs r

racesParticipants :: [Race] -> S.Set Sailor
racesParticipants = foldMap raceParticipants

parseRace :: T.Text -> Race
parseRace t
  | Just rest <- "RC:" `T.stripPrefix` rcLine
  = Race { raceRCs = S.fromList $ map mkSailor $ T.splitOn "," rest
         , raceRanking = Ranking
            [ mkSailor l
            | l <- ls
            , not $ T.null l
            , not $ "dnf:" `T.isPrefixOf` T.toLower l
            ]
         , raceDNFs = S.fromList
            [ mkSailor l
            | l <- ls
            , "dnf:" `T.isPrefixOf` T.toLower l
            ]
         }
  | otherwise
  = error "parseRace: invalid"
  where rcLine:ls = T.lines t

readRace :: FilePath -> IO Race
readRace fname = parseRace <$> T.readFile fname

-- | Read a directory of race results
readRaces :: FilePath -> IO [Race]
readRaces dir = do
    raceFiles <- listDirectory dir
    mapM (readRace . (dir </>)) (sort raceFiles)

readSeries :: FilePath -> IO (M.Map String [Race])
readSeries resultsDir = do
    dirs <- listDirectory resultsDir
    fmap M.fromList $ flip mapM dirs $ \dir -> do
        results <- readRaces (resultsDir </> dir)
        return (dir, results)
