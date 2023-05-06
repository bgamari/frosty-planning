{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternGuards #-}

module Results
    ( Sailor
    , mkSailor
    , sailorName
    , Ranking(..)
    , Race(..)
    , Races(..)
    , raceFinishers
    , raceRanks
    , raceParticipants
    , racesParticipants
    , readRace
    , readRaces
    , readSeries
    ) where

import Control.Applicative
import Control.Exception
import Control.DeepSeq
import Data.List
import Data.Tuple
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory
import System.FilePath
import GHC.Stack
import GHC.Generics

newtype Sailor = Sailor { sailorName :: T.Text }
    deriving (Eq, Ord, Show)
    deriving newtype (NFData)

mkSailor :: HasCallStack => T.Text -> Sailor
mkSailor name
  | T.null name' = error "null sailor name"
  | otherwise    =  Sailor name'
  where name' = T.toLower $ T.strip name

newtype Ranking a = Ranking { getRanking :: [a] }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
    deriving newtype (NFData)
    deriving Applicative via ZipList

data Race = Race { raceRanking :: Ranking Sailor
                 , raceRCs :: S.Set Sailor
                 , raceDNFs :: S.Set Sailor
                 }
    deriving (Show, Generic, NFData)

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
  = Race { raceRCs = S.fromList $ map mkSailor
                     $ filter (not . T.null)
                     $ T.splitOn "," $ T.strip rest
         , raceRanking = Ranking
            [ mkSailor l
            | l <- ls
            , not $ T.null l
            , not $ "dnf:" `T.isPrefixOf` T.toLower l
            ]
         , raceDNFs = S.fromList
            [ mkSailor name
            | l <- ls
            , not $ T.null l
            , Just name <- pure $ "dnf:" `T.stripPrefix` T.toLower l
            ]
         }
  | otherwise
  = error "parseRace: invalid"
  where rcLine:ls = T.lines t

data RaceParseError = RaceParseError { rpeException :: SomeException
                                     , rpeFile     :: FilePath
                                     }
    deriving (Show)

instance Exception RaceParseError

readRace :: FilePath -> IO Race
readRace fname = handle onError $ do
    r <- parseRace <$> T.readFile fname
    return $! force r
  where
    onError e@(SomeException _) = do
        putStrLn $ "error parsing " ++ fname ++ ": " ++ show e
        throwIO $ RaceParseError e fname

readDropouts :: FilePath -> IO Int
readDropouts dir = do
    exists <- doesFileExist file
    if exists
      then read <$> readFile file
      else return 0
  where
    file = dir </> "dropouts"

-- | A set of races.
data Races = Races { races :: [Race]
                   , dropouts :: Int
                   }
    deriving (Show)

-- | Read a directory of race results
readRaces :: FilePath -> IO Races
readRaces dir = do
    raceFiles <- listDirectory dir
    dropouts <- readDropouts dir
    races <- mapM (readRace . (dir </>)) $ filter (/= "dropouts") (sort raceFiles)
    return $ Races races dropouts

readSeries :: FilePath -> IO (M.Map String Races)
readSeries resultsDir = do
    dirs <- listDirectory resultsDir
    fmap M.fromList $ flip mapM dirs $ \dir -> do
        results <- readRaces (resultsDir </> dir)
        return (dir, results)
