{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}

module Results
    ( Sailor
    , mkSailor
    , sailorName
    , Ranking(..)
    , appendRanking
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

appendRanking :: Ranking a -> Ranking a -> Ranking a
appendRanking (Ranking a) (Ranking b) = Ranking (a <> b)

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
parseRace = foldl' f race0 . T.lines
  where
    race0 = Race (Ranking mempty) mempty mempty
    f r l
      | Just rest <- "rc:" `T.stripPrefix` l'
      = r { raceRCs = raceRCs r <> S.fromList (parseSailors rest) }

      | Just rest <- "dnf:" `T.stripPrefix` l'
      = r { raceDNFs = raceDNFs r <> S.fromList (parseSailors rest) }

      | otherwise
      = r { raceRanking = raceRanking r `appendRanking` Ranking (parseSailors l') }
      where
        l' = T.toLower l

        parseSailors :: T.Text -> [Sailor]
        parseSailors = map mkSailor
                     . filter (not . T.null)
                     . map T.strip
                     . T.splitOn ","

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

readIfExists :: FilePath -> IO (Maybe String)
readIfExists fname = do
    exists <- doesFileExist fname
    if exists
      then Just <$> readFile fname
      else return Nothing

readDropouts :: FilePath -> IO Int
readDropouts dir = do
    maybe 0 read <$> readIfExists file
  where file = dir </> "dropouts"

-- | A set of races.
data Races = Races { races :: [Race]
                   , notes :: String
                   , dropouts :: Int
                   }
    deriving (Show)

-- | Read a directory of race results
readRaces :: FilePath -> IO Races
readRaces dir = do
    raceFiles <- listDirectory dir
    dropouts <- readDropouts dir
    notes <- maybe "" id <$> readIfExists (dir </> "notes")
    races <- mapM (readRace . (dir </>))
        $ filter (`notElem` specialFiles) (sort raceFiles)
    return $ Races { notes, races, dropouts }
  where
    specialFiles = ["dropouts", "notes"]

readSeries :: FilePath -> IO (M.Map String Races)
readSeries resultsDir = do
    dirs <- listDirectory resultsDir
    fmap M.fromList $ flip mapM dirs $ \dir -> do
        results <- readRaces (resultsDir </> dir)
        return (dir, results)
