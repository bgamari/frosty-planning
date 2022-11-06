{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Foldable
import Data.Time.Format
import Data.Time.Solar
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import Data.Csv as Csv
import GHC.Generics

portsmouth :: Location
portsmouth = Location
    { latitude = 43.06394
    , longitude = -70.76925
    }

firstDay :: Day
firstDay = fromGregorian 2022 11 6

lastDay :: Day
lastDay = fromGregorian 2023 06 1

instance ToField Day where
    toField = BS.pack . show

instance ToField TimeOfDay where
    toField = BS.pack . show

data Row = Row
    { rowDay :: Day
    , rowSunset :: TimeOfDay
    , rowLowTide :: TimeOfDay
    }
    deriving (Show, Eq, Generic, Csv.ToRecord)

data HighLow = High | Low
    deriving (Show, Eq, Ord)

instance FromField HighLow where
    parseField "L" = pure Low
    parseField "H" = pure High
    parseField _   = fail "unknown HighLow"

readTidePredictions :: FilePath -> IO [(LocalTime, HighLow, Double)]
readTidePredictions fname = do
    r <- Csv.decode HasHeader <$> BL.readFile fname
    v <- either fail return r
    return
        [ (t', hl, depth)
        | (t, depth, hl) <- V.toList v
        , let t' = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M" t
        ]

main :: IO ()
main = do
    tides <- readTidePredictions "tides.csv"
    tz <- getCurrentTimeZone
    let highTides :: M.Map Day TimeOfDay
        highTides = M.fromListWith min
            [ (localDay lt, localTimeOfDay lt)
            | (lt, High, depth) <- tides
            , localTimeOfDay lt > TimeOfDay 6 0 0
            ]
    let days = [firstDay, 7 `addDays` firstDay..lastDay]
    let rows = [ Row day (localTimeOfDay sunsetTime) highTime
               | day <- days
               , let zt = ZonedTime (LocalTime day midday) tz
               , let sunsetTime = zonedTimeToLocalTime $ sunset zt portsmouth
               , let highTime = highTides M.! day
               ]
    BL.putStrLn $ Csv.encode rows

