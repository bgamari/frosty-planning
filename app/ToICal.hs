{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Converts a CSV of races into an ICal calendar.
module Main where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Zones
import Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import GHC.Generics

import ICal

data Row = Row
    { rowRace :: Int
    , rowDay :: Day
    , rowHighTide :: TimeOfDay
    , rowMeetingTime :: TimeOfDay
    , rowWhere :: String
    }
    deriving (Show, Eq, Generic, Csv.FromRecord)

instance FromField Day where
    parseField =
        return . parseTimeOrError True defaultTimeLocale "%Y-%m-%d" . BS.unpack

instance FromField TimeOfDay where
    parseField =
        return . parseTimeOrError True defaultTimeLocale "%H:%M" . BS.unpack

main :: IO ()
main = do
    r <- Csv.decode NoHeader <$> BL.getContents
    rows <- either fail (return . V.toList) r
    tz <- loadSystemTZ "America/New_York"
    let duration = secondsToNominalDiffTime $ 60*60*2
    writeFile "out.vcs" $ toVCal
        [ Event { eventSummary = "Race " ++ show (rowRace r)
                , eventStart = start
                , eventEnd = addUTCTime duration start
                , eventDescription = "Race " ++ show (rowRace r) ++ " at " ++ rowWhere r
                , eventLocation = rowWhere r
                }
        | r <- rows
        , let start = case localTimeToUTCFull tz $ LocalTime (rowDay r) (rowMeetingTime r)  of
                        LTUNone{}      -> error "LTUNone"
                        LTUUnique r _  -> r
                        LTUAmbiguous{} -> error "LTUAmbiguous"
        ]
