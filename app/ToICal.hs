{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
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

main :: IO ()
main = do
    let fname = ""
    r <- Csv.decode NoHeader <$> BL.readFile fname
    rows <- either fail (return . V.toList) r
    tz <- loadSystemTZ "America/New_York"
    writeFile "out.vcs" $ toVCal
        [ Event { eventSummary = "Race " ++ show (rowRace r)
                , eventStart = localTimeToUTCTZ tz $ LocalTime ZonedTime (rowMeetingTime r) 
                }
        | r <- rows
        ]
