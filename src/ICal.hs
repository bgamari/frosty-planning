{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ICal where

import Data.Time.Clock
import Data.Time.Format

data Event = Event { eventSummary     :: String
                   , eventStart       :: UTCTime
                   , eventEnd         :: UTCTime
                   , eventDescription :: String
                   , eventLocation    :: String
                   }

toVCal :: [Event] -> String
toVCal events = renderVCal $
    section "VCALENDAR" $ mconcat
    [ attr "PRODID" "-//Ben Gamari//Frosty export 1.0//EN"
    , attr "VERSION" "1.0"
    , foldMap eventToVCal events
    ]

eventToVCal :: Event -> VCal
eventToVCal ev = section "VEVENT" $ mconcat
    [ attr "SUMMARY" (eventSummary ev)
    , attr "DTSTART" $ renderTime (eventStart ev)
    , attr "DTEND" $ renderTime (eventEnd ev)
    , attr "DESCRIPTION" (eventDescription ev)
    , attr "LOCATION" (eventLocation ev)
    ]

newtype VCal = VCal { vcalContents ::[String] }
    deriving (Show, Monoid, Semigroup)

renderVCal :: VCal -> String
renderVCal (VCal xs) = unlines xs

section :: String -> VCal -> VCal
section sect body = mconcat
    [ attr "BEGIN" sect
    , body
    , attr "END" sect
    ]

attr :: String -> String -> VCal
attr k v = VCal [k ++ ":" ++ v]

renderTime :: UTCTime -> String
renderTime = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"
