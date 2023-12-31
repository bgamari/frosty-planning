{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

module Main where

import Lucid
import Results
import Scoring

import Control.Monad
import Data.List (intersperse, sort, sortBy)
import Data.Tuple (swap)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Foldable
import Data.Maybe
import System.Environment
import System.FilePath

dayAnchor :: String -> T.Text
dayAnchor day = "day-" <> T.pack day

empty :: Html ()
empty = return ()

type ScoredRaces = M.Map String (Races, [M.Map Sailor (DroppedOut Points)])

main :: IO ()
main = do
    [season] <- getArgs
    series <- readSeries ("results" </> season)
    let scored :: ScoredRaces
        scored = M.intersectionWith (,) series (scoreSeries series)

    renderToFile "scores.html" $ doctypehtml_ $ do
        header season
        body_ $ do
            h1_ [classes_ ["title", "is-1"]] "Frosty Fleet 9 Results"
            h2_ [classes_ ["subtitle", "is-2"], id_ "top"] $ toHtml season <> " Season"

            p_ $ do
                "Results for Frosty Fleet 9's " <> toHtml season <> " racing season are tabulated below. See "
                a_ [href_ "https://github.com/bgamari/frosty-planning"] "GitHub "
                "for machine-readable data, tabulation tools, and a discussion of scoring methodology."
            div_ $ do
                i_ "Hint: "
                span_ "Click on a sailor's name to highlight occurrences."
            div_ [classes_ ["mb-4"]] empty
            scoreSummaryTable scored

            h2_ [classes_ ["subtitle", "is-2"]] "Results by Race Day"
            mconcat
                [ do h3_ [id_ (dayAnchor day), classes_ ["subtitle", "is-3"]] $ do
                         span_ $ toHtml day
                         span_ [classes_ ["header-link"]] $ "[" <> a_ [href_ "#top"] "return to top" <> "]"
                     let participants = racesParticipants $ races rs
                     div_ $ toHtml $ notes rs
                     div_ $ do
                         toHtml $ show $ S.size participants
                         " sailors participated this day."
                         when (dropouts rs > 0) $ do
                            "This day is scored with "
                            toHtml $ show (dropouts rs)
                            " dropout races."

                     h4_ [classes_ ["subtitle", "is-4"]] "By position"
                     rankingsTable
                        $ M.fromList
                          [ ("Race " ++ show i, race)
                          | i <- [1..]
                          | race <- races rs
                          ]

                     h4_ [classes_ ["subtitle", "is-4"]] "By sailor"
                     when (any (any isDroppedOut) scores) $ div_ $ do
                         i_ "Hint: "
                         span_ [classes_ ["dropout"], style_ "padding: 0.2em; border-radius: 0.5em;"] "Red highlighting"
                         " denotes dropped-out score"
                     dayRankingsTable participants scores
                | (day, (rs, scores)) <- M.toList scored
                ]

isDroppedOut :: DroppedOut a -> Bool
isDroppedOut (DroppedOut _) = True
isDroppedOut (Scored _) = False

header :: String -> Html ()
header season = head_ $ do
    title_ $ "Frosty Fleet 9 Results " <> toHtml season
    link_ [rel_ "stylesheet", href_ "rankings.css"]
    link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"]
    script_ [src_ "rankings.js"] empty

-- | Summarize a particular day of races by position.
rankingsTable :: M.Map String Race -> Html ()
rankingsTable races =
    table_ [classes_ ["table", "is-striped", "is-hoverable", "race-rankings"]] $ do
        thead_ $ do
            tr_ $ mapM_ (th_ . toHtml) $ ["Position"] ++ M.keys races
            tr_ $ do
                th_ "RC"
                mapM_ (td_ . ul_ . foldMap (li_ . sailor) . S.toList . raceRCs) races

        let mkRow :: Int -> [Maybe Sailor] -> Html ()
            mkRow rank sailors = do
                th_ $ toHtml $ show rank
                mapM_ (maybe (td_ empty) (td_  . sailor)) sailors
        tbody_
            $ mapM_ tr_
            $ zipWith mkRow [1..]
            $ getRanking $ racesRankings
            $ M.elems races

-- | Summarize a particular day of races by sailor.
dayRankingsTable :: S.Set Sailor -> [M.Map Sailor (DroppedOut Points)] -> Html ()
dayRankingsTable participants scores =
    table_ [classes_ ["table", "is-striped", "is-hoverable", "day-rankings"]] $ do
        thead_ $ tr_ $ do
            th_ "Sailor"
            th_ "Total"
            sequence_ [th_ $ toHtml $ "Race " ++ show i | (i,_) <- zip [1..] scores]
        tbody_ $ do
            mapM_ sailorRow ranking
  where
    totals :: M.Map Sailor Points
    totals = totalScore scores `M.restrictKeys` participants

    ranking :: [Sailor]
    ranking = map snd $ sort $ map swap $ M.toList totals

    sailorRow :: Sailor -> Html ()
    sailorRow s = tr_ $ do
        td_ $ sailor s
        td_ [classes_ []] $ toHtml $ totals M.! s
        sequence_
            [ case M.lookup s race of
                Just (DroppedOut s) -> td_ [classes_ ["dropout"]] $ toHtml s
                Just (Scored s)     -> td_ $ toHtml s
                Nothing             -> td_ "dnf"
            | race <- scores
            ]


racesRankings :: [Race] -> Ranking [Maybe Sailor]
racesRankings = 
    truncate . sequenceA . map (pad . raceRanking)
  where
    pad (Ranking xs) = Ranking (fmap Just xs ++ repeat Nothing)
    truncate (Ranking xs) = Ranking $ takeWhile (not . all isNothing) xs

sailor :: Sailor -> Html ()
sailor s =
    span_ [class_ "sailor", data_ "sailor" (sailorName s)]
    $ toHtml $ sailorName s

instance ToHtml Points where
    toHtml (Points n) = toHtml $ show $ round n
    toHtmlRaw = toHtml

-- | Summary of sailors' per-day scores.
scoreSummaryTable :: ScoredRaces
                  -> Html ()
scoreSummaryTable scored =
    table_ [classes_ ["table", "is-striped", "is-hoverable", "score-summary"]] $ do
        thead_ $ do
            tr_ $ mapM_ (th_ . toHtml) $ ["Day", "# Races", "Attendance"] ++ map sailor allSailors
        tbody_ $ do
            forM_ (M.toList scored) $ \(day, (rs, points)) -> tr_ $ do
                th_ $ a_ [href_ ("#" <> dayAnchor day)] $ toHtml day
                let dayPoints :: M.Map Sailor Points
                    dayPoints = M.unionsWith (<>) $ fmap (fmap getScored) points
                    attendees :: S.Set Sailor
                    attendees = foldMap raceFinishers (races rs)
                td_ $ toHtml $ show $ length $ races rs
                td_ $ toHtml $ show $ S.size attendees
                mconcat
                    [ td_ [classes_ classes]
                      $ toHtml $ fromMaybe (error $ show (sailor, dayPoints)) $ M.lookup sailor dayPoints
                    | sailor <- allSailors
                    , let present :: Bool
                          present = sailor `S.member` attendees
                          classes
                            | not present = ["has-text-grey-light"]
                            | otherwise = []
                    ]
            tfoot_ $ tr_ $ do
                th_ "total"
                td_ $ toHtml $ show $ sum $ fmap (length . races . fst) scored
                td_ ""
                sequenceA_
                    [ td_ $ toHtml $ totals M.! sailor
                    | sailor <- allSailors
                    ]

  where
    allSailors :: [Sailor]
    allSailors = map fst $ sortBy (comparing snd) $ M.toList totals

    totals :: M.Map Sailor Points
    totals = M.unionsWith (<>)
        $ fmap (fmap getScored)
        $ concat
        $ map snd
        $ M.elems scored

totalScore :: [M.Map Sailor (DroppedOut Points)] -> M.Map Sailor Points
totalScore races = M.fromListWith (<>)
    [ (sailor, getScored pts)
    | race <- races
    , (sailor, pts) <- M.toList race
    ]

