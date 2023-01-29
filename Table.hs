{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

module Main where

import Lucid
import Results
import Scoring

import Data.List (intersperse, sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Foldable
import Data.Maybe

dayAnchor :: String -> T.Text
dayAnchor day = "day-" <> T.pack day

main :: IO ()
main = do
    series <- readSeries "results"
    renderToFile "scores.html" $ doctypehtml_ $ do
        header
        body_ $ do
            h1_ [class_ "title"] "Score Summary"
            scoreSummaryTable series

            h1_ [class_ "title"] "Per-race results"
            mconcat
                [ do h2_ [id_ (dayAnchor day), class_ "subtitle"] $ toHtml day
                     rankingsTable
                        $ M.fromList
                          [ ("Race " ++ show i, race)
                          | i <- [1..]
                          | race <- races
                          ]
                | (day, races) <- M.toList series
                ]

header :: Html ()
header = head_ $ do
    link_ [rel_ "stylesheet", href_ "rankings.css"]
    link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"]
    script_ [src_ "rankings.js"] (return () :: Html ())

-- | Summarize rankings of a day's races.
rankingsTable :: M.Map String Race -> Html ()
rankingsTable races =
    table_ [classes_ ["table", "is-striped", "is-hoverable", "rankings"]] $ do
        thead_ $ do
            tr_ $ mapM_ (th_ . toHtml) $ ["Position"] ++ M.keys races
            tr_ $ do
                th_ "RC"
                mapM_ (td_ . ul_ . foldMap (li_ . sailor) . S.toList . raceRCs) races

        let mkRow :: Int -> [Maybe Sailor] -> Html ()
            mkRow rank sailors = do
                th_ $ toHtml $ show rank
                mapM_ (maybe (td_ $ return ()) (td_  . sailor)) sailors
        tbody_
            $ mapM_ tr_
            $ zipWith mkRow [1..]
            $ getRanking $ racesRankings
            $ M.elems races

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
scoreSummaryTable :: M.Map String [Race]
                  -> Html ()
scoreSummaryTable series =
    table_ [classes_ ["table", "is-striped", "is-hoverable", "rankings"]] $ do
        thead_ $ do
            tr_ $ mapM_ (th_ . toHtml) $ ["Day", "# Races", "Attendence"] ++ map sailor allSailors
        tbody_ $ do
            forM_ (M.toList scored) $ \(day, (races, points)) -> tr_ $ do
                th_ $ a_ [href_ ("#" <> dayAnchor day)] $ toHtml day
                let dayPoints :: M.Map Sailor Points
                    dayPoints = M.unionsWith (<>) points
                    attendees :: S.Set Sailor
                    attendees = foldMap raceFinishers races
                td_ $ toHtml $ show $ length races
                td_ $ toHtml $ show $ S.size attendees
                mconcat
                    [ td_ [classes_ classes]
                      $ toHtml $ fromJust $ M.lookup sailor dayPoints
                    | sailor <- allSailors
                    , let present :: Bool
                          present = sailor `S.member` attendees
                          classes
                            | not present = ["has-text-grey-light"]
                            | otherwise = []
                    ]
            tfoot_ $ tr_ $ do
                th_ "total"
                td_ $ toHtml $ show $ sum $ fmap length series
                td_ ""
                sequenceA_
                    [ td_ $ toHtml $ totals M.! sailor
                    | sailor <- allSailors
                    ]

  where
    scored :: M.Map String ([Race], [M.Map Sailor Points])
    scored = M.intersectionWith (,) series (scoreSeries series)

    allSailors :: [Sailor]
    allSailors = map fst $ sortBy (comparing snd) $ M.toList totals

    totals :: M.Map Sailor Points
    totals = M.unionsWith (<>) $ concat $ map snd $ M.elems scored

