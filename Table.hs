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
import Data.Foldable
import Data.Maybe

main :: IO ()
main = do
    series <- readSeries "results"
    renderToFile "scores.html" $ doctypehtml_ $ do
        header
        body_ $ do
            h1_ [class_ "title"] "Score Summary"
            dayScoreTable (scoreSeries series)

            h1_ [class_ "title"] "Per-race results"
            mconcat
                [ do h2_ [class_ "subtitle"] $ toHtml day
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
                mapM_ (maybe (return ()) (td_  . sailor)) sailors
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
dayScoreTable :: M.Map String [M.Map Sailor Points]
              -> Html ()
dayScoreTable results =
    table_ [classes_ ["table", "is-striped", "is-hoverable", "rankings"]] $ do
        thead_ $ do
            tr_ $ mapM_ (th_ . toHtml) $ ["Day"] ++ map sailor allSailors
        tbody_ $ do
            forM_ (M.toList results) $ \(day, points) -> tr_ $ do
                th_ $ toHtml day
                let dayPoints = M.unionsWith (<>) points
                mconcat
                    [ td_ $ toHtml $ fromJust $ M.lookup sailor dayPoints
                    | sailor <- allSailors
                    ]
            tfoot_ $ tr_ $ do
                th_ "total"
                sequenceA_
                    [ td_ $ toHtml $ totals M.! sailor
                    | sailor <- allSailors
                    ]

  where
    allSailors :: [Sailor]
    allSailors = map fst $ sortBy (comparing snd) $ M.toList totals

    totals :: M.Map Sailor Points
    totals = M.unionsWith (<>) $ concat $ M.elems results
