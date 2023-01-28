{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lucid
import Results

import Data.List (intersperse)
import qualified Data.Set as S
import Data.Foldable
import Data.Maybe

main :: IO ()
main = do
    results <- readRaces "results/2023-01-08"
    let resultsTable = rankingsTable results
    renderToFile "results.html" resultsTable

rankingsTable :: [Race] -> Html ()
rankingsTable races = doctypehtml_ $ do
    head_ $ do
        link_ [rel_ "stylesheet", href_ "rankings.css"]
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"]
        script_ [src_ "rankings.js"] (return () :: Html ())
    body_ $ do
        table_ [classes_ ["table", "rankings"]] $ do
            thead_ $ do
                tr_ $ mapM_ (th_ . toHtml) $ ["Position"] ++ ["Race " ++ show i | (i, _) <- zip [1..] races]
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
                $ getRanking $ racesRankings races

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
