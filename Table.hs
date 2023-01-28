module Table where

import Lucid
import Results

main :: IO ()
main = do
    results <- readRaces "results/2023-01-08"
    let resultsTable = rankingsTable results
    renderToFile "results.html" resultsTable

rankingsTable :: [Race] -> Html ()
rankingsTable results = doctypehtml_ $ do
    body_ $ do
        table_ $ do
            tr_ $ mapM_ th_ $ ["Position"] ++ ["Race " ++ show i | (i, _) <- zip [1..] results]
            tr_ $ do
                th_ "RC"
                mapM_ (foldMap $ td_ . sailor) raceRCs
            mapM_ resultRow
                [ 
                | map raceRanking results
                ]

sailor :: Sailor -> Html ()
sailor (Sailor s) = toHtml s
