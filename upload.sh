#!/usr/bin/env bash

set -e

current_season="2026-2027"
seasons="$(ls ./results)"

for season in $seasons; do
    echo "Processing $season..."
    cabal run results $season
done

echo "Generating previews..."
chrome="$(command -v google-chrome || command -v chromium)"
profile="$(mktemp -d)"
for page in scores-*.html; do
    "$chrome" --headless=new --no-sandbox --disable-gpu --hide-scrollbars \
        --user-data-dir="$profile" --window-size=1200,630 \
        --screenshot="$PWD/${page%.html}.png" "file://$PWD/$page"
done
rm -rf "$profile"
# Used by archive.html
cp scores-$current_season.png preview.png

echo "Uploading..."
KEY="${KEY:-~/.ssh/frosty}"
dest="frosty@smart-cactus.org:/var/www/frosty.smart-cactus.org"
scp -i $KEY scores-$current_season.html "$dest/index.html"
scp -i $KEY scores-*.html scores-*.png "$dest"
scp -i $KEY archive.html rankings.js rankings.css preview.png "$dest"
