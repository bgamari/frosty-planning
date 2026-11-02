#!/usr/bin/env bash

set -e

current_season="2026-2027"
seasons="$(ls ./results)"

if which apt-get; then
    sudo add-apt-repository universe
    sudo apt-get install wkhtmltopdf
fi

for season in $seasons; do
    echo "Processing $season..."
    cabal run results $season
done

echo "Generating previews..."
for page in scores-*.html; do
    wkhtmltoimage --quiet --quality 10 --enable-local-file-access --width 1200 --height 630 "$page" "${page%.html}.png"
done
# Used by archive.html
cp scores-$current_season.png preview.png

echo "Uploading..."
KEY="${KEY:-~/.ssh/frosty}"
dest="frosty@smart-cactus.org:/var/www/frosty.smart-cactus.org"
scp -i $KEY scores-$current_season.html "$dest/index.html"
scp -i $KEY scores-*.html scores-*.png "$dest"
scp -i $KEY archive.html rankings.js rankings.css preview.png "$dest"
