#!/usr/bin/env bash

set -e

current_season="2024-2025"
seasons="$(ls ./results)"

if which apt-get; then
    sudo add-apt-repository universe
    sudo apt-get install wkhtmltopdf
fi

for season in $seasons; do
    echo "Processing $season..."
    cabal run results $season
done

echo "Generating preview..."
wkhtmltoimage --enable-local-file-access --width 1200 --height 630 scores-$current_season.html preview.png

echo "Uploading..."
KEY="${KEY:-~/.ssh/frosty}"
dest="frosty@smart-cactus.org:/var/www/frosty.smart-cactus.org"
scp -i $KEY scores-$current_season.html "$dest/index.html"
scp -i $KEY scores-*.html "$dest"
scp -i $KEY rankings.js rankings.css preview.png "$dest"
