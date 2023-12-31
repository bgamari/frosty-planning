#!/usr/bin/env bash

set -e 

KEY="${KEY:-~/.ssh/frosty}"
cabal run results 2023-2024
# wkhtmltoimage  --width 1200 --height 630 scores.html preview.png
dest="frosty@smart-cactus.org:/var/www/frosty.smart-cactus.org"
scp -i $KEY scores.html "$dest/index.html"
scp -i $KEY rankings.js rankings.css preview.png "$dest"
