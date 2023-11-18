#!/usr/bin/env bash

set -e 

KEY="${KEY:-~/.ssh/frosty}"
cabal run results 2023-2024
dest="frosty@smart-cactus.org:/var/www/frosty.smart-cactus.org"
scp -i $KEY scores.html "$dest/index.html"
scp -i $KEY rankings.js rankings.css "$dest"
