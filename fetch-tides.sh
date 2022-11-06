#!/usr/bin/env bash

START_DATE="20221106"
END_DATE="20230530"
STATION="8419870"
URL="https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date=${START_DATE}&end_date=${END_DATE}&station=${STATION}&product=predictions&datum=STND&time_zone=gmt&units=metric&format=csv&interval=hilo"

curl "$URL" > tides.csv
