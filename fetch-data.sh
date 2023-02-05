#! bin/bash

cd data
svn checkout https://github.com/epiforecasts/europe-covid-forecast/trunk/submissions

cd crowd-direct-forecast
svn checkout https://github.com/epiforecasts/europe-covid-forecast/trunk/crowd-direct-forecast/processed-forecast-data

cd ../crowd-rt-forecast
svn checkout https://github.com/epiforecasts/europe-covid-forecast/trunk/crowd-rt-forecast/processed-forecast-data

cd ../EuroCOVIDhub-ensemble
svn checkout https://github.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/trunk/data-processed/EuroCOVIDhub-ensemble
mv EuroCOVIDhub-ensemble processed-forecast-data