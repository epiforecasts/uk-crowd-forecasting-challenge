#! bin/bash

cd data
svn checkout https://github.com/epiforecasts/europe-covid-forecast/trunk/submissions

cd crowd-direct-forecast
svn checkout https://github.com/epiforecasts/europe-covid-forecast/trunk/crowd-direct-forecast/processed-forecast-data

cd ../crowd-rt-forecast
svn checkout https://github.com/epiforecasts/europe-covid-forecast/trunk/crowd-rt-forecast/processed-forecast-data
