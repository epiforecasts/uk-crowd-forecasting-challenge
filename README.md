# uk-crowd-forecasting-challenge

> **Archived.** No longer maintained; last updated June 2023.

Code, data and manuscript for the paper "Human judgement forecasting of COVID-19 in the UK", which analysed the human judgement forecasts of UK cases and deaths submitted to the European COVID-19 Forecast Hub during the UK COVID-19 Crowd Forecasting Challenge in 2021. The manuscript was prepared for Wellcome Open Research and edited through Overleaf. The manuscript gives Zenodo archives for the code (https://doi.org/10.5281/zenodo.7897257), the data (https://doi.org/10.5281/zenodo.7897289) and the supplementary information (https://doi.org/10.5281/zenodo.7897513).

- `fetch-data.sh` and `get-paper-data.R`: download the forecasts and truth data from `epiforecasts/europe-covid-forecast` and the European Forecast Hub, and score them with `scoringutils`.
- `Analysis.rmd`: the analysis producing the figures and tables; `old-unused-analyses.R` holds analyses that were dropped.
- `data/`: forecasts, scores, original and revised truth data, and ensemble inclusion records.
- `manuscript/`: LaTeX source of the paper, its tables and the supplementary information.
- `output/figures/`: figures for the paper.
