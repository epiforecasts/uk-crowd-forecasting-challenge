## ========================================================================== ##
##               Code to refetch data from                   ##
## ========================================================================== ##

library(covidHubUtils)
library(dplyr)
library(data.table)
library(purrr)
library(scoringutils)
library(here)
library(stringr)
library(tidyr)

analysis_date <- Sys.Date()
time_start <- as.Date("2021-05-24")
time_stop <- as.Date("2021-08-16")

## -------------------------------------------------------------------------- ##  
##                  Download and process Forecast Hub data                    ##
## -------------------------------------------------------------------------- ##

# ----------------------------- Renew Forecast Hub data ---------------------- #
# bash fetch-data.sh


# -------------------- Helper function to flag anomalies --------------------- #
# function to filter out forecast anomalies based on the Forecast Hub anomalies file
flag_anomalies <- function(data, analysis_date = Sys.Date()) {
  anomalies_file <- here::here("data", "anomalies.csv")
  if (file.exists(anomalies_file)) {
    anomalies <- data.table::fread(file = anomalies_file) 
  } else {
    ## get anomalies file at date of last data made
    owner <- "covid19-forecast-hub-europe"
    repo <- "covid19-forecast-hub-europe"
    path <- "data-truth/anomalies/anomalies.csv"
    commit <- gh::gh(
      "/repos/{owner}/{repo}/commits?path={path}&until={date}",
      owner = owner,
      repo = repo,
      path = path,
      until = analysis_date,
      .limit = 1
    )
    anomalies <- data.table::fread(input = URLencode(URL = paste(
      "https://raw.githubusercontent.com", owner, repo, commit[[1]]$sha, path, sep = "/"
    )))
    data.table::fwrite(x = anomalies, file = anomalies_file)
  }
  
  ## prepare anomalies for mergeing into forecast data
  anomalies <- anomalies[, list(
    target_end_date,
    location,
    target_type = paste0(stringr::str_to_title(stringr::str_remove(
      string = target_variable, pattern = "^inc ")), "s")
  )]
  anomalies <- anomalies[location == "GB"]
  anomalies <- anomalies[target_type %in% c("Cases", "Deaths")]
  anomalies[, target_end_date := as.character(target_end_date)]
  anomalies[, status := "anomaly"]
  
  data <- left_join(data, anomalies)
  setDT(data)
  data[is.na(status), status := "ok"]
  return(data[])
}

# -------------------- helper function to clean forecasts -------------------- #
clean_forecasts <- function(forecasts) {
  forecasts |>
    mutate(target_type = ifelse(grepl("death", target), "Deaths", "Cases")) |>
    mutate(forecast_date := calc_submission_due_date(forecast_date)) |>
    mutate(target_end_date = as.character(target_end_date)) |>
    rename(prediction = value) |>
    mutate(prediction = as.numeric(prediction)) |>
    mutate(horizon = as.numeric(substring(target, 1, 1))) |>
    filter(type == "quantile") |>
    select(location, forecast_date, quantile, prediction, 
                  horizon, model, target_end_date, target, target_type) |>
    filter(forecast_date >= "2021-05-24", 
                  forecast_date <= "2021-08-16") |>
    filter(model != "EpiNow2") |>
    select(-target)
}



# ---------------load truth data using the covidHubutils package ------------- #
if (file.exists("data/weekly-truth.csv")) {
  truth <- fread("data/weekly-truth.csv")
  daily_truth <- fread("data/daily-truth.csv")
  
} else {
  clean_truth <- function(data) {
    data |>
      filter(target_variable %in% c("inc case", "inc death")) |>
      mutate(target_variable = ifelse(target_variable == "inc case", 
                                      "Cases", "Deaths")) |>
      rename(target_type = target_variable, 
             true_value = value) |>
      select(-model) |>
      filter(location == "GB") |>
      filter(target_end_date >= "2021-01-01",
             target_end_date <= "2022-01-01")
  }
  
  truth <- covidHubUtils::load_truth(hub = "ECDC") |>
    clean_truth()
  daily_truth <- covidHubUtils::load_truth(hub = "ECDC", temporal_resolution = "daily") |>
    clean_truth()
  
  truth <- flag_anomalies(mutate(truth, target_end_date = as.character(target_end_date)))
  
  fwrite(truth, "data/weekly-truth.csv")
  fwrite(daily_truth, "data/daily-truth.csv")
}



# ---------------------- load aggregated prediction data --------------------- #
load_single_model <- function(root_dir,
                              model_name) {
  
  locations <- select(truth, location, location_name) |>
    filter(location == "GB") |>
    unique()
  
  folders <- list.files(root_dir)
  files <- map(folders,
               .f = function(folder_name) {
                 files <- list.files(here(root_dir, folder_name))
                 paste(here(root_dir, folder_name, files))
               }) |>
    unlist()
  
  forecasts <- suppressMessages(map_dfr(files, 
                                        function(file) {
                                          fread(file) |>
                                            mutate_all(as.character) |>
                                            mutate(model = model_name,
                                                   quantile = as.numeric(quantile),
                                                   target_end_date = as.character(target_end_date),
                                                   horizon = as.numeric(gsub("([0-9]+).*$", "\\1", target))) |>
                                            filter(grepl("inc", target),
                                                   type == "quantile")
                                        }))
  
  forecasts <- inner_join(forecasts, locations) 
  
  return(forecasts)
}

load_forecasts <- function(directories,
                           model_names) {
  
  forecasts <- list()
  for (i in 1:length(directories)) {
    forecasts[[i]] <- load_single_model(directories[i], model_names[i])
  }
  
  forecasts <- rbindlist(forecasts, use.names = TRUE)
  return(forecasts)
}

forecasts <- load_forecasts(
  directories = c(
    here("data", "submissions", "crowd-forecasts"), 
    here("data", "submissions", "crowd-rt-forecasts"), 
    here("data", "submissions", "crowd-direct-forecasts"), 
    here("data", "submissions", "rt-forecasts")
  ), 
  model_names = c(
    "crowd-ensemble", # is the same as the submitted EpiExpert-ensemble
    "crowd-direct", 
    "crowd-rt", 
    "EpiNow2"
  )
) |>
  clean_forecasts()

# ---------------------- load individual prediction data --------------------- #

root_dirs <- c(here::here("data", "crowd-direct-forecast", "processed-forecast-data"),
               here::here("data", "crowd-rt-forecast", "processed-forecast-data"), 
               here::here("data", "EuroCOVIDhub-ensemble", "processed-forecast-data"))
file_paths_forecast <- c(here::here(root_dirs[1], list.files(root_dirs[1])),
                         here::here(root_dirs[2], list.files(root_dirs[2])), 
                         here::here(root_dirs[3], list.files(root_dirs[3])))

individual_prediction_data <- purrr::map_dfr(file_paths_forecast,
                                             .f = function(x) {
                                               data <- data.table::fread(x)
                                               if ("board_name" %in% names(data)) {
                                                 setnames(data, old = "board_name", new = "model")
                                               }
                                               if (!("model" %in% names(data))) {
                                                 data[, model := "EuroCOVIDhub-ensemble"]
                                               }
                                               data[, target_end_date := as.character(target_end_date)]
                                               data[, forecast_date := calc_submission_due_date(forecast_date)]
                                               data[, submission_date := as.character(forecast_date)]
                                               if (grepl("-rt", x)) {
                                                 data[, model := paste(model, "(Rt)")]
                                               }
                                               return(data)
                                             }) |>
  clean_forecasts()


combined_forecasts <- rbind(forecasts, individual_prediction_data)

data <- merge_pred_and_obs(combined_forecasts, 
                               truth |>
                                 mutate(target_end_date = as.character(target_end_date)) |>
                                 filter(target_end_date >= "2021-01-01", 
                                        target_end_date <= "2022-01-01"), 
                               by = c("location", "target_end_date", 
                                      "target_type")) |>
  unique()

fwrite(data, "data/forecast-data.csv")


# ---- collect data on how many models were included in the hub-ensemble ----- #

root_dirs <- c(here::here("data", "EuroCOVIDhub-ensemble", "criteria"))
file_paths_inclusion <- c(here::here(root_dirs[1], list.files(root_dirs[1])))

ensemble_inclusions <- 
  purrr::map_dfr(file_paths_inclusion,
                 .f = function(x) {
                   data <- data.table::fread(x)
                   data <- data[location == "GB" & included_in_ensemble]
                   data[, target_type := ifelse(
                     grepl("case", target_variable), 
                     "Cases", "Deaths")]
                   
                   date <- substr(x,1,nchar(x)-4)
                   date <- substr(date, nchar(date)-9, nchar(date))
                   data[, forecast_date := date]
                   
                   data <- data[, .SD, .SDcols = c("model", "forecast_date", "target_type")]
                   return(data)
                 }) |>
  filter(forecast_date >= time_start, 
         forecast_date <= time_stop)

fwrite(ensemble_inclusions, "data/ensemble-inclusions.csv")



## -------------------------------------------------------------------------- ##  
##                               Score forecasts                              ##
## -------------------------------------------------------------------------- ##

# remove and ignore anomalies. Otherwise there are no death forecasts left...

data <- select(data, -status)

## Score forecasts, adding versions for the log and sqrt transformations
scores <- data |>
  mutate(scale = "natural") |>
  # add log data
  rbind(data |>
          mutate(
            scale = "log", 
            true_value = log(true_value + 1), 
            prediction = log(pmax(prediction, 0) + 1)
          )) |>
  rbind(data |>
          mutate(scale = "sqrt", 
                 true_value = sqrt(true_value), 
                 prediction = sqrt(prediction))) |>
  score(metrics = c("interval_score")) |>
  summarise_scores(by = c("model", "location",
                          "target_end_date", "forecast_date",
                          "horizon", "target_type", "scale"), 
                   na.rm = FALSE)

scores[, type_and_scale := paste0(target_type, " - ", scale)]

# # add median forecast to scores, needed for some downstream analysis
# add_median_forecast <- function(scores, hub_data) {
#   medians <- hub_data |>
#     filter(quantile == 0.5) |> 
#     rename(median_prediction = prediction) |>
#     select(-quantile, -true_value)
#   
#   out <- scores |>
#     inner_join(medians)
#   
#   return(out)
# }
# scores <- add_median_forecast(scores, hub_data)

fwrite(scores, here("data", "all-scores-crowd-forecasts.csv"))

