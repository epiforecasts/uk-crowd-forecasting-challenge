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
library("gh")
library("lubridate")
library("readr")


analysis_date <- Sys.Date()
time_start <- as.Date("2021-05-24")
time_stop <- as.Date("2021-08-16")

## -------------------------------------------------------------------------- ##  
##                                Download Hub data                           ##
## -------------------------------------------------------------------------- ##

# ----------------------------- Renew Forecast Hub data ---------------------- #
# bash fetch-data.sh


# ---------------load truth data using the covidHubutils package ------------- #
if (file.exists("data/weekly-truth-revised.csv")) {
  truth_revised <- fread("data/weekly-truth-revised.csv")
  daily_truth_revised <- fread("data/daily-truth-revised.csv")
  
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
  
  truth_revised <- covidHubUtils::load_truth(hub = "ECDC") |>
    clean_truth()
  daily_truth_revised <- covidHubUtils::load_truth(hub = "ECDC", temporal_resolution = "daily") |>
    clean_truth()
  
  truth_revised <- flag_anomalies(
    mutate(truth_revised, target_end_date = as.character(target_end_date))
    )
  
  fwrite(truth_revised, "data/weekly-truth-revised.csv")
  fwrite(daily_truth_revised, "data/daily-truth-revised.csv")
}


# ------------------------------ Obtain data revisions ----------------------- #
if (file.exists("data/data-revisions-deaths.csv")) {
  data_revisions <- fread("data/data-revisions-deaths.csv")
} else {
  earliest_date <- NULL
  min_data <- as.Date("2021-03-08") ## earliest date to pot in data
  
  sources <-
    c(Deaths = "JHU")
  
  target_variables <-
    c(Deaths = "inc death")
  
  owner <- "epiforecasts"
  repo <- "covid19-forecast-hub-europe"
  path <- vapply(names(sources), function(x) {
    paste("data-truth", sources[[x]],
          paste0("truth_", sources[[x]], "-Incident ", x, ".csv"),
          sep = "/")
  }, "")
  names(path) <- names(sources)
  
  data <- list()
  for (source in names(sources)) {
    query <- "/repos/{owner}/{repo}/commits?path={path}"
    if (!is.null(earliest_date)) {
      query <- paste0(query, "&since={date}")
    }
    
    commits <-
      gh::gh(query,
             owner = owner,
             repo = repo,
             path = path[source],
             date = earliest_date,
             .limit = Inf
      )
    
    shas <- vapply(commits, "[[", "", "sha")
    dates <- vapply(commits, function(x) x[["commit"]][["author"]][["date"]], "")
    dates <- as_date(ymd_hms(dates))
    
    ## keep multiples of 7 since today
    select_commits <- which(as.integer(max(dates) - dates) %% 7 == 0)
    
    data[[source]] <-
      lapply(
        select_commits,
        function(id)
          readr::read_csv(
            URLencode(
              paste("https://raw.githubusercontent.com", owner, repo,
                    shas[id], path[source], sep = "/")),
            show_col_types = FALSE) %>%
          mutate(commit_date = dates[id])
      )
    # remove empty dataframes
    if (class(data[[source]]) == "list") {
      data[[source]] <- data[[source]][sapply(data[[source]], function(x) nrow(x)>0)]
    }
    
    data[[source]] <- data[[source]] %>%
      bind_rows() %>%
      mutate(type = {{ source }})
  }
  
  data_revisions <- data |>
    bind_rows() |>
    filter(location == "GB") |>
    filter(date >= "2021-01-01", 
           date <= "2022-01-01") |>
    mutate(target_end_date = ceiling_date(date, "week", week_start = 6)) |>
    group_by(location, location_name, target_end_date, commit_date, type) %>%
    mutate(weekly_value = sum(value), 
           n = n()) |>
    ungroup() 
  
  fwrite(data_revisions, file = "data/data-revisions-deaths.csv")
}





## -------------------------------------------------------------------------- ##
# --------------------------------- Process data ----------------------------- #
## -------------------------------------------------------------------------- ##

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





# --------- create version of the truth data without revisions --------------- #
daily_truth_original <- data_revisions |>
  filter(commit_date <= "2022-01-01") |>
  select(location, target_end_date = date, 
         target_type = type, true_value = value) |>
  unique() |>
  mutate(target_end_date = as.Date(target_end_date)) |>
  rbind(daily_truth_revised |>
          filter(target_type == "Cases") |>
          mutate(target_end_date = as.Date(target_end_date)), 
        fill = TRUE) |>
  arrange(target_end_date, target_type)

fwrite(daily_truth_original, "data/daily-truth-original.csv")

weekly_truth_original <- data_revisions |>
  filter(commit_date <= "2022-01-01") |>
  filter(n == 7) |>
  select(location, target_end_date, target_type = type, 
         true_value = weekly_value) |>
  unique() |>
  arrange(target_end_date) |>
  rbind(weekly_truth_revised |>
          filter(target_type == "Cases"), 
        fill = TRUE) |>
  arrange(target_end_date, target_type) |>
  mutate(target_end_date = as.character(target_end_date)) |>
  select(-status) |>
  flag_anomalies() |>
  mutate(target_end_date = as.Date(target_end_date)) 

fwrite(weekly_truth_original, "data/weekly-truth-original.csv")



# ---------------------- load aggregated prediction data --------------------- #
load_single_model <- function(root_dir,
                              model_name) {
  
  locations <- select(weekly_truth_original, location, location_name) |>
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

# ---------------------- load individual prediction data --------------------- #

if (file.exists("data/forecast-data.csv")) {
  data <- fread("data/forecast-data.csv")
} else {
  
  forecasts <- load_forecasts(
    directories = c(
      here("data", "submissions", "crowd-forecasts"), 
      here("data", "submissions", "crowd-rt-forecasts"), 
      here("data", "submissions", "crowd-direct-forecasts"), 
      here("data", "submissions", "rt-forecasts")
    ), 
    model_names = c(
      "crowd-ensemble", # is the same as the submitted EpiExpert-ensemble
      "crowd-rt", 
      "crowd-direct", 
      "EpiNow2"
    )
  ) |>
    clean_forecasts()
  
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
                             weekly_truth_original |>
                               mutate(target_end_date = as.character(target_end_date)) |>
                               filter(target_end_date >= "2021-01-01", 
                                      target_end_date <= "2022-01-01"), 
                             by = c("location", "target_end_date", 
                                    "target_type")) |>
    unique()
  
  fwrite(data, "data/forecast-data.csv")
  
  data_revised <- merge_pred_and_obs(combined_forecasts, 
                                     weekly_truth_revised |>
                                       mutate(target_end_date = as.character(target_end_date)) |>
                                       filter(target_end_date >= "2021-01-01", 
                                              target_end_date <= "2022-01-01"), 
                                     by = c("location", "target_end_date", 
                                            "target_type")) |>
    unique()
  
  fwrite(data_revised, "data/forecast-data-revised.csv")
  
  
}


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
score_data <- function(data) {
  scores <- data |>
    mutate(scale = "natural") |>
    # add log data
    rbind(data |>
            mutate(
              scale = "log", 
              true_value = log(true_value + 1), 
              prediction = log(pmax(prediction, 0) + 1)
            )) |>
    score(metrics = c("interval_score", "coverage")) |>
    add_coverage(by = c("model", "target_type", "horizon")) |>
    select(-coverage_deviation)
  
  scores[, type_and_scale := paste0(target_type, " - ", scale)]
  
  return(scores[])
}



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

scores <- score_data(data)
fwrite(scores, here("data", "all-scores-crowd-forecasts.csv"))


scores_revised <- score_data(data_revised)
fwrite(scores_revised, here("data", "all-scores-crowd-forecasts-revised.csv"))

