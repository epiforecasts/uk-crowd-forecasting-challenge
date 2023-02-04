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

analysis_date <- "2022-12-12"

## -------------------------------------------------------------------------- ##  
##                  Download and process Forecast Hub data                    ##
## -------------------------------------------------------------------------- ##

# -------------------- Download Forecast Hub folder using SVN ---------------- #
# svn checkout https://github.com/epiforecasts/covid19-forecast-hub-europe/trunk/data-processed

# svn checkout https://github.com/epiforecasts/europe-covid-forecast/trunk/submissions


# ---------------load truth data using the covidHubutils package ------------- #
if (file.exists("data/weekly-truth-Europe.csv")) {
  truth <- fread("data/weekly-truth-Europe.csv")
} else {
  truth <- covidHubUtils::load_truth(hub = "ECDC") |>
    filter(target_variable %in% c("inc case", "inc death")) |>
    mutate(target_variable = ifelse(target_variable == "inc case", 
                                    "Cases", "Deaths")) |>
    rename(target_type = target_variable, 
           true_value = value) |>
    select(-model) |>
    filter(location == "GB")
  
  fwrite(truth, "data/weekly-truth-Europe.csv")
}



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
               }) %>%
    unlist()
  
  forecasts <- suppressMessages(map_dfr(files, 
                                        function(file) {
                                          fread(file) |>
                                            mutate_all(as.character) |>
                                            mutate(model = model_name,
                                                   quantile = as.numeric(quantile),
                                                   target_end_date = as.character(target_end_date),
                                                   horizon = as.numeric(gsub("([0-9]+).*$", "\\1", target))) %>%
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
    here("submissions", "crowd-forecasts"), 
    here("submissions", "crowd-rt-forecasts"), 
    here("submissions", "crowd-direct-forecasts"), 
    here("submissions", "rt-forecasts")
  ), 
  model_names = c(
    "combined crowd", 
    "direct crowd", 
    "rt crowd", 
    "EpiNow2"
  )
)


forecasts[, forecast_date := calc_submission_due_date(forecast_date)]

forecasts <- forecasts |>
  mutate(target_type = ifelse(grepl("death", target), "Deaths", "Cases")) %>%
  dplyr::rename(prediction = value) %>%
  dplyr::mutate(horizon = as.numeric(substring(target, 1, 1))) %>%
  dplyr::filter(type == "quantile") %>%
  dplyr::select(location, forecast_date, quantile, prediction, 
                horizon, model, target_end_date, target, target_type) %>%
  dplyr::filter(forecast_date >= "2021-05-24", 
                forecast_date <= "2021-08-16") %>%
  dplyr::filter(model != "EpiNow2") |>
  select(-target)


data <- merge_pred_and_obs(forecasts, 
                               truth |>
                                 mutate(target_end_date = as.character(target_end_date)) |>
                                 filter(target_end_date >= "2021-01-01", 
                                        target_end_date <= "2022-01-01"), 
                               by = c("location", "target_end_date", 
                                      "target_type")) |>
  unique()



# --------------------------- Filter forecast data --------------------------- #

# filter out forecast anomalies based on the Forecast Hub anomalies file
filter_out_anomalies <- function(hub_data, analysis_date = Sys.Date()) {
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
  
  setDT(data)
  data <- data[!anomalies, on = .(target_end_date, location, target_type)]
  data <- data[true_value >= 0][]
  return(data)
}

data[, prediction := as.numeric(prediction)]

data <- filter_out_anomalies(data)


fwrite(data, "data/forecast-data.csv")









## -------------------------------------------------------------------------- ##  
##                               Score forecasts                              ##
## -------------------------------------------------------------------------- ##

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

