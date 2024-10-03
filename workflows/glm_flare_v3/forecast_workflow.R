library(tidyverse)
library(lubridate)
library(tidymodels)
library(xgboost)
library(RcppRoll)

#remotes::install_github('flare-forecast/FLAREr@single-parameter')
remotes::install_github('flare-forecast/FLAREr')
remotes::install_github("rqthomas/GLM3r")
Sys.setenv('GLM_PATH'='GLM3r')

lake_directory <- here::here()
setwd(lake_directory)
forecast_site <- "CANN"
configure_run_file <- "configure_run.yml"
config_set_name <- "glm_flare_v3"

fresh_run <- FALSE

Sys.setenv("AWS_DEFAULT_REGION" = "renc",
           "AWS_S3_ENDPOINT" = "osn.xsede.org",
           "USE_HTTPS" = TRUE,
           "SC_S3_ENDPOINT" = "projects.pawsey.org.au")

#' Source the R files in the repository
#walk(list.files(file.path(lake_directory, "R"), full.names = TRUE), source)

config_obs <- yaml::read_yaml(file.path(lake_directory,'configuration',config_set_name,'observation_processing.yml'))
configure_run_file <- "configure_run.yml"
config <- FLAREr:::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name)

if(fresh_run) unlink(file.path(lake_directory, "restart", "CANN", config$run_config$sim_name, configure_run_file))

# Read in the targets
source('workflows/glm_flare_v3/generate_targets.R')

# Move targets to s3 bucket
message("Successfully generated targets")

FLAREr:::put_targets(site_id =  config$location$site_id,
                    cleaned_insitu_file = file.path(config$file_path$qaqc_data_directory, "CANN-targets-insitu.csv"),
                    cleaned_met_file = file.path(config$file_path$qaqc_data_directory,"CANN-targets-met.csv"),
                    cleaned_inflow_file = file.path(config$file_path$qaqc_data_directory,"CANN-targets-inflow.csv"),
                    use_s3 = config$run_config$use_s3,
                    config = config)

## initialize info for inflow model
forecast_horizon <- config$run_config$forecast_horizon
inflow_model <- config$inflow$forecast_inflow_model
lake_name_code <- config$location$site_id
inflow_bucket <- config$s3$inflow_drivers$bucket
inflow_endpoint <- config$s3$inflow_drivers$endpoint
use_s3_inflow <- config$inflow$use_forecasted_inflow

noaa_ready <- FLAREr::check_noaa_present(lake_directory,
                                          configure_run_file = configure_run_file,
                                          config_set_name = config_set_name)
source('./R/generate_forecast_score_arrow.R')

while(noaa_ready){
  
  setwd(lake_directory)
  config <- FLAREr:::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name)
  
  message("Generating inflow forecast")
  #source(file.path(lake_directory, "workflows", config_set_name, "02_run_inflow_forecast.R"))
  source(file.path(lake_directory, "workflows", config_set_name, "xgboost_inflow.R"))
  
  # Run FLARE
  output <- FLAREr:::run_flare(lake_directory = lake_directory,
                              configure_run_file = configure_run_file,
                              config_set_name = config_set_name)
  
  message("Scoring forecasts")
  forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
  forecast_df <- arrow::open_dataset(forecast_s3) |>
    dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
    dplyr::filter(model_id == 'glm_flare_v3',
                  site_id == forecast_site,
                  reference_date == lubridate::as_datetime(config$run_config$forecast_start_datetime)) |>
    dplyr::collect()
  
  if(config$output_settings$evaluate_past & config$run_config$use_s3){
    #past_days <- lubridate::as_date(forecast_df$reference_datetime[1]) - lubridate::days(config$run_config$forecast_horizon)
    past_days <- lubridate::as_date(lubridate::as_date(config$run_config$forecast_start_datetime) - lubridate::days(config$run_config$forecast_horizon))
    
    #vars <- arrow_env_vars()
    past_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
    past_forecasts <- arrow::open_dataset(past_s3) |>
      dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
      dplyr::filter(model_id == 'glm_flare_v3',
                    site_id == forecast_site,
                    reference_date == past_days) |>
      dplyr::collect()
    #unset_arrow_vars(vars)
  }else{
    past_forecasts <- NULL
  }
  
  combined_forecasts <- dplyr::bind_rows(forecast_df, past_forecasts)
  
  targets_df <- read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),show_col_types = FALSE)
  
  #combined_forecasts <- arrow::open_dataset('./forecasts/parquet/site_id=ccre/model_id=glm_flare_v3/reference_date=2024-09-03/part-0.parquet') |> collect()
  
  scoring <- generate_forecast_score_arrow(targets_df = targets_df,
                                           forecast_df = combined_forecasts, ## only works if dataframe returned from output
                                           use_s3 = TRUE,
                                           bucket = config$s3$scores$bucket,
                                           endpoint = config$s3$scores$endpoint,
                                           local_directory = './SWAN-CANNING-forecast-code/scores/fcre',
                                           variable_types = c("state","parameter"))
  
  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1)
  start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) - lubridate::days(1)
  restart_file <- paste0(config$location$site_id,"-", (lubridate::as_date(forecast_start_datetime)- days(1)), "-",config$run_config$sim_name ,".nc")
  
  FLAREr:::update_run_config(lake_directory = lake_directory,
                             configure_run_file = configure_run_file,
                             restart_file = restart_file,
                             start_datetime = start_datetime,
                             end_datetime = NA,
                             forecast_start_datetime = forecast_start_datetime,
                             forecast_horizon = config$run_config$forecast_horizon,
                             sim_name = config$run_config$sim_name,
                             site_id = config$location$site_id,
                             configure_flare = config$run_config$configure_flare,
                             configure_obs = config$run_config$configure_obs,
                             use_s3 = config$run_config$use_s3,
                             bucket = config$s3$restart$bucket,
                             endpoint = config$s3$restart$endpoint,
                             use_https = TRUE)
  
  RCurl::url.exists("https://hc-ping.com/167dbfa6-1cb1-4bda-824e-33abf5f2069d", timeout = 5)
  
  noaa_ready <- FLAREr:::check_noaa_present(lake_directory,
                                            configure_run_file,
                                            config_set_name = config_set_name)
}