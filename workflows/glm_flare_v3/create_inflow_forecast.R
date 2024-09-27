## code for creating inflow forecasts for swan-canning using xgboost model

library(tidyverse)
library(tidymodels)
library(xgboost)

source('R/fct_awss3Connect_sensorcode.R')
source('R/run_inflow_model.R')

lake_directory <- here::here()

config_set_name <- "default"
configure_run_file <- "configure_run.yml"
config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

## get all necessary data
#reference_datetime <- Sys.Date()
#reference_datetime <- config$run_config$forecast_start_datetime
#noaa_date <- reference_datetime - lubridate::days(1)

forecast_start_datetime <- Sys.Date()
forecast_horizon <- 35
inflow_model <- config$inflow$forecast_inflow_model
noaa_date <- forecast_start_datetime - lubridate::days(1)


#forecast_start_datetime = config$run_config$forecast_start_datetime
#forecast_horizon = config$run_config$forecast_horizon
#inflow_model = config$inflow$forecast_inflow_model

noaa_date <- lubridate::as_datetime(forecast_start_datetime) - lubridate::days(1)

lake_name_code <- config$location$site_id

forecast_date <- lubridate::as_date(forecast_start_datetime)
forecast_hour <- lubridate::hour(forecast_start_datetime)

# create forecast path
if (forecast_horizon > 0) {
  inflow_forecast_path <- file.path(inflow_model, lake_name_code, forecast_hour, forecast_date)
}else {
  inflow_forecast_path <- NULL
}

inflow_bucket <- config$s3$inflow_drivers$bucket
inflow_endpoint <- config$s3$inflow_drivers$endpoint

use_s3_inflow <- config$run_config$use_s3

if(use_s3_inflow){
  FLAREr:::arrow_env_vars()
  inflow_s3 <- arrow::s3_bucket(bucket = file.path(inflow_bucket, inflow_forecast_path), endpoint_override = inflow_endpoint)
  on.exit(FLAREr:::unset_arrow_vars(vars))
}else{
  inflow_s3 <- arrow::SubTreeFileSystem$create(file.path(inflow_local_directory, inflow_forecast_path))
}


run_inflow_model(forecast_start_date = forecast_start_datetime, 
                  site_id = lake_name_code, 
                  endpoint = inflow_endpoint, 
                  s3_save_path = inflow_s3)