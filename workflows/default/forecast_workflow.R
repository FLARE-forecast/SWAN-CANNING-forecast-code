library(tidyverse)
library(lubridate)
library(tidymodels)
library(xgboost)
library(RcppRoll)

fresh_run <- FALSE

Sys.setenv("AWS_DEFAULT_REGION" = "renc",
           "AWS_S3_ENDPOINT" = "osn.xsede.org",
           "USE_HTTPS" = TRUE,
           "SC_S3_ENDPOINT" = "projects.pawsey.org.au")

lake_directory <- here::here()
setwd(lake_directory)
forecast_site <- c("CANN")
site_id <- "CANN"

#configure_run_file <- paste0("configure_run_",forecast_site,".yml")
configure_run_file <- "configure_run.yml"
config_set_name <- "default"

config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

if(fresh_run) unlink(file.path(lake_directory, "restart", "CANN", config$run_config$sim_name, configure_run_file))

# Read in the targets
source('workflows/default/generate_targets.R')

# Move targets to s3 bucket
message("Successfully generated targets")

FLAREr::put_targets(site_id =  config$location$site_id,
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

noaa_ready <- TRUE
while(noaa_ready){
  
  setwd(lake_directory)
  config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
  
  # ## run inflow forecast
  source('R/run_inflow_forecast_full.R')
  
  # Run FLARE
  output <- FLAREr::run_flare(lake_directory = lake_directory,
                              configure_run_file = configure_run_file,
                              config_set_name = config_set_name)
  
  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1)
  start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) - lubridate::days(7) ## SET LONGER LOOK BACK FOR DATA
  restart_file <- paste0(config$location$site_id,"-", (lubridate::as_date(forecast_start_datetime)- days(1)), "-",config$run_config$sim_name ,".nc")
  
  FLAREr::update_run_config2(lake_directory = lake_directory,
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
                             bucket = config$s3$warm_start$bucket,
                             endpoint = config$s3$warm_start$endpoint,
                             use_https = TRUE)
  
  #RCurl::url.exists("https://hc-ping.com/551392ce-43f3-49b1-8a57-6a60bad1c377", timeout = 5)
  
  noaa_ready <- FLAREr::check_noaa_present_arrow(lake_directory,
                                                 configure_run_file,
                                                 config_set_name = config_set_name)
}
