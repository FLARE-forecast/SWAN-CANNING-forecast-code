library(tidyverse)
library(tidymodels)
library(xgboost)

# source('R/fct_awss3Connect_sensorcode.R')
# source('R/fct_awss3Connect.R')
# 
source('R/xg_run_inflow_model.R')
source('R/xg_combine_model_runs.R')

lake_directory <- here::here()
config_set_name <- "glm_flare_v3"
#forecast_site <- c("CANN")
# configure_run_file <- "configure_run.yml"
# config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
# 
# config_obs <- FLAREr::initialize_obs_processing(lake_directory, 
#                                                 observation_yml = "observation_processing.yml", 
#                                                 config_set_name = config_set_name)

#FLAREr::get_targets(lake_directory, config)


message("Forecasting inflow and outflows")

sensorcode_df <- read_csv('configuration/default/sensorcode.csv', show_col_types = FALSE)

reference_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
noaa_date <- reference_datetime - lubridate::days(1)
site_identifier <- 'CANN'
endpoint <- 'renc.osn.xsede.org'


message('done initializing....starting forecasts')

## Run Forecast
xg_flow_forecast <- xg_combine_model_runs(site_id = site_identifier, 
                                    forecast_start_datetime = reference_datetime, 
                                    use_s3_inflow = config$run_config$use_s3, 
                                    inflow_bucket = config$s3$inflow_drivers$bucket,
                                    inflow_endpoint = config$s3$inflow_drivers$endpoint,
                                    inflow_local_directory = file.path(lake_directory, "drivers/inflow"), 
                                    forecast_horizon = config$run_config$forecast_horizon, 
                                    inflow_model = config$inflow$forecast_inflow_model,
                                    include_aed_vars = FALSE)

xg_inflow_future <- xg_flow_forecast[[1]] 
xg_inflow_historic <- xg_flow_forecast[[2]]

oxygenation_df <- oxygenation_inflow(inflow_forecast_future = xg_inflow_future, 
                                     inflow_forecast_historic = xg_inflow_historic,
                                     use_oxygenation = FALSE)

flow_combined_future <- bind_rows(xg_inflow_future,oxygenation_df[[1]])
arrow::write_dataset(flow_combined_future,
                     file.path(lake_directory, "drivers/inflow/future",paste0('model_id=',config$flows$forecast_inflow_model)),
                     partitioning = c('reference_datetime', 'site_id'))

flow_combined_historic <- bind_rows(xg_inflow_historic,oxygenation_df[[2]])
arrow::write_dataset(flow_combined_historic, path = file.path(lake_directory,
                                                       "drivers/inflow/historic",paste0('model_id=',config$flows$forecast_inflow_model),"site_id=CANN"))
