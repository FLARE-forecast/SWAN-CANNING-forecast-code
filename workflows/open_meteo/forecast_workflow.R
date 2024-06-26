library(tidyverse)
library(lubridate)

devtools::install_github('FLARE-forecast/RopenMeteo')
library(RopenMeteo)

lake_directory <- here::here()
setwd(lake_directory)
forecast_site <- c("CANN")

#configure_run_file <- paste0("configure_run_",forecast_site,".yml")
configure_run_file <- "configure_run.yml"
config_set_name <- "open_meteo"

config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

# Generate the targets
source('workflows/default/generate_targets.R')
# Read in the targets
cuts <- tibble::tibble(cuts = as.integer(factor(config$model_settings$modeled_depths)),
                       depth = config$model_settings$modeled_depths)

cleaned_insitu_file <- file.path(lake_directory, "targets", config$location$site_id, config$da_setup$obs_filename)
readr::read_csv(cleaned_insitu_file, show_col_types = FALSE) |> 
  dplyr::mutate(cuts = cut(depth, breaks = config$model_settings$modeled_depths, include.lowest = TRUE, right = FALSE, labels = FALSE)) |>
  dplyr::filter(lubridate::hour(datetime) == 0) |>
  dplyr::group_by(cuts, variable, datetime, site_id) |>
  dplyr::summarize(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(cuts, by = "cuts") |>
  dplyr::select(site_id, datetime, variable, depth, observation) |>
  write_csv(cleaned_insitu_file)

# Move targets to s3 bucket

message("Successfully generated targets")

FLAREr::put_targets(site_id =  config$location$site_id,
                    cleaned_insitu_file = cleaned_insitu_file,
                    cleaned_met_file = NA,
                    cleaned_inflow_file = NA,
                    use_s3 = config$run_config$use_s3,
                    config = config)

if(config$run_config$use_s3){
  message("Successfully moved targets to s3 bucket")
}

noaa_ready <- TRUE
while(noaa_ready){
  
  config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
  
  # Run FLARE
  output <- FLAREr::run_flare(lake_directory = lake_directory,
                              configure_run_file = configure_run_file,
                              config_set_name = config_set_name)
  
  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1)
  start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) - lubridate::days(1)
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