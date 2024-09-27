library(tidyverse)
library(lubridate)

source('R/fct_awss3Connect_sensorcode.R')
source('R/fct_awss3Connect.R')
source('R/collect_insitu_targets.R')
source('R/collect_profile_targets.R')

# lake_directory <- here::here()
# config_set_name <- "default"
# forecast_site <- c("CANN")
# configure_run_file <- "configure_run.yml"
# config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

config_obs <- FLAREr::initialize_obs_processing(lake_directory, 
                                                observation_yml = "observation_processing.yml", 
                                                config_set_name = config_set_name)

dir.create(file.path(lake_directory, "targets", config$location$site_id), showWarnings = FALSE)

sensorcode_df <- read_csv('configuration/default/sensorcode.csv', show_col_types = FALSE)


## RUN CODE TO GENERATE DATA

## IN-LAKE TARGETS
print('Generating Lake Targets')
source('R/generate_in_lake_targets.R')

# Inflow Targets
print('Generating Inflow Targets')
source('R/generate_inflow_targets.R')

# Met Targets
print('Generating Met Targets')
source('R/generate_met_targets.R')



