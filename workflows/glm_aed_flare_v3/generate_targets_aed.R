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

# config_obs <- FLAREr::initialize_obs_processing(lake_directory, 
#                                                 observation_yml = "observation_processing.yml", 
#                                                 config_set_name = config_set_name)

dir.create(file.path(lake_directory, "targets", config$location$site_id), showWarnings = FALSE)

sensorcode_df <- read_csv('configuration/glm_flare_v3/sensorcode.csv', show_col_types = FALSE)


## RUN CODE TO GENERATE DATA



##################  INSITU  ####################

## IN-LAKE TARGETS -- INCLUDE NIC SITE
print('Generating Lake Targets')

# COLLECT INSITU SENSOR DATA
message('Collecting Sensor Data...')
all_site_codes <- c('sensor_repository_81684', 'sensor_repository_81685', 'sensor_repository_81682', # Bacon Down (temp, salt, DO)
                    'sensor_repository_81698', 'sensor_repository_81699', 'sensor_repository_81696', # Bacon Up
                    'sensor_repository_81768', 'sensor_repository_81769', 'sensor_repository_81767', # Nicholson Down
                    'sensor_repository_81782', 'sensor_repository_81783', 'sensor_repository_81780', # Nicholson Up
                    'sensor_repository_81681', 'sensor_repository_81695', 'sensor_repository_81765', 'sensor_repository_81779') # Depths

insitu_obs_df <- awss3Connect_sensorcode(sensorCodes = all_site_codes, code_df = sensorcode_df) |>
  select(-QC, -Date)

#print(nrow(insitu_obs_df))
 ### FIX THIS TO ASSIGN DEPTH VARIABLE A NA DEPTH
lake_insitu_df <- collect_insitu_targets(obs_download = insitu_obs_df,
                                         site_location = 'CANN',
                                         assign_depth = 1.5,
                                         aed_vars = TRUE)


# COLLECT PROFILE DATA
message('Collecting Profile Targets...')
source('R/collect_profile_targets_aed_insitu.R')

profile_obs_df <- awss3Connect(filename = 'arms/wiski.csv')
lake_profile_sites <- c('BAC','BACD300','BACD500','BACU300','CASMID','ELL','GRE','KEN',
                        'KENU300','KS7','KS9','MACD50','MASD50','NIC','NIC-IN',
                        'NICD200','PAC','PO2','RIV','SAL','SCB2')

lake_profile_df <- collect_profile_targets_aed_insitu(profile_data_download = profile_obs_df,
                                                      sites = lake_profile_sites)

## DEPTH BINNING ##
cuts <- tibble::tibble(cuts = as.integer(factor(config$model_settings$modeled_depths)),
                       depth = config$model_settings$modeled_depths)

lake_profile_depth_binned <- lake_profile_df |> 
  dplyr::mutate(cuts = cut(depth, breaks = config$model_settings$modeled_depths, include.lowest = TRUE, right = FALSE, labels = FALSE)) |>
  dplyr::group_by(cuts, variable, datetime, site_id) |>
  dplyr::summarize(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(cuts, by = "cuts") |>
  dplyr::select(site_id, datetime, variable, depth, observation)


data_combined <- bind_rows(lake_insitu_df, lake_profile_depth_binned) |>
  distinct(datetime,depth,variable, .keep_all = TRUE) #|> 
  #mutate(variable = ifelse(variable =='temperature', 'TEMP', variable))
  

write_csv(data_combined,
          file.path(lake_directory,'targets', config_obs$site_id, paste0(config_obs$site_id,"-targets-insitu.csv")))



##################  INFLOW  ####################

# Inflow Targets
print('Generating Inflow Targets')
source('R/collect_profile_targets_aed_inflow.R')

## COLLECT PROFILE DATA FROM INFLOW
#profile_obs_df <- awss3Connect(filename = 'arms/wiski.csv') ## READ IN ABOVE IN INSITU

#inflow_sites <- c('YULEB', 'STHNR', 'CANNR', 'ELL', 'PAC', 'MACD50', 'ELLISB', 'BICKB') # taken from the dashboard as locations upstream of the study site
inflow_sites <- c('STHNR', 'CANNR') # only want southern river and cann river sites


inflow_profile_data <- collect_profile_targets_aed_inflow(sites = inflow_sites,
                                               profile_data_download = profile_obs_df)

# inflow_profile_depth_avg <- inflow_profile_data |>
#   group_by(datetime, variable) |>
#   mutate(observation_avg = mean(observation)) |>
#   ungroup() |>
#   distinct(datetime, variable, .keep_all = TRUE) |>
#   select(-depth, -observation) |>
#   rename(observation = observation_avg)

# ## rename variables (TEMP, SALT)
# inflow_profile_depth_avg$variable <- ifelse(inflow_profile_depth_avg$variable == 'temperature', 'TEMP', inflow_profile_depth_avg$variable)
# inflow_profile_depth_avg$variable <- ifelse(inflow_profile_depth_avg$variable == 'salt', 'SALT', inflow_profile_depth_avg$variable)


## add FLOW data
sensorcode_df <- read_csv('configuration/default/sensorcode.csv')
cann_flow_codes <- 'sensor_repository_00804'
south_flow_codes <- 'sensor_repository_00752'

cann_inflow_download <- awss3Connect_sensorcode(sensorCodes = cann_flow_codes, code_df = sensorcode_df)
south_inflow_download <- awss3Connect_sensorcode(sensorCodes = south_flow_codes, code_df = sensorcode_df)

cann_inflow_download$flow_source <- 'cann_river'
south_inflow_download$flow_source <- 'south_river'

inflow_combined <- dplyr::bind_rows(cann_inflow_download, south_inflow_download)
inflow_combined$Date <- as.Date(inflow_combined$datetime, tz = "Australia/Perth")

#south_inflow_download$Date <- as.Date(south_inflow_download$datetime, tz = "Australia/Perth")

daily_inflow_rate_df <- inflow_combined |>
  group_by(Date, flow_source) |>
  mutate(average_rate = mean(Data, na.rm = TRUE)) |>
  ungroup() |>
  distinct(Date, flow_source, .keep_all = TRUE)

daily_inflow_total <- daily_inflow_rate_df |>
  mutate(daily_total = ifelse(average_rate > 1, average_rate*0.1, average_rate)) # adjust large flow rates

daily_inflow_combined <- daily_inflow_total |>
  group_by(Date) |>
  mutate(combined_rate = ifelse((length(unique(flow_source)) > 1), sum(daily_total), daily_total)) |> # sum both flows if multiple present for day
  ungroup() |>
  drop_na(combined_rate)

inflow_insitu_flow_df <- daily_inflow_combined |>
  pivot_wider(id_cols = c('Date'), names_from = 'flow_source', values_from = 'daily_total') |>
  group_by(Date) |>
  mutate(observation = sum(cann_river + south_river)) |>
  ungroup() |>
  arrange(Date) |>
  mutate(variable = 'FLOW',
         site_id = 'CANN',
         datetime = lubridate::as_datetime(paste0(format(Date, "%Y-%m-%d %H"), ":00:00"))) |>
  select(datetime, site_id, variable, observation)

# inflow_insitu_flow_df <- daily_inflow_total |>
#   pivot_wider(id_cols = c('Date'), names_from = 'flow_source', values_from = 'daily_total') |>
#   # group_by(Date) |>
#   # mutate(observation = sum(cann_river + south_river)) |>
#   # ungroup() |>
#   arrange(Date) |>
#   mutate(variable = 'FLOW',
#          site_id = 'CANN',
#          observation = south_river,
#          datetime = lubridate::as_datetime(paste0(format(Date, "%Y-%m-%d %H"), ":00:00"))) |>
#   select(datetime, site_id, variable, observation)

combined_data <- bind_rows(inflow_profile_data, inflow_insitu_flow_df)  #|> ## ADD TEMP/SALT INSITU DATA HERE ONCE FOUND
  # summarise(observation = mean(observation, na.rm = TRUE), .by = c("datetime","variable")) |>
  # mutate(site_id = 'CANN')

write_csv(combined_data,
          file.path(lake_directory,'targets', config_obs$site_id, paste0(config_obs$site_id,"-targets-inflow.csv")))



##################  MET  ####################

# Met Targets
print('Generating Met Targets')

met_download <- awss3Connect_sensorcode(sensorCodes = c('sensor_repository_84745', 'sensor_repository_84749', 'sensor_repository_00954', 'sensor_repository_00962'),
                                        code_df = sensorcode_df)
# remove duplicates
met_dedup <- met_download |> distinct(datetime, variable, .keep_all = TRUE)

## assign columns
met_dedup$site_id <- 'CANN'

cleaned_met_file <- met_dedup |>
  select(datetime, site_id, Data, variable) |>
  pivot_wider(names_from = variable, values_from = Data) |>
  rename(air_temperature = `Air Temperature Avg`,
         wind_speed = `Wind Speed avg`,
         relative_humidity = `Relative Humidity Avg`,
         surface_downwelling_shortwave_flux_in_air = `Solar Irradiance`) |>
  pivot_longer(cols = c(air_temperature, wind_speed, relative_humidity, surface_downwelling_shortwave_flux_in_air),
               names_to = 'variable',
               values_to = 'observation') |>
  dplyr::filter(!is.nan(observation)) |>
  mutate(datetime =  lubridate::round_date(datetime), unit = 'hour') |>
  select(datetime, site_id, observation, variable)

write_csv(cleaned_met_file,
          file.path(lake_directory,'targets', config_obs$site_id,paste0(config_obs$site_id,"-targets-met.csv")))
