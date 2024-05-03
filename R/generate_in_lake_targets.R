library(tidyverse)

source('R/fct_awss3Connect_sensorcode.R')
source('R/fct_awss3Connect.R')
source('R/collect_insitu_targets.R')
source('R/collect_profile_targets.R')


# COLLECT INSITU SENSOR DATA
all_site_codes <- c('sensor_repository_81684', 'sensor_repository_81685', # Bacon Down (temp, salt)
                    'sensor_repository_81698', 'sensor_repository_81699', # Bacon Up
                    'sensor_repository_81768', 'sensor_repository_81769', # Nicholson Down
                    'sensor_repository_81782', 'sensor_repository_81783') # Nicholson Up

insitu_obs_df <- awss3Connect_sensorcode(sensorCodes = all_site_codes, code_df = sensorcode_df) |> 
  select(-QC, -Date)

print(nrow(insitu_obs_df))

lake_insitu_df <- collect_insitu_targets(obs_download = insitu_obs_df, 
                                        site_location = 'CANN', 
                                        assign_depth = 1.5)


# COLLECT PROFILE DATA 
profile_obs_df <- awss3Connect(filename = 'arms/wiski.csv')
lake_profile_sites <- c('BAC','BACD300','BACD500','BACU300','CASMID','ELL','GRE','KEN',
                        'KENU300','KS7','KS9','MACD50','MASD50','NIC','NIC-IN',
                        'NICD200','PAC','PO2','RIV','SAL','SCB2') 

lake_profile_df <- collect_profile_targets(profile_data_download = profile_obs_df, 
                                           sites = lake_profile_sites)

data_combined <- bind_rows(lake_insitu_df, lake_profile_df) |> 
  distinct(datetime,depth,variable, .keep_all = TRUE)

write_csv(data_combined, 
          file.path(config_obs$file_path$targets_directory, config_obs$site_id, paste0(config_obs$site_id,"-targets-insitu.csv")))
