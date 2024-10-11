library(tidyverse)

# source('R/fct_awss3Connect_sensorcode.R')
# source('R/fct_awss3Connect.R')
# source('R/collect_insitu_targets.R')
# source('R/collect_profile_targets.R')

source('R/collect_profile_targets_aed_insitu.R')


# COLLECT INSITU SENSOR DATA
message('Collecting Sensor Data...')
all_site_codes <- c('sensor_repository_81684', 'sensor_repository_81685', 'sensor_repository_81682', # Bacon Down (temp, salt, DO)
                    'sensor_repository_81698', 'sensor_repository_81699', 'sensor_repository_81696', # Bacon Up
                    'sensor_repository_81768', 'sensor_repository_81769', 'sensor_repository_81767', # Nicholson Down
                    'sensor_repository_81782', 'sensor_repository_81783', 'sensor_repository_81780') # Nicholson Up

insitu_obs_df <- awss3Connect_sensorcode(sensorCodes = all_site_codes, code_df = sensorcode_df) |>
  select(-QC, -Date)

#print(nrow(insitu_obs_df))

lake_insitu_df <- collect_insitu_targets(obs_download = insitu_obs_df,
                                        site_location = 'CANN',
                                        assign_depth = 1.5)


# COLLECT PROFILE DATA
message('Collecting Profile Targets...')
profile_obs_df <- awss3Connect(filename = 'arms/wiski.csv')
lake_profile_sites <- c('BAC','BACD300','BACD500','BACU300','CASMID','ELL','GRE','KEN',
                        'KENU300','KS7','KS9','MACD50','MASD50','NIC','NIC-IN',
                        'NICD200','PAC','PO2','RIV','SAL','SCB2')

lake_profile_df <- collect_profile_targets_aed_insitu(profile_data_download = profile_obs_df,
                                           sites = lake_profile_sites)

data_combined <- bind_rows(lake_insitu_df, lake_profile_df) |>
  distinct(datetime,depth,variable, .keep_all = TRUE)

write_csv(data_combined,
          file.path(lake_directory,'targets', config_obs$site_id, paste0(config_obs$site_id,"-targets-insitu.csv")))
