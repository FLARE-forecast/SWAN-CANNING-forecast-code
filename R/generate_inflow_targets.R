

## COLLECT PROFILE DATA FROM INFLOW
profile_obs_df <- awss3Connect(filename = 'arms/wiski.csv')

inflow_sites <- c('YULEB', 'STHNR', 'CANNR') # taken from the dashboard as locations upstream of the study site

inflow_profile_data <- collect_profile_targets(sites = inflow_sites, 
                                               profile_data_download = profile_obs_df)

inflow_profile_depth_avg <- inflow_profile_data |> 
  group_by(datetime, variable) |> 
  mutate(observation_avg = mean(observation)) |>
  ungroup() |> 
  distinct(datetime, variable, .keep_all = TRUE) |> 
  select(-depth, -observation) |> 
  rename(observation = observation_avg)

## rename variables (TEMP, SALT)
inflow_profile_depth_avg$variable <- ifelse(inflow_profile_depth_avg$variable == 'temperature', 'TEMP', inflow_profile_depth_avg$variable)
inflow_profile_depth_avg$variable <- ifelse(inflow_profile_depth_avg$variable == 'salt', 'SALT', inflow_profile_depth_avg$variable)





## COLLECT SENSOR DATA FROM INFLOW

##### NO SENSOR CODES FOR UPSTREAM TEMP/SALT DATA FOUND YET ###
## JUST USE THE MOST UPSTREAM LAKE SITE FOR NOW TO REPRESNT INFLOW ##
source('R/collect_insitu_targets.R')
upstream_site_code <- c('sensor_repository_81782', 'sensor_repository_81783') #Nicholson Upstream

mock_inflow_obs_df <- awss3Connect_sensorcode(sensorCodes = upstream_site_code, code_df = sensorcode_df) |> 
  select(-QC, -Date)

mock_inflow_insitu_df <- collect_insitu_targets(obs_download = mock_inflow_obs_df, 
                                         site_location = 'CANN', 
                                         assign_depth = 1.5)
mock_inflow_insitu_df <- mock_inflow_insitu_df |>
  select(-depth)

mock_inflow_insitu_df$variable <- ifelse(mock_inflow_insitu_df$variable == 'temperature', 'TEMP', mock_inflow_insitu_df$variable)
mock_inflow_insitu_df$variable <- ifelse(mock_inflow_insitu_df$variable == 'salt', 'SALT', mock_inflow_insitu_df$variable)


## add FLOW data
sensorcode_df <- read_csv('configuration/default/sensorcode.csv')
cann_flow_codes <- 'sensor_repository_00804'
south_flow_codes <- 'sensor_repository_00752'

cann_inflow_download <- awss3Connect_sensorcode(sensorCodes = cann_flow_codes, code_df = sensorcode_df)
south_inflow_download <- awss3Connect_sensorcode(sensorCodes = cann_flow_codes, code_df = sensorcode_df)

cann_inflow_download$flow_source <- 'cann_river'
south_inflow_download$flow_source <- 'south_river'

inflow_combined <- dplyr::bind_rows(cann_inflow_download, south_inflow_download)

inflow_combined$Date <- as.Date(inflow_combined$datetime, tz = "Australia/Perth")

daily_inflow_rate_df <- inflow_combined |> 
  group_by(Date, flow_source) |> 
  mutate(average_rate = mean(Data, na.rm = TRUE)) |> 
  ungroup() |> 
  distinct(Date, flow_source, .keep_all = TRUE)

# daily_inflow_total <- daily_inflow_rate_df |> 
#   mutate(daily_total = average_rate*86400) # second rate (m3/s) to day rate (86400 seconds per day) --> (m3/day)

daily_inflow_total <- daily_inflow_rate_df |> 
  mutate(daily_total = ifelse(average_rate > 1, average_rate*0.1, average_rate)) # second rate (m3/s) to day rate (86400 seconds per day) --> (m3/day)

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
         site_id = 'CANN') |> 
  select(datetime = Date, site_id, variable, observation)


## combine insitu data with profile data 
combined_data <- bind_rows(inflow_profile_depth_avg, inflow_insitu_flow_df, mock_inflow_insitu_df) ## ADD TEMP/SALT INSITU DATA HERE ONCE FOUND

write_csv(combined_data, 
          file.path(config_obs$file_path$targets_directory, config_obs$site_id, paste0(config_obs$site_id,"-targets-inflow.csv")))
