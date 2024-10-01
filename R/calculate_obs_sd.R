# source('R/fct_awss3Connect_sensorcode.R')
# source('R/fct_awss3Connect.R')
# source('R/collect_insitu_targets.R')
# source('R/collect_profile_targets.R')
# 
# 
# # COLLECT INSITU SENSOR DATA
# all_site_codes <- c('sensor_repository_81684', 'sensor_repository_81685', # Bacon Down (temp, salt)
#                     'sensor_repository_81698', 'sensor_repository_81699', # Bacon Up
#                     'sensor_repository_81768', 'sensor_repository_81769', # Nicholson Down
#                     'sensor_repository_81782', 'sensor_repository_81783') # Nicholson Up
# 
# insitu_obs_df <- awss3Connect_sensorcode(sensorCodes = all_site_codes, code_df = sensorcode_df) |> 
#   select(-QC, -Date)
# 
# # remove duplicates 
# obs_dedup <- insitu_obs_df |> 
#   distinct(datetime, Height, variable, s_table_name, .keep_all = TRUE) |> 
#   filter(!is.nan(Data))
# 
# 
# ## temperature data 
# obs_day_group_temp <- obs_dedup |> 
#   mutate(date = as.Date(datetime)) |> 
#   filter(variable == 'Temperature', 
#          date < as.Date('2024-01-01'), 
#          date >= as.Date('2023-01-01')) |> 
#   group_by(date, s_table_name) |> 
#   summarise(daily_site_mean = mean(Data, na.rm = TRUE)) |> 
#   ungroup() |> 
#   group_by(date) |> 
#   mutate(daily_mean = mean(daily_site_mean, na.rm = TRUE), 
#          daily_sd = sd(daily_site_mean)) |> 
#   ungroup() |> 
#   distinct(date, .keep_all = TRUE)
# 
# print(mean(obs_day_group_temp$daily_sd))
# 
# ## salt data 
# obs_day_group_salt <- obs_dedup |> 
#   mutate(date = as.Date(datetime)) |> 
#   filter(variable == 'Salinity (ppt)', 
#          date < as.Date('2024-01-01'), 
#          date >= as.Date('2023-01-01')) |> 
#   group_by(date, s_table_name) |> 
#   summarise(daily_site_mean = mean(Data)) |> 
#   ungroup() |> 
#   group_by(date) |> 
#   mutate(daily_mean = mean(daily_site_mean), 
#          daily_sd = sd(daily_site_mean)) |> 
#   ungroup() |> 
#   distinct(date, .keep_all = TRUE)
# 
# print(mean(obs_day_group_salt$daily_sd))
# 
# 
# 
# 
# ## Profile data -- NOT CURRENTLY INCLUDED BUT CAN ADD THIS LATER IF NEEDED
# profile_data <- profile_obs_df %>%  
#   dplyr::filter(`Program Site Ref` %in% lake_profile_sites &
#                   `Collection Method` %in% 'Insitu' &
#                   `Data Category` %in% 'Instrument log') |> 
#   select(site_ref = `Site Ref`, 
#          program = `Program Site Ref`, 
#          time = `Collect Time`, 
#          date = `Collect Date`, 
#          depth = `Sample Depth (m)`, 
#          salt = `Salinity (ppt)`, 
#          temperature = `Temperature (deg C)`) |> 
#   mutate(time = format(strptime(time, "%I:%M:%S %p"), "%H:%M:%S")) |> # convert from AM/PM to 24-hour 
#   mutate(datetime = paste(date, time))
# 
# profile_data_grouped_temp <- profile_data |> 
#   mutate(depth_rounded = plyr::round_any(depth, 0.25))  |> # bin depths by rounding -- matches depth configuration 
#   select(-depth) |> 
#   rename(depth = depth_rounded) |> 
#   filter(!is.na(depth), 
#          depth <= 6.0) |> 
#   pivot_longer(cols = c("salt", "temperature"), names_to = 'variable', values_to = 'data') |> 
#   mutate(datetime = lubridate::force_tz(lubridate::as_datetime(datetime, format = '%d/%m/%Y %H:%M:%S')), tzone = 'Australia/Perth') |>
#   mutate(date = as.Date(datetime)) |> 
#   filter(variable == 'temperature', 
#          depth == 1.5) |> 
#   summarise(daily_site_mean = mean(data, na.rm = TRUE), .by = c('date', 'site_ref'))
#   
# 
# 
# profile_data_grouped_salt <- profile_data |> 
#   mutate(depth_rounded = plyr::round_any(depth, 0.25))  |> # bin depths by rounding -- matches depth configuration 
#   select(-depth) |> 
#   rename(depth = depth_rounded) |> 
#   filter(!is.na(depth), 
#          depth <= 6.0) |> 
#   pivot_longer(cols = c("salt", "temperature"), names_to = 'variable', values_to = 'data') |> 
#   mutate(datetime = lubridate::force_tz(lubridate::as_datetime(datetime, format = '%d/%m/%Y %H:%M:%S')), tzone = 'Australia/Perth') |>
#   mutate(date = as.Date(datetime)) |> 
#   filter(variable == 'salt', 
#          depth == 1.5) |> 
#   summarise(daily_site_mean = mean(data, na.rm = TRUE), .by = c('date', 'site_ref'))