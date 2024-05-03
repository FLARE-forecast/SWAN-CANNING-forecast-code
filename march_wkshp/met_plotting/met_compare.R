source('R/fct_awss3Connect.R')
source('R/fct_awss3Connect_sensorcode.R')

Sys.setenv("AWS_DEFAULT_REGION" = "renc",
           "AWS_S3_ENDPOINT" = "osn.xsede.org",
           "USE_HTTPS" = TRUE,
           "SC_S3_ENDPOINT" = "projects.pawsey.org.au")

### CANN data for NOAA ##

reference_datetime <- lubridate::as_datetime('2024-03-25 00:00:00')
forecast_hour <- lubridate::hour(0)
endpoint <- 'renc.osn.xsede.org'
## pull in past NOAA data
met_s3_past <- arrow::s3_bucket(paste0("bio230121-bucket01/flare/drivers/met/gefs-v12/stage3/site_id=CANN"),
                                endpoint_override = endpoint,
                                anonymous = TRUE)


years_prior <- reference_datetime - lubridate::days(365)

df_past <- arrow::open_dataset(met_s3_past) |> 
  #select(datetime, parameter, variable, prediction) |> 
  filter(variable %in% c("air_temperature"),
         # ((datetime <= min_datetime  & variable == "precipitation_flux") | 
         #    datetime < min_datetime  & variable == "air_temperature"),
         datetime > years_prior) |> 
  collect() |> 
  rename(ensemble = parameter) |> 
  mutate(variable = ifelse(variable == "air_temperature", "temperature_2m", variable),
         prediction = ifelse(variable == "temperature_2m", prediction - 273.15, prediction)) |> 
  select(-reference_datetime) |> 
  mutate(Date = as.Date(datetime)) |> 
  filter(lubridate::hour(datetime) == 0) |> 
  group_by(Date, variable) |> 
  mutate(prediction = mean(prediction, na.rm = TRUE)) |> 
  ungroup() |> 
  distinct(Date, variable, .keep_all = TRUE)


### Observed data for CANN
sensorcode_df <- read_csv('configuration/default/sensorcode.csv', show_col_types = FALSE)

met_sites <- c('sensor_repository_84745')

met_obs_df <- awss3Connect_sensorcode(sensorCodes = met_sites, code_df = sensorcode_df) |> 
  select(-QC, -Date)

cleaned_met_file <- met_obs_df |> 
  mutate(Date = as.Date(datetime)) |> 
  filter(lubridate::hour(datetime) == 0) |> # only want midnight observations for the daily value
  group_by(Date, variable) |> 
  mutate(observation = mean(Data, na.rm = TRUE)) |> 
  ungroup() |> 
  distinct(Date, variable, .keep_all = TRUE) |> 
  mutate(datetime = as.POSIXct(paste(Date, '00:00:00'), tz = "UTC")) |> 
  select(datetime, met_obs = observation)
  #mutate(depth = 1.5) |> # assign depth to match model config depths (median depth value is 1.6)
  #select(datetime, site_id, depth, observation, variable)

met_noaa_obs <- df_past |> 
  right_join(cleaned_met_file, by=c('datetime')) |> 
  drop_na(variable)


met_noaa_obs |> 
  ggplot(aes(x = Date)) +
  geom_point(aes(y=met_obs), alpha = 0.5) +
  geom_line(aes(y=prediction), color = 'red') +
  ylab('Air Temp (degC)')
  


### Windspeed 
df_past_ws <- arrow::open_dataset(met_s3_past) |> 
  #select(datetime, parameter, variable, prediction) |> 
  filter(variable %in% c("eastward_wind", 'northward_wind'),
         # ((datetime <= min_datetime  & variable == "precipitation_flux") | 
         #    datetime < min_datetime  & variable == "air_temperature"),
         datetime > years_prior) |> 
  collect() |> 
  rename(ensemble = parameter) |> 
  # mutate(variable = ifelse(variable == "air_temperature", "temperature_2m", variable),
  #        prediction = ifelse(variable == "temperature_2m", prediction - 273.15, prediction)) |> 
  select(-reference_datetime) |> 
  mutate(Date = as.Date(datetime)) |> 
  filter(lubridate::hour(datetime) == 0) |> 
  group_by(Date, variable) |> 
  mutate(prediction = mean(prediction, na.rm = TRUE)) |> 
  ungroup() |> 
  pivot_wider(names_from = variable, values_from = prediction) |> 
  mutate(windspeed = sqrt(eastward_wind**2 + northward_wind**2)) |> 
  distinct(Date, .keep_all = TRUE)

### Observed data for CANN
sensorcode_df <- read_csv('configuration/default/sensorcode.csv', show_col_types = FALSE)

wind_sites <- c('sensor_repository_84749')

wind_obs_df <- awss3Connect_sensorcode(sensorCodes = wind_sites, code_df = sensorcode_df) |> 
  select(-QC, -Date)

cleaned_met_file_ws <- wind_obs_df |> 
  mutate(Date = as.Date(datetime)) |> 
  filter(lubridate::hour(datetime) == 0) |> # only want midnight observations for the daily value
  group_by(Date, variable) |> 
  mutate(observation = mean(Data, na.rm = TRUE)) |> 
  ungroup() |> 
  distinct(Date, variable, .keep_all = TRUE) |> 
  mutate(datetime = as.POSIXct(paste(Date, '00:00:00'), tz = "UTC")) |> 
  select(datetime, wind_obs = observation)
#mutate(depth = 1.5) |> # assign depth to match model config depths (median depth value is 1.6)
#select(datetime, site_id, depth, observation, variable)

met_noaa_obs <- df_past_ws |> 
  right_join(cleaned_met_file_ws, by=c('datetime')) |> 
  drop_na(variable)

met_noaa_obs |> 
  ggplot(aes(x = Date)) +
  geom_point(aes(y=windspeed), alpha = 0.5) +
  geom_line(aes(y=wind_obs), color = 'red') +
  ylab('Wind Speed (m/s)')




### Shortwave 
df_past_sw <- arrow::open_dataset(met_s3_past) |> 
  #select(datetime, parameter, variable, prediction) |> 
  filter(variable %in% c('surface_downwelling_shortwave_flux_in_air'),
         # ((datetime <= min_datetime  & variable == "precipitation_flux") | 
         #    datetime < min_datetime  & variable == "air_temperature"),
         datetime > years_prior) |> 
  collect() |> 
  rename(ensemble = parameter) |> 
  # mutate(variable = ifelse(variable == "air_temperature", "temperature_2m", variable),
  #        prediction = ifelse(variable == "temperature_2m", prediction - 273.15, prediction)) |> 
  select(-reference_datetime) |> 
  mutate(Date = as.Date(datetime)) |> 
  filter(lubridate::hour(datetime) == 0) |> 
  #group_by(Date, variable) |> 
  #mutate(prediction = mean(prediction, na.rm = TRUE)) |> 
  #ungroup() |> 
  pivot_wider(names_from = variable, values_from = prediction) |> 
  distinct(Date, .keep_all = TRUE)

### Observed data for CANN
sensorcode_df <- read_csv('configuration/default/sensorcode.csv', show_col_types = FALSE)

sw_sites <- c('sensor_repository_00962')

sw_obs_df <- awss3Connect_sensorcode(sensorCodes = sw_sites, code_df = sensorcode_df) |> 
  select(-QC, -Date)

cleaned_met_file_sw <- sw_obs_df |> 
  mutate(Date = as.Date(datetime)) |> 
  #filter(lubridate::hour(datetime) == 12) |> # only want midnight observations for the daily value
  drop_na(Data) |> 
  #group_by(Date, variable) |> 
  #mutate(observation = mean(Data, na.rm = TRUE)) |> 
  #ungroup() |> 
  #distinct(Date, variable, .keep_all = TRUE) |> 
  mutate(datetime = as.POSIXct(paste(Date, '00:00:00'), tz = "UTC")) |> 
  select(datetime, sw_obs = Data)
#mutate(depth = 1.5) |> # assign depth to match model config depths (median depth value is 1.6)
#select(datetime, site_id, depth, observation, variable)

met_noaa_obs <- df_past_sw |> 
  right_join(cleaned_met_file_sw, by=c('datetime')) |> 
  drop_na(surface_downwelling_shortwave_flux_in_air)

met_noaa_obs |> 
  ggplot(aes(x = Date)) +
  geom_point(aes(y=sw_obs), alpha = 0.5) +
  geom_line(aes(y=surface_downwelling_shortwave_flux_in_air), color = 'red') +
  ylab('Short Wave (W/m2)')

df_past_sherry <- arrow::open_dataset(met_s3_past) |> filter(variable %in% c('surface_downwelling_shortwave_flux_in_air', 'eastward_wind', 'northward_wind')) |> collect()


write_csv(df_past_sherry, '/home/addelany/SWAN-CANNING-forecast-code/march_wkshp/met_plotting/cann_met_file.csv')
# 
# # combine past and future noaa data
# df_combined <- bind_rows(df_future, df_past) |> 
#   arrange(variable, datetime, ensemble)
# 
# forecast_precip <- df_combined |> 
#   filter(variable == 'precipitation') |> 
#   summarise(precip_hourly = median(prediction, na.rm = TRUE), .by = c("datetime")) |> # get the median hourly precip across all EMs
#   mutate(date = lubridate::as_date(datetime)) |> 
#   summarise(precip = sum(precip_hourly, na.rm = TRUE), .by = c("date")) |> # get the total precip for each day
#   mutate(sevenday_precip = RcppRoll::roll_sum(precip, n = 7, fill = NA,align = "right")) |> 
#   mutate(doy = lubridate::yday(date))
# 
# forecast_temp <- df_combined |> 
#   filter(variable == 'temperature_2m') |> 
#   summarise(temp_hourly = median(prediction, na.rm = TRUE), .by = c("datetime")) |> # get the median hourly temp across all EMs
#   mutate(date = lubridate::as_date(datetime)) |> 
#   summarise(temperature = median(temp_hourly, na.rm = TRUE), .by = c("date")) # get median temp across hours of the day
# 
# forecast_met <- forecast_precip |> 
#   right_join(forecast_temp, by = c('date'))
