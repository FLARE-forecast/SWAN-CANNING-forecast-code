source('R/fct_awss3Connect_sensorcode.R')
sensorcode_df <- read_csv('configuration/default/sensorcode.csv')

configure_run_file <- "configure_run.yml"
config_set_name <- "default"

lake_directory <- here::here()
config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml", config_set_name)
config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

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
          file.path(config_obs$file_path$targets_directory, config_obs$site_id,paste0(config_obs$site_id,"-targets-met.csv")))
