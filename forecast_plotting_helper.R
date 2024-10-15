library(dplyr)
library(arrow)
library(ggplot2)

## read in scores
#s3_cann <- arrow::s3_bucket('bio230121-bucket01/flare/forecasts/parquet/site_id=CANN/model_id=glm_aed_flare_v3/',
#                            endpoint_override = "renc.osn.xsede.org", anonymous = TRUE)

# score_df <- arrow::open_dataset(s3_cann) |>
#   filter(variable == 'CHLA') |>
#   collect()


# s3_cann <- arrow::open_dataset('/forecasts/parquet/site_id=CANN/model_id=glm_aed_flare_v3/') |> 
#   filter(reference_datetime < as.Date('2024-10-08'), 
#          variable == 'OXY_oxy') |> 
# dplyr::collect()





## FORECAST CODE ##

s3_cann <- arrow::open_dataset(s3_bucket('bio230121-bucket01/flare/forecasts/parquet/site_id=CANN/model_id=glm_aed_flare_v3/', 
                                         endpoint_override = 'renc.osn.xsede.org', 
                                         anonymous = TRUE)) |> 
  filter(reference_datetime >= as.Date('2024-10-11'), 
         variable == 'CHLA') |> 
  dplyr::collect()

obs <- read_csv('./targets/CANN/CANN-targets-insitu.csv') |> 
  filter(variable == 'CHLA', 
         #datetime >= as.Date('2024-10-08'),
         observation != 0.0) |> 
  #mutate(variable = 'salt')
  select(-site_id)
  
#targets_df <- read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),show_col_types = FALSE)

#depths = c(0.5, 3.5)

forecast_obs <- s3_cann |> full_join(obs, by = c('datetime','depth','variable'))

#plot
forecast_obs |>
  # Filter the score_df and get in the right format
  # dplyr::filter(depth %in% obs$depth,#obs$depth, 
  #               variable %in% obs$variable) |>
  dplyr::mutate(datetime = lubridate::with_tz(lubridate::as_datetime(datetime), "America/New_York"),
                reference_datetime = lubridate::with_tz(lubridate::as_datetime(reference_datetime), "America/New_York"),
                depth = paste0("Depth: ", depth)) |>
  dplyr::filter(datetime >= reference_datetime) |>
  mutate(parameter = paste0(reference_date, parameter)) |> 
  #ggplot(aes(x = datetime, group=parameter, color = reference_date)) +
  ggplot(aes(x = datetime, group=parameter)) +
  #geom_ribbon(aes(ymin = quantile10, ymax = quantile90), fill = "lightblue", color = "lightblue") +
  geom_line(aes(y = prediction), size = 0.1) +
  geom_point(aes(y = observation), color = 'black') +
  #scale_x_continuous(breaks = my_breaks, labels = my_labels) +
  facet_wrap(~depth +variable) +
  labs(y = 'Value') +
  #ylim(c(-5,40)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.2)) +
  theme(text = element_text(size = 20)) +
  labs(caption = "Disclaimer: Experimental Forecast")




## SCORE CODE ##

s3_cann_scores <- arrow::open_dataset(s3_bucket('bio230121-bucket01/flare/scores/parquet/site_id=CANN/model_id=glm_aed_flare_v3/', 
                                         endpoint_override = 'renc.osn.xsede.org', 
                                         anonymous = TRUE)) |> 
  filter(reference_datetime >= as.Date('2024-10-11'), 
         variable == 'salt') |> 
  dplyr::collect()

# obs <- read_csv('./targets/CANN/CANN-targets-insitu.csv') |> 
#   filter(datetime >= as.Date('2024-10-08'), 
#          observation != 0.0, 
#          variable == 'OXY_oxy') |> 
#   #mutate(variable = 'salt')
#   select(-site_id)

#targets_df <- read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),show_col_types = FALSE)

#depths = c(0.5, 3.5)

#score_obs <- s3_cann_scores |> full_join(obs, by = c('datetime','depth','variable'))

#plot
s3_cann_scores |>
  drop_na(depth) |> 
  # Filter the score_df and get in the right format
  # dplyr::filter(depth %in% obs$depth,#obs$depth, 
  #               variable %in% obs$variable) |>
  dplyr::mutate(datetime = lubridate::with_tz(lubridate::as_datetime(datetime), "America/New_York"),
                reference_datetime = lubridate::with_tz(lubridate::as_datetime(reference_datetime), "America/New_York"),
                depth = paste0("Depth: ", depth)) |>
  dplyr::filter(datetime >= reference_datetime) |>
  #mutate(parameter = paste0(reference_date, parameter)) |>
  ggplot(aes(x = datetime)) +
  #ggplot(aes(x = datetime, color = reference_date)) +
  #ggplot(aes(x = datetime, group=parameter, color = reference_date)) +
  #geom_ribbon(aes(ymin = quantile10, ymax = quantile90), fill = "lightblue", color = "lightblue") +
  geom_ribbon(aes(ymin = quantile10, ymax = quantile90), fill = "lightblue", color = "lightblue") +
  geom_line(aes(y = mean), size = 0.5) +
  geom_point(aes(y = observation), color = 'black') +
  #geom_ribbon(aes(ymin = quantile10, ymax = quantile90), fill = reference_date, color = reference_date) +
  #scale_x_continuous(breaks = my_breaks, labels = my_labels) +
  facet_wrap(~depth +variable) +
  labs(y = 'Value') +
  #ylim(c(-5,40)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.2)) +
  theme(text = element_text(size = 20)) +
  labs(caption = "Disclaimer: Experimental Forecast")



###### old code #######
s3_cann |>
  # Filter the score_df and get in the right format
  dplyr::filter(depth %in% depths) |>
  dplyr::mutate(datetime = lubridate::with_tz(lubridate::as_datetime(datetime), "America/New_York"),
                reference_datetime = lubridate::with_tz(lubridate::as_datetime(reference_datetime), "America/New_York"),
                depth = paste0("Depth: ", depth)) |>
  dplyr::filter(datetime >= reference_datetime) |>
  mutate(parameter = paste0(reference_date, parameter)) |> 
  ggplot(aes(x = datetime, group=parameter, color = reference_date)) +
  #geom_ribbon(aes(ymin = quantile10, ymax = quantile90), fill = "lightblue", color = "lightblue") +
  geom_line(aes(y = prediction), size=0.1) +
  #geom_point(aes(y = prediction)) +
  #scale_x_continuous(breaks = my_breaks, labels = my_labels) +
  facet_wrap(~depth) +
  labs(y = 'Value')




## check inflow forecasts 
# edit these to make example forecast
flow_targets <- inflow_targets |>
  dplyr::filter(variable == 'FLOW') |>
  rename(date = datetime) |>
  filter(date < as.Date('2024-08-01'))

flow_drivers <- forecast_met |>
  left_join(flow_targets, by = c('date')) |>
  drop_na(observation)

flow_training_df <- flow_drivers |>
  dplyr::filter(date < as.Date('2024-07-01'))

flow_rec <- recipe(observation ~ precip + sevenday_precip + doy + temperature, # daily_temperature, threeday_temp
                   data = flow_training_df)

### RUN THIS FUNCTION TO FIND ERROR
flow_predictions <- xg_run_inflow_model(train_data = flow_training_df,
                                        model_recipe = flow_rec,
                                        met_combined = df_combined,
                                        targets_df = flow_targets,
                                        drivers_df = flow_drivers,
                                        var_name = 'FLOW')



flow_test <- flow_predictions |> filter(flow_type == 'inflow') |> filter(datetime <= as.Date('2024-08-01'), datetime >= as.Date('2024-07-01'))
targets_test <- flow_targets |> rename(datetime = date) |> filter(variable == 'FLOW') |> select(datetime, observation) |>
  filter(datetime <= as.Date('2024-08-01'), datetime >= as.Date('2024-07-01'))


flow_test <- flow_test |> full_join(targets_test, by = c('datetime')) #|> drop_na(prediction)

flow_test |>
  ggplot(aes(x = datetime, group = parameter)) + 
  geom_line(aes(y = prediction), color = 'black') +
  geom_point(aes(y= observation), color = 'red', size = 0.5)

