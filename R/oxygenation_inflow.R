# ## THIS IS A TEST FUNCTION FOR INCORPORATING OXYGENATION DATA -- WILL NEED TO BE EDITED IN THE FUTURE
# source('R/xg_run_inflow_model.R')
# 
# lake_directory <- here::here()
# setwd(lake_directory)
# forecast_site <- "CANN"
# site_id <- 'CANN'
# configure_run_file <- "configure_run.yml"
# config_set_name <- "glm_aed_flare_v3"
# 
# config <- FLAREr:::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name,clean_start = TRUE)
# 
# reference_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
# noaa_date <- reference_datetime - lubridate::days(1)
# site_identifier <- 'CANN'
# endpoint <- 'renc.osn.xsede.org'
# 
# ## pull in observed met data
# observed_airtemp <- read_csv(file.path(lake_directory,'targets', config_obs$site_id,
#                                        paste0(config_obs$site_id,'-targets-met.csv')), show_col_types = FALSE) |>
#   filter(variable == 'air_temperature') |>
#   mutate(date = as.Date(datetime),
#          temp = observation) |> 
#   summarise(daily_temperature = median(temp, na.rm = TRUE), .by = c("date"))
# 
# 
# insitu_targets <- read_csv(file.path(lake_directory,'targets', config_obs$site_id,
#                                      paste0(config_obs$site_id,"-targets-insitu.csv")), show_col_types = FALSE)
# 
# met_s3_future <- arrow::s3_bucket(file.path("bio230121-bucket01/flare/drivers/met/gefs-v12/stage2",paste0("reference_datetime=",noaa_date),paste0("site_id=",site_id)),
#                                   endpoint_override = endpoint,
#                                   anonymous = TRUE)
# 
# df_future <- arrow::open_dataset(met_s3_future) |> 
#   dplyr::filter(variable %in% c("air_temperature")) |> 
#   collect() |> 
#   rename(ensemble = parameter) |> 
#   mutate(variable = ifelse(variable == "air_temperature", "temperature_2m", variable),
#          prediction = ifelse(variable == "temperature_2m", prediction - 273.15, prediction))
# 
# min_datetime <- min(df_future$datetime)
# 
# met_s3_past <- arrow::s3_bucket(paste0("bio230121-bucket01/flare/drivers/met/gefs-v12/stage3/site_id=",site_id),
#                                 endpoint_override = 'renc.osn.xsede.org',
#                                 anonymous = TRUE)
# 
# df_past <- arrow::open_dataset(met_s3_past) |> 
#   dplyr::filter(variable %in% c("air_temperature"),
#                 (datetime < min_datetime  & variable == "air_temperature"),
#                 datetime > as.Date('2023-01-01')) |> 
#   collect() |> 
#   rename(ensemble = parameter) |> 
#   mutate(variable = ifelse(variable == "air_temperature", "temperature_2m", variable),
#          prediction = ifelse(variable == "temperature_2m", prediction - 273.15, prediction)) |> 
#   select(-reference_datetime)
# 
# # combine past and future noaa data
# df_combined <- bind_rows(df_future, df_past) |> 
#   arrange(variable, datetime, ensemble)
# 
# forecast_temp <- df_combined |>
#   dplyr::filter(variable == 'temperature_2m') |>
#   summarise(temp_hourly = median(prediction, na.rm = TRUE), .by = c("datetime")) |> # get the median hourly temp across all EMs
#   mutate(date = lubridate::as_date(datetime)) |>
#   summarise(air_temperature = median(temp_hourly, na.rm = TRUE), .by = c("date")) #|> # get median temp across hours of the day
# 
# 
# oxygenation_data <- read_csv('https://raw.githubusercontent.com/Sherry-Zhai/GLM_Examples/refs/heads/master/CanningWeir_oxyplant/bcs/Inflow_oxyplant_nic_bac.csv')
# 
# flow_df <- oxygenation_data |> 
#   select(date = time, flow) |> 
#   filter(flow != 0.00) |> 
#   right_join(forecast_temp, by = c('date')) |> 
#   #right_join(wtemp_observed, by = c('date')) |> 
#   drop_na(flow) |> 
#   mutate(doy = as.numeric(strftime(date, format = "%j")))
# 
# flow_rec <- recipe(flow ~ air_temperature + doy, # daily_temperature, threeday_temp
#                    data = flow_df)
# 
# 
# ## POTENTIALLY USE THIS TO CALL ENTIRE XGBOOST WORKFLOW (surrounding code used in function)
# # flow_predictions <- xg_run_inflow_model(train_data = flow_df,
# #                                         model_recipe = flow_rec,
# #                                         met_combined = df_combined,
# #                                         targets_df = flow_targets,
# #                                         drivers_df = flow_drivers,
# #                                         var_name = 'FLOW')
# 
# 
# ## define folds in training data
# folds <- vfold_cv(flow_df, v = 5) # orginally set to 10
# 
# rec_preprocess <- flow_rec |>
#   step_normalize(all_numeric_predictors()) 
# 
# ## define model and tunining parameters (tuning 2/8 parameters right now)
# xgboost_mod <- boost_tree(tree_depth = tune(), trees = tune()) |> #, learn_rate = tune()) |>
#   set_mode("regression") |>
#   set_engine("xgboost")
# 
# # define the model workflow
# xgboost_inflow_wkflow <-
#   workflow() %>%
#   add_model(xgboost_mod) %>%
#   add_recipe(rec_preprocess)
# 
# # tune the hyper-parameters
# inflow_resample_fit <- xgboost_inflow_wkflow |>
#   tune_grid(resamples = folds,
#             grid = 25,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(rmse))
# 
# # select the best tuned hyper-parameters
# best_hyperparameters <- inflow_resample_fit %>%
#   select_best(metric = "rmse")
# 
# final_wrorkflow <- xgboost_inflow_wkflow |>
#   finalize_workflow(best_hyperparameters)
# 
# ## fit the model (using all available data (past and future) for now but could just use training data)
# #xgboost_inflow_fit <- fit(final_wrorkflow, data = drivers_df)
# xgboost_inflow_fit <- fit(final_wrorkflow, data = flow_df)
# 
# forecast_met_ens <- df_combined |>
#   dplyr::filter(variable == 'temperature_2m') |>
#   mutate(date = lubridate::as_date(datetime)) |>
#   summarise(air_temperature = median(prediction, na.rm = TRUE), .by = c("date","ensemble"))  |> ## change value and varname to 'daily_temperature' for using observed met
#   mutate(doy = lubridate::yday(date)) |> 
#   arrange(date,ensemble)
# 
# #make empty dataframe to store predictions
# data_build <- data.frame()
# 
# for (i in unique(forecast_met_ens$ensemble)){
#   
#   ens_df <- forecast_met_ens |>
#     dplyr::filter(ensemble == i)
#   
#   ens_inflow <- predict(xgboost_inflow_fit, new_data = ens_df)
#   
#   ens_predictions <- cbind(ens_df,ens_inflow) |>
#     rename(prediction = .pred) |>
#     mutate(prediction = ifelse(prediction < 0, 0, prediction))
#   
#   
#   data_build <- bind_rows(data_build,ens_predictions)
#   
# }
# 
# ## join observations back onto predictions
# 
# ## overwrite predictions with observed data when present
# update_historical_df <- data_build |>
#   left_join(targets_df, by = c('date')) |>
#   #mutate(prediction = ifelse(!is.na(observation), observation, prediction)) |> ## COME BACK TO THIS -- DO WE WANT THIS???
#   mutate(model_id = config$flows$forecast_inflow_model) |>
#   mutate(site_id = config$location$site_id) |>
#   mutate(reference_datetime = config$run_config$forecast_start_datetime) |>
#   mutate(family = 'ensemble') |>
#   mutate(variable = var_name) |>
#   mutate(flow_type = 'inflow') |>
#   mutate(flow_number = 1) |>
#   rename(parameter = ensemble, datetime = date) |>
#   select(model_id, site_id, reference_datetime, datetime, family, parameter, variable, prediction, flow_type, flow_number)
# 
# 

oxygenation_inflow <- function(inflow_forecast_future, inflow_forecast_historic, use_oxygenation){
  oxygen_data <- read_csv('https://raw.githubusercontent.com/Sherry-Zhai/GLM_Examples/refs/heads/master/CanningWeir_oxyplant/bcs/Inflow_oxyplant_nic_bac.csv')
  
  # mean_do_in <- mean(oxygen_data$OXY_oxy)*100 ## increase concentration to account for decreased flow
  # mean_temp_in <- mean(oxygen_data$temp) ## fill in for temp (dont want zero degree water)
  # mean_flow_in <- mean(oxygen_data$flow)/100 ## decrease flow to limit dilution
  
  mean_do_in <- mean(oxygen_data$OXY_oxy)
  mean_temp_in <- mean(oxygen_data$temp) ## fill in for temp (dont want zero degree water)
  mean_flow_in <- mean(oxygen_data$flow)
  
  ## convert inflow to be 1e-8 [m3/s] -- adjust concentration to keep same amount of mass inflow
  adjusted_flow_in <- 0.00000001 #m3/s
  mean_do_in_converted <- (mean_do_in*mean_flow_in)/adjusted_flow_in
  
  oxygen_inflow_future <- inflow_forecast_future
  oxygen_inflow_future$prediction <- 0
  
  oxygen_inflow_historic <- inflow_forecast_historic
  oxygen_inflow_historic$prediction <- 0
  
  
  if(use_oxygenation == TRUE){
    oxygen_inflow_future <- oxygen_inflow_future |> 
      mutate(prediction = ifelse(variable == 'FLOW', adjusted_flow_in, prediction),
             prediction = ifelse(variable == 'OXY_oxy', mean_do_in_converted, prediction),
             prediction = ifelse(variable == 'TEMP', mean_temp_in, prediction))
    
    oxygen_inflow_historic <- oxygen_inflow_historic |> 
      mutate(prediction = ifelse(variable == 'FLOW', adjusted_flow_in, prediction),
             prediction = ifelse(variable == 'OXY_oxy', mean_do_in_converted, prediction),
             prediction = ifelse(variable == 'TEMP', mean_temp_in, prediction))
  } else{
    oxygen_inflow_future <- oxygen_inflow_future |> 
      mutate(prediction = ifelse(variable == 'FLOW', 0, prediction), ## SET FLOW TO ZERO IF NOT USING OXYGENATION
             prediction = ifelse(variable == 'OXY_oxy', mean_do_in_converted, prediction),
             prediction = ifelse(variable == 'TEMP', mean_temp_in, prediction))
    
    oxygen_inflow_historic <- oxygen_inflow_historic |> 
      mutate(prediction = ifelse(variable == 'FLOW', 0, prediction), ## SET FLOW TO ZERO IF NOT USING OXYGENATION
             prediction = ifelse(variable == 'OXY_oxy', mean_do_in_converted, prediction),
             prediction = ifelse(variable == 'TEMP', mean_temp_in, prediction))
  }
  
  oxygen_inflow_future$flow_number <- 2
  oxygen_inflow_historic$flow_number <- 2
  
  oxygenation_inflows <- list(oxygen_inflow_future, oxygen_inflow_historic)
  
  return(oxygenation_inflows)
}