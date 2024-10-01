# library(tidyverse)
# library(lubridate)
# 
# source('R/fct_awss3Connect_sensorcode.R')
# 
# lake_directory <- here::here()
# config_set_name <- "default"
# forecast_site <- c("CANN")
# #configure_run_file <- paste0("configure_run_",forecast_site,".yml")
# configure_run_file <- "configure_run.yml"
# config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
# 
# config_obs <- FLAREr::initialize_obs_processing(lake_directory, 
#                                                 observation_yml = "observation_processing.yml", 
#                                                 config_set_name = config_set_name)
# 
# #dir.create(file.path(lake_directory, "targets", config$location$site_id), showWarnings = FALSE)
# 
# 
# ## RUN CODE TO GENERATE DATA
# sensorcode_df <- read_csv('configuration/default/sensorcode.csv')
# 
# 
# ### GENERATE INSITU Inflow TARGETS
# cann_inflow_download <- awss3Connect_sensorcode(sensorCodes = c('sensor_repository_00804'), code_df = sensorcode_df)
# south_inflow_download <- awss3Connect_sensorcode(sensorCodes = c('sensor_repository_00752'), code_df = sensorcode_df)
# 
# cann_inflow_download$site_id <- 'cann_river'
# south_inflow_download$site_id <- 'south_river'
# 
# inflow_combined <- dplyr::bind_rows(cann_inflow_download, south_inflow_download)
# 
# inflow_combined$Date <- as.Date(inflow_combined$datetime, tz = "Australia/Perth")
# 
# site_daily_inflow_rate_df <- inflow_combined |> 
#   group_by(Date, site_id) |> 
#   mutate(site_average_rate = mean(Data, na.rm = TRUE)) |> 
#   ungroup() |> 
#   distinct(Date, site_id, .keep_all = TRUE)
# 
# site_daily_inflow_total <- site_daily_inflow_rate_df |> 
#   mutate(daily_total = site_average_rate*86400) # second rate (m3/s) to day rate (86400 seconds per day) 
# 
# 
# daily_inflow_combined <- site_daily_inflow_total |> 
#   group_by(Date) |> 
#   mutate(combined_rate = ifelse((length(unique(site_id)) > 1), sum(daily_total), NA)) |> 
#   ungroup() |> 
#   drop_na(combined_rate)
# 
# site_inlfow_wide <- daily_inflow_combined |> 
#   pivot_wider(id_cols = c('Date'), names_from = 'site_id', values_from = 'daily_total') |> 
#   arrange(Date)
# 
# plotting_df <- daily_inflow_combined |> 
#   right_join(site_inlfow_wide, by = 'Date') |> 
#   distinct(Date, .keep_all = TRUE) #|> 
#   #filter(combined_rate < 50000)
# ## PLOTTING 
# 
# ## plot all years
# ggplot(plotting_df, aes(x=Date)) + 
#   geom_line(aes(y = combined_rate), color = "darkblue") + 
#   geom_line(aes(y = cann_river), color="darkorange") +
#   geom_line(aes(y = south_river), color='darkgreen') +
#   #ylim(c(0,50000))
#   ylab('Daily Inflow Rate (m3/day)') +
#   scale_colour_manual(values = c("darkblue", "darkorange", "darkgreen"))
# 
# ## annual plots
# ggplot(plotting_df, aes(x=Date)) + 
#   geom_line(aes(y = combined_rate), color = "darkblue") + 
#   geom_line(aes(y = cann_river), color="darkorange") +
#   geom_line(aes(y = south_river), color='darkgreen') +
#   xlim(c(as.Date('2022-01-01'),as.Date('2023-01-01'))) +
#   ylab('Daily Inflow Rate (m3/day)') +
#   scale_colour_manual(values = c("darkblue", "darkorange", "darkgreen"))
