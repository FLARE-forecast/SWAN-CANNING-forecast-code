
collect_insitu_targets <- function(obs_download, site_location, assign_depth){
  
  print(names(obs_download))
  
  # remove duplicates 
  obs_dedup <- obs_download |>
  distinct(Height, variable, datetime, .keep_all = TRUE)
  
  print('obs_dedup')
  print(names(obs_dedup))
  
  obs_df_wide <- obs_dedup |> pivot_wider(names_from = variable, values_from = Data) |> rename(salt = `Salinity (ppt)`, temperature = 'Temperature')
  
  obs_df <- obs_df_wide |> pivot_longer(cols = c('temperature','salt'),
                                        names_to = 'variable', 
                                        values_to = 'observation')
  
  ## assign columns
  obs_df$site_id <- site_location
  
  print('obs_df')
  print(names(obs_df))
  
  group_insitu <- obs_df |> 
    mutate(Date = as.Date(datetime)) |> 
    mutate(hour = lubridate::hour(datetime))
  #print(group_insitu)
  
  group_insitu <- group_insitu |> 
    dplyr::filter(hour == 0)
  
  print('group_insitu')
  print(names(group_insitu))
  
  cleaned_insitu_file <- group_insitu |> # only want midnight observations for the daily value
    group_by(Date, variable) |> 
    mutate(observation = mean(observation, na.rm = TRUE)) |> 
    ungroup() |> 
    distinct(Date, variable, .keep_all = TRUE) |> 
    mutate(datetime = as.POSIXct(paste(Date, '00:00:00'), tz = "UTC")) |> 
    mutate(depth = 1.5) |> # assign depth to match model config depths (median depth value is 1.6)
    dplyr::select(datetime, site_id, depth, observation, variable)
  
  print('cleaned_insitu_file')
  print(names(cleaned_insitu_file))
  
  print('trying roll temp')
  
  roll_temp <- cleaned_insitu_file |> 
    dplyr::filter(variable == 'temperature', 
           observation != 0) |> 
    arrange(datetime) |> 
    mutate(mean_roll = RcppRoll::roll_mean(x = observation, n = 7, fill = NA, na.rm = TRUE)) |> 
    mutate(sd_roll = RcppRoll::roll_sd(x = mean_roll, n = 7, fill = NA, na.rm = TRUE)) |> 
    mutate(mean_roll = zoo::na.fill(mean_roll, "extend")) |> 
    mutate(sd_roll = zoo::na.fill(sd_roll, "extend")) |>
    mutate(sd_check_under = mean_roll - (sd_roll*3)) |> 
    mutate(sd_check_over = mean_roll + (sd_roll*3)) |> 
    dplyr::filter(!(sd_roll > 1 & (observation < sd_check_under)), 
           !(sd_roll > 1 & (observation < sd_check_over)))
  
  print('roll_temp')
  print(names(roll_temp))
  
  # roll_salt <- cleaned_insitu_file |> 
  #   filter(variable == 'salt') |> 
  #   arrange(datetime) |> 
  #   mutate(mean_roll = RcppRoll::roll_mean(x = observation, n = 7, fill = NA, na.rm = TRUE)) |> 
  #   mutate(sd_roll = RcppRoll::roll_sd(x = mean_roll, n = 7, fill = NA, na.rm = TRUE)) |> 
  #   mutate(mean_roll = zoo::na.fill(mean_roll, "extend")) |> 
  #   mutate(sd_roll = zoo::na.fill(sd_roll, "extend")) |>
  #   mutate(obs_test = observation < (mean_roll - (sd_roll*2))) |> 
  #   mutate(obs_num = mean_roll - (sd_roll*3)) |> 
  #   filter(!((observation < (mean_roll - (sd_roll*3)))), 
  #          !((observation < (mean_roll + (sd_roll*3)))))
  
  
  roll_salt <- cleaned_insitu_file |>
    dplyr::filter(variable == 'salt') |>
    arrange(datetime) |> 
    dplyr::filter(!(observation == 0))
  
  updated_data <- bind_rows(roll_temp, roll_salt)
  
  return(updated_data)
}
