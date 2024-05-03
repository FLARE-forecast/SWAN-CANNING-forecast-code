#library(tidyverse)
#library(lubridate)
#library(cron) ## needed for converting time objects

#source('R/fct_awss3Connect.R')
# 
# rawwiski <- awss3Connect(filename = 'arms/wiski.csv')
# 
# # test objects
# wiski_data <- rawwiski
# 
# ## pull sites upstream of the Kent St weir
# #sites <- c('KEN', 'BARC', 'KS7', 'NIC', 'ELL')
# sites <- c('YULEB', 'STHNR', 'CANNR')

collect_profile_targets <- function(profile_data_download, sites){
cannsites <- sites # ('KEN','KENU300')  #these are all the sites in Canning River, and 'KEN' is Kent st weir. Note 'Bacon Downstream'  is not included in this dataset as it belongs to a different program. 

# profile_data <- rawwiski %>%  
#   dplyr::filter(`Collect Date` %in% as.Date(plotDataWeek,format="%Y-%m-%d") &
#                   `Program Site Ref` %in% cannsites &
#                   `Collection Method` %in% 'Insitu' &
#                   `Data Category` %in% 'Instrument log')

profile_data <- profile_data_download %>%  
  dplyr::filter(`Program Site Ref` %in% cannsites &
                  `Collection Method` %in% 'Insitu' &
                  `Data Category` %in% 'Instrument log') |> 
  select(site_ref = `Site Ref`, 
         program = `Program Site Ref`, 
         time = `Collect Time`, 
         date = `Collect Date`, 
         depth = `Sample Depth (m)`, 
         salt = `Salinity (ppt)`, 
         temperature = `Temperature (deg C)`, 
         oxygen = `O2-{DO conc} (mg/L)`) |> 
  mutate(time = format(strptime(time, "%I:%M:%S %p"), "%H:%M:%S")) |> # convert from AM/PM to 24-hour 
  mutate(datetime = paste(date, time))

profile_data_grouped <- profile_data |> 
  mutate(depth_rounded = plyr::round_any(depth, 0.25))  |> # bin depths by rounding -- matches depth configuration 
  select(-depth) |> 
  rename(depth = depth_rounded) |> 
  dplyr::filter(!is.na(depth), 
         depth <= 6.0) |> 
  pivot_longer(cols = c("salt", "temperature", "oxygen"), names_to = 'variable', values_to = 'data') |> 
  summarise(observation = mean(data, na.rm = TRUE), .by = c("datetime","variable","depth")) |> 
  mutate(datetime = lubridate::force_tz(lubridate::as_datetime(datetime, format = '%d/%m/%Y %H:%M:%S')), tzone = 'Australia/Perth') |>
  mutate(datetime = lubridate::with_tz(datetime, tzone = "UTC")) |> 
  mutate(datetime = lubridate::round_date(datetime, unit = 'hour')) |> 
  mutate(site_id = 'CANN') |> 
  select(datetime, site_id, depth, observation, variable) |> 
  mutate(date = as.Date(datetime)) |> 
  group_by(date, variable) |> 
  mutate(min_datetime = min(datetime)) |> 
  ungroup() |> 
  group_by(date, variable) |> 
  dplyr::filter(datetime == min_datetime) |> 
  ungroup() |> 
  dplyr::filter(!is.nan(observation)) |> 
  select(datetime = date, site_id, depth, observation, variable)

return(profile_data_grouped)
}
