#library(tidyverse)
#library(lubridate)
#library(cron) ## needed for converting time objects

#source('R/fct_awss3Connect.R')

#rawwiski <- awss3Connect(filename = 'arms/wiski.csv')
# 
# # test objects
# wiski_data <- rawwiski
# 
# ## pull sites upstream of the Kent St weir
#sites <- c('KEN', 'BARC', 'KS7', 'NIC', 'ELL')
# sites <- c('YULEB', 'STHNR', 'CANNR')

collect_profile_targets_aed_insitu <- function(profile_data_download, sites){
  cannsites <- sites # ('KEN','KENU300')  #these are all the sites in Canning River, and 'KEN' is Kent st weir. Note 'Bacon Downstream'  is not included in this dataset as it belongs to a different program. 
  
  # profile_data <- rawwiski %>%
  #   dplyr::filter(`Collect Date` %in% as.Date(plotDataWeek,format="%Y-%m-%d") &
  #                   `Program Site Ref` %in% cannsites &
  #                   `Collection Method` %in% 'Insitu' &
  #                   `Data Category` %in% 'Instrument log')
  
  profile_data_subset <- profile_data_download[,1:123]
  
  remove_cols <- grep('Qual|Sign|Security|Wind|Tide|Comments|Primary|Null', names(profile_data_subset))
  profile_data_subset <- profile_data_subset[,-remove_cols]
  
  
  aed_vars <- c("datetime", 
                "TEMP", # "Temperature (deg C)"
                "salt", # "Salinity (ppt)"
                "CHLA", # `Chlorophyll a (in situ) (ug/L)`
                'OXY_oxy', #available as is
                'CAR_dic', #
                'CAR_ch4', #
                'SIL_rsi', # "SiO2 (sol react) (ug/L)", "SiO2-Si (sol react) (ug/L)" (?)  
                'NIT_amm', # "NH3-N/NH4-N (sol) (ug/L)"
                'NIT_nit', #"NO3-N (sol) (ug/L)" + NO2-N (sol) (ug/L)
                'PHS_frp', #"PO4-P (sol react) {SRP FRP} (ug/L)" 
                'OGM_doc', #"C (sol org) {DOC DOC as NPOC} (ug/L)" * 0.1 (labile pool)
                'OGM_docr', # "C (sol org) {DOC DOC as NPOC} (ug/L)" * 0.9 (assume 90% of DOC pool is recalcitrant)
                'OGM_poc', # 10% of DOC pool is POC (Wetzel)
                'OGM_don', #  "N (sum sol org) {DON} (ug/L)"
                'OGM_donr', # 10% of total DON pool
                'OGM_pon', # PON = TN - Amm - Nit - DON
                'OGM_dop', # ("PO4-P (sol react) {SRP FRP} (ug/L)" - (0.1*'PO4-P (sol react) {SRP FRP} (ug/L)')) * 0.4
                'OGM_dopr', # 10% of dop (from fcr code)
                'OGM_pop', #("PO4-P (sol react) {SRP FRP} (ug/L)" - (0.1*'PO4-P (sol react) {SRP FRP} (ug/L)')) * 0.5
                'PHY_cyano', #CHLA * 0.1
                'PHY_green', # CHLA * 0.15
                'PHY_diatom') # CHLA * 0.5
  
  
  aed_vars <- c("datetime", 
                "TEMP", # "Temperature (deg C)"
                "salt", # "Salinity (ppt)"
                "CHLA", # `Chlorophyll a (in situ) (ug/L)`
                'OXY_oxy', #available as is
                'SIL_rsi', # "SiO2 (sol react) (ug/L)", "SiO2-Si (sol react) (ug/L)" (?)  
                'NIT_amm', # "NH3-N/NH4-N (sol) (ug/L)"
                'NIT_nit', #"NO3-N (sol) (ug/L)" + NO2-N (sol) (ug/L)
                'PHS_frp', #"PO4-P (sol react) {SRP FRP} (ug/L)" 
                'OGM_doc_total',
                'OGM_don_total') 
  
  profile_data <- profile_data_subset %>%  
    dplyr::filter(`Program Site Ref` %in% cannsites) |> #&
    #`Collection Method` %in% c('Insitu',"Integrated over depth")) |>  #&
    #`Data Category` %in% 'Instrument log') |>
    dplyr::mutate(CHLA = `Chlorophyll a (in situ) (ug/L)`,
                  salt = `Salinity (ppt)`, 
                  TEMP = `Temperature (deg C)`, 
                  OXY_oxy = `O2-{DO conc} (mg/L)`*1000*(1/32), #convert to ug/L
                  SIL_rsi = `SiO2-Si (sol react) (ug/L)` * (1/60.08), 
                  NIT_amm = `NH3-N/NH4-N (sol) (ug/L)` * (1/18.04),
                  NIT_nit = `N (sum sol ox) {NOx-N TON} (ug/L)` * (1/62.00),
                  PHS_frp = `PO4-P (sol react) {SRP FRP} (ug/L)` * (1/94.9714),
                  OGM_doc_total = `C (sol org) {DOC DOC as NPOC} (ug/L)` * (1/12.01),
                  OGM_don_total =  `N (sum sol org) {DON} (ug/L)` * (5/6)) |> 
    select(site_ref = `Site Ref`, 
           program = `Program Site Ref`, 
           time = `Collect Time`, 
           date = `Collect Date`, 
           depth = `Sample Depth (m)`,
           dplyr::any_of(aed_vars)) |> 
    mutate(time = format(strptime(time, "%I:%M:%S %p"), "%H:%M:%S")) |> # convert from AM/PM to 24-hour 
    mutate(datetime = lubridate::force_tz(lubridate::as_datetime(paste(date, time), format = '%d/%m/%Y %H:%M:%S'), tzone = 'Australia/Perth')) |> 
    mutate(datetime = lubridate::with_tz(datetime, tzone = "UTC")) |> 
    mutate(datetime = lubridate::round_date(datetime, unit = 'hour'))
  
  profile_data_grouped <- profile_data |> 
    # mutate(depth_rounded = plyr::round_any(depth, 0.25))  |> # bin depths by rounding -- matches depth configuration 
    #select(-depth) |> 
    #rename(depth = depth_rounded) |> 
    #dplyr::filter(!is.na(depth), 
    #              depth <= 6.0) |> 
    select(-site_ref, -program, -time, - date) |> 
    pivot_longer(-c("datetime", "depth"), names_to = 'variable', values_to = 'data') |> 
    mutate(date = as.Date(datetime),
           datetime = lubridate::as_datetime(paste0(format(date, "%Y-%m-%d %H"), ":00:00"))) |> 
    filter(!is.na(data),
           depth <= 6.0) |> 
    #summarise(observation = mean(data, na.rm = TRUE), .by = c("datetime","variable","depth")) |> 
    summarise(observation = mean(data, na.rm = TRUE), .by = c("datetime","variable","depth")) |> 
    mutate(site_id = 'CANN') |>  
           #depth = 0) |> ## AVERAGED VARIABLES OVER WATER COLUMN FOR INFLOW (ASSUMPTION BASED ON HOW SHALLOW CANN INFLOW IS (0-5m))
    select(datetime, depth, site_id, observation, variable)
  # group_by(date, variable) |> 
  # mutate(min_datetime = min(datetime)) |> 
  # ungroup() |> 
  # group_by(date, variable) |> 
  # dplyr::filter(datetime == min_datetime) |> 
  # ungroup() |> 
  # dplyr::filter(!is.nan(observation)) |> 
  # select(datetime = date, site_id, depth, observation, variable)
  
  return(profile_data_grouped)
}
