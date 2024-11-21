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

collect_profile_targets_aed_inflow <- function(profile_data_download, sites){
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
                "FLOW",
                "TEMP", # "Temperature (deg C)"
                "SALT", # "Salinity (ppt)"
                'NCS_ss1',
                'OXY_oxy', #available as is
                'SIL_rsi', # "SiO2 (sol react) (ug/L)", "SiO2-Si (sol react) (ug/L)" (?)  
                'NIT_amm', # "NH3-N/NH4-N (sol) (ug/L)"
                'NIT_nit', #"NO3-N (sol) (ug/L)" + NO2-N (sol) (ug/L)
                'PHS_frp', #"PO4-P (sol react) {SRP FRP} (ug/L)" 
                'PHS_frp_ads',
                'OGM_doc', #"C (sol org) {DOC DOC as NPOC} (ug/L)" * 0.1 (labile pool)
                'OGM_docr', # "C (sol org) {DOC DOC as NPOC} (ug/L)" * 0.9 (assume 90% of DOC pool is recalcitrant)
                'OGM_poc', # 10% of DOC pool is POC (Wetzel)
                'OGM_don', #  "N (sum sol org) {DON} (ug/L)"
                'OGM_donr', # 10% of total DON pool
                'OGM_pon', # PON = TN - Amm - Nit - DON
                'OGM_dop', # ("PO4-P (sol react) {SRP FRP} (ug/L)" - (0.1*'PO4-P (sol react) {SRP FRP} (ug/L)')) * 0.4
                'OGM_dopr', # 10% of dop (from fcr code)
                'OGM_pop', #("PO4-P (sol react) {SRP FRP} (ug/L)" - (0.1*'PO4-P (sol react) {SRP FRP} (ug/L)')) * 0.5
                'OGM_cpom',
                'PHY_cyano', #CHLA * 0.1
                'PHY_green', # CHLA * 0.15
                'PHY_diatom') # CHLA * 0.5
  
  profile_data <- profile_data_subset %>%  
    dplyr::filter(`Program Site Ref` %in% cannsites) |> #&
                    #`Collection Method` %in% c('Insitu',"Integrated over depth")) |>  #&
                    #`Data Category` %in% 'Instrument log') |>
    dplyr::mutate(SALT = `Salinity (ppt)`, 
                  TEMP = `Temperature (deg C)`, 
                  OXY_oxy = `O2-{DO conc} (mg/L)`*1000*(1/32), #convert to ug/L
                  SIL_rsi = 70.5, #`SiO2-Si (sol react) (ug/L)` * (1/60.08), 
                  NIT_amm = `NH3-N/NH4-N (sol) (ug/L)` * (1/14.01),
                  NIT_nit = `N (sum sol ox) {NOx-N TON} (ug/L)` * (1/14.01),
                  PHS_frp = `PO4-P (sol react) {SRP FRP} (ug/L)` * (1/30.973762) ,
                  PHS_frp_ads = `PO4-P (sol react) {SRP FRP} (ug/L)` * (1/30.973762) * 0.1,  
                  OGM_doc = `C (sol org) {DOC DOC as NPOC} (ug/L)` * 0.1 * (1/12.01),
                  OGM_docr = `C (sol org) {DOC DOC as NPOC} (ug/L)` * 0.9 * (1/12.01),
                  OGM_poc = `C (sol org) {DOC DOC as NPOC} (ug/L)` * 0.1 *(1/12.01),
                  OGM_don =  `N (sum sol org) {DON} (ug/L)` * 0.9  * (1/14),
                  OGM_donr = `N (sum sol org) {DON} (ug/L)` * 0.1 * (1/14),
                  OGM_pon = `N (tot) {TN pTN} (ug/L)`* (1/14) - NIT_amm - NIT_nit - OGM_don - OGM_donr,
                  OGM_dop = `P (tot) {TP pTP} (ug/L)` * (1/30.973762) - PHS_frp - PHS_frp_ads * 0.1, 
                  OGM_dopr = `P (tot) {TP pTP} (ug/L)` * (1/30.973762) - PHS_frp - PHS_frp_ads * 0.8, 
                  OGM_pop =  `P (tot) {TP pTP} (ug/L)` * (1/30.973762) - PHS_frp - PHS_frp_ads * 0.1,
                  NCS_ss1 = `Suspended Solids (Total) {TSS} (mg/L)`, 
                  OGM_cpom = `C (sol org) {DOC DOC as NPOC} (ug/L)`*(1/12.01) * 0.5, #Assuming that CPOM is 50% of DOC
                  PHY_cyano = 0.0,  #0.3 * `Chlorophyll a (in situ) (ug/L)` * (1/12) * 100, #100 is the Xcc. NEED TO UPDATE WITH THE REAL ONE FROM THE NEW CONFIGURATION
                  PHY_green = 0.0, #0.3 * `Chlorophyll a (in situ) (ug/L)` * (1/12) * 30, #30 is the Xcc
                  PHY_diatom = 0.0) |>  #0.4 *`Chlorophyll a (in situ) (ug/L)` * (1/12) * 30) |>  #30 is the Xcc
    select(site_ref = `Site Ref`, 
           program = `Program Site Ref`, 
           time = `Collect Time`, 
           date = `Collect Date`, 
           depth = `Sample Depth (m)`,
           dplyr::any_of(aed_vars)) |> 
    mutate(time = format(strptime(time, "%I:%M:%S %p"), "%H:%M:%S")) |> # convert from AM/PM to 24-hour 
    mutate(datetime = lubridate::force_tz(lubridate::as_datetime(paste(date, time), format = '%d/%m/%Y %H:%M:%S')), tzone = 'Australia/Perth') |> 
    mutate(datetime = lubridate::with_tz(datetime, tzone = "UTC")) |> 
    mutate(datetime = lubridate::round_date(datetime, unit = 'hour'))
  
  profile_data_grouped <- profile_data |> 
   # mutate(depth_rounded = plyr::round_any(depth, 0.25))  |> # bin depths by rounding -- matches depth configuration 
    #select(-depth) |> 
    #rename(depth = depth_rounded) |> 
    #dplyr::filter(!is.na(depth), 
    #              depth <= 6.0) |> 
    select(-site_ref, -program, -time, - date, -tzone, -depth) |> 
    pivot_longer(-c("datetime"), names_to = 'variable', values_to = 'data') |> 
    mutate(date = as.Date(datetime),
           datetime = lubridate::as_datetime(paste0(format(date, "%Y-%m-%d %H"), ":00:00"))) |> 
    filter(!is.na(data)) |> 
    #summarise(observation = mean(data, na.rm = TRUE), .by = c("datetime","variable","depth")) |> 
    summarise(observation = mean(data, na.rm = TRUE), .by = c("datetime","variable")) |> 
    mutate(site_id = 'CANN', 
           depth = 0) |> ## AVERAGED VARIABLES OVER WATER COLUMN FOR INFLOW (ASSUMPTION BASED ON HOW SHALLOW CANN INFLOW IS (0-5m))
    select(datetime, site_id, observation, variable) |> 
    mutate(observation = ifelse(observation < 0, 0, observation))
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
