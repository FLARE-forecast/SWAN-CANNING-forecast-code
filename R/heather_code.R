# # Basic script to run GLM for BVR
# # FCR script: CCC for 2013-2019 run
# # written originally 16 July 2018
# # last updated 2 June 2020
# # Updated for BVR: A Hounshell, 29 Jun 2020
# 
# # Load packages, set sim folder, load nml file ####
# pacman::p_load(GLMr,glmtools,tidyverse,lubridate,ncdf4)
# 
# #setwd("../BVR-GLM") #if pulling from github, sets it to proper wd, which should be "/FCR_2013_2019GLMHistoricalRun_GLMv3beta"
# sim_folder <- getwd()
# 
# #look at glm and aed nml files
# nml_file <- paste0(sim_folder,"/16Mar23_ch4cal_glm3.nml")
# aed_file <- paste0(sim_folder,"/aed/16Mar23_ch4cal_aed2.nml")
# nml <- read_nml(nml_file) 
# aed <- read_nml(aed_file) #you may get a warning about an incomplete final line but it doesn't matter
# print(nml)
# print(aed)
# 
# file.copy('14Feb24_tempcal_glm3.nml', 'glm3.nml', overwrite = TRUE)
# file.copy('aed/22Jan24_po4cal_aed2.nml', 'aed/aed2.nml', overwrite = TRUE)
# 
# #run the model!
# system2(paste0(sim_folder,"/glm.app/Contents/MacOS/glm"), 
#         stdout = TRUE, stderr = TRUE, 
#         env = paste0("DYLD_LIBRARY_PATH=", sim_folder,
#                      "/glm.app/Contents/MacOS"))
# # Above from CCC
# 
# #sometimes, you'll get an error that says "Error in file, 'Time(Date)' is not first column!
# #in this case, open the input file in Excel, set the column in Custom ("YYYY-MM-DD") format, resave, and close the file
# #nc_file <- file.path(sim_folder, 'output/output.nc') #defines the output.nc file 
# nc_file <- file.path(sim_folder, '1/output.nc') #defines the output.nc file 
# 
# #reality check of temp heat map
# plot_var(nc_file, var="temp")
# 
# #get water level
# water_level<-get_surface_height(nc_file, ice.rm = TRUE, snow.rm = TRUE) |> 
#   filter(DateTime < as.POSIXct("2024-04-01"))
# 
# # Read in and plot water level observations
# wlevel <- read_csv("./inputs/BVR_Daily_WaterLevel_Vol_2015_2022_interp.csv") |> select(-...1)
# 
# wlevel$Date <- as.POSIXct(strptime(wlevel$Date, "%Y-%m-%d", tz="EST"))
# wlevel <- wlevel %>% filter(Date>as.POSIXct("2015-07-08") & Date<as.POSIXct("2020-12-31"))
# 
# plot(water_level$DateTime,water_level$surface_height,type='l')
# points(wlevel$Date, wlevel$WaterLevel_m, type="p",col="red",cex=0.4)
# 
# #calculate RMSE
# RMSE(water_level$surface_height,wlevel$WaterLevel_m)
# 
# summary(lm(water_level$surface_height ~ wlevel$WaterLevel_m))$r.squared
# 
# #get WRT
# volume<-get_var(nc_file, "lake_volume", reference="surface") |> 
#   filter(DateTime < as.POSIXct("2020-12-31")) #in m3
# 
# sa <-get_var(nc_file, "surface_area", reference="surface") 
# evap<-get_var(nc_file, "evaporation", reference="surface")
# precip<-get_var(nc_file, "precipitation", reference="surface")
# colnames(volume)[1]<-"time"
# colnames(precip)[1]<-"time"
# colnames(evap)[1]<-"time"
# plot(volume$DateTime, volume$lake_volume)
# plot(sa$DateTime, sa$surface_area)
# points(wlevel$Date, wlevel$vol_m3, type="l",col="red")
# plot(evap$time, evap$evaporation)
# plot(precip$time, precip$precipitation)
# 
# outflow<-read.csv("inputs/BVR_spillway_outflow_2015_2022_metInflow.csv", header=T)
# inflow_weir<-read.csv("inputs/BVR_inflow_2015_2022_allfractions_2poolsDOC_withch4_metInflow_0.65X_silica_0.2X_nitrate_0.4X_ammonium_1.9X_docr_1.7Xdoc.csv", header=T)
# 
# #plot inflow temp
# plot(as.Date(inflow_weir$time), inflow_weir$TEMP, type="l")
# 
# outflow$time<-as.POSIXct(strptime(outflow$time, "%Y-%m-%d", tz="EST"))
# inflow_weir$time<-as.POSIXct(strptime(inflow_weir$time, "%Y-%m-%d", tz="EST"))
# 
# #plot modeled docr with scaled inflows
# plot(as.Date(inflow_weir$time), inflow_weir$OGM_docr, type="l")
# 
# # Calculate WRT from modeled volume and measured outflow
# volume$time<-as.POSIXct(strptime(volume$time, "%Y-%m-%d", tz="EST"))
# wrt<-merge(volume, outflow, by='time')
# wrt$wrt <- ((wrt$lake_volume)/(wrt$FLOW))*(1/60)*(1/60)*(1/24) #residence time in days
# plot(wrt$time,wrt$wrt)
# 
# plot(volume$time,volume$lake_volume)
# 
# hist(wrt$wrt, xlim = c(0,2000), breaks=1000)
# median(wrt$wrt)
# mean(wrt$wrt)
# range(wrt$wrt)
# 
# #get ice
# ice<-get_var(nc_file,"white_ice_thickness")
# iceblue<-get_var(nc_file,"blue_ice_thickness")
# plot(ice$DateTime,rowSums(cbind(ice$white_ice_thickness,iceblue$blue_ice_thickness)))
# 
# #read in cleaned CTD temp file with long-term obs at focal depths
# obstemp<-read_csv('field_data/CleanedObsTemp.csv') %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))
# 
# #get modeled temperature readings for focal depths
# depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) 
# modtemp <- get_temp(nc_file, reference="surface", z_out=depths) %>%
#   pivot_longer(cols=starts_with("temp_"), names_to="Depth", names_prefix="temp_", values_to = "temp") %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 
# 
# #lets do depth by depth comparisons of the obs vs mod temps for each focal depth
# watertemp<-merge(modtemp, obstemp, by=c("DateTime","Depth")) %>%
#   rename(modtemp = temp.x, obstemp = temp.y)
# for(i in 1:length(unique(watertemp$Depth))){
#   tempdf<-subset(watertemp, watertemp$Depth==depths[i])
#   plot(tempdf$DateTime, tempdf$obstemp, col='red',
#        ylab='temperature', xlab='time',
#        main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]),ylim=c(0,30))
#   points(tempdf$DateTime, tempdf$obstemp, type = "p", col='red', pch=19)
#   points(tempdf$DateTime, tempdf$modtemp, type="l",col='black')
# }
# 
# #thermocline depth comparison
# field_file<-file.path(sim_folder,'field_data/CleanedObsTemp.csv')
# therm_depths <- compare_to_field(nc_file, field_file, metric="thermo.depth", precision="days",method='interp',as_value=TRUE, na.rm=T)
# compare_to_field(nc_file, field_file, metric="thermo.depth", precision="days", method='interp',as_value=F, na.rm=TRUE) #prints RMSE
# plot(therm_depths$DateTime,therm_depths$mod, type="l", ylim=c(1,13),main = paste0("ThermoclineDepth: Obs=Red, Mod=Black"),
#      ylab="Thermocline depth, in m")
# points(therm_depths$DateTime, therm_depths$obs,col="red", pch=19)
# points(therm_depths$DateTime, therm_depths$obs, type="l",col="red")
# 
# #also plot upper and lower metalimnetic depths
# therm_depths <- compare_to_field(nc_file, field_file, metric="thermo.depth", precision="days",method='interp',as_value=TRUE, na.rm=T)
# 
# #can use this function to calculate RMSE at specific depth layers, e.g., from one depth or range of depths
# RMSE = function(m, o){
#   sqrt(mean((m - o)^2))
# }
# 
# # Use this function to calculate RMSE for water level
# RMSE(water_level$surface_height,wlevel$WaterLevel_m)
# 
# temps <- resample_to_field(nc_file, field_file, precision="mins", method='interp')
# temps<-temps[complete.cases(temps),]
# 
# m_temp <- temps$Modeled_temp[temps$Depth==c(0.1)] #1m depth (epi) RMSE
# o_temp <- temps$Observed_temp[temps$Depth==c(0.1)] 
# RMSE(m_temp,o_temp)
# summary(lm(m_temp ~ o_temp))$r.squared
# 
# m_temp <- temps$Modeled_temp[temps$Depth==c(5)] #1m depth (meta) RMSE
# o_temp <- temps$Observed_temp[temps$Depth==c(5)] 
# RMSE(m_temp,o_temp)
# summary(lm(m_temp ~ o_temp))$r.squared
# 
# m_temp <- temps$Modeled_temp[temps$Depth==c(9)] #9m depth (hypo) RMSE
# o_temp <- temps$Observed_temp[temps$Depth==c(9)] 
# RMSE(m_temp,o_temp)
# summary(lm(m_temp ~ o_temp))$r.squared
# 
# m_temp <- temps$Modeled_temp[temps$Depth>=0 & temps$Depth<=11] #depths from 0.1-11m (all depths)
# o_temp <- temps$Observed_temp[temps$Depth>=0 & temps$Depth<=11] 
# RMSE(m_temp,o_temp)
# 
# summary(lm(temps$Modeled_temp ~ temps$Observed_temp))$r.squared
# 
# 
# #######################################################
# #### oxygen data #######
# 
# #read in cleaned CTD temp file with long-term obs at focal depths
# var="OXY_oxy"
# obs_oxy<-read.csv('field_data/CleanedObsOxy.csv') %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST")))
# field_file <- file.path(sim_folder,'/field_data/CleanedObsOxy.csv') 
# depths<- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) 
# #plot_var_compare(nc_file,field_file,var_name = var, precision="days",col_lim = c(0,1000)) #compare obs vs modeled
# 
# #get modeled oxygen concentrations for focal depths
# mod_oxy <- get_var(nc_file, "OXY_oxy", reference='surface', z_out=depths) %>%
#   pivot_longer(cols=starts_with("OXY_oxy_"), names_to="Depth", names_prefix="OXY_oxy_", values_to = "OXY_oxy") %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) 
# 
# #lets do depth by depth comparisons of modeled vs. observed oxygen
# oxy_compare <- merge(mod_oxy, obs_oxy, by=c("DateTime","Depth")) %>%
#   rename(mod_oxy = OXY_oxy.x, obs_oxy = OXY_oxy.y)
# for(i in 1:length(unique(oxy_compare$Depth))){
#   tempdf<-subset(oxy_compare, oxy_compare$Depth==depths[i])
#   plot(tempdf$DateTime,tempdf$obs_oxy, type='p', col='red', pch=19,
#        ylab='Oxygen mmol/m3', xlab='time',
#        main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#   points(tempdf$DateTime, tempdf$mod_oxy, type="l",col='black')
# }
# 
# 
# #can use this function to calculate RMSE at specific depth layers, e.g., from one depth or range of depths
# RMSE = function(m, o){
#   sqrt(mean((m - o)^2))
# }
# 
# #calculate RMSE for oxygen
# oxygen <- resample_to_field(nc_file, field_file, precision="days", method='interp', 
#                             var_name="OXY_oxy")
# oxygen <-oxygen[complete.cases(oxygen),] #remove missing data
# 
# m_oxygen <- oxygen$Modeled_OXY_oxy[oxygen$Depth>=0.1 & oxygen$Depth<=0.1] #1m depth
# o_oxygen <- oxygen$Observed_OXY_oxy[oxygen$Depth>=0.1 & oxygen$Depth<=0.1] 
# RMSE(m_oxygen,o_oxygen)
# summary(lm(m_oxygen ~ o_oxygen))$r.squared
# 
# m_oxygen <- oxygen$Modeled_OXY_oxy[oxygen$Depth>=9 & oxygen$Depth<=9] #9m depth
# o_oxygen <- oxygen$Observed_OXY_oxy[oxygen$Depth>=9 & oxygen$Depth<=9] 
# RMSE(m_oxygen,o_oxygen)
# summary(lm(m_oxygen ~ o_oxygen))$r.squared
# 
# m_oxygen <- oxygen$Modeled_OXY_oxy[oxygen$Depth>=0 & oxygen$Depth<=11] #all depths
# o_oxygen <- oxygen$Observed_OXY_oxy[oxygen$Depth>=0 & oxygen$Depth<=11] 
# RMSE(m_oxygen,o_oxygen)
# 
# summary(lm(oxygen$Modeled_OXY_oxy ~ oxygen$Observed_OXY_oxy))$r.squared
# 
# mod_oxy9 <- get_var(nc_file, "OXY_oxy", reference="surface", z_out=c(9)) 
# plot(mod_oxy9$DateTime, mod_oxy9$OXY_oxy_9, type="l")
# #diagnostic plot of DO at 9 m (just above sediments)
# 
# 
# #######################################################
# #### dissolved inorganic carbon (DIC) data ###########
# var="CAR_dic"
# field_file <- file.path(sim_folder,'/field_data/field_chem_2DOCpools.csv') 
# 
# obs<-read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) %>% #read in observed chemistry data
#   dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   select(DateTime, Depth, var) %>%
#   group_by(DateTime, Depth) %>%
#   summarise(CAR_dic = mean(CAR_dic)) %>%
#   na.omit() 
# obs<-as.data.frame(obs)
# #write.csv(obs, "field_data/field_DIC.csv", row.names =F)
# 
# #get modeled concentrations for focal depths
# depths<- as.numeric(unique(obs$Depth))
# mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
#   pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   mutate(Depth = as.numeric(Depth)) %>%
#   na.omit()
# 
# #lets do depth by depth comparisons of the sims
# compare<-merge(mod, obs, by=c("DateTime","Depth"))
# 
# compare<-na.omit(compare)
# for(i in 1:length(depths)){
#   tempdf<-subset(compare, compare$Depth==depths[i])
#   if(nrow(tempdf>1)){
#     plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
#          ylab=var, xlab='time', pch=19,
#          main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#     points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
#   }
# }  
# 
# #calculate RMSE 
# DIC <- resample_to_field(nc_file, field_file, precision="mins", method='interp', 
#                          var_name=var)
# DIC <-DIC[complete.cases(DIC),]
# 
# m_DIC <- DIC$Modeled_CAR_dic[DIC$Depth>=0.1 & DIC$Depth<=0.1] #0.1
# o_DIC <- DIC$Observed_CAR_dic[DIC$Depth>=0.1 & DIC$Depth<=0.1] 
# RMSE(m_DIC,o_DIC)
# summary(lm(m_DIC ~ o_DIC))$r.squared
# 
# m_DIC <- DIC$Modeled_CAR_dic[DIC$Depth>=9 & DIC$Depth<=9] #9m
# o_DIC <- DIC$Observed_CAR_dic[DIC$Depth>=9 & DIC$Depth<=9] 
# RMSE(m_DIC,o_DIC)
# summary(lm(m_DIC ~ o_DIC))$r.squared
# 
# m_DIC <- DIC$Modeled_CAR_dic[DIC$Depth>=0 & DIC$Depth<=11] #all depths
# o_DIC <- DIC$Observed_CAR_dic[DIC$Depth>=0 & DIC$Depth<=11]
# RMSE(m_DIC,o_DIC)
# 
# summary(lm(DIC$Modeled_CAR_dic ~ DIC$Observed_CAR_dic))$r.squared
# 
# #######################################################
# #### dissolved methane data #######
# var="CAR_ch4"
# field_file <- file.path(sim_folder,'/field_data/field_gases.csv') 
# 
# obs<-read.csv('field_data/field_gases.csv', header=TRUE) %>% #read in observed chemistry data
#   dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   select(DateTime, Depth, var) %>%
#   na.omit()
# 
# #get modeled concentrations for focal depths
# depths<- sort(as.numeric(unique(obs$Depth)))
# 
# mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
#   pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   mutate(Depth=as.numeric(Depth)) %>%
#   na.omit()
# 
# #lets do depth by depth comparisons of the sims
# compare<-merge(mod, obs, by=c("DateTime","Depth"))
# compare<-na.omit(compare)
# for(i in 1:length(depths)){
#   tempdf<-subset(compare, compare$Depth==depths[i])
#   if(nrow(tempdf)>1){
#     plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
#          ylab=var, xlab='time', pch=19,
#          main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#     points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
#   }
# }
# 
# #calculate RMSE
# CH4 <- resample_to_field(nc_file, field_file, precision="mins", method='interp', 
#                          var_name=var)
# CH4 <-CH4[complete.cases(CH4),]
# 
# m_CH4 <- CH4$Modeled_CAR_ch4[CH4$Depth>=0.1 & CH4$Depth<=0.1] #0.1
# o_CH4 <- CH4$Observed_CAR_ch4[CH4$Depth>=0.1 & CH4$Depth<=0.1]
# RMSE(m_CH4,o_CH4)
# summary(lm(m_CH4 ~ o_CH4))$r.squared
# 
# m_CH4 <- CH4$Modeled_CAR_ch4[CH4$Depth>=9 & CH4$Depth<=9] #9m
# o_CH4 <- CH4$Observed_CAR_ch4[CH4$Depth>=9 & CH4$Depth<=9]
# RMSE(m_CH4,o_CH4)
# summary(lm(m_CH4 ~ o_CH4))$r.squared
# 
# m_CH4 <- CH4$Modeled_CAR_ch4[CH4$Depth>=0 & CH4$Depth<=11] #all depths
# o_CH4 <- CH4$Observed_CAR_ch4[CH4$Depth>=0 & CH4$Depth<=11]
# RMSE(m_CH4,o_CH4)
# 
# summary(lm(CH4$Modeled_CAR_ch4 ~ CH4$Observed_CAR_ch4))$r.squared
# 
# #######################################################
# #### silica  data #######
# 
# var="SIL_rsi"
# field_file <- file.path(sim_folder,'/field_data/field_silica.csv') 
# 
# obs<-read.csv('field_data/field_silica.csv', header=TRUE) %>% #read in observed chemistry data
#   dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   select(DateTime, Depth, var) %>%
#   na.omit()
# 
# #get modeled concentrations for focal depths
# depths<- sort(as.numeric(unique(obs$Depth)))
# 
# mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
#   pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   mutate(Depth=as.numeric(Depth)) %>%
#   na.omit()
# 
# #lets do depth by depth comparisons of the sims
# compare<-merge(mod, obs, by=c("DateTime","Depth"))
# compare<-na.omit(compare)
# for(i in 1:length(depths)){
#   tempdf<-subset(compare, compare$Depth==depths[i])
#   if(nrow(tempdf)>1){
#     plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='l', col='red',
#          ylab=var, xlab='time',
#          main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#     points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
#   }
# }
# 
# #plot obs and mod
# plot(obs$DateTime[obs$Depth==0],obs$SIL_rsi[obs$Depth==0]) #mean 92.5
# plot(obs$DateTime[obs$Depth==4],obs$SIL_rsi[obs$Depth==4]) #mean 94.5
# plot(obs$DateTime[obs$Depth==8],obs$SIL_rsi[obs$Depth==8]) #mean 66.0
# 
# 
# plot(mod$DateTime[mod$Depth==0],mod$SIL_rsi[mod$Depth==0] ) #mean 87.8
# plot(mod$DateTime[mod$Depth==4],mod$SIL_rsi[mod$Depth==4] ) #mean 85.4
# plot(mod$DateTime[mod$Depth==8],mod$SIL_rsi[mod$Depth==8] ) #mean 99.9
# 
# #######################################################
# #### ammonium #######
# 
# var="NIT_amm"
# field_file <- file.path(sim_folder,'/field_data/field_chem_2DOCpools.csv') 
# 
# obs<-read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) %>% #read in observed chemistry data
#   dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   select(DateTime, Depth, var) %>%
#   na.omit()
# 
# #get modeled concentrations for focal depths
# depths<- sort(as.numeric(unique(obs$Depth)))
# 
# mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
#   pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   mutate(Depth=as.numeric(Depth)) %>%
#   na.omit()
# 
# #lets do depth by depth comparisons of the sims
# compare<-merge(mod, obs, by=c("DateTime","Depth"))
# compare<-na.omit(compare)
# for(i in 1:length(depths)){
#   tempdf<-subset(compare, compare$Depth==depths[i])
#   if(nrow(tempdf)>1){
#     plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
#          ylab=var, xlab='time', pch=19,
#          main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#     points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
#   }
# }
# 
# #calculate RMSE
# NH4 <- resample_to_field(nc_file, field_file, precision="mins", method='interp', 
#                          var_name=var)
# NH4 <-NH4[complete.cases(NH4),]
# 
# m_NH4 <- NH4$Modeled_NIT_amm[NH4$Depth>=0.1 & NH4$Depth<=0.1] #0.1
# o_NH4 <-  NH4$Observed_NIT_amm[NH4$Depth>=0.1 & NH4$Depth<=0.1]
# RMSE(m_NH4,o_NH4)
# summary(lm(m_NH4 ~ o_NH4))$r.squared
# 
# m_NH4 <- NH4$Modeled_NIT_amm[NH4$Depth>=9 & NH4$Depth<=9] #9m
# o_NH4 <-  NH4$Observed_NIT_amm[NH4$Depth>=9 & NH4$Depth<=9] 
# RMSE(m_NH4,o_NH4)
# summary(lm(m_NH4 ~ o_NH4))$r.squared
# 
# m_NH4 <- NH4$Modeled_NIT_amm[NH4$Depth>=0 & NH4$Depth<=11] #all depths
# o_NH4 <-  NH4$Observed_NIT_amm[NH4$Depth>=0 & NH4$Depth<=11] 
# RMSE(m_NH4,o_NH4)
# 
# summary(lm(NH4$Modeled_NIT_amm ~ NH4$Observed_NIT_amm))$r.squared
# 
# #######################################################
# #### nitrate #########################################
# 
# var="NIT_nit"
# field_file <- file.path(sim_folder,'/field_data/field_chem_2DOCpools.csv') 
# 
# obs<-read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) %>% #read in observed chemistry data
#   dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   select(DateTime, Depth, var) %>%
#   na.omit()
# 
# #get modeled concentrations for focal depths
# depths<- sort(as.numeric(unique(obs$Depth)))
# 
# mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
#   pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   mutate(Depth=as.numeric(Depth)) %>%
#   na.omit()
# 
# #lets do depth by depth comparisons of the sims
# compare<-merge(mod, obs, by=c("DateTime","Depth"))
# compare<-na.omit(compare)
# for(i in 1:length(depths)){
#   tempdf<-subset(compare, compare$Depth==depths[i])
#   if(nrow(tempdf)>1){
#     plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
#          ylab=var, xlab='time', pch=19,
#          main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#     points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
#   }
# }
# 
# #calculate RMSE
# NO3 <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
#                          var_name=var)
# NO3 <-NO3[complete.cases(NO3),]
# 
# m_NO3 <- NO3$Modeled_NIT_nit[NO3$Depth>=0.1 & NO3$Depth<=0.1] #0.1
# o_NO3 <-  NO3$Observed_NIT_nit[NO3$Depth>=0.1 & NO3$Depth<=0.1]
# RMSE(m_NO3,o_NO3)
# summary(lm(m_NO3 ~ o_NO3))$r.squared
# 
# m_NO3 <- NO3$Modeled_NIT_nit[NO3$Depth>=9 & NO3$Depth<=9] #9m
# o_NO3 <-  NO3$Observed_NIT_nit[NO3$Depth>=9 & NO3$Depth<=9] 
# RMSE(m_NO3,o_NO3)
# summary(lm(m_NO3 ~ o_NO3))$r.squared
# 
# m_NO3 <- NO3$Modeled_NIT_nit[NO3$Depth>=0 & NO3$Depth<=11] #all depths
# o_NO3 <-  NO3$Observed_NIT_nit[NO3$Depth>=0 & NO3$Depth<=11] 
# RMSE(m_NO3,o_NO3)
# 
# summary(lm(NO3$Modeled_NIT_nit ~ NO3$Observed_NIT_nit))$r.squared
# 
# #######################################################
# #### phosphate ########################################
# 
# var="PHS_frp"
# field_file <- file.path(sim_folder,'/field_data/field_chem_2DOCpools.csv') 
# 
# obs<-read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) %>% #read in observed chemistry data
#   dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   select(DateTime, Depth, var) %>%
#   na.omit()
# 
# #get modeled concentrations for focal depths
# depths<- sort(as.numeric(unique(obs$Depth)))
# 
# mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
#   pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   mutate(Depth=as.numeric(Depth)) %>%
#   na.omit()
# 
# #lets do depth by depth comparisons of the sims
# compare<-merge(mod, obs, by=c("DateTime","Depth"))
# compare<-na.omit(compare)
# for(i in 1:length(depths)){
#   tempdf<-subset(compare, compare$Depth==depths[i])
#   if(nrow(tempdf)>1){
#     plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
#          ylab=var, xlab='time', pch=19,
#          main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#     points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
#   }
# }
# 
# #calculate RMSE 
# PO4 <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
#                          var_name=var)
# PO4 <-PO4[complete.cases(PO4),]
# 
# m_PO4 <- PO4$Modeled_PHS_frp[PO4$Depth>=0.1 & PO4$Depth<=0.1] #0.1
# o_PO4 <-  PO4$Observed_PHS_frp[PO4$Depth>=0.1 & PO4$Depth<=0.1]
# RMSE(m_PO4,o_PO4)
# summary(lm(m_PO4 ~ o_PO4))$r.squared
# 
# m_PO4 <- PO4$Modeled_PHS_frp[PO4$Depth>=9 & PO4$Depth<=9] #9m
# o_PO4 <-  PO4$Observed_PHS_frp[PO4$Depth>=9 & PO4$Depth<=9] 
# RMSE(m_PO4,o_PO4)
# summary(lm(m_PO4 ~ o_PO4))$r.squared
# 
# m_PO4 <- PO4$Modeled_PHS_frp[PO4$Depth>=0 & PO4$Depth<=11] #all depths
# o_PO4 <-  PO4$Observed_PHS_frp[PO4$Depth>=0 & PO4$Depth<=11] 
# RMSE(m_PO4,o_PO4)
# 
# summary(lm(PO4$Modeled_PHS_frp ~ PO4$Observed_PHS_frp))$r.squared
# 
# 
# #######################################################
# #### dissolved organic carbon: recalcitrant ###########
# 
# var="OGM_docr"
# field_file <- file.path(sim_folder,'/field_data/field_chem_2DOCpools.csv') 
# 
# obs<-read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) %>% #read in observed chemistry data
#   dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   select(DateTime, Depth, var) %>%
#   na.omit()
# 
# #get modeled concentrations for focal depths
# depths<- sort(as.numeric(unique(obs$Depth)))
# 
# mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
#   pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   mutate(Depth=as.numeric(Depth)) %>%
#   na.omit()
# 
# #lets do depth by depth comparisons of the sims
# compare<-merge(mod, obs, by=c("DateTime","Depth"))
# compare<-na.omit(compare)
# for(i in 1:length(depths)){
#   tempdf<-subset(compare, compare$Depth==depths[i])
#   if(nrow(tempdf)>1){
#     plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
#          ylab=var, xlab='time', pch=19,
#          main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#     points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
#   }
# }
# 
# #calculate RMSE
# docr <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
#                           var_name=var)
# docr <-docr[complete.cases(docr),]
# 
# m_docr <- docr$Modeled_OGM_docr[docr$Depth>=0.1 & docr$Depth<=0.1] #0.1
# o_docr <-  docr$Observed_OGM_docr[docr$Depth>=0.1 & docr$Depth<=0.1]
# RMSE(m_docr,o_docr)
# 
# m_docr <- docr$Modeled_OGM_docr[docr$Depth>=9 & docr$Depth<=9] #9m
# o_docr <-  docr$Observed_OGM_docr[docr$Depth>=9 & docr$Depth<=9] 
# RMSE(m_docr,o_docr)
# 
# m_docr <- docr$Modeled_OGM_docr[docr$Depth>=0 & docr$Depth<=11] #all depths
# o_docr <-  docr$Observed_OGM_docr[docr$Depth>=0 & docr$Depth<=11] 
# RMSE(m_docr,o_docr)
# 
# summary(lm(docr$Modeled_OGM_docr ~ docr$Observed_OGM_docr))$r.squared
# 
# 
# #######################################################
# #### dissolved organic carbon: labile ###########
# 
# var="OGM_doc"
# 
# obs<-read.csv('field_data/field_chem_2DOCpools.csv', header=TRUE) %>% #read in observed chemistry data
#   dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   select(DateTime, Depth, var) %>%
#   na.omit()
# 
# #get modeled concentrations for focal depths
# depths<- sort(as.numeric(unique(obs$Depth)))
# 
# mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
#   pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   mutate(Depth=as.numeric(Depth)) %>%
#   na.omit()
# 
# #lets do depth by depth comparisons of the sims
# compare<-merge(mod, obs, by=c("DateTime","Depth"))
# compare<-na.omit(compare)
# for(i in 1:length(depths)){
#   tempdf<-subset(compare, compare$Depth==depths[i])
#   if(nrow(tempdf)>1){
#     plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
#          ylab=var, xlab='time', pch=19,
#          main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#     points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
#   }
# }
# 
# #calculate RMSE for DOC labile
# doc <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
#                          var_name=var)
# doc <-doc[complete.cases(doc),]
# 
# m_doc <- doc$Modeled_OGM_doc[doc$Depth>=0.1 & doc$Depth<=0.1] #0.1
# o_doc <-  doc$Observed_OGM_doc[doc$Depth>=0.1 & doc$Depth<=0.1]
# RMSE(m_doc,o_doc)
# 
# m_doc <- doc$Modeled_OGM_doc[doc$Depth>=9 & doc$Depth<=9] #9m
# o_doc <-  doc$Observed_OGM_doc[doc$Depth>=9 & doc$Depth<=9] 
# RMSE(m_doc,o_doc)
# 
# m_doc <- doc$Modeled_OGM_doc[doc$Depth>=0 & doc$Depth<=11] #all depths
# o_doc <-  doc$Observed_OGM_doc[doc$Depth>=0 & doc$Depth<=11] 
# RMSE(m_doc,o_doc)
# 
# summary(lm(doc$Modeled_OGM_doc ~ doc$Observed_OGM_doc))$r.squared
# 
# #######################################################
# #### chlorophyll a #######
# 
# var="PHY_tchla"
# field_file <- file.path(sim_folder,'/field_data/CleanedObsChla.csv') 
# 
# obs<-read.csv('field_data/CleanedObsChla.csv', header=TRUE) %>% #read in observed chemistry data
#   dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   dplyr::select(DateTime, Depth, var) %>%
#   na.omit()
# 
# #get modeled concentrations for focal depths
# depths<- sort(as.numeric(unique(obs$Depth)))
# 
# mod<- get_var(nc_file, var, reference="surface", z_out=depths) %>%
#   pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
#   mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
#   mutate(Depth=as.numeric(Depth)) %>%
#   na.omit()
# 
# #lets do depth by depth comparisons of the sims
# compare<-merge(mod, obs, by=c("DateTime","Depth"))
# compare<-na.omit(compare)
# for(i in 1:length(depths)){
#   tempdf<-subset(compare, compare$Depth==depths[i])
#   if(nrow(tempdf)>1){
#     plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
#          ylab=var, xlab='time', pch=19,
#          main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#     points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
#   }
# }
# 
# #calculate RMSE
# chl <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
#                          var_name=var)
# chl <-chl[complete.cases(chl),]
# 
# m_chl <- chl$Modeled_PHY_tchla[chl$Depth>=0.1 & chl$Depth<=0.1] #0.1
# o_chl <-  chl$Observed_PHY_tchla[chl$Depth>=0.1 & chl$Depth<=0.1]
# RMSE(m_chl,o_chl)
# 
# m_chl <- chl$Modeled_PHY_tchla[chl$Depth>=9 & chl$Depth<=9] #9m
# o_chl <-  chl$Observed_PHY_tchla[chl$Depth>=9 & chl$Depth<=9] 
# RMSE(m_chl,o_chl)
# 
# m_chl <- chl$Modeled_PHY_tchla[chl$Depth>=0 & chl$Depth<=11] #all depths
# o_chl <-  chl$Observed_PHY_tchla[chl$Depth>=0 & chl$Depth<=11] 
# RMSE(m_chl,o_chl)
# 
# summary(lm(chl$Modeled_PHY_tchla ~ chl$Observed_PHY_tchla))$r.squared
# 
# #community
# cyano <- get_var(file=nc_file,var_name = 'PHY_cyano',z_out=0.1,reference = 'surface') 
# plot(cyano$DateTime, cyano$PHY_cyano_0.1, col="cyan", type="l", ylab="Phyto C mmol/m3", ylim=c(0,100))
# green <- get_var(file=nc_file,var_name = 'PHY_green',z_out=0.1,reference = 'surface') 
# lines(green$DateTime, green$PHY_green_0.1, col="green")
# diatoms <- get_var(file=nc_file,var_name = 'PHY_diatom',z_out=0.1,reference = 'surface') 
# lines(diatoms$DateTime, diatoms$PHY_diatom_0.1, col="brown")
# legend("topleft", legend=c("Cyano", "Greens", "Diatoms"), fill= c("cyan", "green","brown"), cex=0.8)
# chla <- get_var(file=nc_file,var_name = 'PHY_tchla',z_out=0.1,reference = 'surface') 
# lines(chla$DateTime, chla$PHY_tchla_0.1, col="red")
# 
# phytos <- get_var(file=nc_file,var_name = 'PHY_tphy',z_out=0.1,reference = 'surface') 
# plot(phytos$DateTime, phytos$PHY_tphy_0.1, col="cyan", type="l", ylab="Phyto C mmol/m3", ylim=c(0,100))
# 
# #######################################################
# #### ZOOPS! #######
# 
# grz <- get_var(file=nc_file,var_name = 'ZOO_grz',z_out=1,reference = 'surface')
# plot(grz$DateTime, grz$ZOO_grz_1)
# 
# resp <- get_var(file=nc_file,var_name = 'ZOO_resp',z_out=1,reference = 'surface')
# plot(resp$DateTime, resp$ZOO_resp_1)
# 
# mort <- get_var(file=nc_file,var_name = 'ZOO_mort',z_out=1,reference = 'surface')
# plot(mort$DateTime, mort$ZOO_mort_1)
# 
# 
# var="ZOO_cladoceran"
# var="ZOO_copepods"
# var="ZOO_rotifers"
# 
# obs<-read.csv('field_data/field_zoops.csv', header=TRUE) |>  
#   dplyr::mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) |> 
#   dplyr::select(DateTime, var) %>%
#   na.omit()
# 
# zoops <- get_var(file=nc_file,var_name = var,z_out=0.1,reference = 'surface') 
# plot(zoops$DateTime, zoops$ZOO_cladocerans_0.1, type='l')
# points(obs$DateTime, obs$ZOO_cladocerans, col="red")
# 
# #get modeled concentrations for focal depths
# depths<- sort(as.numeric(unique(obs$Depth)))
# 
# #mod<- get_var(nc_file, var, reference="surface") %>%
# #  pivot_longer(cols=starts_with(paste0(var,"_")), names_to="Depth", names_prefix=paste0(var,"_"), values_to = var) %>%
# #  mutate(DateTime = as.POSIXct(strptime(DateTime, "%Y-%m-%d", tz="EST"))) %>%
# #  na.omit()
# 
# #lets do depth by depth comparisons of the sims
# compare<-merge(mod, obs, by=c("DateTime","Depth"))
# compare<-na.omit(compare)
# for(i in 1:length(depths)){
#   tempdf<-subset(compare, compare$Depth==depths[i])
#   if(nrow(tempdf)>1){
#     plot(tempdf$DateTime,eval(parse(text=paste0("tempdf$",var,".y"))), type='p', col='red',
#          ylab=var, xlab='time', pch=19,
#          main = paste0("Obs=Red,Mod=Black,Depth=",depths[i]))
#     points(tempdf$DateTime, eval(parse(text=paste0("tempdf$",var,".x"))), type="l",col='black')
#   }
# }
# 
# 
# 
# 
# 
# #calculate RMSE for DOC labile - delete??
# 
# doc <- resample_to_field(nc_file, field_file, precision="hours", method='interp', 
#                          var_name=var)
# doc <-doc[complete.cases(doc),]
# 
# m_doc <- doc$Modeled_OGM_doc[doc$Depth>=0.1 & doc$Depth<=0.1] #0.1
# o_doc <-  doc$Observed_OGM_doc[doc$Depth>=0.1 & doc$Depth<=0.1]
# RMSE(m_doc,o_doc)
# 
# m_doc <- doc$Modeled_OGM_doc[doc$Depth>=9 & doc$Depth<=9] #9m
# o_doc <-  doc$Observed_OGM_doc[doc$Depth>=9 & doc$Depth<=9] 
# RMSE(m_doc,o_doc)
# 
# m_doc <- doc$Modeled_OGM_doc[doc$Depth>=0 & doc$Depth<=11] #all depths
# o_doc <-  doc$Observed_OGM_doc[doc$Depth>=0 & doc$Depth<=11] 
# RMSE(m_doc,o_doc)
# 
# summary(lm(doc$Modeled_OGM_doc ~ doc$Observed_OGM_doc))$r.squared
# 
# 
# 
# 
# #######################################################
# #### ancillary code #######
# 
# secchi_obs <- read.csv("./field_data/field_secchi.csv")
# 
# #plot Secchi depth & light extinction
# lec <- get_var(file=nc_file,var_name = 'extc',z_out=1,reference = 'surface')
# plot(lec$DateTime, 1.7/lec$extc_1)
# points(as.POSIXct(secchi_obs$DateTime), secchi_obs$Secchi_m, col="red", type="l")
# plot(lec$DateTime, lec$extc_1)
# 
# #see what vars are available for diagnostics
# sim_metrics(with_nml = TRUE)
# 
# #plot pH
# pH <- get_var(file=nc_file,var_name = 'CAR_pH',z_out=0.1,reference = 'surface') 
# plot(pH$DateTime, pH$CAR_pH_0.1)
# 
# DICish <- get_var(file=nc_file,var_name = 'CAR_dic',z_out=0.1,reference = 'surface') 
# plot(DICish$DateTime, DICish$CAR_dic_0.1)
# 
# #Particulate organic P
# pop9 <- get_var(file=nc_file,var_name = 'OGM_pop',z_out=9,reference = 'surface') 
# plot(pop9$DateTime, pop9$OGM_pop_9, ylim=c(0,2.3))
# 
# ch4 <- get_var(file=nc_file,var_name = 'CAR_ch4_atm',reference = "bottom",z_out = 1) #, z_out=1, reference='surface')
# plot(ch4$DateTime, ch4$CAR_ch4_atm)
# points(ch4$DateTime, (ch4$CAR_ch4_atm + sample(rnorm(1,mean=0.3,sd=0.3))), col="red")
# legend("topleft", c("Baseline","+2 degrees"), fill=c("black","red"))
# 
# nc <- ncdf4::nc_open(nc_file)
# names(nc$var)#get list of variables in data
# 
# #exchange across sed water interface
# ads <- get_var(file=nc_file,var_name = 'PHS_frp_dsf',z_out=c(1,5,9),reference = 'surface') 
# plot(ads$DateTime, ads$PHS_frp_dsf)