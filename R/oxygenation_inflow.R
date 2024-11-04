## THIS IS A TEST FUNCTION FOR INCORPORATING OXYGENATION DATA -- WILL NEED TO BE EDITED IN THE FUTURE

oxygenation_inflow <- function(inflow_forecast_future, inflow_forecast_historic, use_oxygenation){
  oxygen_data <- read_csv('https://raw.githubusercontent.com/Sherry-Zhai/GLM_Examples/refs/heads/master/CanningWeir_oxyplant/bcs/Inflow_oxyplant_nic_bac.csv')
  mean_do_in <- mean(oxygen_data$OXY_oxy)*100 ## increase concentration to account for decreased flow
  mean_temp_in <- mean(oxygen_data$temp) ## fill in for temp (dont want zero degree water)
  mean_flow_in <- mean(oxygen_data$flow)/100 ## decrease flow to limit dilution
  
  oxygen_inflow_future <- inflow_forecast_future
  oxygen_inflow_future$prediction <- 0
  
  oxygen_inflow_historic <- inflow_forecast_historic
  oxygen_inflow_historic$prediction <- 0
  
  
  if(use_oxygenation == TRUE){
    oxygen_inflow_future <- oxygen_inflow_future |> 
      mutate(prediction = ifelse(variable == 'FLOW', mean_flow_in, prediction),
             prediction = ifelse(variable == 'OXY_oxy', mean_do_in, prediction),
             prediction = ifelse(variable == 'TEMP', mean_temp_in, prediction))
    
    oxygen_inflow_historic <- oxygen_inflow_historic |> 
      mutate(prediction = ifelse(variable == 'FLOW', mean_flow_in, prediction),
             prediction = ifelse(variable == 'OXY_oxy', mean_do_in, prediction),
             prediction = ifelse(variable == 'TEMP', mean_temp_in, prediction))
  } else{
    oxygen_inflow_future <- oxygen_inflow_future |> 
      mutate(prediction = ifelse(variable == 'FLOW', 0, prediction), ## SET FLOW TO ZERO IF NOT USING OXYGENATION
             prediction = ifelse(variable == 'OXY_oxy', mean_do_in, prediction),
             prediction = ifelse(variable == 'TEMP', mean_temp_in, prediction))
    
    oxygen_inflow_historic <- oxygen_inflow_historic |> 
      mutate(prediction = ifelse(variable == 'FLOW', 0, prediction), ## SET FLOW TO ZERO IF NOT USING OXYGENATION
             prediction = ifelse(variable == 'OXY_oxy', mean_do_in, prediction),
             prediction = ifelse(variable == 'TEMP', mean_temp_in, prediction))
  }
  
  oxygen_inflow_future$flow_number <- 2
  oxygen_inflow_historic$flow_number <- 2
  
  oxygenation_inflows <- list(oxygen_inflow_future, oxygen_inflow_historic)
  
  return(oxygenation_inflows)
}
