#' Make quantiles from a forecast 
#' 
#' @param forecast dataframe with two columns- mean_demand and sd_demand
#' @param num_q_vals number of quantiles plus 1 (defaults to 101)
#' @return matrix of quantiles
make_quantiles <- function(forecast, num_q_vals = 101){
  
  q_vals <- c(seq(0,1, 1/(num_q_vals-1))) # set up vector of quantile levels - evenly spaced for now
  
  Forecast_quantiles <- matrix(nrow = nrow(forecast), ncol = num_q_vals)
  
  for (t in 1 : nrow(simulated_forecast)){
    for (i in 2 : num_q_vals - 1){
      
      # use normal input to set test quantiles
      
      Forecast_quantiles[t,i] <- max(0, qnorm(q_vals[i], forecast$mean_demand[t], 
                                              forecast$sd_demand[t]))
    }  
    # force a 0th percentile  
    
    Forecast_quantiles[t,1] <- max(0, 2 * Forecast_quantiles[t,2] - Forecast_quantiles[t,3])
    
    # force a 100th percentile
    
    Forecast_quantiles[t,num_q_vals] <- 2 * Forecast_quantiles[t,num_q_vals - 1] - 
      Forecast_quantiles[t, num_q_vals - 2] 
  }
  
  return(Forecast_quantiles)
}