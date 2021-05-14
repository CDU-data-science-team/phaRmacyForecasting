pwlcdf <-function(Forecast_quantiles, q_vals, num_q_vals, time_point, x){ # set up as piece wise linear from input.
    
    res <- numeric(length(x))
    
    for(i in seq(1:length(x))){
      
    
    
    
    low_cut <- Forecast_quantiles[time_point,1]
    high_cut <- Forecast_quantiles[time_point, num_q_vals]

        
    res[i][x[i] >= high_cut] = 1
    
    for(j in seq(1,num_q_vals-1,1)) {
      res[i][x[i] >= Forecast_quantiles[time_point,j] & x[i] < Forecast_quantiles[time_point,j+1]] = q_vals[j] + (q_vals[j+1]-q_vals[j])*(x[i] -Forecast_quantiles[time_point,j])/(Forecast_quantiles[time_point,j+1]-Forecast_quantiles[time_point,j])
      
    }

    res[i][x[i] <= low_cut] = 0
    
    }
    return(res)
    
  }
  

  
  
  
  
  
