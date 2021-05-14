pwlquant <-function(Forecast_quantiles, q_vals, num_q_vals,time_point,p){ # set up as piece wise linear from input.
  
  res <- numeric(length(p))
  
  for (i in seq(1,length(p))){

    for(j in seq(1,num_q_vals-1,1)) {
      
      res[i][p[i] >= q_vals[j] & p[i] < q_vals[j+1]] = Forecast_quantiles[time_point, j] + 
        (Forecast_quantiles[time_point,j+1] - 
           Forecast_quantiles[time_point,j])*(p[i]-q_vals[j])/(q_vals[j+1]-q_vals[j])
      
    }
    
    res[i][p[i] >= q_vals[num_q_vals]] = Forecast_quantiles[time_point, num_q_vals]
    
  }
  
  return(res)
}

