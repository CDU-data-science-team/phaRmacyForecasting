Q_toomuch_Q <-function() {

# function indicates if Q_i meets storage condition and, if not, gives next Q_i to try  
    
  epsilon <- 0.01
  flag2 = 0
  P_Q_toomuch <- c(rep(1, Forecast_length)) # initialise as too much
  Prob_x <-c(rep(0, Forecast_length)) # initialise to zero
  term_x <- c(rep(0, Forecast_length))  
  
  Q_out <- sum(Outstanding_orders$Ord_quant)
  
  for (x in seq(1,Forecast_length,1)){
    
    Prob_x[x] <- lead_time_dis$cdf(x)- lead_time_dis$cdf(x-1)  # probability that this delivery will be on day x
#    P_Q_toomuch[x] <-  pnorm((Inv_i+Q_out+Q_i-Storage_constraint),Forecast$mean_cumulative_demand[x],Forecast$sd_cumulative[x], lower.tail = TRUE)    
#    P_Q_toomuch[x] <- Fcast[x]$cdf(Inv_i+Q_out+Q_i-Storage_constraint,lower.tail = TRUE )   
    P_Q_toomuch[x] <- pwlcdf(Forecast_quantiles,q_vals,num_q_vals,x,Inv_i+Q_out+Q_i-Storage_constraint) # gives probability that, if this delivery arrives at time x, it will breach storage constraint
  }
  
  term_x <- Prob_x * P_Q_toomuch  # joint probability that delivery occurs at x and that Q_i breaches storage if so
  
  gam <- sum(term_x)   # summming over all possible x, this is prob that delivery will breach storage on arrival
  
  if (gam > tol_storage) {   # compare to tolerance 
    
    sc <-  (tol_storage / gam) * (1-epsilon) # get reduction required in prob of breach
    x_peak <- which.max(term_x) # find x with biggest contribution to prob of breach
    
    P_target <- P_Q_toomuch[x_peak] * sc   # get target for reduced contribution from biggest term
    
#    B <- qnorm(P_target,Forecast$mean_cumulative_demand[x_peak],Forecast$sd_cumulative[x_peak],lower.tail = TRUE)
#    B <- Fcast[x_peak]$quantile(P_target,lower.tail = TRUE)  
     B <- pwlquant(Forecast_quantiles,q_vals,num_q_vals,x_peak,P_target)   # get from forecast the demand associated with target prob
    
        Q_i <- B - Inv_i - Q_out + Storage_constraint # set Q_i to meet that demand.
                                                      # note this is not guaranteed to be enough of a reduction.  
  } else {
    
    flag2 = 1        
    
  }
  
  res <- c(Q_i,flag2)
  
  return(res)
}