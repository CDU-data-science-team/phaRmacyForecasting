Q_enough_Delta <- function(forecast_q, choose_distribution, d_i, inv_i, current_q_i,
                           min_stock, p_min, outstanding_orders){
  
  # determines whether Q_i is sufficient and, if not, suggests next Delta_i to try 
  # function used when Q_i has been reduced to meet storage condition
  
  epsilon <- 0.01
  flag1 <- 0
  P_Q_insuff <- c(rep(1, nrow(forecast_q))) # initialise as insufficient
  Prob_y <-c(rep(0, nrow(forecast_q))) # initialise to zero
  term_y <- c(rep(0, nrow(forecast_q)))
  Q_i <- current_q_i
  num_q_vals <- ncol(forecast_q)
  q_vals <- c(seq(0,1, 1 / (num_q_vals - 1))) # set up vector of quantile levels - evenly spaced for now
  Delta_i <- d_i
  Q_out <- sum(outstanding_orders)
  
  # now set probabilities for next delivery arriving at time y CHECK DISCRETISATION AGAINST FORECAST
  # and Q_i being insufficient if that is the case
  
  for (y in seq(1, nrow(forecast_q), 1)){    # all this as per Q_enough_Q
    
    Prob_y[y] <- choose_distribution$cdf(y - Delta_i) - choose_distribution$cdf(y - Delta_i - 1)
    P_Q_insuff[y] <- 1 - phaRmacyForecasting:::pwlcdf(
      forecast_q, q_vals, num_q_vals, y, inv_i + Q_out + Q_i - min_stock)
  }
  
  term_y <- Prob_y * P_Q_insuff  
  
  phi <- sum(term_y)
  
  if (phi > p_min) { # Q_i insufficient given current value of Delta_i
    
    sc <-  (p_min / phi) * (1 - epsilon) # get reduction required in phi
    y_peak <- which.max(term_y) # find biggest term in phi
    
    P_target <- Prob_y[y_peak] * sc # find target probability for next delivery being ordered at y
    
    y_hat <- y_peak 
    test <- Prob_y[y_hat]
    
    # effectively we are bringing the order of the next forward until we get 
    # desired reduction in the (currently) biggest contribution to the prob of Q_i being insufficient 
    
    while(test > P_target){   
      
      y_hat <- y_hat + 1  
      test <- Prob_y[y_hat]
    }
    
    Delta_i <- Delta_i -(y_hat - y_peak)
    
  } else {
    
    flag1 = 1        
  }
  if (Delta_i < 1) {
    print("CANNOT BE SOLVED WITH CURRENT INPUTS - REVISE STORAGE CONSTRAINT, 
                          EMERGENCY STOCK OR TOLERANCES")
  } 
  res <- c(Delta_i, flag1)
  
  return(res)
}
