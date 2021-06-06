Q_enough_Q <- function(lead_time_dis, inv_i, Outstanding_orders, 
                       Forecast_quantiles, Delta_i, min_stock, p_min){
  
  # This function assesses whether Q_i will be sufficient and, if not, suggests next Q_i to try  
  epsilon <- 0.01
  flag1 = 0     # initialise as insufficient
  phi = 1 # this is the probability that order is insufficient
  P_Q_insuff <- c(rep(1, nrow(Forecast_quantiles))) # this is the prob that, if next  order delivered at time y, Q_i for this order will be insufficient to meet demand to then
  Prob_y <-c(rep(0, nrow(Forecast_quantiles))) # initialise to zero - this is the probability that the next order is delivered at time y
  term_y <- c(rep(0, nrow(Forecast_quantiles))) # 
  num_q_vals <- nrow(Forecast_quantiles)
  q_vals <- c(seq(0,1, 1 / (num_q_vals - 1))) # set up vector of quantile levels - evenly spaced for now
  
  Q_out <- sum(Outstanding_orders$Ord_quant) # quantity associated with outstanding orders
  
  # now set probabilities for next delivery arriving at time y. NOTE NEED TO CHECK DISCRETISATION AGAINST FORECAST
  # and Q_i being insufficient if that is the case
  
  for (y in seq(1, nrow(Forecast_quantiles), 1)){
    
    Prob_y[y] <- lead_time_dis$cdf(y - Delta_i) - lead_time_dis$cdf(y - Delta_i - 1)
    
    P_Q_insuff[y] <- 1 - pwlcdf(
      Forecast_quantiles, q_vals, num_q_vals, y, inv_i + Q_out + Q_i - min_stock) # returns probability that demand up to and including day y eats into emergency stock
  }
  
  term_y <- Prob_y * P_Q_insuff  # the joint probability that delivery of next order occurs at y and that q_i insufficient if so
  
  phi <- sum(term_y) # sums over all possible delivery times y to give probability that Q_i insufficient
  
  if (phi > p_min) { # if Q_i not sufficient
    
    sc <-  (p_min / phi) * (1 - epsilon) # get scaling factor required to bring prob phi under tolerance value - with slight over-adjustment to stop any asymptotic behaviour
    y_peak <- which.max(term_y)  # find biggest term in phi
    
    P_target <- P_Q_insuff[y_peak] * sc # get target for reduced contribution from biggest term 
    
    B <- pwlquant(Forecast_quantiles, q_vals, num_q_vals, y_peak, (1 - P_target))     # get from forecast the demand associated with that target probability 
    
    Q_i <- B + min_stock - inv_i - Q_out # set Q_i to be sufficient reduce bring biggest term in phi by sc - amount determined above.

  } else {
    
    flag1 = 1        
    
  }
  
  res <- c(Q_i,flag1)
  
  return(res)
}
