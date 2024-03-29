Q_toomuch_Q <- function(forecast_q, o_orders, choose_distribution, current_q_i,
                        max_stock, p_max, inv_i) {
  
  # function indicates if Q_i meets storage condition and, if not, gives next Q_i to try  
  
  epsilon <- 0.01
  flag2 = 0
  P_Q_toomuch <- c(rep(1, nrow(forecast_q))) # initialise as too much
  Prob_x <-c(rep(0, nrow(forecast_q))) # initialise to zero
  term_x <- c(rep(0, nrow(forecast_q)))
  num_q_vals <- ncol(forecast_q)
  Forecast_length <- nrow(forecast_q)
  q_vals <- c(seq(0,1, 1 / (num_q_vals - 1)))
  Q_out <- sum(o_orders)
  Q_i <- current_q_i
  
  for (x in seq(1, Forecast_length, 1)){
    
    Prob_x[x] <- choose_distribution$cdf(x) - choose_distribution$cdf(x - 1)  # probability that this delivery will be on day x
    
    P_Q_toomuch[x] <- phaRmacyForecasting:::pwlcdf(
      forecast_q, q_vals, num_q_vals, x, inv_i + Q_out + Q_i - max_stock
    ) # gives probability that, if this delivery arrives at time x, it will breach storage constraint
  }
  
  term_x <- Prob_x * P_Q_toomuch  # joint probability that delivery occurs at x and that Q_i breaches storage if so
  
  gam <- sum(term_x)   # summming over all possible x, this is prob that delivery will breach storage on arrival
  
  if (gam > p_max) {   # compare to tolerance 
    
    sc <- (p_max / gam) * (1 - epsilon) # get reduction required in prob of breach
    x_peak <- which.max(term_x) # find x with biggest contribution to prob of breach
    
    P_target <- P_Q_toomuch[x_peak] * sc   # get target for reduced contribution from biggest term
    
    B <- phaRmacyForecasting:::pwlquant(forecast_q, q_vals, num_q_vals, x_peak, P_target)   # get from forecast the demand associated with target prob
    
    Q_i <- B - inv_i - Q_out + max_stock # set Q_i to meet that demand.
    # note this is not guaranteed to be enough of a reduction.  
  } else {
    
    flag2 = 1        
    
  }
  
  res <- c(Q_i, flag2)
  
  return(res)
}
