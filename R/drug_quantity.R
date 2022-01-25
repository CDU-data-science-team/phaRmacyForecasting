#' Return the amount of a drug to be ordered
#' 
#' @param forecast dataframe with two columns- mean_demand and sd_demand
#' @param distribution distr6 object defining the distribution of order arrival
#' @param min_stock number level below which we want a probability < p_min of falling 
#' @param max_stock number level above which we want a prob < p_max of rising
#' @param p_min number probability of going into emergency stock
#' @param p_max number probability of ordering too much
#' @param inv_i int the inventory at the time of this (the i'th) order
#' @param delta_pref int the preferred order interval for drug in question
#' @param outstanding_orders int the number of units in outstanding orders
#' @return number giving amount to order
#' @export
drug_quantity <- function(forecast, distribution, min_stock, max_stock,
                          p_min, p_max, inv_i, delta_pref,
                          outstanding_orders) {
  
  MaxLoops = 100  # maximum number of loops permitted in each while loop below
  Q_i = 0 # initialise order quantity
  Delta_i = delta_pref # initialise time to next order
  flag_suf = 0 # initialise a flag indicating (=1) that Q_i is sufficent 
  flag_stor = 0 # initialise a flag indicating (=1) that Q_i is not too much 
  Forecast_quantiles <- phaRmacyForecasting:::make_quantiles(forecast)
  
  tmp1 <- c(0,0) # dummy storage for Q_i and flag_suf 
  dummy_counter <- 0
  
  while(flag_suf < 1 & dummy_counter < MaxLoops){ # repeat until Q_i sufficient
    
    dummy_counter <- dummy_counter + 1
    
    # returns whether Q_i sufficient and next Q_i to try
    
    tmp1 <- phaRmacyForecasting:::Q_enough_Q(
      distribution, inv_i, Q_i, outstanding_orders, Forecast_quantiles, 
      Delta_i, min_stock, p_min)
    
    Q_i = tmp1[1]
    
    flag_suf = tmp1[2]
    
  }
  
  # ERROR HANDLING ON POSSIBLE TIME-OUT ABOVE
  
  tmp2 <- c(Q_i, flag_stor)
  
  # returns whether Q_i satisfies storage condition and next Q_i to try
  tmp2 <- phaRmacyForecasting:::Q_toomuch_Q(forecast_q = Forecast_quantiles, 
                                            o_orders = outstanding_orders,
                                            choose_distribution = distribution,
                                            current_q_i = Q_i,
                                            max_stock = max_stock,
                                            p_max = p_max,
                                            inv_i = inv_i)
  
  Q_i <- tmp2[1]
  flag_stor <- tmp2[2]
  
  # if sufficient Q_i breaches storage, need to reduce and shorten Delta
  
  if(flag_stor < 1){
    
    flag_suf <- 0    # reset sufficiency flag to zero as about to reduce Q_i
    
    dummy_counter_2 <- 0
    # look for Q_i that meets storage condition 
    
    while(flag_stor<1 & dummy_counter_2 < MaxLoops){
      dummy_counter_2 <- dummy_counter_2 + 1
      
      # returns whether Q_i satisfies storage condition and next Q_i to try
      
      tmp2 <- Q_toomuch_Q(forecast_q = Forecast_quantiles, 
                          o_orders = outstanding_orders,
                          choose_distribution = distribution,
                          current_q_i = Q_i,
                          max_stock = max_stock,
                          p_max = p_max,
                          inv_i = inv_i) 
      
      Q_i <- tmp2[1]
      flag_stor <- tmp2[2]
      
    }
    
    tmp3 <- c(0,0)   # temporary storage of Delta_i and flag_suf
    dummy_counter_3 <- 0
    
    # now reduce Delta_i until reduced Q_i sufficient     
    
    while(flag_suf < 1 & dummy_counter_3 < MaxLoops){
      dummy_counter_3 <- dummy_counter_3 + 1
      tmp3 <- Q_enough_Delta(Forecast_quantiles,
                             choose_distribution = distribution,
                             d_i = Delta_i,
                             inv_i = inv_i,
                             current_q_i = Q_i,
                             min_stock, 
                             p_min, 
                             outstanding_orders = outstanding_orders)
      
      Delta_i <- tmp3[1]
      flag_suf <- tmp3[2]
      
    }
  }
  return(list("Q_i" = Q_i, 
              "Delta_i" = Delta_i))
}

pwlcdf <- function(Forecast_quantiles, q_vals, num_q_vals, time_point, x){ # set up as piece wise linear from input.
  
  res <- numeric(length(x))
  
  for(i in seq(1:length(x))){
    
    low_cut <- Forecast_quantiles[time_point,1]
    high_cut <- Forecast_quantiles[time_point, num_q_vals]
    
    res[i][x[i] >= high_cut] = 1
    
    for(j in seq(1, num_q_vals - 1, 1)) {
      res[i][x[i] >= Forecast_quantiles[time_point,j] & 
               x[i] < Forecast_quantiles[time_point,j+1]] = 
        q_vals[j] + (q_vals[j+1] - q_vals[j]) * (x[i] - Forecast_quantiles[time_point, j]) / 
        (Forecast_quantiles[time_point,j+1] - Forecast_quantiles[time_point, j])
      
    }
    
    res[i][x[i] <= low_cut] = 0
    
  }
  return(res)
}

pwlquant <- function(Forecast_quantiles, q_vals, num_q_vals, time_point, p){ # set up as piece wise linear from input.
  
  res <- numeric(length(p))
  
  for (i in seq(1, length(p))){
    
    for(j in seq(1, num_q_vals - 1, 1)) {
      
      res[i][p[i] >= q_vals[j] & p[i] < q_vals[j + 1]] = Forecast_quantiles[time_point, j] + 
        (Forecast_quantiles[time_point, j + 1] - 
           Forecast_quantiles[time_point, j])*(p[i] - q_vals[j]) / (q_vals[j + 1] - q_vals[j])
      
    }
    
    res[i][p[i] >= q_vals[num_q_vals]] = Forecast_quantiles[time_point, num_q_vals]
  }
  return(res)
}

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

Q_enough_Q <- function(lead_time_dis, inv_i, current_q_i, outstanding_orders, 
                       Forecast_quantiles, Delta_i, min_stock, p_min){
  
  # This function assesses whether Q_i will be sufficient and, if not, suggests next Q_i to try  
  epsilon <- 0.01
  flag1 = 0     # initialise as insufficient
  phi = 1 # this is the probability that order is insufficient
  P_Q_insuff <- c(rep(1, nrow(Forecast_quantiles))) # this is the prob that, if next  order delivered at time y, Q_i for this order will be insufficient to meet demand to then
  Prob_y <-c(rep(0, nrow(Forecast_quantiles))) # initialise to zero - this is the probability that the next order is delivered at time y
  term_y <- c(rep(0, nrow(Forecast_quantiles))) # 
  num_q_vals <- ncol(Forecast_quantiles)
  q_vals <- c(seq(0,1, 1 / (num_q_vals - 1))) # set up vector of quantile levels - evenly spaced for now
  Q_i <- current_q_i
  Q_out <- sum(outstanding_orders) # quantity associated with outstanding orders
  
  # now set probabilities for next delivery arriving at time y
  # NOTE NEED TO CHECK DISCRETISATION AGAINST FORECAST
  # and Q_i being insufficient if that is the case
  
  for (y in seq(1, nrow(Forecast_quantiles), 1)){
    
    Prob_y[y] <- lead_time_dis$cdf(y - Delta_i) - lead_time_dis$cdf(y - Delta_i - 1)
    
    P_Q_insuff[y] <- 1 - phaRmacyForecasting:::pwlcdf(
      Forecast_quantiles, q_vals, num_q_vals, y, inv_i + Q_out + Q_i - min_stock) # returns probability that demand up to and including day y eats into emergency stock
  }
  
  term_y <- Prob_y * P_Q_insuff  # the joint probability that delivery of next order occurs at y and that q_i insufficient if so
  
  phi <- sum(term_y) # sums over all possible delivery times y to give probability that Q_i insufficient
  
  if (phi > p_min) { # if Q_i not sufficient
    
    sc <-  (p_min / phi) * (1 - epsilon) # get scaling factor required to bring prob phi under tolerance value - with slight over-adjustment to stop any asymptotic behaviour
    y_peak <- which.max(term_y)  # find biggest term in phi
    
    P_target <- P_Q_insuff[y_peak] * sc # get target for reduced contribution from biggest term 
    
    B <- phaRmacyForecasting:::pwlquant(Forecast_quantiles, q_vals, num_q_vals, y_peak, (1 - P_target))     # get from forecast the demand associated with that target probability 
    
    Q_i <- B + min_stock - inv_i - Q_out # set Q_i to be sufficient reduce bring biggest term in phi by sc - amount determined above.
    
  } else {
    
    flag1 = 1        
    
  }
  
  res <- c(Q_i,flag1)
  
  return(res)
}

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
