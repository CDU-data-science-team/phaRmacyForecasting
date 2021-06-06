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
#' @return number giving amount to order
#' @export
drug_quantity <- function(forecast, distribution, min_stock, max_stock,
                          p_min, p_max, inv_i, delta_pref) {

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
      distribution, inv_i, Q_i, Outstanding_orders, Forecast_quantiles, 
      Delta_i, min_stock, p_min)
    
    Q_i = tmp1[1]
    
    flag_suf = tmp1[2]
    
  }
  
  # ERROR HANDLING ON POSSIBLE TIME-OUT ABOVE
  
  tmp2 <- c(Q_i, flag_stor)
  
  # returns whether Q_i satisfies storage condition and next Q_i to try
  tmp2 <- phaRmacyForecasting:::Q_toomuch_Q(forecast_q = Forecast_quantiles, 
                                            o_orders = Outstanding_orders,
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
      
      tmp2 <- phaRmacyForecasting:::Q_toomuch_Q(forecast_q = Forecast_quantiles, 
                                                o_orders = Outstanding_orders,
                                                choose_distribution = distribution,
                                                current_q_i = Q_i,
                                                max_stock = max_stock,
                                                p_max = p_max,
                                                inv_i = inv_i) 
      
      Q_i <- tmp2[1]
      flag_stor <- tmp2[2]
      
    }
    
    tmp3 <- c(0,0)   # temporary storage of Delta_i  and flag_suf
    dummy_counter_3 <- 0
    
    # now reduce Delta_i until reduced Q_i sufficient     
    
    while(flag_suf < 1 & dummy_counter_3 < MaxLoops){
      dummy_counter_3 <- dummy_counter_3 + 1
      tmp3 <- phaRmacyForecasting:::Q_enough_Delta(Forecast_quantiles,
                                                   choose_distribution = distribution,
                                                   d_i = Delta_i,
                                                   inv_i = inv_i,
                                                   current_q_i = Q_i,
                                                   min_stock, 
                                                   p_min)
      
      Delta_i <- tmp3[1]
      flag_suf <- tmp3[2]
      
    }
  }
  return(list("Q_i" = Q_i, 
              "Delta_i" = Delta_i))
}
