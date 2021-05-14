Q_enough_Q <-function(lead_time_dis,Inv_i, Outstanding_orders, Forecast){

# This function assesses whether Q_i will be sufficient and, if not, suggests next Q_i to try  
    
  epsilon <- 0.01
  flag1 = 0     # initialise as insufficient
  phi = 1 # this is the probability that order is insufficient
  P_Q_insuff <- c(rep(1,Forecast_length)) # this is the prob that, if next  order delivered at time y, Q_i for this order will be insufficient to meet demand to then
  Prob_y <-c(rep(0,Forecast_length)) # initialise to zero - this is the probability that the next order is delivered at time y
  term_y <- c(rep(0,Forecast_length)) # 
  
  Q_out <- sum(Outstanding_orders$Ord_quant) # quantity associated with outstanding orders
  
# now set probabilities for next delivery arriving at time y. NOTE NEED TO CHECK DISCRETISATION AGAINST FORECAST
# and Q_i being insufficient if that is the case
  
  for (y in seq(1,Forecast_length,1)){
    
   Prob_y[y] <- lead_time_dis$cdf(y-Delta_i)- lead_time_dis$cdf(y-Delta_i-1)

#   P_Q_insuff[y] <- pnorm((Inv_i+Q_out+Q_i-Emergency_stock),Forecast$mean_cumulative_demand[y],Forecast$sd_cumulative[y], lower.tail = FALSE)     
#   P_Q_insuff[y] <- Fcast[y]$cdf(Inv_i+Q_out+Q_i-Emergency_stock,lower.tail=FALSE)    # - keep in case want to revert to Distr6 vectors of distributions

       P_Q_insuff[y] <- 1-pwlcdf(Forecast_quantiles,q_vals,num_q_vals,y,Inv_i+Q_out+Q_i-Emergency_stock) # returns probability that demand up to and including day y eats into emergency stock
   
          }
  
   term_y <- Prob_y * P_Q_insuff  # the joint probability that delivery of next order occurs at y and that q_i insufficient if so
  
   phi <- sum(term_y) # sums over all possible delivery times y to give probability that Q_i insufficient
   
   if (phi > tol_sufficiency) {   # if Q_i not sufficient
     
      sc <-  (tol_sufficiency / phi) * (1-epsilon) # get scaling factor required to bring prob phi under tolerance value - with slight over-adjustment to stop any asymptotic behaviour
      y_peak <- which.max(term_y)  # find biggest term in phi
      
      P_target <- P_Q_insuff[y_peak] * sc # get target for reduced contribution from biggest term 
      
#      B <- qnorm(P_target,Forecast$mean_cumulative_demand[y_peak],Forecast$sd_cumulative[y_peak],lower.tail = FALSE) 
#       B <- Fcast[y_peak]$quantile(P_target,lower.tail = FALSE)
 
        B <- pwlquant(Forecast_quantiles,q_vals, num_q_vals,y_peak,(1-P_target))     # get from forecast the demand associated with that target probability 
        
        Q_i <- B + Emergency_stock - Inv_i - Q_out # set Q_i to be sufficient reduce bring biggest term in phi by sc - amount determined above.
                                                   # note this is not guaranteed to be enough - hence function called in loop.      
   } else {
      
     flag1 = 1        
     
         }
  
   res <- c(Q_i,flag1)
  
   return(res)
}