#' Make quantiles from a forecast 
#' 
#' @param forecast dataframe with two columns- mean_demand and sd_demand
#' @param num_q_vals number of quantiles plus 1 (defaults to 100)
#' @return matrix of quantiles
make_quantiles <- function(forecast, num_q_vals = 100){
  
  quantiles <- purrr::map_dfc(seq(1/num_q_vals, 1-(1/num_q_vals), 1/num_q_vals), 
                              function(q) {

    purrr::map_dbl(1 : nrow(forecast), function(x){
      
      forecast %>% 
        tibble::as_tibble() %>% 
        dplyr::slice_head(n = x) %>% 
        dplyr::summarise(centile = quantile(sum(quantity), q)) %>% 
        dplyr::pull(centile) %>% 
        pmax(0, .)
    })
  })
  
  return(quantiles %>% as.matrix())
}
