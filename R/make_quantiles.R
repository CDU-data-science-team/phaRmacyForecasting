#' Make quantiles from a forecast 
#' 
#' @param forecast dataframe with two columns- mean_demand and sd_demand
#' @param num_q_vals number of quantiles plus 1 (defaults to 101)
#' @return matrix of quantiles
make_quantiles <- function(forecast, model = "ETS"){
  
  final_forecast <- forecast %>% 
    dplyr::filter(.model == model)
  
  quantiles <- purrr::map_dfc(seq(0.1, .9, .1), function(q) {
    
    purrr::map_dbl(1 : nrow(final_forecast), function(x){
      
      final_forecast %>% 
        tibble::as_tibble() %>% 
        dplyr::slice_head(n = x) %>% 
        dplyr::summarise(centile = quantile(sum(quantity), q)) %>% 
        dplyr::pull(centile) %>% 
        pmax(0, .)
    })
  })
  
  return(quantiles)
}