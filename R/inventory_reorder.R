#' Forecast and run inventory management algorithm, returning reorder quantities
#' by site and drug
#'
#' @param site String. Site code (selected from dynamic UI listing all sites)
#' @param supplier String. Supplier (changes weekly, selected in Shiny interface)
#'
#' @return
#' @export
#'
inventory_reorder <- function(site, supplier){
  
  order_list <- product_sup_profile %>% 
    dplyr::filter(Site == site, Supplier_name == supplier) %>% 
    dplyr::arrange(Drug_name)
}

#' Set function for counting days excluding weekends and holidays
#'
#' @param from Date. Beginning of date sequence
#' @param to Date. End of date sequence
#'
#' @return
#' @export
#'
Nweekdays <- function(from, to) {
  
  weekend <- c("Saturday", "Sunday")
  
  holidays <- phaRmacyForecasting::get_holidays()
  
  possible_days <- seq(a, b, "days")
  # Count all days that are not weekend and are not holidays
  sum(!weekdays(possible_days) %in% weekend & !possible_days %in% holidays)
}
