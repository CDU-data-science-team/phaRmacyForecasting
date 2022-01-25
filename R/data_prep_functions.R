#' Calculate mode
#' 
#' @description Funnily enough, R doesn't have a mode, so this is it. 
#' Be careful because ties just return whichever is the furthest to the 
#' beginning of the vector with no warning given
#' 
#' @param v Vector of numerical values
#' @return number- mode of the supplied vector
#' @export

calc_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Get holidays
#' 
#' @description Download English bank and other holidays from gov.uk
#' 
#' @return vector of dates as string YYYY-MM-DD
#' @export
get_holidays <- function(){
  
  jsonlite::fromJSON(
    "https://www.gov.uk/bank-holidays.json")$`england-and-wales`$events$date
}

#' return number of weekdays
#' 
#' @description return the number of weekdays between two days, excluding 
#' weekends (obviously) and holidays (user defined)
#' @param from Date. Date to count from (inclusive)
#' @param to Date. Date to count to (inclusive)
#' @param holidays you can get this (for England) with 
#' \code{\link{get_holidays}}
#' 
#' @return integer. Number of weekdays between two dates
#' @export

n_weekdays <- function(from, to, holidays) { 
  
  possible_days <- seq(from, to, "days")
  # Count all days that are not weekend and are not holidays
  sum(!weekdays(possible_days) %in% c("Saturday", "Sunday") & !possible_days %in% holidays)
}
