#'Make a tsibble suitable for forecsating
#' @param data Dataframe with two columns 'Date', and 'Total_Qty'
#' @param frequency String. One of 'Daily' or 'Weekly'
#' @param remove_weekends Boolean. Set to TRUE to remove weekends
#' @return tsibble with two columns 'Date' and 'quantity'
#' @export

make_tsibble <- function(data, frequency = "Daily", remove_weekends = FALSE){
  
  if(frequency == "Weekly"){
    
    return(
      data %>% 
        dplyr::mutate(Date = lubridate::floor_date(Date, "week"),
                      Date = tsibble::yearweek(Date)) %>% 
        dplyr::filter(Total_Qty >= 0) %>% 
        dplyr::group_by(Date) %>%
        dplyr::summarise(quantity = sum(Total_Qty, na.rm = TRUE)) %>% 
        dplyr::ungroup() %>% 
        head(-1) %>% # remove the last row in case it isn't a complete week
        tsibble::tsibble(index = Date) %>% 
        tsibble::fill_gaps(quantity = 0)
    )
    
  } else {
    
    if(remove_weekends){
      
      return(
        data %>%
          dplyr::filter(Total_Qty >= 0,
                        !lubridate::wday(Date, label = TRUE) %in% c("Sat", "Sun")) %>% 
          dplyr::group_by(Date) %>%
          dplyr::summarise(quantity = sum(Total_Qty, na.rm = TRUE)) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(observation = dplyr::row_number()) %>% 
          tsibble::tsibble(key = observation) %>% 
          tsibble::fill_gaps(quantity = 0)
      )
    } else {
      
      return(
        data %>%
          dplyr::filter(Total_Qty >= 0) %>% 
          dplyr::group_by(Date) %>%
          dplyr::summarise(quantity = sum(Total_Qty, na.rm = TRUE)) %>% 
          dplyr::ungroup() %>% 
          tsibble::tsibble(index = Date) %>% 
          tsibble::fill_gaps(quantity = 0)
      )
    }
  }
}

#' Make a forecast of drug issues
#' @param data tsibble with two columns 'Date' and 'Quantity'. You can make 
#' this with \code{\link{make_tsibble}}
#' @param horizon integer. number of days/ weeks to forecast
#' @param frequency String. One of 'Daily' or 'Weekly'- return daily or 
#' weekly forecasts
#' @return mable with different forecast functions applied - mean, 
#' snaive (weekly seasonality), ARIMA (auto), and ETS (weekly seasonality for 
#' daily forecast, and none with weekly forecast)
#' @export

forecast_series <- function(data, horizon, frequency = "Daily"){
  
  drug_train <- data
  
  if(frequency == "Daily"){
    
    values <- c("week", "A")
    
  } else {
    
    values <- c("year", "N")
  }
  
  drug_train %>% 
    fabletools::model("MEAN" = fable::MEAN(quantity),
                      "SNAIVE" = fable::SNAIVE(quantity ~ lag(values[1])), 
                      # "PROPHET" = fable.prophet::prophet(quantity),
                      "ARIMA" = fable::ARIMA(quantity, approximation = FALSE),
                      "ETS" = fable::ETS(quantity ~ season(method = values[2]))) %>%
    fabletools::forecast(h = horizon)
}

#' Plot the forecast
#' @description Produce a faceted plot showing the performance of different 
#' forecasting models compared with the actual issues over that time span
#' @param data A tsibble, the same one the forecast is based on
#' @param forecast_value A forecast. You can make this with 
#' \code{\link{forecast_series}}
#' @return A faceted ggplot
#' @export

plot_forecast <- function(forecast_value, data, horizon, model){
  
  tibble::as_tibble(forecast_value) %>% 
    dplyr::bind_rows(tibble::as_tibble(data) %>% 
                       tail(horizon) %>% 
                       dplyr::rename(.mean = quantity) %>% 
                       dplyr::mutate(.model = "Actual")) %>% 
    dplyr::mutate(Date = as.Date(Date)) %>% 
    ggplot2::ggplot(ggplot2::aes(x = Date, y = .mean, 
                                 group = .model, colour = .model)) + 
    ggplot2::geom_line() + ggplot2::facet_wrap( ~ .model)
}

#' Show the accuracy of a set of forecasts
#' @param data tsibble with two columns 'Date' and 'Quantity'. You can make 
#' this with \code{\link{make_tsibble}}
#' @param forecast_value. A forecast. You can make this with 
#' \code{\link{forecast_series}}
#' @return dataframe with two columns showing model and MAE
#' @description Take a mable of forecasts and the actual drug issues and compute
#' mean absolute error
#' @export

show_accuracy <- function(forecast_value, data, horizon){
  
  forecast_value %>% 
    fabletools::accuracy(data) %>% 
    dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), signif, 3)) %>%
    dplyr::select(model = .model, MAE)
}
