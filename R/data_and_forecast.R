
# data and forecasts

make_tsibble <- function(data, frequency = "Daily"){
  
  if(frequency == "Weekly"){
    
    data <- data %>% 
      dplyr::mutate(Date = lubridate::floor_date(Date, "week"),
                    Date = tsibble::yearweek(Date))
  }
  
  data %>%
    dplyr::filter(Total_Qty >= 0) %>% 
    dplyr::group_by(Date) %>%
    dplyr::summarise(quantity = sum(Total_Qty, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    head(-1) %>% # remove the last row in case it isn't a complete week
    tsibble::tsibble(index = Date) %>% 
    tsibble::fill_gaps(quantity = 0)
}

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
                      "PROPHET" = fable.prophet::prophet(quantity),
                      "ARIMA" = fable::ARIMA(quantity),
                      "ETS" = fable::ETS(quantity ~ season(method = values[2]))) %>%
    fabletools::forecast(h = horizon)
}

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

show_accuracy <- function(forecast_value, data, horizon){
  
  forecast_value %>% 
    fabletools::accuracy(data) %>% 
    dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), signif, 3)) %>%
    dplyr::select(model = .model, MAE)
}
