
# data and forecasts

make_tsibble <- function(data, frequency = c("Daily", "Weekly")){
  
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

forecast_series <- function(data, horizon, frequency = c("Daily", "Weekly")){
  
  drug_train <- data %>% 
    head(-horizon)
  
  if(frequency == "Daily"){
    
    values <- c("week", "A")
    
  } else {
    
    values <- c("year", "N")
  }
  
  drug_train %>% 
    fabletools::model(fable::SNAIVE(quantity ~ lag(values[1])), 
                      fable::ARIMA(quantity),
                      fable::ETS(quantity ~ season(method = values[2])),
                      fable.prophet::prophet(quantity)) %>%
    forecast::forecast(h = horizon)
}

plot_forecast <- function(forecast_series, data, horizon){
  
  forecast_series %>% 
    dplyr::bind_rows(data %>% 
                       tail(horizon) %>% 
                       dplyr::rename(.mean = quantity) %>% 
                       dplyr::mutate(.model = "Actual")) %>% 
    dplyr::mutate(Date = as.Date(Date)) %>% 
    ggplot2::ggplot(ggplot2::aes(x = Date, y = .mean, 
                                 group = .model, colour = .model)) + 
    ggplot2::geom_line() + ggplot2::facet_wrap( ~ .model)
}
