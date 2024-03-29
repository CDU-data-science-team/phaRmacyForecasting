test_that("Data produced correctly", {
  
  test_data <- pharmacy %>% 
    dplyr::filter(Site1 == "Site A", NSVCode == "Drug A")
  
  daily_data <- make_tsibble(test_data, frequency = "Daily")
  
  weekly_data <- make_tsibble(test_data, frequency = "Weekly")
  
  testthat::expect_equal(nrow(daily_data), 2313)
  
  testthat::expect_equal(nrow(weekly_data), 330)
  
})

test_that("Forecast produced", {
  
  test_data <- pharmacy %>%
    dplyr::filter(Site1 == "Site A", NSVCode == "Drug A")
  
  daily_data <- make_tsibble(test_data, frequency = "Daily")
  
  weekly_data <- make_tsibble(test_data, frequency = "Weekly")
  
  daily_forecast <- forecast_series(
    daily_data %>% head(-42), 42, frequency = "Daily")
  
  weekly_forecast <- forecast_series(
    daily_data %>% head(-6), 6, frequency = "Weekly")
  
  plot_forecast(forecast_value = daily_forecast, 
                                      data = daily_data, 
                                      horizon = 42)
  
  show_accuracy(forecast_value = daily_forecast, 
                                      data = daily_data, 
                                      horizon = 42)
  
})
