test_that("quantiles generated", {
  
  # load("data/pharmacy.rda")
  
  test_data <- pharmacy %>%
    dplyr::filter(Site1 == "Site C", NSVCode == "Drug A")
  
  daily_data <- make_tsibble(test_data, frequency = "Daily")
  
  test_forecast <- forecast_series(daily_data, 42, frequency = "Daily")
  
  quantiles <- make_quantiles(test_forecast)
  
  testthat::expect_equal(nrow(quantiles), 42)
})