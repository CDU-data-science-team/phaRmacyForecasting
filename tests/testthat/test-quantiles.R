test_that("quantiles generated", {
  
  # load("data/pharmacy.rda")
  
  test_data <- pharmacy %>%
    dplyr::filter(Site1 == "Site C", NSVCode == "Drug A")
  
  daily_data <- phaRmacyForecasting:::make_tsibble(test_data, frequency = "Daily")
  
  test_forecast <- phaRmacyForecasting:::forecast_series(daily_data, 
                                                         42, frequency = "Daily")
  
  quantiles <- phaRmacyForecasting:::make_quantiles(test_forecast)
  
  testthat::expect_equal(nrow(quantiles), 42)
})