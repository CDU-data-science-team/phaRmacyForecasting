testthat::test_that("drug ordering quantity works", {
  
  # set seed for sampling for dev/test
  
  lower_lead = 1.5 # discrete min - 0.5
  upper_lead = 10.5 # discrete max - 0.5
  mode_lead = 4 # discrete mode
  
  # set distribution for delivery lead time
  
  lead_time_dis <- distr6::Triangular$new(lower = lower_lead, 
                                          upper = upper_lead,
                                          mode = mode_lead)
  
  test_data <- pharmacy %>%
    dplyr::filter(Site1 == "Site C", NSVCode == "Drug A")
  
  daily_data <- phaRmacyForecasting:::make_tsibble(test_data, frequency = "Daily")
  
  test_forecast <- phaRmacyForecasting:::forecast_series(daily_data, 
                                                         42, frequency = "Daily")
  
  test_stock <- drug_quantity(forecast = test_forecast, # simulated forecast for now
                              distribution = lead_time_dis,
                              min_stock = 100,
                              max_stock = 10000,
                              p_min = 0.01,
                              p_max = 0.05,
                              inv_i = 150,
                              delta_pref = 14)
  
  testthat::expect_equal(test_stock$Q_i, 380.1792, tolerance = 1)
  testthat::expect_equal(test_stock$Delta_i, 8)
  
})
