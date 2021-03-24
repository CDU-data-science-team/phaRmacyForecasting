test_that("Data produced correctly", {
  
  library(dplyr)
  
  test_data <- pharmacy %>% 
    filter(Site1 == "Site A", NSVCode == "Drug A")
  
  daily_data <- make_tsibble(test_data, frequency = "Daily")
  
  weekly_data <- make_tsibble(test_data, frequency = "Weekly")
  
  testthat::expect_equal(nrow(daily_data), 2313)
  
  testthat::expect_equal(nrow(weekly_data), 330)
  
})

# test_that("Forecast produced", {
#   
#   library(dplyr)
#   
#   test_data <- pharmacy %>% 
#     filter(Site1 == "Site A")
#   
#   daily_data <- make_tsibble(test_data, frequency = "Daily")
#   
#   weekly_data <- make_tsibble(test_data, frequency = "Weekly")
#   
#   daily_forecast <- forecast_series(daily_data, 42, frequency = "Daily")
#   
#   weekly_forecast <- forecast_series(daily_data, 6, frequency = "Weekly")
# })
