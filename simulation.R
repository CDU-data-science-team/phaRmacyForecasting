
library(tidyverse)

load("data/pharmacy.rda")
load("data/simulated_forecast.rda")
load("data/Outstanding_orders.rda")

# set seed for sampling for dev/test

lower_lead = 1.5 # discrete min - 0.5
upper_lead = 10.5 # discrete max - 0.5
mode_lead = 4 # discrete mode

# set distribution for delivery lead time

lead_time_dis <- distr6::Triangular$new(lower = lower_lead, 
                                        upper = upper_lead,
                                        mode = mode_lead)

# produce data

pharmacy_sub <- pharmacy %>% 
  filter(Site1 == "Site C", NSVCode == "Drug A")

daily_data <- phaRmacyForecasting:::make_tsibble(pharmacy_sub, frequency = "Daily")

daily_data <- daily_data %>% 
  mutate(inventory = sample(2000 : 6000, nrow(.)))

daily_forecast <- phaRmacyForecasting:::forecast_series(daily_data, horizon = 28, 
                                                        frequency = "Daily")

# produce forecast

for(day in 2310 : nrow(daily_data)){

  daily_forecast <- phaRmacyForecasting:::forecast_series(daily_data %>% 
                                                            slice(1 : day), 28, 
                                                          frequency = "Daily")
  
  actual_forecast <- daily_forecast %>% 
    filter(.model == "ETS")
  
  step <- phaRmacyForecasting:::drug_quantity(forecast = daily_forecast,
                distribution = lead_time_dis,
                min_stock = 1000,
                max_stock = 20000,
                p_min = 0.01,
                p_max = 0.05,
                inv_i = daily_data$inventory[day],
                delta_pref = 14)
  
  print(step$Q_i)
  
  print("####")
  
  print(step$Delta_i)
}

