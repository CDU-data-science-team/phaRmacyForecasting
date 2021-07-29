
library(magrittr)

# load data

# calculate lead time using averages from leadtime.rda

load("data/leadtime.rda")

# (this should probably go in data_raw really)

holidays <- get_holidays()

leadtime <- leadtime %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(leadtime = phaRmacyForecasting:::n_weekdays(
    DateOrdered, DateReceived, holidays)
  )

# summary of the packsizes and reorder levels for each drug

load("data/drug1.rda")

# look at issues and returns per day and summarise to show in and out

load("data/use.rda")

# outstanding orders are currently ignored

begin_date <- "2015-08-01"
last_date <- "2020-07-31"
UsualOrderDay <- "Tuesday"

# let's start with drug A

drug <- "Drug A"

# issues of drug A

# CHECK does Ward always require same filters

issues <- use %>% 
  dplyr::filter(NSVCode == drug,
                !Ward %in% c("ADJ", "HIGH", "MILL", "MOCK", "WASTE", "PHWRC"))

# lead time distribution of drug A

leadmin <- min(leadtime$leadtime)
leadmax <- max(leadtime$leadtime)
leadmode <- phaRmacyForecasting:::mode(leadtime$leadtime)

# maximum has ceiling of 10

leadmax <- ifelse(leadmax > 10, 10, leadmax)

# set distribution for delivery lead time
lead_time_dis <- distr6::Triangular$new(lower = leadmin - 0.5, 
                                        upper = leadmax - 0.5,
                                        mode = leadmode)

start_row <- 978

method <- "ETS"

use_issues <- use %>% 
  dplyr::rename(Date = Log_Date_Time) %>% 
  phaRmacyForecasting:::make_tsibble(frequency = "Daily") %>% 
  dplyr::mutate(day_of_week = weekdays(Date))

daily_forecast <- phaRmacyForecasting:::forecast_series(
  use_issues %>% 
    dplyr::slice(1 : start_row), 28, 
  frequency = "Daily")

actual_forecast <- daily_forecast %>% 
  dplyr::filter(.model == method)

step <- phaRmacyForecasting:::drug_quantity(forecast = actual_forecast,
                                            distribution = lead_time_dis,
                                            min_stock = packsize,
                                            max_stock = 30000,
                                            p_min = 0.005,
                                            p_max = 0.05,
                                            inv_i = sim_table$DailyInvLevel[day],
                                            delta_pref = sim_table$Delta_pref[day] )