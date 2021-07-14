# library("tidyverse")
# library("stringr")
# library("data.table")
# library("extraDistr")
# library("lubridate")

load("data/Outstanding_orders.rda")
load("data/leadtime.rda")
load("data/drug1.rda")
load("data/use.rda")

begin_date <- "2015-08-01"
last_date <- "2020-07-31"
UsualOrderDay <- "Tuesday"

# Specify weekend days and bank holiday dates
weekend <-  c("Saturday", "Sunday")

holidays <- jsonlite::fromJSON(
  "https://www.gov.uk/bank-holidays.json")$`england-and-wales`$events$date

# Set function for counting days excluding weekends and holidays
Nweekdays <- function(a, b, holidays, weekend) { 
  possible_days <- seq(a, b, "days")
  # Count all days that are not weekend and are not holidays
  sum(!weekdays(possible_days) %in% weekend & !possible_days %in% holidays)
}

# Set up tables

forecast_method <- c("ETS", "SNAIVE", "ARIMA")

Run_number <- 1:200
results <- tibble::tibble(DrugDescription = NA, NSVCode = NA, Run_number, 
                          DailyInv = 0, No.Stockouts = 0, No.Orders = 0, 
                          No.OrdersNotonOrderDay = 0, ForecastMethod = NA, 
                          DateRange = "00/00/0000 - 00/00/0000", MinInv = 0, 
                          MaxInv = 0, MinStockoutLength = 0, 
                          MaxStockoutLength = 0)

Raw_Res <- tibble::tibble(Log_Date_Time = lubridate::ymd("2020-01-01"), 
                          WeekDay = NA, NSVCode = NA, Site1 = 0, Issues = 0, 
                          Returns = 0, DailyInvLevel = 0, Q_i = 0, 
                          OrderQtyinPacksize = 0, Delta_i = 0, Delta_pref = 0, 
                          OrderDay = NA, ReceivedQty = 0, Stockouts = 0, 
                          RunNum = 0, Forecast = NA)

TestDrugs <- tibble::tibble(
  NSV_code = c("Drug C", "Drug A", "Drug B", "Drug D", "Drug H")
)

# Make it so there are no outstanding orders
Outstanding_orders$Ord_quant[1] <- 0

z <- 1

# chem = individual drug
chem <- 1   
inv_row <- 977
start_row <- 978
end_row <- 1291
start_date <- "2019-07-01"
end_date <- "2020-06-30"
site <- 240

# For loop for each drug in TestDrugs table
for(chem in 1:nrow(TestDrugs)){
  
  # filter time series data for specific site and drug and exclude issues to other 
  # dispensaries and housekeeping codes
  use1 <- use %>% 
    dplyr::filter(Site1 == site,
                  NSVCode == TestDrugs$NSV_code[chem],
                  !Ward %in% c("ADJ", "HIGH", "MILL", "MOCK", "WASTE", "PHWRC"))
  
  nsvcode <- TestDrugs$NSV_code[chem]
  packsize <- drug1 %>% 
    dplyr::filter(NSV_Code == nsvcode)
  packsize <- packsize$Re_Order_Packsize
  
  # Calculate number of days in leadtime excluding weekends and holidays & 
  # exclude leadtime of greater than 10 days
  
  lt2 <- leadtime %>% 
    dplyr::filter(NSVCode == nsvcode,
                  !SupplierSupplier_Value %in% c("COMSP", "MILL")) %>% 
    unique() %>% 
    dplyr::group_by(OrderNum, SupplierSupplier_Value, DateOrdered, DateReceived) %>% 
    dplyr::summarise(totq = sum(QtyRec_Value)) %>% 
    dplyr::rowwise() %>%  
    dplyr::mutate(leadtime = Nweekdays(DateOrdered, DateReceived, holidays, 
                                       weekend)) %>% 
    dplyr::filter(leadtime <= 10)
  
  # Find value of min, max & mode for leadtime
  mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  leadmin <- min(lt2$leadtime)
  leadmax <- max(lt2$leadtime)
  leadmode <- mode(lt2$leadtime)
  
  # set distribution for delivery lead time
  lead_time_dis <- distr6::Triangular$new(lower = leadmin - 0.5, 
                                          upper = leadmax - 0.5,
                                          mode = leadmode)
  
  # Extract Issue quantites
  use_issues <- use1 %>% 
    dplyr::filter(Total_Qty >0) %>%
    rbind(data.frame(Log_Date_Time = begin_date, NSVCode = nsvcode, 
                     Total_Qty = 0, Ward = "NA", Site1 = site)) %>% 
    rbind(data.frame(Log_Date_Time = last_date, NSVCode = nsvcode, 
                     Total_Qty = 0, Ward = "NA", Site1 = site)) %>% 
    dplyr::arrange(Log_Date_Time) %>% 
    dplyr::group_by(Log_Date_Time, NSVCode, Site1) %>% 
    dplyr::summarise(Issues = sum(Total_Qty)) %>% 
    dplyr::ungroup() %>% 
    tidyr::complete(Log_Date_Time = seq.Date(min(Log_Date_Time), 
                                             max(Log_Date_Time), by = "day")) %>% 
    tidyr::replace_na(list(NSVCode = nsvcode, 
                           Site1 = site, 
                           Issues = 0))
  
  # Extract returned quantities
  use_returns <- use1 %>% 
    dplyr::filter(Total_Qty < 0) %>% 
    dplyr::group_by(Log_Date_Time, NSVCode, Site1) %>% 
    dplyr::summarise(Return = sum(Total_Qty)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(Returns = abs(Return))
  
  # Create time series table for simulation
  sim_table1 <- dplyr::left_join(use_issues, use_returns, 
                                 by = c("Log_Date_Time" = "Log_Date_Time", 
                                        "NSVCode" = "NSVCode", 
                                        "Site1" = "Site1")) %>% 
    tidyr::replace_na(list(Returns = 0)) %>% 
    dplyr::mutate(DailyInvLevel = 0, 
                  Q_i = 0, 
                  OrderQtyinPacksize = 0, 
                  Delta_i = 0, 
                  Delta_pref = 0, 
                  OrderDay = "N",
                  ReceivedQty = 0, 
                  Stockouts = 0) 
  
  sim_table1$WeekDay <- weekdays(as.Date(sim_table1$Log_Date_Time))
  sim_table1 <- sim_table1 %>% 
    dplyr::select(Log_Date_Time, WeekDay, everything())
  
  # for loop for each forecasting method
  # for(method in forecast_method){
  
  method <- "ETS"
  
  RunNo <- 1
  
  # for loop for number of runs for each drug / forecasting method combination
  # for (i in 1:1){
  i <- 1
  
  sim_table <- sim_table1 %>% 
    dplyr::filter(WeekDay != "Saturday", WeekDay != "Sunday")
  
  day <- start_row
  leadrow <- 1
  RunNo <- 1
  
  # Nominal starting inventory of 10 packs
  starting_inv <- packsize * 10
  sim_table$DailyInvLevel[inv_row] <- starting_inv
  sim_table$OrderDay[start_row] <- "Y"
  sim_table$Delta_pref[start_row] <- 10
  
  # for loop for each day in specified date range
  for (day in start_row : nrow(sim_table)){
    if(day <= end_row){
      if (sim_table$OrderDay[day] == "Y") {
        
        # Calculate daily inventory level 
        invlevel <- sim_table$DailyInvLevel[day - 1] - sim_table$Issues[day - 1] +
          sim_table$Returns[day - 1] + sim_table$ReceivedQty[day - 1]
        
        # can't have negative inventory level - make it zero
        sim_table$DailyInvLevel[day] <- ifelse(invlevel < 0, 0, invlevel)
        
        # if order is not on usual order day adjust delta_pref so next order day is back on usual order day
        if(sim_table$WeekDay[day] == UsualOrderDay){
          Delta_pref <- 10
          sim_table$Delta_pref[day] <- 10
        }else if(sim_table$WeekDay[day] == "Monday"){
          Delta_pref <- 6
          sim_table$Delta_pref[day] <- 6
        }else if(sim_table$WeekDay[day] == "Wednesday"){
          Delta_pref <- 9
          sim_table$Delta_pref[day] <- 9
        }else if(sim_table$WeekDay[day] == "Thursday"){
          Delta_pref <- 8
          sim_table$Delta_pref[day] <- 8
        }else if(sim_table$WeekDay[day] == "Friday"){
          Delta_pref <- 7
          sim_table$Delta_pref[day] <- 7
        }
        
        # Change names on sim table so that will run through forecating function 
        sim_table2 <- sim_table %>% 
          dplyr::rename(Date = Log_Date_Time, Total_Qty = Issues)
        
        sim_table2 <- phaRmacyForecasting:::make_tsibble(sim_table2, frequency = "Daily")
        
        # extract date from sim (i.e. order date) for forecasting period
        daterow <- sim_table$Log_Date_Time[day]
        ForecastDate <- which(grepl(daterow, sim_table2$Date))
        
        # code takes into account that forecasting looks back 28 days, so adjusts start date
        daily_forecast <- phaRmacyForecasting:::forecast_series(sim_table2 %>% 
                                                                  dplyr::slice(1 : ForecastDate + 27), 28, 
                                                                frequency = "Daily")
        
        actual_forecast <- daily_forecast %>% 
          dplyr::filter(.model == method)
        
        # run inventory model to get Q_i and Delta_i
        step <- phaRmacyForecasting:::drug_quantity(forecast = actual_forecast,
                                                    distribution = lead_time_dis,
                                                    min_stock = packsize,
                                                    max_stock = 30000,
                                                    p_min = 0.005,
                                                    p_max = 0.05,
                                                    inv_i = sim_table$DailyInvLevel[day],
                                                    delta_pref = sim_table$Delta_pref[day] )
        orderqty <- step$Q_i
        sim_table$Delta_i[day] <- step$Delta_i
        
        print(step$Q_i)
        print("####")
        print(step$Delta_i)
        
        # can't order less than 1 box
        
        sim_table$Q_i[day] <- ifelse(orderqty < 0, 0, orderqty)
        
        # round order quantity into whole packs
        sim_table$OrderQtyinPacksize[day] <- ceiling((sim_table$Q_i[day]) / packsize) * packsize
        
        # Use triangular distribution to provide a day to receive the order
        if(sim_table$OrderQtyinPacksize[day] > 0){
          ActualLeadtime <- extraDistr::rtriang(1, a = leadmin, b = leadmax, c = leadmode) %>% 
            floor()
          
          # If leadtime means stock arrives before that of previously order them rerun leadtime calculation
          # Assume that orders are processed by wholesaler sequentially
          while(day + ActualLeadtime - 1 < leadrow) { 
            
            print(ActualLeadtime)
            
            ActualLeadtime <- rtriang(1, a = leadmin, b = leadmax, c = leadmode) %>% floor()
          }
          
          # Multiple orders may arrive on same day, so need to add last quantity from latest order to that of previous order 
          if(day + ActualLeadtime <= 1305){
            
            sim_table$ReceivedQty[day + ActualLeadtime -1] <-  sim_table$ReceivedQty[day + ActualLeadtime -1] + 
              sim_table$OrderQtyinPacksize[day]
          }
        }else{
          ActualLeadtime <- 0
        }
        
        # need to consider whether this should be delta_pref or Delta_i
        # what happens if Delta_i is less than the numbers here?
        if(day + 10 <= end_row){
          if(sim_table$WeekDay[day] != UsualOrderDay){
            if(sim_table$WeekDay[day] == "Monday"){
              sim_table$OrderDay[day + 6] <- "Y"
              sim_table$Delta_pref[day + 6] <- 10
            }else if(sim_table$WeekDay[day] == "Wednesday"){
              sim_table$OrderDay[day + 9] <- "Y"
              sim_table$Delta_pref[day + 9] <- 10
            }else if(sim_table$WeekDay[day] == "Thursday"){
              sim_table$OrderDay[day + 8] <- "Y"
              sim_table$Delta_pref[day + 8] <- 10
            }else if(sim_table$WeekDay[day] == "Friday"){
              sim_table$OrderDay[day + 7] <- "Y"
              sim_table$Delta_pref[day + 7] <- 10
            }
            
          }else{
            
            # Needs to be just Delta_i if current order day isn't included in Delta_i
            sim_table$OrderDay[day + step$Delta_i] <- "Y"
          }}
        
        leadrow <- day + ActualLeadtime -1
      }else{
        # Calculate daily inventory level 
        invlevel <- sim_table$DailyInvLevel[day-1] - sim_table$Issues[day-1] +
          sim_table$Returns[day-1] + sim_table$ReceivedQty[day-1]
        
        if(invlevel > 0){
          sim_table$DailyInvLevel[day] <- invlevel
        }else{
          sim_table$DailyInvLevel[day] <- 0
        }     
      }
      
      # extract data from sim - results table is one line for each sim run
      # Raw_res is full daily sim table for each run (to be able to produce daily inventory level graph)
      ExtractA <-subset(sim_table, Log_Date_Time >= start_date & Log_Date_Time <= end_date)
      Extract <- mutate(ExtractA, RunNum = z, Forecast = forecast_type$Method[1])
      results$DailyInv[z] <- mean(Extract$DailyInvLevel) %>% ceiling()
      results$No.Orders[z] <- nrow(Extract[Extract$OrderQtyinPacksize > 0,])
      
      no <- 1
      
      for (entry in 1:nrow(Extract)[1]){
        if(entry == 1){
          if(Extract$DailyInvLevel[entry] == 0){
            Extract$Stockouts[entry] <- no
            no <- (Extract$Stockouts[entry] + 1) 
          }
        }else{
          if(Extract$DailyInvLevel[entry] == 0){
            Extract$Stockouts[entry] <- no
            no <- (Extract$Stockouts[entry] + 1)
            if(Extract$DailyInvLevel[entry - 1] == 0){
              Extract$Stockouts[entry] <- Extract$Stockouts[entry - 1]
              no <- (Extract$Stockouts[entry - 1] + 1)
            }}}
      }   
      
      results$DrugDescription[z] <- drugdescription
      results$NSVCode[z] <- nsvcode
      results$No.Stockouts[z] <- max(Extract$Stockouts)
      results$DateRange[z] <- paste(Extract$Log_Date_Time[1], "-", Extract$Log_Date_Time[nrow(Extract)])
      results$ForecastMethod[z] <- forecast_type$Method[1]
      # ForecastMethod$Method[lin]
      results$No.OrdersNotonOrderDay[z]<-  Extract %>% filter(OrderDay == "Y" & WeekDay != UsualOrderDay) %>% 
        count()
      results$MaxInv[z] <- max(Extract$DailyInvLevel) %>% ceiling()
      results$MinInv[z] <- min(Extract$DailyInvLevel) %>% ceiling()
      Extract1 <- Extract %>% group_by(Stockouts) %>% count() %>% filter(Stockouts != 0)
      results$MinStockoutLength[z] <- min(Extract1$n)
      results$MaxStockoutLength[z] <- max(Extract1$n)   
      
      
      
      
    }}
  
  Raw_Res <- rbind(Raw_Res, Extract)
  z <-  z + 1
  leadrow <- 1 
  
}
RunNo <- RunNo + 1 
}

}

results1 <- select(results, -(No.OrdersNotonOrderDay))
write_csv(results1, "Output/Results First Run for AG 12.6.21.csv")
write_csv(Raw_Res, "Output/Raw Results First Run for AG 12.6.21.csv")


# Create table to average results for multiple runs for each drug / forecasting method
number <-  1:100
Averaged_Results <- tibble(DrugDescription = NA, NSVCode = NA, Number = number,  MeanDailyInv= 0, MeanNo.Stockouts = 0,
                           MeanNo.Orders = 0, Forecast_Method = "type", DateRange = "00/00/0000 - 00/00/0000",
                           MeanMinInv = 0, MeanMaxInv = 0)


rw <- 1 
chem <- 1
lin <-1

for(chem in 1:nrow(TestDrugs)[1]){
  for(lin in 1:nrow(ForecastMethod)[1]){
    
    forecast_type <- ForecastMethod %>% filter( Label == lin) 
    
    Averaged_Results$DrugDescription[rw] <- TestDrugs$DrugDescription[chem]
    Averaged_Results$NSVCode[rw] <- TestDrugs$NSV_code[chem]
    
    Av_res1 <- results %>% filter(NSVCode == TestDrugs$NSV_code[chem])
    
    Av_res2 <- Av_res1 %>%  filter(ForecastMethod == forecast_type$Method[1])
    
    
    Averaged_Results$Forecast_Method[rw] <- Av_res2$ForecastMethod[1]
    Averaged_Results$DateRange[rw] <- Av_res2$DateRange[1]
    
    Averaged_Results$MeanDailyInv[rw] <- mean(Av_res2$DailyInv) %>% ceiling()
    Averaged_Results$MeanNo.Stockouts[rw] <- mean(Av_res2$No.Stockouts)
    Averaged_Results$MeanNo.Orders[rw] <- mean(Av_res2$No.Orders) %>% ceiling()
    Averaged_Results$MeanMinInv[rw] <- mean(Av_res2$MinInv) %>% ceiling()
    Averaged_Results$MeanMaxInv[rw] <- mean(Av_res2$MaxInv)%>% ceiling()
    
    rw <- rw + 1
  }
}

write_csv(Averaged_Results, "Output/Averaged Results First Run for AG 12.6.21.csv")

