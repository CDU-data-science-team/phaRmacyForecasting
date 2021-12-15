
# library("tidyverse")
library("stringr")
library("data.table")
library("extraDistr")

nsvcode <- "NOT012B"
site <- 240
packsize <- 84
begin_date <- "2015-08-01"
last_date <- "2020-07-31"
starting_inv <- 5000

# Calculate Min, Max & Mode ofr leadtimes & exclude any > 10
leadtime <- readr::read_csv("Received Goods Report 2 years 240.csv",
                            col_types = cols(DateOrdered = col_date("%d/%m/%Y"), 
                                             DateReceived = col_date("%d/%m/%Y")))

# Filter for NSV code and exclude supplies by other pharmacy sites
lt <- dplyr::filter(leadtime, NSVCode == nsvcode & 
                      SupplierSupplier_Value != "COMSP" &
                      SupplierSupplier_Value != "MILL")
# may need to exclude WRC, HH & Rampton if not calculating leadtime by supplier

# Get rid of multiple lines for same order & received date
lt1 <- lt %>% unique() %>% 
  dplyr::group_by(OrderNum, SupplierSupplier_Value, DateOrdered, DateReceived) %>% 
  dplyr::summarise(totq = sum(QtyRec_Value)) 

# Set function for counting days excluding weekends and holidays

# I've deleted this because this function is now defined in the package

# Nweekdays <- function(a, b, holidays, weekend) { 
#   possible_days <- seq(a, b, "days")
#   # Count all days that are not weekend and are not holidays
#   sum(!weekdays(possible_days) %in% weekend & !possible_days %in% holidays)
# }

# Specify weekend days
weekend <- c("Saturday", "Sunday")

# Specify holiday dates
# hols <- c("ChristmasDay", "BoxingDay", "NewYearsDay", "GoodFriday", 
# "EasterMonday", "GBMayDay", "GBBankHoliday", "GBSummerBankHoliday") 
# holidays <- holiday(2017:currentYear, Holiday = hols)
# holidays <- holidayLONDON(year = 2019:getRmetricsOptions("currentYear")) %>% 
# trunc()
# could also use bizdays package

holidays <- as.Date(c("2017-04-14", "2017-04-17", "2017-05-01", "2017-05-29", 
                      "2017-08-28", "2017-12-25", "2017-12-26", "2018-01-01", 
                      "2018-03-30", "2018-04-02", "2018-05-07", "2018-05-28", 
                      "2018-08-27", "2018-12-25", "2018-12-26", "2019-01-01", 
                      "2019-04-19", 
                      "2019-04-22", "2019-05-06", "2019-05-27", "2019-08-26", 
                      "2019-12-25", "2019-12-26",
                      "2020-01-01", "2020-04-10", "2020-04-13", "2020-05-08", 
                      "2020-05-25", "2020-08-31", "2020-12-25", "2020-12-26"))

# Calculate number of days in leadtime excluding weekends and holidays & 
# exclude leadtime of greater than 10 days

lt2 <- lt1 %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(leadtime = (Nweekdays(DateOrdered, DateReceived, holidays, weekend))) %>% 
  subset(leadtime <= 10)


# Find value for min, max & mode for leadtime
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

leadmin <- min(lt2$leadtime)
leadmax <- max(lt2$leadtime)
leadmode <- mode(lt2$leadtime)


# Create product profile

load("data/Outstanding_orders.rda")

drug <- read_csv("Alphabetical drug list 5.5.21.csv")
drug1 <- drug %>% 
  dplyr::filter(Site1 == site & In_Use == "Y" & Stocked == "Y")

# Create time series table for simulation including issue quantities and 
# returned quantities
use <- readr::read_csv("Issues Aug 15 to Jul 20.csv", 
                       col_types = cols(Log_Date_Time = col_date("%d/%m/%Y")))

# for (i in 1:20){

# for (i in 1:dim(drug1)[1]){

# need to select site
# nsvcode <- drug1$NSV_Code[1]  
# packsize <- drug1$Re_Order_Packsize[1]
use1 <- use %>% 
  dplyr::filter(Site1 == site & NSVCode == nsvcode & Ward != "ADJ" & 
                  Ward != "HIGH" & Ward != "MILL" & Ward != "MOCK" &
                  Ward != "WASTE" & Ward != "PHWRC") %>% 
  select(1,3,4,5,9,10) 

drugdescription <- use1$Drug_Description[1]

# Extract Issue quantites
use_issues <- use1 %>% filter(Total_Qty >0) %>%
  rbind(data.frame(Log_Date_Time = begin_date, NSVCode = nsvcode, 
                   Drug_Description = drugdescription, 
                   Total_Qty = 0, Ward = "NA", Site1 = site)) %>% 
  rbind(data.frame(Log_Date_Time = last_date, NSVCode = nsvcode, 
                   Drug_Description = drugdescription, 
                   Total_Qty = 0, Ward = "NA", Site1 = site)) %>% 
  dplyr::arrange(Log_Date_Time) %>% 
  dplyr::group_by(Log_Date_Time, NSVCode, Drug_Description, Site1) %>% 
  dplyr::summarise(Issues = sum(Total_Qty)) %>% ungroup() %>% 
  tidyr::complete(Log_Date_Time = 
                    seq.Date(min(Log_Date_Time), 
                             max(Log_Date_Time), by="day")) %>% 
  tidyr::replace_na(list(NSVCode = nsvcode, 
                         Drug_Description = drugdescription, 
                         Site1 = site, 
                         Issues = 0))

# Extract returned quantities
use_returnsv <- use1 %>% 
  dplyr::filter(Total_Qty < 0) %>% 
  dplyr::group_by(Log_Date_Time, NSVCode, Drug_Description, Site1) %>% 
  dplyr::summarise(Return = sum(Total_Qty)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Returns = abs(Return))

# Create time series table for simulation
sim_table1 <- dplyr::left_join(use_issues, 
                               use_returns, 
                               by = c("Log_Date_Time" = "Log_Date_Time", 
                                      "NSVCode" = "NSVCode", 
                                      "Drug_Description" = "Drug_Description", 
                                      "Site1" = "Site1")) %>% 
  dplyr::select(1,2,3,4,5,7) %>% 
  dplyr::replace_na(list(Returns = 0)) %>% 
  dplyr::mutate(DailyInvLevel = 0, 
                Q_i = 0, 
                OrderQtyinPacksize = 0, 
                Delta_i = 0, 
                Delta_pref = 0, 
                OrderDay = "N", 
                ReceivedQty = 0, 
                Stockouts = 0) 

sim_table1$WeekDay <- weekdays(as.Date(sim_table1$Log_Date_Time))
sim_table1 <- sim_table1[,c(1,15,2,3,4,5,6,7,8,9,10,11,12,13,14)]
# sim_table <- sim_table1 %>% filter(WeekDay != "Saturday", WeekDay != "Sunday")

UsualOrderDay <- "Tuesday" 
Run_number <- 1:50
results <- tibble::tibble(DrugDescription = drugdescription, NSVCode = nsvcode, 
                  Run_number, DailyInv= 0, No.Stockouts = 0, No.Orders = 0, 
                  No.OrdersNotonOrderDay = 0,
                  ForecastMethod = NA, 
                  DateRange = "00/00/0000 - 00/00/0000", MinInv = 0, 
                  MaxInv = 0, MinStockoutLength = 0,
                  MaxStockoutLength = 0)
ForecastMethod <- tibble::tibble(Method = c("ETS", "SNAIVE", "ARIMA", "Prophet"))
z <- 1

# set distribution for delivery lead time - from 
lead_time_dis <- distr6::Triangular$new(lower = leadmin - 0.5, 
                                        upper = leadmax - 0.5,
                                        mode = leadmode)
Outstanding_orders$Ord_quant[1] <- 0

# for(lin in 1:nrow(ForecastMethod)){

for (i in 1:1){
  
  sim_table <- sim_table1 %>% filter(WeekDay != "Saturday", WeekDay != "Sunday")
  day <- 978
  leadrow <- 1
  sim_table$DailyInvLevel[977] <- starting_inv
  sim_table$OrderDay[978] <- "Y"
  sim_table$Delta_pref[978] <- 10
  
  
  
  # Sim May 18 to July 20
  for (day in 978:nrow(sim_table)[1]){
    if (sim_table$OrderDay[day] == "Y") {
      
      # Calculate daily inventory level 
      invlevel <- sim_table$DailyInvLevel[day-1] - sim_table$Issues[day-1] +
        sim_table$Returns[day-1] + sim_table$ReceivedQty[day-1]
      
      if(invlevel > 0){
        sim_table$DailyInvLevel[day] <- invlevel
      }else{
        sim_table$DailyInvLevel[day] <- 0
      }
      
      
      
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
      
      # Average forecast replace with actual code - input would be Q_i i.e. order quantity
      
      # daily_forecast <- sim_table %>% slice(1 : day-1) 
      
      # sim_table$Q_i[day] <- (Delta_pref*(mean(daily_forecast$Issues))) - sim_table$DailyInvLevel[day]
      
      # Need to make sure that don;t get negative quantities where inventory is sufficient, but this may not be necessary 
      # in actual inv model - if so use code above to directly add order quantity
      # orderqty <- (Delta_pref*(mean(daily_forecast$Issues))) - sim_table$DailyInvLevel[day]
      sim_table2 <- sim_table %>% rename(Date = 1, Total_Qty = 6)  
      sim_table2 <- phaRmacyForecasting:::make_tsibble(sim_table3, 
                                                       frequency = "Daily")
      # sim_table3$WeekDay <- weekdays(as.Date(sim_table3$Date))
      # sim_table2 <- sim_table3 %>% filter(WeekDay != "Saturday", WeekDay != "Sunday") %>% select(-(WeekDay))
      # sim_table2 <- phaRmacyForecasting:::make_tsibble(sim_table2, frequency = "Daily")
      # daily_forecast <- phaRmacyForecasting:::forecast_series(sim_table2, horizon = 28, 
      # frequency = "Daily")
      
      # for(day in 978 : nrow(sim_table)){
      
      daily_forecast <- phaRmacyForecasting:::forecast_series(sim_table2 %>% 
                                                                slice(1 : (day + 27)), 
                                                              28, 
                                                              frequency = "Daily")
      
      actual_forecast <- daily_forecast %>% 
        filter(.model == "ARIMA")
      
      # ForecastMethod$Method[lin]
      step <- phaRmacyForecasting:::drug_quantity(
        forecast = actual_forecast,
        distribution = lead_time_dis,
        min_stock = 84,
        max_stock = 30000,
        p_min = 0.01,
        p_max = 0.05,
        inv_i = sim_table$DailyInvLevel[day],
        delta_pref = sim_table$Delta_pref[day] )
      
      orderqty <- step$Q_i
      sim_table$Delta_i[day] <- step$Delta_i
      
      print(step$Q_i)
      print("####")
      print(step$Delta_i)
      
      # }
      
      if(orderqty < 1){
        sim_table$Q_i[day] <- 0
      }else{
        sim_table$Q_i[day] <- orderqty
      }
      
      # round order quantity into whole packs
      sim_table$OrderQtyinPacksize[day] <- (ceiling((sim_table$Q_i[day])/packsize))*packsize
      
      # Use triangular distribution to provide a day to receive the order
      if(sim_table$OrderQtyinPacksize[day] >0){
        ActualLeadtime <- rtriang(1, a = leadmin, b = leadmax, c = leadmode) %>% floor()
        
        # If leadtime means stock arrives before that of previously order them rerun leadtime calculation
        # Assume that orders are processed by wholesaler sequentially
        while(day + ActualLeadtime - 1 < leadrow){ 
          print(ActualLeadtime)
          ActualLeadtime <- rtriang(1, a = leadmin, b = leadmax, c = leadmode) %>% floor()   }
        
        # Multiple orders may arrive on same day, so need to add last quantity from latest order to that of previous order 
        if(day + ActualLeadtime <= 1305){
          
          sim_table$ReceivedQty[day + ActualLeadtime -1] <-  sim_table$ReceivedQty[day + ActualLeadtime -1] + 
            sim_table$OrderQtyinPacksize[day]
        }
      }
      
      # need to consider whether this should be delta_pref or Delta_i
      # what happens if Delta_i is less than the numbers here?
      if(day + 10 <= 1293){
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
    
    
    Extract <-subset(sim_table, Log_Date_Time >= "2019-07-01" & Log_Date_Time <= "2020-06-30")  
    results$DailyInv[z] <- mean(Extract$DailyInvLevel) %>% ceiling()
    results$No.Orders[z] <- nrow(Extract[Extract$OrderQtyinPacksize > 0,])
    
    no <- 1
    
    for (entry in 1:nrow(Extract)[1]){
      if(entry == 1){
        if(Extract$DailyInvLevel[entry] == 0){
          Extract$Stockouts[entry] <- no
          no <- (Extract$Stockouts[entry] + 1) 
        }
      } else {
        if(Extract$DailyInvLevel[entry] == 0){
          Extract$Stockouts[entry] <- no
          no <- (Extract$Stockouts[entry] + 1)
          if(Extract$DailyInvLevel[entry - 1] == 0){
            Extract$Stockouts[entry] <- Extract$Stockouts[entry - 1]
            no <- (Extract$Stockouts[entry - 1] + 1)
          }}}
    }   
    
    results$No.Stockouts[z] <- max(Extract$Stockouts)
    results$DateRange[z] <- paste(Extract$Log_Date_Time[1], "-", Extract$Log_Date_Time[nrow(Extract)])
    results$ForecastMethod[z] <- "ARIMA"
    # ForecastMethod$Method[lin]
    results$No.OrdersNotonOrderDay[z]<-  Extract %>% 
      dplyr::filter(OrderDay == "Y" & WeekDay != UsualOrderDay) %>% 
      dplyr::count()
    results$MaxInv[z] <- max(Extract$DailyInvLevel)
    results$MinInv[z] <- min(Extract$DailyInvLevel)
    Extract1 <- Extract %>% 
      dplyr::group_by(Stockouts) %>% 
      dplyr::count() %>% 
      dplyr::filter(Stockouts != 0)
    results$MinStockoutLength[z] <- min(Extract1$n)
    results$MaxStockoutLength[z] <- max(Extract1$n)            
    
  }
  z <-  z + 1
  leadrow <- 1 
}
# }

