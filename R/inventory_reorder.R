#' Forecast and run inventory management algorithm, returning reorder quantities
#' by site and drug
#'
#' @param site String. Site code (selected from dynamic UI listing all sites)
#' @param supplier String. Supplier (changes weekly, selected in Shiny interface)
#' @param product dataframe. Contents of product_sup_profile, loaded in 
#' app_server.R
#' @param w_order dataframe. Contents of w_order_log_df1, loaded in 
#' app_server.R
#'
#' @return
#' @export
#'
inventory_reorder <- function(site, supplier, product, w_order, requis,
                              updateProgress = NULL){
  
  # load holidays
  
  holidays <- get_holidays()
  
  # Settings which aren't yet provided within the code
  risk_of_min_stock <-  0.01
  risk_of_exceeding_max_stock <- 0.05
  time_til_next_order <- 10
  max_storage_capacity <-  30000
  
  # add in other waste / expiry / adjustment codes
  waste_adjust_codes <- c("ADJ","COMSP","EXP", "HADJ","HEXP", "HWAST", "MOCK", 
                          "RWAST", "TEST", "TRG", "WAST", "WASTE", "XXXX")
  
  # takes two reactive inputs- site and supplier
  
  order_list <- product %>% 
    dplyr::filter(Site == site, Supplier_name == supplier) %>% 
    dplyr::arrange(Drug_name)
  
  # select orders placed within the last 2 years
  order_log <- w_order %>% 
    subset(DateOrdered > Sys.Date() - 730) %>% 
    dplyr::filter(Site == site)
  
  purrr::pmap_dfr(order_list, function(Drug_code, ProductID, ...){
    
    # Filter product for relevant product
    
    product_info <- product %>% 
      dplyr::filter(.data$Drug_code == .env$Drug_code, Site == site)
    
    # calculate leadtimes
    
    # Get rid of multiple lines for same order & received date
    ord_log <- order_log %>% 
      dplyr::filter(Kind %in% c("O", "I") & 
                      .data$Drug_code == .env$Drug_code) %>% 
      dplyr::select(OrderNum, Drug_code, DateOrdered, QtyOrd)
    
    if(nrow(ord_log) == 0){
      
      return(
        data.frame(drug = Drug_code, 
                   order = 0, 
                   days_to_order = time_til_next_order)
      )
    }
    
    rec_log <- order_log %>% 
      dplyr::filter(Site == site & Kind == "R" & 
                      .data$Drug_code == .env$Drug_code) %>% 
      dplyr::group_by(OrderNum, Drug_code, Supplier_name, 
                      DateOrdered, DateReceived, Site, Kind) %>%  
      dplyr::summarise(QtyRec = sum(QtyRec)) %>% 
      dplyr::ungroup()
    
    if(nrow(rec_log) == 0){
      
      return(
        data.frame(drug = Drug_code, 
                   order = 0, 
                   days_to_order = time_til_next_order)
      )
    }
    
    ord_rec_log <- dplyr::left_join(rec_log, ord_log, 
                                    by = c("OrderNum" = "OrderNum", 
                                           "Drug_code" = "Drug_code", 
                                           "DateOrdered" = "DateOrdered"))
    ord_rec_log <- ord_rec_log %>% 
      dplyr::mutate(Outstanding_order = QtyOrd - QtyRec)
    
    leadtime <- ord_rec_log %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(leadtime = 
                      n_weekdays(DateOrdered, DateReceived, holidays)) %>% 
      subset(leadtime <= 10)
    
    leadmin <- min(leadtime$leadtime)
    leadmax <- max(leadtime$leadtime)
    leadmode <- calc_mode(leadtime$leadtime)
    
    # set distribution for delivery lead time
    lead_time_dis <- distr6::Triangular$new(
      lower = leadmin - 0.5, 
      upper = leadmax + 0.5, # Have had to make upper +0.5 rather than -0.5 in  
      # original code otherwise if min, max & mode is 1 distr6 doesn't work 
      # as upper is less than mode
      mode = leadmode)
    
    # find outstanding order quantity for orders placed within last 14 days
    outstanding_order_qty  <- ord_rec_log %>%
      subset(DateOrdered > Sys.Date() - 14) %>%
      dplyr::mutate(out_ord_qty = 
                      Outstanding_order * product_info$Packsize) %>% 
      dplyr::summarise(total_out_ord_qty = sum(out_ord_qty))
    
    # find outstanding requisitions which need fulfilling
    
    outstanding_requis <- requis %>% 
      dplyr::filter(SiteID == site, .data$Drug_code == .env$Drug_code)
    
    outstanding_requis <- outstanding_requis %>%
      # do we need to have a cut off i.e. DateOrdered > Sys.Date() - 14?
      subset(DateOrdered > Sys.Date() - 14) %>% 
      dplyr::summarise(total_out_requis = 
                         sum(Outstanding) * product_info$Packsize[1])
    
    # Filter out issues to adjustment and waste codes
    # to consider in future deleting issues done in error i.e. 
    # where issue is followed by a return to stock
    
    demand_data <- w_trans_log_df1 %>% 
      dplyr::filter(Site == site,
                    .data$Drug_code == .env$Drug_code,
                    !Ward %in% waste_adjust_codes) %>% 
      rbind(data.frame(WTranslogID = "NA", Date = Sys.Date(), 
                       Drug_code = Drug_code,  
                       Qty = 0, Ward = "NA", Site = site)) %>% 
      dplyr::arrange(Date) %>% 
      dplyr::group_by(Date) %>% 
      dplyr::summarise(Total_Qty = sum(Qty)) %>% 
      dplyr::ungroup() %>% 
      tidyr::complete(Date = 
                        seq.Date(min(Date), 
                                 max(Date), by="day")) %>% 
      tidyr::replace_na(list(Total_Qty = 0))
    
    # if no orders in this range jump out of function
    
    if(sum(demand_data$Total_Qty) == 0){
      
      return(
        data.frame(drug = Drug_code, 
                   order = 0, 
                   days_to_order = time_til_next_order)
      )
    }
    
    # Calculate min_stock_level
    min_stock_level <- demand_data %>% 
      subset(Date > Sys.Date() - 365)
    
    min_stock_level <- stats::quantile(min_stock_level$Total_Qty, 
                                       .98, na.rm = TRUE) %>% 
      ceiling()
    
    # Run forecast
    demand_data <- make_tsibble(demand_data, frequency = "Daily")
    
    daily_forecast <- forecast_series(demand_data, 28, frequency = "Daily")
    
    actual_forecast <- daily_forecast %>% 
      dplyr::filter(.model == "ARIMA")
    
    # work out where outstanding requisitions fit into the process - 
    # the value is found at outstanding_requis$total_out_requis[1]
    
    step <- drug_quantity(
      forecast = actual_forecast,
      distribution = lead_time_dis,
      min_stock = min_stock_level,
      max_stock = max_storage_capacity,
      p_min = risk_of_min_stock,
      p_max = risk_of_exceeding_max_stock,
      inv_i = product_info$stocklvl,
      delta_pref = time_til_next_order,
      outstanding_orders = outstanding_order_qty$total_out_ord_qty)
    
    to_return <- data.frame(drug = Drug_code, 
                            order = ceiling(step$Q_i/product_info$Packsize[1]), 
                            days_to_order = step$Delta_i)
    
    updateProgress(detail = paste0("Drug: ", to_return$drug, ", order:",
                          to_return$order))
    
    # Populate order quantity & time 'til next order in output table
    return(to_return)
    
  })
  
}
