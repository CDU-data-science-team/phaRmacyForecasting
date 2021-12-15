#' Forecast and run inventory management algorithm, returning reorder quantities
#' by site and drug
#'
#' @param site String. Site code (selected from dynamic UI listing all sites)
#' @param supplier String. Supplier (changes weekly, selected in Shiny interface)
#'
#' @return
#' @export
#'
inventory_reorder <- function(site, supplier){
  
  # takes two reactive inputs- site and supplier
  
  order_list <- product_sup_profile %>% 
    dplyr::filter(Site == site, Supplier_name == supplier) %>% 
    dplyr::arrange(Drug_name)
  
  # select orders placed within the last 2 years
  order_log <- w_order_log_df1 %>% 
    subset(DateOrdered > Sys.Date() - 730) %>% 
    dplyr::filter(Site == site)
  
  purrr::pmap(order_list, function(Drug_code, ProductID, ...){
    
  # for (drug in 1:nrow()){
    
    # Filter product_sup_profile for relevant product
    
    product_info <- product_sup_profile %>% 
      dplyr::filter(Drug_code == Drug_code)

    # calculate leadtimes
    
    # Get rid of multiple lines for same order & received date
    ord_log <- order_log %>% 
      dplyr::filter(Kind %in% c("O", "I") & Drug_code == Drug_code) %>% 
      dplyr::select(OrderNum, Drug_code, DateOrdered, QtyOrd)
    
    rec_log <- order_log %>% 
      dplyr::filter(Site == site & Kind == "R" & Drug_code == Drug_code) %>% 
      dplyr::group_by(OrderNum, Drug_code, Supplier_name, 
                      DateOrdered, DateReceived, Site, Kind) %>%  
      dplyr::summarise(QtyRec = sum(QtyRec)) %>% 
      dplyr::ungroup()
    
    
    ord_rec_log <- dplyr::left_join(rec_log, ord_log, 
                                    by = c("OrderNum" = "OrderNum", 
                                           "Drug_code" = "Drug_code", 
                                           "DateOrdered" = "DateOrdered"))
    ord_rec_log <- ord_rec_log %>% 
      dplyr::mutate(Outstanding_order = ord_rec_log$QtyOrd - ord_rec_log$QtyRec)
    
    
    leadtime <- ord_rec_log %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(leadtime = n_weekdays(DateOrdered, DateReceived)) %>% 
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
      dplyr::mutate(out_ord_qty = ord_rec_log$Outstanding_order * product_info$Packsize[1]) %>% 
      dplyr::summarise(total_out_ord_qty = sum(out_ord_qty))
    
    # find outstanding requisitions which need fulfilling
    
    outstanding_requis <- w_requis_df1 %>% 
      dplyr::filter(Site == site, Drug_code == order_list$Drug_code[drug])
    outstanding_requis <- outstanding_requis %>%
      # do we need to have a cut off i.e. DateOrdered > Sys.Date() - 14?
      subset(DateOrdered > Sys.Date() - 14) %>% 
      dplyr::summarise(total_out_requis = sum(Outstanding) * product_info$Packsize[1])
    
    # Filter out issues to adjustment and waste codes
    # to consider in future deleting issues done in error i.e. 
    # where issue is followed by a return to stock
    
    demand_data <- w_trans_log_df1 %>% 
      dplyr::filter(Site == site,
                    Drug_code == order_list$Drug_code[drug],
                    !Ward %in% waste_adjust_codes) %>% 
      rbind(data.frame(WTranslogID = "NA", Date = Sys.Date(), Drug_code = order_list$Drug_code[drug],  
                       Qty = 0, Ward = "NA", Site = site)) %>% 
      dplyr::arrange(Date) %>% 
      dplyr::group_by(Date) %>% 
      dplyr::summarise(Total_Qty = sum(Qty)) %>% ungroup() %>% 
      tidyr::complete(Date = 
                        seq.Date(min(Date), 
                                 max(Date), by="day")) %>% 
      tidyr::replace_na(list(Drug_code = drug,
                             Total_Qty = 0))
    
    # Calculate min_stock_level
    min_stock_level <- demand_data %>% 
      subset(Date > Sys.Date() - 365)
    min_stock_level <- stats::quantile(min_stock_level$Total_Qty, c(.98), na.rm = TRUE) %>% 
      ceiling()
    
    # Run forecast
    demand_data <- phaRmacyForecasting:::make_tsibble(demand_data, 
                                                      frequency = "Daily")
    
    daily_forecast <- phaRmacyForecasting:::forecast_series(demand_data, 
                                                            28, 
                                                            frequency = "Daily")
    
    actual_forecast <- daily_forecast %>% 
      dplyr::filter(.model == "ARIMA")
    
    # Use this for now until outstanding order quantity added as input to 'step'
    
    Outstanding_orders$Ord_quant[1] <- outstanding_order_qty$total_out_ord_qty[1] 
    # work out where outstanding requisitions fit into the process - 
    # the value is found at outstanding_requis$total_out_requis[1]
    
    step <- phaRmacyForecasting:::drug_quantity(
      forecast = actual_forecast,
      distribution = lead_time_dis,
      min_stock = min_stock_level,
      max_stock = max_storage_capacity,
      p_min = risk_of_min_stock,
      p_max = risk_of_exceeding_max_stock,
      inv_i = product_info$stocklvl[1],
      delta_pref = time_til_next_order)
    
    # Populate order quantity & time 'til next order in output table
    Output_table$Order_Quantity[drug] <- ceiling(step$Q_i/product_info$Packsize[1])
    
    Output_table$Days_Until_Next_Order[drug] <- step$Delta_i
    
  })
  
}
