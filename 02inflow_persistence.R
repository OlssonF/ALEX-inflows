
# Generate the forecast(s) --------------

all_forecast_dates <- seq.Date(from = as_date('2020-01-01'), by = '1 days', length.out = 365*2)
horizon <- 35

for (date_use in all_forecast_dates) {
  
  reference_date <- as_date(date_use)
  
  message(reference_date)
  
  forecast_dates <- data.frame(Date = seq.Date(reference_date + days(1), 
                                               by = 'day',
                                               length.out = horizon),
                               horizon = seq(1:(horizon)))
  
  
  fc_dat <- 
    all_dat |> 
    filter(Date <= reference_date) |> 
    mutate(QSA = zoo::na.approx(QSA, na.rm = F, rule = 1:2, maxgap = 5),
           L1 = zoo::na.approx(L1, na.rm = F, rule = 1:2, maxgap = 5)) |> # TESTING
    full_join(forecast_dates, by = 'Date') |>
    mutate(#W_lag1 = lag(Wellington,1),
      "L1_lag{ccf_L1_W_lag}" := lag(L1, ccf_L1_W_lag),
      "QSA_lag{ccf_QSA_L1_lag}" := lag(QSA, ccf_QSA_L1_lag),
      "QSA_lag{ccf_QSA_W_lag}" := lag(QSA, ccf_QSA_W_lag)) |> 
    filter(Date %in% forecast_dates$Date)
  

  forecast_persistence <- tibble(Date = forecast_dates$Date,
                                 horizon = forecast_dates$horizon,
                                 parameter = 1,
                                 variable = "Q",
                                 prediction = as.numeric(NA)) 
  
  # For 1-6 days use the lag of L1 in the model
  for(i in 1:nrow(forecast_dates)) {
    
    # pull dataframes for the relevant date
    dat_use <- fc_dat %>%
      filter(Date == forecast_dates$Date[i])
    
    forecast_use <- forecast_persistence |> 
      filter(Date == forecast_dates$Date[i])
    
    # run model with process uncertainty added 
    forecast_use$prediction <- m1_coefficients[1] + dat_use$L1_lag6 * m1_coefficients[2] 
    
    # insert values back into the forecast dataframe
    forecast_persistence <- forecast_persistence %>%
      rows_update(forecast_use, by = c("Date","parameter","variable"))
  }
  
  # For any missing dates remaining use the QSA data (including the persistence model)
  missing_dates <- forecast_persistence |> 
    filter(is.na(prediction)) |> 
    distinct(Date)
  
  RW_mod <- all_dat |>
    mutate(QSA = zoo::na.approx(QSA, na.rm = F, rule = 1:2, maxgap = 5)) |>
    mutate(L1_lag := lag(L1, ccf_L1_W_lag)) |> 
    filter(Date <= reference_date + days(ccf_L1_W_lag)) |>
    as_tsibble(index = 'Date') |>
    model(RW = RW(L1_lag))
  
  RW_fc <- RW_mod |>
    forecast(h = horizon - ccf_L1_W_lag) |>
    mutate(parameter = 1) |> 
    rename(L1_lag6= .mean) |>
    as_tibble()
  
  
  # 7-35 days
  for(i in 1:nrow(missing_dates)) {
    
    forecast_date <- missing_dates$Date[i]
    
    # pull dataframes for the relevant date
    dat_use <- RW_fc %>%
      filter(Date == forecast_date)
    
    forecast_use <- forecast_persistence |> 
      filter(Date == forecast_date)
    
    # run model with process uncertainty added 
    forecast_use$prediction <- m1_coefficients[1] + dat_use$L1_lag6 * m1_coefficients[2] 
    
    # insert values back into the forecast dataframe
    forecast_persistence <- forecast_persistence %>%
      rows_update(forecast_use, by = c("Date","parameter","variable"))
  }
  
  forecast_persistence |> 
    mutate(prediction = exp(prediction),
           reference_date = reference_date,
           model_id = 'persistence') |> 
    arrow::write_dataset(path = 'Forecasts', format = "parquet", partitioning = c('model_id', 'reference_date'))
  
}


#---------------------------------#

forecast_persistence |> 
  left_join(filter(all_dat, Date %in% forecast_dates$Date)) |> 
  ggplot(aes(x=Date, y=exp(prediction), group = parameter)) +
  geom_line() +
  geom_point(aes(y=exp(Wellington)))
