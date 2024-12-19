

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
  
  n_members <- 31
  
  forecast_process_unc <- tibble(Date = rep(forecast_dates$Date, times = n_members),
                                 parameter = rep(1:n_members, each = length(forecast_dates$Date)),
                                 variable = "Q",
                                 prediction = as.numeric(NA)) 
  
  # For 1-6 days use the lag of L1 in the model
  for(i in 1:nrow(forecast_dates)) {
    
    # pull dataframes for the relevant date
    dat_use <- fc_dat %>%
      filter(Date == forecast_dates$Date[i])
    
    forecast_use <- forecast_process_unc |> 
      filter(Date == forecast_dates$Date[i])
    
    # run model with process uncertainty added 
    forecast_use$prediction <- m1_coefficients[1] + 
      dat_use$L1_lag6 * m1_coefficients[2] + 
      rnorm(n = n_members,
            mean = 0, 
            sd = sd(m1_resid))
    
    # insert values back into the forecast dataframe
    forecast_process_unc <- forecast_process_unc %>%
      rows_update(forecast_use, by = c("Date","parameter","variable"))
  }
  
  # For any missing dates remaining use the QSA data (including the persistence model)
  missing_dates <- forecast_process_unc |> 
    filter(is.na(prediction)) |> 
    distinct(Date)
  
  # 7-17 days
  for(i in 1:nrow(missing_dates)) {
    
    forecast_date <- missing_dates$Date[i]
    
    # pull dataframes for the relevant date
    dat_use <- fc_dat %>%
      filter(Date == forecast_date)
    
    forecast_use <- forecast_process_unc |> 
      filter(Date == forecast_date)
    
    # run model with process uncertainty added 
    forecast_use$prediction <- m2_coefficients[1] + 
      dat_use$QSA_lag17 * m2_coefficients[2] + 
      rnorm(n = n_members,
            mean = 0,
            sd = sd(m2_resid))
    
    # insert values back into the forecast dataframe
    forecast_process_unc <- forecast_process_unc %>%
      rows_update(forecast_use, by = c("Date","parameter","variable"))
  }
  
  # How to deal with extra missing dates?
  missing_dates_2 <- forecast_process_unc |> 
    filter(is.na(prediction)) |> 
    distinct(Date)
  
  # # Option 1: no uncertainty just persistence
  # fc_dat <- fc_dat |>
  #   # fill down the rest of the QSA values assuming persistence
  #   mutate("QSA_lag{ccf_QSA_W_lag}" := zoo::na.locf(!!as.name(glue::glue("QSA_lag{ccf_QSA_W_lag}"))))
  
  # Option 2: fit a RW model and then generate an ensemble forecast with uncertainty

  RW_mod <- all_dat |>
    mutate(QSA = zoo::na.approx(QSA, na.rm = F, rule = 1:2, maxgap = 5)) |>
    mutate(QSA_lag := lag(QSA, ccf_QSA_W_lag)) |> 
    filter(Date <= reference_date + days(ccf_QSA_W_lag)) |>
    as_tsibble(index = 'Date') |>
    model(RW = RW(QSA_lag))
  
  RW_fc <- RW_mod |>
    generate(h = horizon - ccf_QSA_W_lag, times = 31) |>
    rename(parameter = .rep,
           QSA_lag17 = .sim) |>
    as_tibble()
  
  
  # 18-35 days
  for(i in 1:nrow(missing_dates_2)) {
    
    forecast_date <- missing_dates_2$Date[i]
    
    # pull dataframes for the relevant date
    dat_use <- RW_fc %>%
      filter(Date == forecast_date)
    
    forecast_use <- forecast_process_unc |> 
      filter(Date == forecast_date)
    
    # run model with process uncertainty added 
    forecast_use$prediction <- m2_coefficients[1] + 
      dat_use$QSA_lag17 * m2_coefficients[2] + 
      rnorm(n = n_members,
            mean = 0,
            sd = sd(m2_resid))
    
    # insert values back into the forecast dataframe
    forecast_process_unc <- forecast_process_unc %>%
      rows_update(forecast_use, by = c("Date","parameter","variable"))
  }
  
  forecast_process_unc |> 
    mutate(prediction = exp(prediction),
           reference_date = reference_date,
           model_id = 'lag') |> 
    arrow::write_dataset(path = 'Forecasts', format = "parquet", partitioning = c('model_id', 'reference_date'))
  
}


#---------------------------------#

forecast_process_unc |> 
  reframe(.by = c(Date),
          median = exp(median(prediction)),
          q97.5 = exp(quantile(prediction, 0.975, na.rm = T)),
          q02.5 = exp(quantile(prediction, 0.025, na.rm = T))) |> 
  left_join(filter(all_dat, Date %in% forecast_dates$Date)) |> 
  ggplot(aes(x=Date, y=median)) +
  geom_line() +
  geom_ribbon(aes(ymax =  q97.5, ymin = q02.5), alpha = 0.3) +
  geom_point(aes(y=exp(Wellington))) +
  geom_vline(xintercept = forecast_dates$Date[c(6,17)])

forecast_process_unc |> 
  left_join(filter(all_dat, Date %in% forecast_dates$Date)) |> 
  ggplot(aes(x=Date, y=exp(prediction), group = parameter)) +
  geom_line() +
  geom_point(aes(y=exp(Wellington)))
