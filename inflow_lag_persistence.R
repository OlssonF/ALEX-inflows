
reference_date <- as_date(date_use) - days(90)

message(reference_date)

forecast_dates <- data.frame(Date = seq.Date(reference_date + days(1), 
                                             by = 'day',
                                             length.out = horizon),
                             horizon = seq(1:(horizon)))


fc_dat <- 
  all_dat |> 
  filter(Date <= reference_date) |> 
  mutate(QSA = zoo::na.approx(QSA, na.rm = F, rule = 1:2, maxgap = 5)) |> # TESTING
  full_join(forecast_dates, by = 'Date') |>
  mutate(#W_lag1 = lag(Wellington,1),
    "L1_lag{ccf_L1_W_lag}" := lag(L1, ccf_L1_W_lag),
    "QSA_lag{ccf_QSA_L1_lag}" := lag(QSA, ccf_QSA_L1_lag),
    "QSA_lag{ccf_QSA_W_lag}" := lag(QSA, ccf_QSA_W_lag)) |> 
  filter(Date %in% forecast_dates$Date)

n_members <- 1

forecast_process_unc <- tibble(Date = forecast_dates$Date,
                               horizon = forecast_dates$horizon,
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
    dat_use$L1_lag6 * m1_coefficients[2] #+ 
    # rnorm(n = n_members,
    #       mean = 0, 
    #       sd = sd(m1_resid))
  
  # insert values back into the forecast dataframe
  forecast_process_unc <- forecast_process_unc %>%
    rows_update(forecast_use, by = c("Date","parameter","variable"))
}

# For any missing dates remaining use the QSA data (including the persistence model)
missing_dates <- forecast_process_unc |> 
  filter(is.na(prediction)) |> 
  distinct(Date)

weights <- seq(from = 0.05, to = 1, length.out = ccf_QSA_W_lag - ccf_L1_W_lag)

# 7-17 days
for(i in 1:nrow(missing_dates)) {
  
  forecast_date <- missing_dates$Date[i]
  
  # pull dataframes for the relevant date
  dat_use <- fc_dat %>%
    filter(Date == forecast_date)
  
  forecast_use <- forecast_process_unc |> 
    filter(Date == forecast_date)
  
  # run model with process uncertainty added 
  forecast_use$prediction <- m2_coefficients[1] + dat_use$QSA_lag17 * m2_coefficients[2] #+ 
    # rnorm(n = n_members,
    #       mean = 0,
    #       sd = sd(m2_resid))
  
  
  # Generate a persistence forecast
  forecast_use$persistence <- forecast_process_unc |>
    filter(Date == forecast_date - days(1)) |>
    pull(prediction)

  # find a weighted mean of the values - the proportion depends on the horizon (day 7 = 90% persistence etc. )
  forecast_use <- forecast_use |>
    mutate(#mean = sum(prediction, persistence)/2,
           wmean = weighted.mean(c(prediction, persistence), c(weights[i], 1 - weights[i]))) |>
    mutate(prediction = wmean) |>
    select(-wmean, -persistence)
  
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
  forecast(h = horizon - ccf_QSA_W_lag) |>
  mutate(parameter = 1) |> 
  rename(QSA_lag17 = .mean) |>
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
    dat_use$QSA_lag17 * m2_coefficients[2] #+ 
    # rnorm(n = n_members,
    #       mean = 0,
    #       sd = sd(m2_resid))
  
  # insert values back into the forecast dataframe
  forecast_process_unc <- forecast_process_unc %>%
    rows_update(forecast_use, by = c("Date","parameter","variable"))
}
                                         
                                            
forecast_process_unc |> 
  left_join(filter(all_dat, Date %in% forecast_dates$Date)) |> 
  ggplot(aes(x=Date, y=exp(prediction), group = parameter)) +
  geom_line() +
  geom_point(aes(y=exp(Wellington))) +
  geom_vline(xintercept = forecast_dates$Date[c(6,17)]) +
  labs(y='prediction')
