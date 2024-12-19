library(tidyverse)
library(fable)

# Read in data --------------------
dat_historical <- read_csv('../../../Downloads/lock1_wellington_flow.csv', skip = 3, col_names = c('Date', 'L1_historical', 'Wellington')) |> 
  # this is in ML/day, convert to m3/s
  mutate(L1_historical = L1_historical/86.4,
         Wellington = Wellington/86.4)


dat_realtime <- read.csv('../../../Downloads/BulkExport-6 Locations-20241203035817.csv', skip = 5,
                         col.names = c('Date', 'date2', 'L1', 'L10', 'L9', 'L8', 'L7', 'QSA')) |> 
  mutate(Date = as_date(Date),
         date2 = as_date(date2)) |> 
  select(Date, L1, QSA) |> 
  na.omit()

all_dat <- full_join(select(dat_historical, -L1_historical),
                     dat_realtime) |> 
  mutate(QSA = zoo::na.approx(QSA, na.rm = F, rule = 1:2, maxgap = 5)) # TESTING


all_dat_noNA <- all_dat |> 
  na.omit()
#-----------------------------------#
ggplot(all_dat_noNA) + 
  geom_point(aes(x=Date, y=QSA, colour = 'QSA')) + 
  geom_point(aes(x=Date, y=L1, colour = 'L1'))+ 
  geom_point(aes(x=Date, y=Wellington, colour = 'W'))

# Filter data to remove flood periods
# q90 <- quantile(all_dat$QSA, 0.9, na.rm = T)
# all_dat_q90 <- all_dat |> 
#   filter(QSA <= q90)

# CCF analysis --------------------
# Calculate the optimal lags between the different stations

# find the max lags
ccf_L1_W <- ccf(all_dat_noNA$Wellington, all_dat_noNA$L1)
ccf_L1_W_lag <- ccf_L1_W$lag[which(ccf_L1_W$acf == max(ccf_L1_W$acf))]

ccf_QSA_W <- ccf(all_dat_noNA$Wellington, all_dat_noNA$QSA)
ccf_QSA_W_lag <- ccf_QSA_W$lag[which(ccf_QSA_W$acf == max(ccf_QSA_W$acf))]

ccf_QSA_L1 <- ccf(all_dat_noNA$L1, all_dat_noNA$QSA)
ccf_QSA_L1_lag <- ccf_QSA_L1$lag[which(ccf_QSA_L1$acf == max(ccf_QSA_L1$acf))]

#-----------------------------------#

# Fit models -----------------------
# use the log of the discharges
all_dat <- all_dat |>
  mutate(across(c('Wellington', 'L1', 'QSA'), log)) |> 
  mutate(#W_lag1 = lag(Wellington,1),
    "L1_lag{ccf_L1_W_lag}" := lag(L1, ccf_L1_W_lag),
    "QSA_lag{ccf_QSA_L1_lag}" := lag(QSA, ccf_QSA_L1_lag),
    "QSA_lag{ccf_QSA_W_lag}" := lag(QSA, ccf_QSA_W_lag)) 

# all_dat <- all_dat |>
#   filter(Wellington < quantile(all_dat$Wellington, 0.8, na.rm = T))
formula_m1 <- paste0("Wellington ~ L1_lag", ccf_L1_W_lag)
formula_m2 <- paste0("Wellington ~ QSA_lag", ccf_QSA_W_lag)

m1 <- lm(as.formula(formula_m1), all_dat)
m2 <- lm(as.formula(formula_m2), all_dat)
# m3 <- lm(L1 ~ lag(QSA, ccf_QSA_L1_lag), all_dat)


summary(m1)
summary(m2)
# summary(m3)

m1_resid <- residuals(m1)
m2_resid <- residuals(m2)
# m3_resid <- residuals(m3)

mean(m1_resid)
sd(m1_resid)

mean(m2_resid)
sd(m2_resid)

# mean(m3_resid)
# sd(m3_resid)

m1_coefficients <- m1$coefficients
m2_coefficients <- m2$coefficients
# m3_coefficients <- m3$coefficients

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
    forecast_use$prediction <- m1_coefficients[1] + 
      dat_use$L1_lag6 * m1_coefficients[2] 
    
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
    mutate(QSA_lag := lag(QSA, ccf_QSA_W_lag)) |> 
    filter(Date <= reference_date + days(ccf_L1_W_lag)) |>
    as_tsibble(index = 'Date') |>
    model(RW = RW(QSA_lag))
  
  RW_fc <- RW_mod |>
    forecast(h = horizon - ccf_L1_W_lag) |>
    mutate(parameter = 1) |> 
    rename(QSA_lag17 = .mean) |>
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
    forecast_use$prediction <- m2_coefficients[1] + 
      dat_use$QSA_lag17 * m2_coefficients[2] 
    
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


# Analyse forecasts ------------

inflow_fc <- arrow::open_dataset('Forecasts') |> 
  collect() |> 
  mutate(prediction = exp(prediction))

inflow_fc |> 
  filter(reference_date %in% as.character(seq.Date(from = as_date('2020-01-01'),
                                                   by = '1 month', 
                                                   length.out = 9))) |> 
  reframe(.by = c(Date, reference_date), 
          median = median(prediction, na.rm = T),
          q97.5 = quantile(prediction, na.rm = T, 0.975),
          q02.5 = quantile(prediction, na.rm = T, 0.025)) |> 
  left_join(all_dat, by = 'Date') |> 
  ggplot(aes(x=Date, y=median)) +
  geom_ribbon(aes(ymax =  q97.5, ymin = q02.5), alpha = 0.5, fill = 'lightblue') +
  geom_line() +
  geom_point(aes(y=exp(Wellington)), size = 0.7) +
  facet_wrap(~reference_date, scales = 'free_x', labeller = label_both) +
  theme_bw() +
  labs(y='Flow (m3/s)')


