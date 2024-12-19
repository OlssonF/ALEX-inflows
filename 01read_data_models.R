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