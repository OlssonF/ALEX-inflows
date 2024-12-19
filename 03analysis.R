# Forecast analysis
inflow_fc <- arrow::open_dataset('Forecasts') |> 
  collect() |> 
  mutate(horizon = as.numeric(Date - as_date(reference_date)))


inflow_fc |> 
  filter(reference_date %in% as.character(seq.Date(from = as_date('2021-01-01'),
                                                   by = '1 month', 
                                                   length.out = 12))) |> 
  reframe(.by = c(Date, reference_date, model_id),
          prediction = median(prediction)) |>
  left_join(all_dat, by = 'Date') |> 
  ggplot(aes(x=Date, y = prediction)) +
  geom_point(aes(y=exp(Wellington)), size = 0.7) +
  geom_line(aes(colour = model_id), linewidth = 1) +
  facet_wrap(~reference_date, scales = 'free_x', labeller = label_both) +
  theme_bw() +
  labs(y='Flow (m3/s)')


inflow_fc |> 
  reframe(.by = c(Date, reference_date, model_id, horizon),
          prediction = median(prediction)) |>
  left_join(all_dat, by = 'Date') |> 
  mutate(Wellington = exp(Wellington),
         sq_error = (prediction - Wellington)^2) |> 
  group_by(model_id, horizon) |> 
  summarise(rmse = sqrt(mean(sq_error, na.rm = T))) |> 
  ggplot(aes(x=horizon, y=rmse, colour = model_id)) + 
  geom_point()


inflow_fc |> 
  reframe(.by = c(Date, reference_date, model_id, horizon),
          prediction = median(prediction)) |>
  left_join(all_dat, by = 'Date') |> 
  mutate(Wellington = exp(Wellington),
         sq_error = (prediction - Wellington)^2) |> 
  group_by(model_id) |> 
  summarise(rmse = sqrt(mean(sq_error, na.rm = T)))
