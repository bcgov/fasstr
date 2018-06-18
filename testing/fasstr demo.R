library(dplyr)


flow_data <- tidyhydat::hy_daily_flows("08HB048")
flow_data <- summarize(flow_data,
                       MEAN = mean(Value),
                       MIN = min(Value),
                       MAX = max(Value))


flow_data <- tidyhydat::hy_daily_flows("08HB048") %>% 
  summarize(MEAN = mean(Value),
            MIN = min(Value),
            MAX = max(Value))


flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  fill_missing_dates() %>% 
  add_basin_area() %>% 
  add_date_variables(water_year = T) %>%
  add_rolling_means() %>%
  add_daily_volume() %>%
  add_cumulative_volume() %>% 
  add_daily_yield() %>%
  add_cumulative_yield() %>% 
  add_seasons()

calc_longterm_stats(data = NULL, dates = Date, values = Value,
                    groups = STATION_NUMBER, station_number = "08HB048", percentiles = c(10, 90),
                    roll_days = 1, roll_align = "right", water_year = FALSE,
                    water_year_start = 10, start_year = 0, end_year = 9999,
                    exclude_years = NULL, months = 1:12, complete_years = FALSE,
                    include_longterm = TRUE, custom_months = NULL,
                    custom_months_label = "Custom-Months", transpose = FALSE,
                    ignore_missing = FALSE)



