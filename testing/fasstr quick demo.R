### Quick fasstr demo



# Install
install.packages("devtools")
devtools::install_github("bcgov/fasstr")


library(fasstr)
library(dplyr)

# Download HYDAT
tidyhydat::download_hydat()


## Examples with a data frame

flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  fill_missing_dates() %>% 
  add_basin_area() %>% 
  add_date_variables(water_year = T) %>%
  add_rolling_means() %>%
  add_daily_volume() %>%
  add_cumulative_volume() %>% 
  add_daily_yield(basin_area = 10.3) %>%
  add_cumulative_yield(basin_area = 10.3) %>% 
  add_seasons()

results <- calc_annual_stats(data = flow_data,
                             start_year = 1973,
                             end_year = 2010,
                             exclude_years = 1999:2001,
                             water_year = TRUE,
                             water_year_start = 10,
                             months = 7:9,
                             roll_days = 7,
                             percentiles = c(5,10,20,25),
                             ignore_missing = TRUE)


results <- screen_flow_data(data = flow_data)
results <- calc_longterm_stats(data = flow_data)
results <- calc_longterm_stats(data = flow_data, ignore_missing = TRUE)
results <- calc_annual_stats(data = flow_data)
results <- calc_annual_cumulative_stats(data = flow_data, incl_seasons = TRUE)
results <- calc_annual_flow_timing(data = flow_data)
results <- calc_annual_lowflows(data = flow_data)
results <- calc_annual_outside_normal(data = flow_data)
results <- calc_all_annual_stats(data = flow_data)

results <- calc_daily_stats(data = flow_data, ignore_missing = TRUE)
results <- calc_daily_cumulative_stats(data = flow_data, ignore_missing = TRUE)

results <- calc_monthly_stats(data = flow_data)
results <- calc_monthly_cumulative_stats(data = flow_data)

results <- calc_flow_percentile(data = flow_data, flow_value =  0.801)
results <- calc_lt_mad(data = flow_data, percent_MAD = c(5,10,20))
results <- calc_lt_percentile(data = flow_data, percentiles =  50)

plot_flow_data(data = flow_data)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T)
plot_annual_flow_timing(data = flow_data)
plot_annual_outside_normal(data = flow_data)
plot_annual_stats(data = flow_data)
plot_annual_stats(data = flow_data, values = Volume_m3)
plot_annual_stats(data = flow_data, values = Yield_mm)
plot_daily_cumulative_stats(data = flow_data)
plot_daily_stats(data = flow_data)
plot_daily_stats(data = flow_data, ignore_missing = TRUE)


plot_daily_stats(data = flow_data, values = Volume_m3)
plot_daily_stats(data = flow_data, values = Yield_mm)
plot_data_screening(data = flow_data)
plot_data_screening(data = flow_data, values = Volume_m3)
plot_data_screening(data = flow_data, values = Yield_mm)
plot_flow_duration(data = flow_data, ignore_missing = TRUE)
plot_flow_duration(data = flow_data, values = Volume_m3)
plot_flow_duration(data = flow_data, values = Yield_mm)
plot_longterm_stats(data = flow_data, ignore_missing = TRUE)
plot_longterm_stats(data = flow_data, values = Volume_m3)
plot_longterm_stats(data = flow_data, values = Yield_mm)
plot_missing_dates(data = flow_data)
plot_monthly_cumulative_stats(data = flow_data)
plot_monthly_stats(data = flow_data, ignore_missing = TRUE)
plot_monthly_stats(data = flow_data, values = Volume_m3)
plot_monthly_stats(data = flow_data, values = Yield_mm)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T)



## Examples with a station_number


flow_data <- fill_missing_dates(station_number = "08HB048") %>% 
  add_basin_area() %>% 
  add_seasons() %>% 
  add_date_variables(water_year = T) %>% 
  add_rolling_means() %>% 
  add_daily_volume() %>% 
  add_cumulative_volume() %>% 
  add_daily_yield() %>% 
  add_cumulative_yield(basin_area = 10.2)

results <- calc_longterm_stats(station_number = "08HB048", ignore_missing = F)
results <- calc_annual_stats(station_number = "08HB048")
results <- calc_all_annual_stats(station_number = "08HB048")
results <- calc_annual_cumulative_stats(station_number = "08HB048", use_yield = T, incl_seasons = T)
results <- calc_annual_flow_timing(station_number = "08HB048")
results <- calc_annual_lowflows(station_number = "08HA066")
results <- calc_annual_outside_normal(station_number = "08HB048")
results <- calc_daily_stats(station_number = "08HB048", months = 6:7)
results <- calc_daily_cumulative_stats(station_number = "08NM116", start_year = 1990)
results <- calc_flow_percentile(station_number = "08HB048", flow_value = 10000)
results <- calc_lt_mad(station_number = "08HB048")
results <- calc_lt_percentile(station_number = "08HB048", percentiles = 50, complete_years = T)
results <- calc_monthly_cumulative_stats(station_number = "08HB048")
results <- calc_monthly_stats(station_number = "08HB048")
results <- screen_flow_data(station_number = "08HA066")

plot_flow_data(station_number = "08HB048")
plot_annual_cumulative_stats(station_number = "08HB048", incl_seasons = T)
plot_annual_flow_timing(station_number = "08HB048")
plot_annual_outside_normal(station_number = "08HB048")
plot_annual_stats(station_number = "08HB048", percentiles = 1:20)
plot_annual_lowflows(station_number = "08HB048")
plot_daily_cumulative_stats(station_number = "08HB048", use_yield = T, start_year = 1980)
plot_daily_stats(station_number = "08HB048", start_year = 1973)
plot_data_screening(station_number = "08HB048")
plot_flow_duration(station_number = "08HB048", custom_months = 1:3, custom_months_label = "WINTER", ignore_missing = T)
plot_longterm_stats(station_number = "08HB048", ignore_missing = T)
plot_missing_dates(station_number = "08HB048")
plot_monthly_cumulative_stats(station_number = "08HB048", use_yield = T)
plot_monthly_stats(station_number = "08HB048")
plot_annual_cumulative_stats(station_number = "08HB048", use_yield = T)

## Mulitple stations

results <- calc_longterm_stats(station_number = c("08HB048","08NM116"))

plot_daily_stats(station_number = c("08HB048","08NM116"), complete_years = T, include_title = T)



# Trending

trending <- compute_annual_trends(station_number = "08HB048", zyp_method = "yuepilon")
trending_plots <- plot_annual_trends(trending)


# Volume Frequency analysis


data <- compute_annual_frequencies(station_number = "08HB048", water_year = F, start_year = 1980, end_year = 2010, exclude_years = 1999)
data <- compute_hydat_peak_frequencies(station_number = "08NM116", use_max = TRUE)

# Writing

write_flow_data(station_number = c("08HB048","08NM116"))

write_full_analysis(station_number = "08HB048", 
                    start_year = 1980, 
                    end_year = 2010, 
                    exclude_years = c(1995:1997, 1999),
                    table_filetype = "xlsx",
                    plot_filetype = "png",
                    foldername = "Carn",
                    ignore_missing = TRUE)




