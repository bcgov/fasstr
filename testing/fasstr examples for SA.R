


#devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr devel",repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr devel",repos = NULL, type = "source")

library(fasstr)
library(dplyr)


### Using FLOW_DATA arguments
### ---------------


# One station with Date and Value
flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  add_basin_area() %>%
  fill_missing_dates() %>%
  add_date_variables() %>%
  add_rolling_means(days = 7) %>%
  add_daily_volume() %>%
  add_cumulative_volume() %>%
  add_daily_yield() %>%
  add_cumulative_yield() %>% 
  calc_annual_stats(transpose = T)
  calc_longterm_stats()
  plot_longterm_stats()
  plot_annual_stats(log_discharge = T)
  calc_lt_mad(percent_MAD = c(1:4))
  calc_annual_flow_timing()


# Multiple stations and custom Date and Value column names
flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  rename(Dates=Date, Q=Value) %>% 
  fill_missing_dates(flow_dates = Dates, flow_values = Q) %>% 
  add_date_variables(flow_dates = Dates, water_year = T) %>% 
  add_rolling_means(flow_dates = Dates, flow_values = Q) %>% 
  add_daily_volume(flow_values = Q) %>% 
  add_cumulative_volume(flow_dates = Dates, flow_values = Q) %>% 
  add_basin_area() %>% 
  rename(BASINAREA=Basin_Area_sqkm) %>% 
  add_daily_yield(flow_values = Q) %>% 
  add_cumulative_yield(flow_dates = Dates, flow_values = Q,flow_basin_areas = BASINAREA)  %>% 
 # calc_annual_stats(flow_dates = Dates, flow_values = Q, exclude_years = 1980)
  #calc_longterm_stats(flow_dates = Dates, flow_values = Q)
 # plot_longterm_stats(flow_dates = Dates, flow_values = Q)
  #plot_annual_stats(flow_dates = Dates, flow_values = Q)
  #calc_lt_mad(flow_dates = Dates, flow_values = Q,percent_MAD = c(5,10,20))
  calc_annual_flow_timing(flow_dates = Dates, flow_values = Q)





# Station no STATION_NUMBER
flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  select(Date,Value) %>% 
  fill_missing_dates() %>% 
  add_date_variables(water_year = T) %>% 
  add_rolling_means() %>% 
  add_daily_volume() %>% 
  add_cumulative_volume() %>% 
  add_daily_yield(basin_area = 10.3) %>%
  add_cumulative_yield(basin_area = 10.3) %>% 
  plot_annual_flow_timing(HYDAT = "08HB048")
  #calc_longterm_stats()
  #calc_annual_stats()
  #plot_longterm_stats()
  #plot_annual_stats(percentiles = 99)
  #calc_lt_mad()
 # calc_annual_flow_timing()



### Using HYDAT
### ----------

# Single stations
flow_data <- fill_missing_dates(HYDAT = "08HB048")
flow_data <- add_date_variables(HYDAT = "08HB048", water_year = T)
flow_data <- add_rolling_means(HYDAT = "08HB048")
flow_data <- add_daily_volume(HYDAT = "08HB048")
flow_data <- add_cumulative_volume(HYDAT = "08HB048")
flow_data <- add_daily_yield(HYDAT = "08HB048")
flow_data <- add_cumulative_yield(HYDAT = "08HB048")
flow_data <- calc_longterm_stats(HYDAT = "08HB048")
flow_data <- calc_annual_stats(HYDAT = "08HB048")
plot_longterm_stats(HYDAT = "08HB048")
plot_annual_stats(HYDAT = "08HB048")
calc_lt_mad(HYDAT = "08HB048", percent_MAD = c(5,10,20))
data <- calc_annual_flow_timing(HYDAT = "08NM116")
data <- calc_annual_flow_timing(HYDAT = "08NM116")
plot_annual_flow_timing(HYDAT = "08HB048")


# Multiple stations
flow_data <- fill_missing_dates(HYDAT = c("08HB048","08NM116"))
flow_data <- fill_missing_dates(HYDAT = c("08HB048","08NM116"))
flow_data <- add_date_variables(HYDAT = c("08HB048","08NM116"), water_year = T)
flow_data <- add_rolling_means(HYDAT = c("08HB048","08NM116"))
flow_data <- add_daily_volume(HYDAT = c("08HB048","08NM116"))
flow_data <- add_cumulative_volume(HYDAT = c("08HB048","08NM116"))
flow_data <- add_daily_yield(HYDAT = c("08HB048","08NM116"))
flow_data <- add_cumulative_yield(HYDAT = c("08HB048","08NM116"), basin_area = c("08HB048"=10.2))
flow_data <- calc_longterm_stats(HYDAT = c("08HB048","08NM116"))
flow_data <- calc_annual_stats(HYDAT = c("08HB048","08NM116"), months = 8)
plot_longterm_stats(HYDAT = c("08HB048","08NM116"))
plot_annual_stats(HYDAT = c("08HB048","08NM116"))
calc_lt_mad(HYDAT = c("08HB048","08NM116"), percent_MAD = c(5,10,20))
data <- calc_annual_flow_timing(HYDAT = c("08HB048","08NM116"), percent_total = 1:5, transpose = T)




