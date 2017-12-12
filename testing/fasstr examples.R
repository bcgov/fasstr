


devtools::document()
install.packages("/Users/jongoetz/Documents/R/fasstr devel",repos = NULL, type = "source")

library(fasstr)
library(dplyr)



### FLOW_DATA
### ----------


# One station with Date and Value
flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  fill_missing_dates() %>% 
  add_date_variables(water_year = T) %>% 
  add_rolling_means() %>% 
  add_daily_volume() %>% 
  add_cumulative_volume()


# Multiple stations and custom Date and Value column names
flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  rename(Datesss=Date, Valuesss=Value) %>% 
  fill_missing_dates(flow_dates = Datesss, flow_values = Valuesss) %>% 
  add_date_variables(flow_dates = Datesss, water_year = T) %>% 
  add_rolling_means(flow_dates = Datesss, flow_values = Valuesss) %>% 
  add_daily_volume(flow_values = Valuesss) %>% 
  add_cumulative_volume(flow_dates = Datesss, flow_values = Valuesss)


# Station no STATION_NUMBER
flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  select(Date,Value) %>% 
  fill_missing_dates() %>% 
  add_date_variables(water_year = T) %>% 
  add_rolling_means() %>% 
  add_daily_volume() %>% 
  add_cumulative_volume()


### HYDAT
### ----------

# Multiple stations
flow_data <- fill_missing_dates(HYDAT = "08HB048")
flow_data <- add_date_variables(HYDAT = "08HB048", water_year = T)
flow_data <- add_rolling_means(HYDAT = "08HB048")
flow_data <- add_daily_volume(HYDAT = "08HB048")
flow_data <- add_cumulative_volume(HYDAT = "08HB048")

# Multiple stations
flow_data <- fill_missing_dates(HYDAT = c("08HB048","08NM116"))
flow_data <- fill_missing_dates(HYDAT = c("08HB048","08NM116"))
flow_data <- add_date_variables(HYDAT = c("08HB048","08NM116"), water_year = T)
flow_data <- add_rolling_means(HYDAT = c("08HB048","08NM116"))
flow_data <- add_daily_volume(HYDAT = c("08HB048","08NM116"))
flow_data <- add_cumulative_volume(HYDAT = c("08HB048","08NM116"))







add_date_variables(data, water_year = T, water_year_start = 2)
add_cumulative_volume(HYDAT = "08HB048")


data <- fill_missing_dates(flow_data, flow_dates = Datesss, water_year = T)
