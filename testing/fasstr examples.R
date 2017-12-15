


devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr devel",repos = NULL, type = "source")
install.packages("C:/Users/jgoetz/R/fasstr devel",repos = NULL, type = "source")

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
  add_cumulative_volume() %>% 
  add_daily_yield() %>% 
  add_cumulative_yield()


# Multiple stations and custom Date and Value column names
flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  rename(Datesss=Date, Valuesss=Value) %>% 
  fill_missing_dates(flow_dates = Datesss, flow_values = Valuesss) %>% 
  add_date_variables(flow_dates = Datesss, water_year = T) %>% 
  add_rolling_means(flow_dates = Datesss, flow_values = Valuesss) %>% 
  add_daily_volume(flow_values = Valuesss) %>% 
  add_cumulative_volume(flow_dates = Datesss, flow_values = Valuesss) %>% 
  add_basin_area() %>% 
  rename(BASINAREA=Basin_Area_sqkm) %>% 
  add_daily_yield(flow_values = Valuesss) %>% 
  add_cumulative_yield(flow_dates = Datesss, flow_values = Valuesss,flow_basin_areas = BASINAREA)


###########FIX MEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
# Station no STATION_NUMBER
flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  select(Date,Value) %>% 
 # fill_missing_dates() %>% 
  add_date_variables(water_year = T) %>% 
  add_rolling_means() %>% 
  add_daily_volume() %>% 
  add_cumulative_volume() %>% 
  add_daily_yield(basin_area = 100) %>% 
  add_daily_yield(basin_area = 200)


### HYDAT
### ----------

# Multiple stations
flow_data <- fill_missing_dates(HYDAT = "08HB048")
flow_data <- add_date_variables(HYDAT = "08HB048", water_year = T)
flow_data <- add_rolling_means(HYDAT = "08HB048")
flow_data <- add_daily_volume(HYDAT = "08HB048")
flow_data <- add_cumulative_volume(HYDAT = "08HB048")
flow_data <- add_daily_yield(HYDAT = "08HB048")

# Multiple stations
flow_data <- fill_missing_dates(HYDAT = c("08HB048","08NM116"))
flow_data <- fill_missing_dates(HYDAT = c("08HB048","08NM116"))
flow_data <- add_date_variables(HYDAT = c("08HB048","08NM116"), water_year = T)
flow_data <- add_rolling_means(HYDAT = c("08HB048","08NM116"))
flow_data <- add_daily_volume(HYDAT = c("08HB048","08NM116"))
flow_data <- add_cumulative_volume(HYDAT = c("08HB048","08NM116"))
flow_data <- add_daily_yield(HYDAT = c("08HB048","08NM116"))
flow_data <- add_daily_yield(HYDAT = c("08HB048","08NM116"), basin_area = c("08HB048"=10.2))



flow_data <- add_cumulative_yield(HYDAT = c("08HB048","08NM116"))
flow_data <- flow_data %>% select(-Yield_mm, BASIN=Basin_Area_sqkm)
flowtest <- add_daily_yield(flow_data, flow_basin_areas = BASIN)


flowdata <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  add_cumulative_yield(water_year = T, water_year_start = 12)


  add_basin_area(basin=10)

flow_data <- add_basin_area(HYD="08HB048")
flow


add_date_variables(data, water_year = T, water_year_start = 2)
add_cumulative_volume(HYDAT = "08HB048")


data <- fill_missing_dates(flow_data, flow_dates = Datesss, water_year = T)
