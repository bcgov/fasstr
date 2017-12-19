


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
  add_date_variables() %>% 
  add_rolling_means(days = 7) %>% 
  add_daily_volume() %>% 
  add_cumulative_volume() %>% 
  add_daily_yield() %>%
  add_cumulative_yield() 
# Multiple stations and custom Date and Value column names
flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  rename(Datesss=Date, Valuesss=Value) %>% 
  fill_missing_dates(flow_dates = Datesss, flow_values = Q) %>% 
  add_date_variables(flow_dates = Datesss, water_year = T) %>% 
  add_rolling_means(flow_dates = Datesss, flow_values = Valuesss) %>% 
  add_daily_volume(flow_values = Valuesss) %>% 
  add_cumulative_volume(flow_dates = Datesss, flow_values = Valuesss) %>% 
  add_basin_area() %>% 
  rename(BASINAREA=Basin_Area_sqkm) %>% 
  add_daily_yield(flow_values = Valuesss) %>% 
  add_cumulative_yield(flow_dates = Datesss, flow_values = Valuesss,flow_basin_areas = BASINAREA) %>% 
  calc_longterm_stats_2(flow_dates = Datesss, flow_values = Valuesss)



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
  calc_longterm_stats_2()




### HYDAT
### ----------

# Multiple stations
flow_data <- fill_missing_dates(HYDAT = "08HB048")
flow_data <- add_date_variables(HYDAT = "08HB048", water_year = T)
flow_data <- add_rolling_means(HYDAT = "08HB048")
flow_data <- add_daily_volume(HYDAT = "08HB048")
flow_data <- add_cumulative_volume(HYDAT = "08HB048")
flow_data <- add_daily_yield(HYDAT = "08HB048")
flow_data <- add_cumulative_yield(HYDAT = "08HB048")
flow_data <- calc_longterm_stats_2(HYDAT = "08HB048")

# Multiple stations
flow_data <- fill_missing_dates(HYDAT = c("08HB048","08NM116"))
flow_data <- fill_missing_dates(HYDAT = c("08HB048","08NM116"))
flow_data <- add_date_variables(HYDAT = c("08HB048","08NM116"), water_year = T)
flow_data <- add_rolling_means(HYDAT = c("08HB048","08NM116"))
flow_data <- add_daily_volume(HYDAT = c("08HB048","08NM116"))
flow_data <- add_cumulative_volume(HYDAT = c("08HB048","08NM116"))
flow_data <- add_daily_yield(HYDAT = c("08HB048","08NM116"))
flow_data <- add_cumulative_yield(HYDAT = c("08HB048","08NM116"), basin_area = c("08HB048"=10.2))
flow_data <- calc_longterm_stats_2(HYDAT = c("08HB048"))











flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  #group_by(STATION_NUMBER) %>% 
  mutate(MonthName=month.abb[lubridate::month(Date)])
flow_data <-   
  dplyr::summarize(flow_data,
                   Mean = mean(Value, na.rm = TRUE),
                   Median = median(Value, na.rm = TRUE),
                   Maximum = max(Value, na.rm = TRUE),
                   Minimum = min(Value, na.rm = TRUE))

results <- data.frame()
for (month in unique(flow_data$MonthName)) {
  flow_data_month <- dplyr::filter(flow_data,MonthName==month)
  results_month <-       dplyr::summarize(flow_data_month,
                     Mean = mean(Value, na.rm = TRUE),
                     Median = median(Value, na.rm = TRUE),
                     Maximum = max(Value, na.rm = TRUE),
                     Minimum = min(Value, na.rm = TRUE))
  results_month$Month <- month
  results <- rbind(results,results_month)
}


flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  rename(Station=STATION_NUMBER) %>% 
  group_by(Station) %>% 
  calc_longterm_stats_2(water_year = T, custom_months = 1:4, custom_months_label = "WINTER", transpose = T)

  add_basin_area() %>% 
  group_by(Basin_Area_sqkm) %>% 
  add_cumulative_volume() %>% 
  add_cumulative_yield() %>% 
  add_cumulative_volume() %>% 
  add_daily_yield() %>% 
  add_daily_volume() %>% 
  fill_missing_dates() %>% 
  add_date_variables() %>% 
  add_rolling_means()

Q_months <- dplyr::summarize(dplyr::group_by(flow_data,MonthName,STATION_NUMBER),
                             Mean = mean(Value, na.rm = TRUE),
                             Median = median(Value, na.rm = TRUE),
                             Maximum = max(Value, na.rm = TRUE),
                             Minimum = min(Value, na.rm = TRUE))














