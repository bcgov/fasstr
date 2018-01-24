

library(fasstr)
library(dplyr)
library(ggthemes)


devtools::document()
install.packages("/Users/jongoetz/Documents/R/fasstr devel", repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr devel",repos = NULL, type = "source")



fasstr::calc_monthly_cumulative_stats("08HB048", use_yield = T)
write_flow_data(data = "08HB048", file = "d.xls", digits = 1)

write_results(data = calc_longterm_stats(data = c("08HA002", "08HA011"),
                                         start_year = 1971, end_year = 2000), 
              file = "Cowichan River Long-term Flows (1971-2000).xlsx", 
              digits = 1)



data <- fasstr::calc_annual_cumulative_stats(c("08NM116","08HB048"), water_year = T, water_year_start = 3, incl_seasons = T, use_yield = T)

writexl::write_xlsx(data, "c.xls")


test <- data %>% filter(Year==1973, Month %in% c(1:3)) %>% 
  summarise(sum=sum(daily_total))



fasstr::write_flow_data(flow_data, file = "test2.xlsx", value_digits = 1)
data <- fasstr::calc_annual_stats("08HB048", start_year = 1973)

writexl::write_xlsx(data, path = "test.xlsx")

fasstr::plot_flow_data(c("08HA002","08HA011"), plot_by_year = T, start_year = 1971, end_year = 1975)


fasstr::plot_daily_stats("08HB048", start_year = 1980, end_year = 2000)

data + ggplot2::ylab("Depth Below Surface (m)")




  fasstr::plot_daily_stats("08NM116", start_year = 1980, include_year = 1990, water_year = T, water_year_start = 5)



data <- fasstr::compute_frequency_stat(flow_data, use_hydat_peaks = T, use_max = T,return_period = 10)
data <- fasstr::compute_frequency_analysis("08HB048")
theme_gdocs()
theme_calc()

### MAKE LINES ON ANNUAL LIGHTER/ALPHA.5
#### theme_calc with themegdoc axis font size

data <- fasstr::plot_daily_stats(HYDAT = "08HB048", log_discharge = T)$daily_statisitics


data + theme_calc()  #+ scale_colour_calc() #+ ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "grey80"))
data + theme_calc()  + ggplot2::theme(panel.grid = ggplot2::element_blank())
data + theme_calc() + scale_colour_calc() + ggplot2::theme(panel.grid = ggplot2::element_blank())
data + theme_gdocs() + scale_color_gdocs()+ ggplot2::theme(panel.grid = ggplot2::element_blank())+ ggplot2::ylab("YESSSS")


data <- fasstr::compute_frequency_analysis("08NM116")
data <- fasstr::calc_lt_percentile(flow_data, percentiles = 1)

fasstr::plot_annual_stats(HYDAT = "08HB048")


if(nrow(data) == 1 & ncol(data) == 1){
  dplyr::pull(data[1,1])
} else {
  dplyr::as_tibble(data)
}

# flow_data <- flow_data[,c(as.character(substitute(Date)),
#                           as.character(substitute(Value)))]
# 
# 
 flow_data <- flow_data[,c(as.character(substitute(STATION_NUMBER)),
                           as.character(substitute(Date)),
                           as.character(substitute(Value)))]
### FLOW_DATA
### ----------


# One station with Date and Value
 flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  #n_area() %>%
   fill_missing_dates() %>%
   add_date_variables(water_year = T, water_year_start = 3) %>% 
   add_daily_volume() %>% 
   write_flow_data(file="total.xlsx")
   
  add_rolling_means(days = 7) %>%
  add_daily_volume() %>%
  add_cumulative_volume() %>%
  add_daily_yield() %>%
  add_cumulative_yield() %>% 
  calc_annual_stats()
  #calc_longterm_stats()
  #plot_longterm_stats()
  #plot_annual_stats(log_discharge = T)
  #calc_lt_mad(percent_MAD = c(1:4))
  #calc_annual_flow_timing()


# Multiple stations and custom Date and Value column names
flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  rename(Datesss = Date, Valuesss = Value) %>% 
  fill_missing_dates(dates = Datesss, values = Valuesss) %>% 
  add_date_variables(dates = Datesss, water_year = T) %>% 
  add_rolling_means(dates = Datesss, values = Valuesss) %>% 
  add_daily_volume(values = Valuesss) %>% 
  add_cumulative_volume(dates = Datesss, values = Valuesss) %>% 
  add_basin_area() %>% 
  add_daily_yield(values = Valuesss) %>% 
  add_cumulative_yield(dates = Datesss, values = Valuesss)  %>% 
  calc_annual_stats(dates = Datesss, values = Valuesss, exclude_years = 1980)
 # calc_longterm_stats(dates = Datesss, values = Valuesss)
 # plot_longterm_stats(dates = Datesss, values = Valuesss)
  #plot_annual_stats(dates = Datesss, values = Valuesss)
  #calc_lt_mad(dates = Datesss, values = Valuesss,percent_MAD = c(5,10,20))
  #calc_annual_flow_timing(dates = Datesss, values = Valuesss)





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
  #fasstr::plot_annual_flow_timing(HYDAT = "08HB048")
  #calc_longterm_stats()
  calc_annual_stats()
  #plot_longterm_stats()
  #plot_annual_stats(percentiles = 99)
  #calc_lt_mad()
 # calc_annual_flow_timing()





# flow_data[, as.character(substitute(STATION_NUMBER))] <- "XXXXXXX"
# flow_data <- flow_data[,c(as.character(substitute(STATION_NUMBER)),
#                           as.character(substitute(Date)),
#                           as.character(substitute(Value)))]
# colnames(flow_data) <- c("STATION_NUMBER","Date","Value")


### HYDAT
### ----------

# Single stations
flow_data <- fill_missing_dates(data = "08HB048")
flow_data <- add_date_variables(data = "08HB048", water_year = T)
flow_data <- add_rolling_means(data = "08HB048")
flow_data <- add_daily_volume(data = "08HB048")
flow_data <- add_cumulative_volume(data = "08HB048")
flow_data <- add_daily_yield(data = "08HB048")
flow_data <- add_cumulative_yield(data = "08HB048")
flow_data <- calc_longterm_stats(data = "08HB048")
flow_data <- calc_annual_stats(data = "08HB048")
plot_longterm_stats(data = "08HB048")
plot_annual_stats(data = "08HB048")
calc_lt_mad(data = "08HB048", percent_MAD = c(5,10,20))
data <- calc_annual_flow_timing(data = "08NM116")
data <- calc_annual_flow_timing(data = "08NM116")
plot_annual_flow_timing(data = "08HB048")


# Multiple stations
flow_data <- fill_missing_dates(data = c("08HB048","08NM116"))
flow_data <- fill_missing_dates(data = c("08HB048","08NM116"))
flow_data <- add_date_variables(data = c("08HB048","08NM116"), water_year = T)
flow_data <- add_rolling_means(data = c("08HB048","08NM116"))
flow_data <- add_daily_volume(data = c("08HB048","08NM116"))
flow_data <- add_cumulative_volume(data = c("08HB048","08NM116"))
flow_data <- add_daily_yield(data = c("08HB048","08NM116"))
flow_data <- add_cumulative_yield(data = c("08HB048","08NM116"), basin_area = c("08HB048"=10.2))
flow_data <- calc_longterm_stats(data = c("08HB048","08NM116"))
flow_data <- calc_annual_stats(data = c("08HB048","08NM116"))
plot_longterm_stats(data = c("08HB048","08NM116"))
plot_annual_stats(data = c("08HB048","08NM116"))
calc_lt_mad(data = c("08HB048","08NM116"), percent_MAD = c(5,10,20))
data <- calc_annual_flow_timing(data = c("08HB048","08NM116"), percent_total = 1:5, transpose = T)






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
  select(Date,Value,Station=STATION_NUMBER) %>% 
  fill_missing_dates(flow_stations = Station) %>% 
  add_basin_area(flow_stations = Station)


flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  rename(datess=Date)
fill_missing_dates() %>% 
  add_basin_area() %>% 
  add_cumulative_volume() %>% 
  add_cumulative_yield() %>% 
  add_daily_volume() %>% 
  add_daily_yield() %>% 
  add_rolling_means()

testin <- plot_longterm_stats(flow_data, flow_dates = Datesssss)
plot_longterm_stats(flow_data = flow_data,flow_dates = datess,plot_station = "08HB048", water_year = T, water_year_start = 5)





flow_data=NULL
flow_dates=Date
flow_values=Value
flow_stations=STATION_NUMBER
HYDAT="08HB048"
station=NULL##################################
water_year=FALSE
water_year_start=10
start_year=NULL
end_year=NULL
exclude_years=NULL
log_discharge=TRUE
ignore_missing=TRUE







calc_longterm_stats_3(water_year = T,custom_months = 1:4,custom_months_label = "WINTER", ignore_missing = T, start_year = 1973)

calc_longterm_stats_2(water_year = T, custom_months = 1:4, custom_months_label = "WINTER", transpose = T, start_year = 1971)

test <- calc_longterm_stats_3(HYDAT = c("08HB048","08NM116"),flow_stations = 44,custom_months = 1:4,custom_months_label = "WINTER", ignore_missing = T, start_year = 1973)

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





#Examples

#single station
Q_data <- tidyhydat::hy_daily_flows(station_number = "08HB048")
longterm <- calc_longterm_stats_3(flow_data = Q_data)

#mulitple stations
Q_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116"))







devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr devel",repos = NULL, type = "source")
install.packages("C:/Users/jgoetz/R/fasstr devel",repos = NULL, type = "source")


# 
# test <- tidyhydat::hy_stations(station_number = "08HA011")$DRAINAGE_AREA_GROSS
# screening <- fasstr::screen_flow_data(HYDAT = "08HA011")
# 
# 
# devtools::install_github("bcgov/fasstr")
# trends <- fasstr::compute_annual_trends(HYDAT = "08HA011", start_year = 1965, end_year = 2015, zyp_method = "yuepilon")
# lowflows <- fasstr::calc_annual_lowflows(HYDAT = "08HA002", start_year = 1965, end_year = 2015)
# trends2 <- fasstr::compute_annual_trends(trendsdata = lowflows, zyp_method = "yuepilon")
# 
# total <- fasstr::calc_annual_total_flows(HYDAT = "08HA002", incl_seasons = T, start_year = 1965, end_year = 2015)


