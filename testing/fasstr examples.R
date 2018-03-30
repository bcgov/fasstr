


devtools::document()
install.packages("/Users/jongoetz/Documents/R/fasstr devel", repos = NULL, type = "source")
install.packages("C:/Users/jgoetz/R/fasstr devel",repos = NULL, type = "source")
#devtools::check()



### FLOW_DATA
### ----------

library(fasstr)
library(dplyr)



# One station with Date and Value
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
results <- calc_longterm_stats(data = flow_data)
results <- calc_annual_stats(data = flow_data)
results <- calc_all_annual_stats(data = flow_data)
results <- calc_annual_cumulative_stats(data = flow_data)
results <- calc_annual_flow_timing(data = flow_data)
results <- calc_annual_lowflows(data = flow_data)
results <- calc_annual_outside_normal(data = flow_data)
results <- calc_daily_stats(data = flow_data)
results <- calc_daily_cumulative_stats(data = flow_data)
results <- calc_flow_percentile(data = flow_data, flow_value =  0.801)
results <- calc_lt_mad(data = flow_data)
results <- calc_lt_percentile(data = flow_data, percentiles =  50)
results <- calc_monthly_cumulative_stats(data = flow_data)
results <- calc_monthly_stats(data = flow_data)
results <- screen_flow_data(data = flow_data)
plot_flow_data(data = flow_data)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T)
plot_annual_flow_timing(data = flow_data)
plot_annual_outside_normal(data = flow_data)
plot_annual_stats(data = flow_data)
#plot_annual_trends(data = flow_data)
plot_daily_cumulative_stats(data = flow_data)
plot_daily_stats(data = flow_data)
plot_data_screening(data = flow_data)
plot_flow_duration(data = flow_data)
plot_longterm_stats(data = flow_data)
plot_missing_dates(data = flow_data)
plot_monthly_cumulative_stats(data = flow_data)
plot_monthly_stats(data = flow_data)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T)



# Multiple stations and custom Date and Value column names
flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116")) %>% 
  rename(Datesss = Date, Valuesss = Value) %>% 
  fill_missing_dates(dates = Datesss, values = Valuesss) %>% 
  add_basin_area() %>% 
  add_date_variables(dates = Datesss) %>% 
  add_rolling_means(dates = Datesss, values = Valuesss) %>% 
  add_daily_volume(values = Valuesss) %>% 
  add_cumulative_volume(dates = Datesss, values = Valuesss) %>% 
  add_daily_yield(values = Valuesss) %>%
  add_cumulative_yield(dates = Datesss, values = Valuesss) %>% 
  add_seasons(dates = Datesss)
results <- calc_longterm_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_annual_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_all_annual_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_annual_cumulative_stats(data = flow_data, dates = Datesss, values = Valuesss, incl_seasons = T)
results <- calc_annual_flow_timing(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_annual_lowflows(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_annual_outside_normal(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_daily_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_daily_cumulative_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_flow_percentile(data = flow_data, flow_value =  0.801, dates = Datesss, values = Valuesss)
results <- calc_lt_mad(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_lt_percentile(data = flow_data, percentiles =  50, dates = Datesss, values = Valuesss)
results <- calc_monthly_cumulative_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- calc_monthly_stats(data = flow_data, dates = Datesss, values = Valuesss)
results <- screen_flow_data(data = flow_data, dates = Datesss, values = Valuesss)
plot_flow_data(data = flow_data, dates = Datesss, values = Valuesss)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T, dates = Datesss, values = Valuesss)
plot_annual_flow_timing(data = flow_data, dates = Datesss, values = Valuesss)
plot_annual_outside_normal(data = flow_data, dates = Datesss, values = Valuesss)
plot_annual_stats(data = flow_data, dates = Datesss, values = Valuesss)
#plot_annual_trends(data = flow_data, dates = Datesss, values = Valuesss)
plot_daily_cumulative_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_daily_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_data_screening(data = flow_data, dates = Datesss, values = Valuesss)
plot_flow_duration(data = flow_data, dates = Datesss, values = Valuesss)
plot_longterm_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_missing_dates(data = flow_data, dates = Datesss, values = Valuesss)
plot_monthly_cumulative_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_monthly_stats(data = flow_data, dates = Datesss, values = Valuesss)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T, dates = Datesss, values = Valuesss)




# Station no STATION_NUMBER
flow_data <- tidyhydat::hy_daily_flows(station_number = "08HB048") %>% 
  select(Date,Value) %>% 
  fill_missing_dates() %>%
  add_basin_area(basin_area = 10.555) %>%
  add_date_variables(water_year = T) %>%
  add_rolling_means() %>%
  add_daily_volume() %>%
  add_cumulative_volume() %>% 
  add_daily_yield(basin_area = 10.3) %>%
  add_cumulative_yield(basin_area = 10.3) %>% 
  add_seasons()
results <- calc_longterm_stats(data = flow_data)
results <- calc_annual_stats(data = flow_data)
results <- calc_all_annual_stats(data = flow_data)
results <- calc_annual_cumulative_stats(data = flow_data, use_yield = T)
results <- calc_annual_flow_timing(data = flow_data)
results <- calc_annual_lowflows(data = flow_data)
results <- calc_annual_outside_normal(data = flow_data)
results <- calc_daily_stats(data = flow_data)
results <- calc_daily_cumulative_stats(data = flow_data, use_yield = T)
results <- calc_flow_percentile(data = flow_data, flow_value =  0.801)
results <- calc_lt_mad(data = flow_data)
results <- calc_lt_percentile(data = flow_data, percentiles =  50)
results <- calc_monthly_cumulative_stats(data = flow_data, use_yield = T)
results <- calc_monthly_stats(data = flow_data)
results <- screen_flow_data(data = flow_data)
plot_flow_data(data = flow_data)
plot_annual_cumulative_stats(data = flow_data, incl_seasons = T, use_yield = T)
plot_annual_flow_timing(data = flow_data)
plot_annual_outside_normal(data = flow_data)
plot_annual_stats(data = flow_data)
#plot_annual_trends(data = flow_data)
plot_daily_cumulative_stats(data = flow_data, use_yield = T, basin_area = 10)
plot_daily_stats(data = flow_data, include_year = 1990)
plot_data_screening(data = flow_data)
plot_flow_duration(data = flow_data)
plot_longterm_stats(data = flow_data)
plot_missing_dates(data = flow_data)
plot_monthly_cumulative_stats(data = flow_data, use_yield = T, basin_area = 10)
plot_monthly_stats(data = flow_data)

trending <- compute_annual_trends(data = flow_data, zyp_method = "yuepilon")
trending_plots <- plot_annual_trends(trending)




### HYDAT
### ----------

# Single stations
flow_data <- fill_missing_dates(station_number = "08HB048")
flow_data <- add_basin_area(station_number = "08HB048")
flow_data <- add_seasons(station_number = "08HB048")
flow_data <- add_date_variables(station_number = "08HB048", water_year = T)
flow_data <- add_rolling_means(station_number = "08HB048")
flow_data <- add_daily_volume(station_number = "08HB048")
flow_data <- add_cumulative_volume(station_number = "08HB048")
flow_data <- add_daily_yield(station_number = "08HB048")
flow_data <- add_cumulative_yield(station_number = "08HB048", basin_area = 10.2)
results <- calc_longterm_stats(station_number = "08HB048", ignore_missing = F)
results <- calc_annual_stats(station_number = "08HB048")
results <- calc_all_annual_stats(station_number = "08HB048")
results <- calc_annual_cumulative_stats(station_number = "08HB048", use_yield = T, incl_seasons = T)
results <- calc_annual_flow_timing(station_number = "08HB048")
results <- calc_annual_lowflows(station_number = "08HB048")
results <- calc_annual_outside_normal(station_number = "08HB048")
results <- calc_daily_stats(station_number = "08HB048", months = 6:7)
results <- calc_daily_cumulative_stats(station_number = "08NM116", start_year = 1990)
results <- calc_flow_percentile(station_number = "08HB048", flow_value = 10000)
results <- calc_lt_mad(station_number = "08HB048")
results <- calc_lt_percentile(station_number = "08HB048", percentiles = 50, complete_years = T)
results <- calc_monthly_cumulative_stats(station_number = "08HB048")
results <- calc_monthly_stats(station_number = "08HB048")
results <- screen_flow_data(station_number = "08HB048")
trending <- compute_annual_trends(station_number = "08HB048", zyp_method = "yuepilon")
trending_plots <- plot_annual_trends(trending)

plot_flow_data(station_number = "08HB048", exclude_years = 2000)
plot_annual_cumulative_stats(station_number = "08HB048", incl_seasons = T)
plot_annual_flow_timing(station_number = "08HB048")
plot_annual_outside_normal(station_number = "08HB048")
plot_annual_stats(station_number = "08HB048")
plot_annual_lowflows(station_number = "08HB048")
#plot_annual_trends(data = "08HB048")
plot_daily_cumulative_stats(station_number = "08HB048")
plot_daily_stats(station_number = "08HB048", include_year = 1999)
plot_data_screening(station_number = "08HB048")
plot_flow_duration(station_number = "08HB048", custom_months = 1:3, custom_months_label = "WINTER", ignore_missing = T)
plot_longterm_stats(station_number = "08HB048")
plot_missing_dates(station_number = "08HB048")
plot_monthly_cumulative_stats(station_number = "08HB048")
plot_monthly_stats(station_number = "08HB048")
plot_annual_cumulative_stats(station_number = "08HB048", use_yield = T)
write_flow_data(station_number = c("08HB048","08NM116"))



# Multiple stations
flow_data <- fill_missing_dates(station_number = c("08HB048","08NM116"))
flow_data <- add_basin_area(station_number = c("08HB048","08NM116"))
flow_data <- add_seasons(station_number = c("08HB048","08NM116"))
flow_data <- add_date_variables(station_number = c("08HB048","08NM116"), water_year = T)
flow_data <- add_rolling_means(station_number = c("08HB048","08NM116"))
flow_data <- add_daily_volume(station_number = c("08HB048","08NM116"))
flow_data <- add_cumulative_volume(station_number = c("08HB048","08NM116"))
flow_data <- add_daily_yield(station_number = c("08HB048","08NM116"))
flow_data <- add_cumulative_yield(station_number = c("08HB048","08NM116"), basin_area = c("08HB048"=10.2))
results <- calc_longterm_stats(station_number = c("08HB048","08NM116"))
results <- calc_annual_stats(station_number = c("08HB048","08NM116"))
results <- calc_all_annual_stats(station_number = c("08HB048","08NM116"), transpose = T)
results <- calc_annual_cumulative_stats(station_number = c("08HB048","08NM116"))
results <- calc_annual_flow_timing(station_number = c("08HB048","08NM116"))
results <- calc_annual_lowflows(station_number = c("08HB048","08NM116"))
results <- calc_annual_outside_normal(station_number = c("08HB048","08NM116"))
results <- calc_daily_stats(station_number = c("08HB048","08NM116"))
results <- calc_daily_cumulative_stats(station_number = c("08HB048","08NM116"))
results <- calc_flow_percentile(station_number = c("08HB048","08NM116"), flow_value = 10000)
results <- calc_lt_mad(station_number = c("08HB048","08NM116"))
results <- calc_lt_percentile(station_number = c("08HB048","08NM116"), percentiles = 50)
results <- calc_monthly_cumulative_stats(station_number = c("08HB048","08NM116"))
results <- calc_monthly_stats(station_number = c("08HB048","08NM116"))
results <- screen_flow_data(station_number = c("08HB048","08NM116"))
trending <- compute_annual_trends(station_number = c("08HB048","08NM116"), zyp_method = "yuepilon")
trending_plots <- plot_annual_trends(trending)

plot_flow_data(station_number = c("08HB048","08NM116"))
plot_annual_cumulative_stats(station_number = c("08HB048","08NM116"))
plot_annual_flow_timing(station_number = c("08HB048","08NM116"))
plot_annual_outside_normal(station_number = c("08HB048","08NM116"))
plot_annual_stats(station_number = c("08HB048","08NM116"))
#plot_annual_trends(data = c("08HB048","08NM116"))
plot_daily_cumulative_stats(station_number = c("08HB048","08NM116"))
plot_daily_stats(station_number = c("08HB048","08NM116"))
plot_data_screening(station_number = c("08HB048","08NM116"))
plot_flow_duration(station_number = c("08HB048","08NM116"))
plot_longterm_stats(station_number = c("08HB048","08NM116"))
plot_missing_dates(station_number = c("08HB048","08NM116"))
plot_monthly_cumulative_stats(station_number = c("08HB048","08NM116"))
plot_monthly_stats(station_number = c("08HB048","08NM116"))
plot_annual_cumulative_stats(station_number = c("08HB048","08NM116"))






devtools::document()
install.packages("/Users/jongoetz/Documents/R/fasstr devel", repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr devel",repos = NULL, type = "source")
#devtools::check()

library(fasstr)





#### TRENDING


trending <- compute_annual_trends(station_number = "08HB048", zyp_method = "yuepilon", start_year = 1973)
plots <- plot_annual_trends(trending)





alldata <- fasstr::calc_annual_flow_timing("08HB048", transpose = T)
alldata <- alldata[,2:ncol(alldata)]
test2 <- fasstr::plot_annual_trends(trendsdata = alldata, zyp_method = "yuepilon")


# more recent
data <- tidyhydat::hy_daily_flows("08HB048") %>% select(-STATION_NUMBER)

flow_data <- calc_all_annual_stats(data = data)

data <- compute_annual_trends(data, zyp_method = "yuepilon", start_year = 1973)
plots <- plot_annual_trends(data)
data <- compute_annual_trends(station_number = c("08HB048","08NM116"), zyp_method = "yuepilon", start_year = 1973)


### FREQUENCY

data <- compute_annual_frequencies(station_number = "08HB048", water_year = TRUE)
data <- compute_hydat_peak_frequencies(station_number = "08NM116")

data <- compute_annual_frequencies(station_number = c("08HB048","08NM116"))
data <- compute_frequency_stat(station_number = "08NM116", roll_day = 7, return_period = 10)





######## WRITING


fasstr::plot_annual_flow_timing("08HB048")

write_plots(month_plot, file = "pdftestkkkk", format = "jpeg")

month_plot <- plot_monthly_stats("08HB048")$Monthly_Mean
ggplot2::ggsave(month_plot, filename = "test.pdf")
lowflows <- calc_annual_lowflows("08HB048", transpose = T)

data <- compute_annual_trends("08HB048", zyp_method = "yuepilon", incl_data=F)

data <- calc_all_annual_stats("08HB048", transpose = T)
write_results(data = data, file = "all.xlsx", digits = 3)
calc_annual_flow_timing("08HB048")

fasstr::calc_monthly_cumulative_stats("08HB048", use_yield = T)
write_flow_data(data = "08HB048", file = "d.xls", digits = 1)

write_results(data = calc_longterm_stats(data = c("08HA002", "08HA011"),
                                         start_year = 1971, end_year = 2000), 
              file = "Cowichan River Long-term Flows (1971-2000).xlsx", 
              digits = 1)

ggplot2::ggsave("month",month_plots, device = "pdf")

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








####PLOTS

library(ggthemes)


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







#### BASIN AREA



basin_area=NA
basin_area=10000

basin_area <- c("08HB048"=966,"08NM116"=NA)
basin_area <- c("08NM116"=750)
basin_area <- c("XXXX"=750)


flow_data <- tidyhydat::hy_daily_flows(station_number = c("08HB048","08NM116"))
flow_data <- dplyr::select(flow_data,Date,Value)
flow_data$STATION_NUMBER <- "XXXXXXX"


if(all(is.na(basin_area))){
  basin_stations <- data.frame(STATION_NUMBER = unique(flow_data$STATION_NUMBER))
  basin_HYDAT <- suppressMessages(tidyhydat::hy_stations(station_number = basin_stations$STATION_NUMBER))
  basin_HYDAT <- dplyr::select(basin_HYDAT, STATION_NUMBER, Basin_Area_sqkm = DRAINAGE_AREA_GROSS)
  basin_area_table <- merge(basin_HYDAT, basin_stations, by = "STATION_NUMBER", all.y = TRUE)
}
if(!all(is.na(basin_area))){
  if(!is.numeric(basin_area)) stop("basin_area arguments must be numeric.")
  if(is.null(names(basin_area)) & length(basin_area) == 1) {
    if(length(unique(flow_data$STATION_NUMBER)) > 1) warning("Just one basin_area area applied without a corresponding STATION_NUMBER, the basin_area will be applied to all stations.")
    basin_area_table <- data.frame(STATION_NUMBER = unique(flow_data$STATION_NUMBER), Basin_Area_sqkm = basin_area)
  } else {
    if(length(basin_area)!=length(unique(flow_data$STATION_NUMBER)) | !all(names(basin_area) %in% unique(flow_data$STATION_NUMBER))) 
      warning("The number/names of STATION_NUMBERS and basin_area values provided do not match the number/names of STATION_NUMBERS in the flow data. Only those that match will be applied.")
    #if(!all(names(basin_area) %in% unique(flow_data$STATION_NUMBER))) warning("All STATION_NUMBERS listed in basin_area do not match those in the flow data. Only those that match will be applied.")
    basin_area_table <- data.frame(STATION_NUMBER = names(basin_area), Basin_Area_sqkm = basin_area)
  }
}
if(all(is.na(basin_area_table$Basin_Area_sqkm))) warning("No basin_area values provided or extracted from HYDAT. All Yield_mm values will be NA.")




flow_data <- merge(flow_data, basin_area_table, by = "STATION_NUMBER", all.x = TRUE)







basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = c("08HB048","08NM116"))[,c(1,9)]) 


basin <- c("08HB048"=10,"08NM116"=750)


flow_data$basin <- basin[match(flow_data$STATION_NUMBER,names(basin))]


basin2 <- data.frame(STATION_NUMBER=names(basin),
                     basin_area=basin)
flow_data <- merge(flow_data,basin2,by="STATION_NUMBER")

flowdata <- dplyr::select(flow_data,Date,Value) %>%
  mutate(STATION_NUMBER="XXXX")





stations =c("XXXX","08HB048")# <- unique(flowdata$STATION_NUMBER)
basin2 <- data.frame(STATION_NUMBER=stations)
basin_area_HYDAT <- suppressMessages(tidyhydat::hy_stations(station_number = stations)[,c(1,9)]) %>% 
  rename(basin_area=DRAINAGE_AREA_GROSS)
basin3 <- merge(basin2,basin_area_HYDAT, by="STATION_NUMBER", all.x = T)


basin <- c(STATION_NUMBER=c("08HB048","08NM116"), basin_area=c(10.1,975))
flow_data$basin <- basin[match(flow_data$STATION_NUMBER,names(basin))]
basin


