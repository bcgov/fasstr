devtools::document()
install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
devtools::install_github("bcgov/fasstr")



test <- fasstr_freq_analysis(HYDAT = "08NM116")


test2 <- fasstr::fasstr_daily_stats_plots(HYDAT = "08NM116",log_discharge = T,water_year = F,start_year = 1980,
                                          plot_type="png", write_plot = F,plot_title = "Mission Creek near East Kelowna")

test2 <- fasstr::fasstr_daily_stats(HYDAT = "08HB048",percentiles = c(5,10,15,20,45,99.9),
                                   transpose = F,write_table = T, water_year = T,water_year_start = 4,
                                   rolling_days = 7)
test <- fasstr::fasstr_daily_stats(HYDAT = "08HB048")
test <- fasstr::fasstr_longterm_stats(HYDAT = "08HB048",percentiles = c(5,6,7,90,99),
                                      water_year = T,water_year_start = 7)

test <- fasstr_add_date_vars(HYDAT = "08HB048",water_year_start = 2)

test <- fasstr::fasstr_longterm_ptiles(HYDAT = "08NM116",transpose = T,water_year = T,
                                       water_year_start = 6)
test <- fasstr::fasstr_annual_stats(HYDAT="08HB048",water_year = T)

water_year <- T
HYDAT <- "08HB048"
water_year_start <- 2
data <- tidyhydat::DLY_FLOWS(STATION_NUMBER = HYDAT)
data <- dplyr::select(data,Date,Q=Value)
test <- fasstr_fill_missing_dates(flowdata = flowdata)
test <- fasstr_fill_missing_dates(HYDAT = "08HB048")
test <- fasstr_fill_missing_dates(HYDAT = "08HB048",water_year = T, water_year_start = 1)
test2 <- fasstr_add_rolling_means(HYDAT = "08NM116")
test2 <- fasstr_fill_missing_dates(flowdata = test2)
test2 <- fasstr_add_date_vars(flowdata = data)

test <- fasstr_add_total_volume(HYDAT = "08HB048")

test2 <- fasstr_fill_missing_dates(HYDAT = "08NM116")
test2 <- fasstr_add_rolling_means(flowdata = test2)
test2 <- fasstr_add_date_vars(test2)

test2 <- fasstr_add_rolling_means(HYDAT = "08NM116",days =  c(1,2,3,5,7,15,30))
test2 <- fasstr_fill_missing_dates(test2)
test2 <- fasstr_add_date_vars(test2)

test2 <- fasstr_add_date_vars(HYDAT = "08NM116")
test2 <- fasstr_add_rolling_means(test2)
test2 <- fasstr_fill_missing_dates(test2)


test2 <- fasstr_fill_missing_dates(HYDAT = "08NM116")


test <- fasstr::fasstr_add_date_vars(HYDAT = "08HB048")
test <- fasstr::fasstr_add_rolling_means(test)
test <- fasstr::fasstr_fill_missing_dates(test)

test <- fasstr_annual_stats(HYDAT = "08NM116",water_year = T)

test <- fasstr::fasstr_annual_stats(HYDAT = "08HB048",water_year = T,basin_area = 10.3,
                                    exclude_years=c(1990,1992),
                                    station_name = "Carn",
                            transpose = F)
test <- fasstr_annual_trends(HYDAT = "08HB048",
                             zyp_method = "zhang")

terst <- fasstr_longterm_stats(HYDAT = "08HB048")

trends_test <- tidyr::gather(test,Statistic,Value,-Year)
trends_test <- tidyr::spread(trends_test,Year,Value)

test1 <- fasstr_annual_trends(trendsdata = trends_test, zyp_method = "yuepilon",
                              write_trends_results=T,write_trends_data=T)
test2 <- fasstr_annual_trends(zyp_method = "yuepilon", HYDAT = "08NM116",water_year = F,start_year = 1970,end_year = 2001,basin_area = 500, exclude_years=c(1990,1992))
data <- tidyhydat::DLY_FLOWS(STATION_NUMBER = "08NM116")
data <- dplyr::select(data,Date,R=Value)

test3 <- fasstr_annual_trends(zyp_method = "yuepilon", flowdata = data,water_year = F,start_year = 1970,end_year = 2001,basin_area = 500, exclude_years=c(1990,1992))

ann <- test$Qstat_annual

test2 <- fasstr_add_rolling_means(HYDAT = "08HB048")



test <- fasstr::fasstr_add_date_vars(HYDAT = "08HB048")
test <- fasstr::fasstr_add_rolling_means(HYDAT = "08HB048")
test <- fasstr::fasstr_longterm_stats(HYDAT = "08HB048")




flowdata=NULL
HYDAT="08HB048"
station_name="fasstr"
water_year=T #create another for own water year????
water_year_start=5
start_year=1980
end_year=2000
exclude_years=NULL # list of stations
rolling_days=1
rolling_align="right"
write_plot=T        # write out statistics on calendar year
plot_type="png"        # write out statistics on calendar year
log_discharge=T
report_dir="."
na.rm=list(na.rm.global=FALSE)
table_nddigits=3




