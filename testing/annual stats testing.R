
devtools::document()
install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr",repos = NULL, type = "source")



data <- tidyhydat::hy_daily_flows(station_number = "08HB048")
fasstr::fasstr_LTMAD(HYDAT = "08HB048",months = 11)
fasstr::fasstr_percentile_rank(HYDAT = "08HB048", flowvalue = .8109)
fasstr::fasstr_annual_missing_plots(HYDAT = "08HB048")

percentiles <- fasstr::fasstr_longterm_stats(HYDAT = "08HB048",percentiles = 1:99,transpose = T,custom_months = 1:5)

fasstr::fasstr_percentile_rank(HYDAT = "08HB048",flowvalue = 0.0361400,months = 1)
fasstr::fasstr_percentile_rank(HYDAT = "08HB048",flowvalue = fasstr::fasstr_LTMAD(HYDAT = "08HB048",percent_MAD = 20),months = 1:5)

fasstr::fasstr_longterm_stats(HYDAT = "08HB048",custom_months = 7:8)
fasstr::fasstr_longterm_stats_plot(HYDAT = "08HB048",write_plot = T,write_imgsize = c(5,11))
fasstr::fasstr_daily_stats_plots(HYDAT = "08NM116",start_year = 1980)[2]
fasstr::fasstr_flow_duration_curves(HYDAT = "08HB048",incl_months = NA,incl_longterm = F,custom_months = 4:7)
fasstr::fasstr_daily_flows_plot(HYDAT = "08HB048",start_year = 2000,plot_by_year = T,
                                log_discharge = F)


fasstr::fasstr_LTMAD(HYDAT = "08HB048",excluded_years = T)
data <- fasstr::fasstr_add_cumulative_volume(HYDAT = "08HB048")
data <- fasstr::fasstr_add_cumulative_yield(data)


data <- fasstr::fasstr_LTMAD(HYDAT = "08HB048",start_year = 1973)

data <- fasstr::fasstr_add_daily_volume(HYDAT = "08HB048")
data <- fasstr::fasstr_add_daily_yield(HYDAT = "08NM116")
data <- fasstr_add_date_vars(data,water_year = T, water_year_start = 6)
data <- fasstr::fasstr_add_rolling_means(data)
data <- fasstr::fasstr_add_missing_dates(HYDAT = "08NM116")
data <- fasstr::fasstr_add_cumulative_yield(data, basin_area = 10.3)


fasstr_annual_days_outside_normal_plots(HYDAT = "08HB048", start_year = 1980, exclude_years = 1995)

trends <- fasstr::fasstr_annual_trends_plots(HYDAT = "08HB048",zyp_method = "yuepilon",write_plots = T)



data <- fasstr::fasstr_longterm_stats(HYDAT = "08HB048", percentiles = c(1:20))
data <- fasstr::fasstr_longterm_percentiles(HYDAT = "08HB048")
fasstr_lookup(HYDT, lookvalue=10.3, month)

data <- fasstr::fasstr_daily_cumulative_stats(HYDAT = "08HB048",use_yield = T, percentiles = c(1:10))

data <- fasstr::fasstr_daily_cumulative_stats(data,use_yield = T, basin_area = 10.3)

fasstr::fasstr_write_daily_flows(HYDAT = "08NM116",na="",fill_missing_dates = T)


data <- fasstr::fasstr_daily_cumulative_stats(data,use_yield = T, basin_area = 10.3)

fasstr::fasstr_daily_cumulative_plots(HYDAT = "08HB048",use_yield = T)[6]

fasstr::fasstr_annual_lowflows_plots(HYDAT = "08NM116",
                                     start_year = 1969,
                                     exclude_years = c(2000,2002:2004))[1]


data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
data <- fasstr::fasstr_add_rolling_means(data)
data <- fasstr::fasstr_add_total_volume(data)
data <- fasstr_fill_missing_dates(data)
data <- fasstr::fasstr_add_date_vars(data)

fasstr::fasstr_write_daily_flows(data,na="")

fasstr::fasstr_write_daily_flows(HYDAT = "08HA002", fill_missing_dates = T)


test <- fasstr::fasstr_monthly_stats_plots(HYDAT = "08NM116",
                                           write_plot = T,
                                           percentiles = c(1:100))

fasstr::fasstr_annual_flow_timing_plots(HYDAT = "08NM116",
                                               # write_plot = T,
                                               # water_year = T,
                                               start_year = 1967,
                                               exclude_years = c(1980:1985,1987)
                                        )

data <- tidyhydat::hy_daily_flows(station_number = "08HB048")
fasstr::fasstr_annual_total_flows_plots(data,basin_area = 10.4,
                                                water_year = F,
                                        water_year_start = 6)[4]
fasstr::fasstr_annual_total_flows_plots(HYDAT = "08HB048",basin_area = 10.4,
                                        water_year = T,
                                        water_year_start = 5)[4]

all_ann <- fasstr::fasstr_annual_all_stats(HYDAT = "08NM116")

plots <- fasstr::fasstr_annual_trends_plots(HYDAT = "08HB048",zyp_method = "yuepilon",write_plots = T)


fasstr::fasstr_flow_duration_plots(HYDAT="08HB048")



low.flows <- fasstr::fasstr_annual_lowflows(HYDAT = "08HB048",water_year = T)
low.flows <- dplyr::select(low.flows,-dplyr::contains("Date"))


months <- fasstr::fasstr_monthly_stats(HYDAT = "08HB048",percentiles = c(10,25,75,90),water_year = T,water_year_start = 5)
months2 <- with(months, months[order(Year, Month),])

flowdata=NULL
HYDAT="08HB048"
station_name=NA
water_year=T
water_year_start=10
start_year=NULL
end_year=NULL
exclude_years=NULL
percentiles=c(25,75)
transpose=FALSE
write_table=FALSE
report_dir="."
na.rm=list(na.rm.global=FALSE)
table_nddigits=3



fasstr::fasstr_add_date_vars(HYDAT = "08HB048")




flowdata=NULL
HYDAT="08HB048"
station_name=NA
water_year=FALSE
water_year_start=10
start_year=NULL
end_year=NULL
exclude_years=NULL
basin_area=NA
seasons=TRUE
write_plot=TRUE
plot_type="pdf"        # write out statistics on calendar year
log_discharge=FALSE
report_dir="."
na.rm=list(na.rm.global=FALSE)



flowdata=data
HYDAT=NULL
station_name="fasstr"
water_year=FALSE
water_year_start=10
start_year=NULL
end_year=NULL
exclude_years=NULL
rolling_days=c(1,3,7,30)
rolling_align="right"
write_plot=FALSE
plot_type="pdf"      
report_dir="."
na.rm=list(na.rm.global=FALSE)




flowdata=NULL
HYDAT="08HB048"
station_name="fasstr"
water_year=FALSE
water_year_start=10
start_year=NULL
end_year=NULL
exclude_years=NULL
use_yield=FALSE
basin_area=NA
write_plot=FALSE
plot_type="pdf"
#plot_all_years=TRUE,
log_discharge=FALSE
plot_title=NA
report_dir="."
na.rm=list(na.rm.global=FALSE)




data <- tidyhydat::hy_daily_flows(station_number = "08HB048")
A <- fasstr::fasstr_add_cumulative_volume(data)
A <- fasstr::fasstr_add_cumulative_yield(data)
A <- fasstr::fasstr_add_daily_volume(data)
A <- fasstr::fasstr_add_daily_yield(data)
A <- fasstr::fasstr_add_date_vars(data)
A <- fasstr::fasstr_add_missing_dates(data)
A <- fasstr::fasstr_add_rolling_means(data)
A <- fasstr::fasstr_annual_all_stats(data)
A <- fasstr::fasstr_annual_days_outside_normal(data)
fasstr::fasstr_annual_days_outside_normal_plots(data)
A <- fasstr::fasstr_annual_flow_timing(data)
fasstr::fasstr_annual_flow_timing_plots(data)
A <- fasstr::fasstr_annual_freq_analysis(data)
A <- fasstr::fasstr_annual_lowflows(data)
fasstr::fasstr_annual_lowflows_plots(data)
fasstr::fasstr_annual_missing_plots(data)
A <- fasstr::fasstr_annual_stats(data)
A <- fasstr::fasstr_annual_total_flows(data)
fasstr::fasstr_annual_total_flows_plots(data)
A <- fasstr::fasstr_annual_trends_analysis(data,zyp_method = "yuepilon")
fasstr::fasstr_annual_trends_plots(data,zyp_method = "yuepilon")
fasstr::fasstr_daily_cumulative_plots(data, use_yield = T)
A <- fasstr::fasstr_daily_cumulative_stats(data,use_yield = T)
A <- fasstr::fasstr_daily_stats(data)
fasstr::fasstr_daily_stats_plots(data)
A <- fasstr::fasstr_data_screening(data)
fasstr::fasstr_data_screening_plots(data)
fasstr::fasstr_flow_duration_curves(data)
A <- fasstr::fasstr_longterm_percentiles(data)
A <- fasstr::fasstr_longterm_stats(data)
fasstr::fasstr_longterm_stats_plot(data)
A <- fasstr::fasstr_monthly_stats(data)
fasstr::fasstr_monthly_stats_plots(data)
fasstr::fasstr_timeseries_plot(data)
fasstr::fasstr_write_daily_flows(data)


stn.number="08NM116"
wt_yr=F
A <- fasstr::fasstr_add_cumulative_volume(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_add_cumulative_yield(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_add_daily_volume(HYDAT = stn.number)
A <- fasstr::fasstr_add_daily_yield(HYDAT = stn.number)
A <- fasstr::fasstr_add_date_vars(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_add_missing_dates(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_add_rolling_means(HYDAT = stn.number)
A <- fasstr::fasstr_annual_all_stats(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_days_outside_normal(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_days_outside_normal_plots(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_flow_timing(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_flow_timing_plots(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_freq_analysis(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_lowflows(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_lowflows_plots(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_missing_plots(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_stats(HYDAT = stn.number,water_year = wt_yr,custom_months = 6:8)
fasstr::fasstr_annual_stats_plots(HYDAT = stn.number,water_year = wt_yr,months = 8:10)
A <- fasstr::fasstr_annual_total_flows(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_total_flows_plots(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_annual_trends_analysis(HYDAT = stn.number,zyp_method = "yuepilon",water_year = wt_yr)
A <- fasstr::fasstr_annual_trends_plots(HYDAT = stn.number,zyp_method = "yuepilon",water_year = wt_yr)
A <- fasstr::fasstr_daily_cumulative_plots(HYDAT = stn.number, use_yield = T,water_year = wt_yr)
A <- fasstr::fasstr_daily_cumulative_stats(HYDAT = stn.number,use_yield = T,water_year = wt_yr)
A <- fasstr::fasstr_daily_stats(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_daily_stats_plots(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_data_screening(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_data_screening_plots(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_flow_duration_curves(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_longterm_percentiles(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_longterm_stats(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_longterm_stats_plot(HYDAT = stn.number,log_discharge = T,water_year = wt_yr)
A <- fasstr::fasstr_monthly_stats(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_monthly_stats_plots(HYDAT = stn.number,log_discharge = T,water_year = wt_yr)
A <- fasstr::fasstr_daily_flows_plot(HYDAT = stn.number,water_year = wt_yr)
A <- fasstr::fasstr_write_daily_flows(HYDAT = stn.number,water_year = wt_yr)






# Compute daily percentiles (if 10 or more years of data)
if (!all(is.na(percentiles))){
  for (ptile in percentiles) {
    Q_daily_ptile <- dplyr::summarise(dplyr::group_by(flowdata,AnalysisDate,AnalysisDoY),
                                      Percentile=ifelse(sum(!is.na(RollingValue))>=10,quantile(RollingValue,ptile/100, na.rm=TRUE),NA))
    colnames(Q_daily_ptile)[3] <- paste0("P",ptile)
    Q_daily <- merge(Q_daily,Q_daily_ptile,by=c("AnalysisDate","AnalysisDoY"))
  }
}






