
devtools::document()
install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr",repos = NULL, type = "source")

data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
data <- fasstr::fasstr_add_rolling_means(data)
data <- fasstr::fasstr_add_total_volume(data)
data <- fasstr_fill_missing_dates(data)
data <- fasstr::fasstr_add_date_vars(data)

fasstr::fasstr_write_daily_flows(data,na="")

fasstr::fasstr_write_daily_flows(HYDAT = "08HA002")


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
station_name="fasstr"
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
station_name="fasstr"
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





