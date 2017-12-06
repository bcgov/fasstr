
devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
install.packages("C:/Users/jgoetz/R/fasstr",repos = NULL, type = "source")

fasstr::fasstr_longterm_stats_plot(HYDAT = "08NM116",log_discharge = T,write_plot =T,plot_type = "png",start_year = 1974)


fasstr::fasstr_daily_cumulative_plots(HYDAT = "08HB048",water_year = T,
                                      water_year_start = 7)$cumulative_2009

fasstr::fasstr_flow_duration_plots(HYDAT = "08HB048", start_year = 1990,end_year = 1991)

test <- fasstr::fasstr_daily_stats_plots(HYDAT = "08HB048",log_discharge = T)

fasstr::fasstr_longterm_ptiles(HYDAT = "08HB048")#,
                                                 # start_year = 1990,
                                                 # end_year = 1991)#,
                                                  #water_year = F,
                                #       water_year_start = 2,
                                #       write_plot = T,
                                #       plot_type = "png",
                                #       log_discharge = F,
                                # facet_wrap = F)

data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
terst <- fasstr::fasstr_daily_stats_plots(flowdata = data)


fasstr::fasstr_annual_missing_plots(data,water_year = F,water_year_start = 6,start_year = 1970,write_plot = T)

test <- fasstr_annual_freq_analysis(HYDAT = "08NM116"#,
                                    #HYDAT_peaks = "MAX",
                             #water_year = T#, 
                             #start_year = 1980,
                            # end_year = 2000,
                            # exclude_years=c(1990,1992)
                            )

test2 <- test$fit$`Q003-day Min`


flowdata=NULL
HYDAT="08HB048"
station_name=NA
HYDAT_peaks=NA
water_year=FALSE
water_year_start=10
start_year=1980
end_year=2000
exclude_years=NULL
months=1:4
rolling_days=c(1,3,7,30)
rolling_align="right"
use_log=FALSE
use_max=FALSE
prob_plot_position=c("weibull","median","hazen")
prob_scale_points=c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001)
fit_distr=c("PIII","weibull")
fit_distr_method=ifelse(fit_distr=="PIII","MOM","MLE")
fit_quantiles=c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01)
na.rm=list(na.rm.global=TRUE)
write_overview=FALSE
write_stat_table=FALSE
write_plotdata_table=FALSE
write_quantiles_table=FALSE
write_frequency_plot=FALSE
write_frequency_imgtype=c("pdf","png")
write_dir='.'
write_digits=3





flowdata=NULL
HYDAT="08HB048"
station_name="fasstr"
water_year=FALSE #create another for own water year????
start_year=NULL
end_year=NULL
water_year_start=10
rolling_days=1
rolling_align="right"
transpose=FALSE
write_table=FALSE        # write out statistics on calendar year
report_dir="."
table_nddigits=3




flowdata=NULL
HYDAT="08HB048"
station_name="fasstr"
water_year=FALSE #create another for own water year????
water_year_start=10
start_year=NULL
end_year=NULL
exclude_years=NULL # list of stations
percentiles=c(1:99)
transpose=FALSE
write_table=FALSE        # write out statistics on calendar year
report_dir="."
na.rm=list(na.rm.global=FALSE)
table_nddigits=3


