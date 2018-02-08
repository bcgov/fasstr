
#devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr",repos = NULL, type = "source")


library(fasstr)
# Parameters
stn_number <- "08NM116"
start_year = 1981 #NULL
end_year = 2000 #NULL


folder <- "testing/MissionCreek/"
dir.create(path = folder)


### Time series
##########################
timeseriesfolder <- "1-TimeSeries/"
dir.create(path=paste0(folder,timeseriesfolder))

timeseries <- fill_missing_dates(data = stn_number)
timeseries <- add_date_variables(timeseries)
timeseries <- add_rolling_means(timeseries)
write_flow_data(timeseries, file = paste0(folder, timeseriesfolder, "daily_record.csv"))
timeseries_plot <- plot_flow_data(data = stn_number, start_year = start_year,end_year = end_year)
timeseries_annual_plot <- plot_flow_data(data = stn_number, start_year = start_year,end_year = end_year,
                                         plot_by_year = TRUE)


flow_summary <- screen_flow_data(timeseries,start_year = start_year,end_year = end_year)
summary_plot <- plot_data_screening(timeseries,start_year = start_year,end_year = end_year)
missing_plot <- plot_missing_dates(timeseries,start_year = start_year,end_year = end_year)



### Long-term Stats
##########################
longtermfolder <- "2-Longterm/"
dir.create(path=paste0(folder,longtermfolder))

longterm_table <- calc_longterm_stats(timeseries, start_year = start_year,end_year = end_year, percentiles = 1:99)
longterm_plot <- plot_longterm_stats(timeseries,start_year = start_year,end_year = end_year)

flow_curves <- plot_flow_duration(timeseries,start_year = start_year,end_year = end_year)



### Annual Stats
##########################
annualfolder <- "3-Annual/"
dir.create(path=paste0(folder,annualfolder))

annual_stats <- calc_annual_stats(timeseries,start_year = start_year,end_year = end_year)
annual_total <- calc_annual_cumulative_stats(timeseries,start_year = start_year,end_year = end_year)
annual_yield <- calc_annual_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, use_yield = T)
annual_timing <- calc_annual_flow_timing(timeseries,start_year = start_year,end_year = end_year)# DAYS OUTSIDE NORMAL
annual_normal <- calc_annual_outside_normal(timeseries,start_year = start_year,end_year = end_year)


### Monthly Stats
##########################
monthfolder <- "4-Month/"
dir.create(path=paste0(folder,monthfolder))

# MONTHLY STATS
monthly_stats <- calc_monthly_stats(timeseries,start_year = start_year,end_year = end_year)
monthly_total <- calc_monthly_cumulative_stats(timeseries,start_year = start_year,end_year = end_year)
monthly_yield <- calc_monthly_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, use_yield = T)




### Daily Stats
##########################
dailyfolder <- "5-Daily/"
dir.create(path=paste0(folder,dailyfolder))

daily_table <- fasstr_daily_stats(flowdata=timeseries,
                                  #HYDAT = stn_number,
                                  write_table = T,report_dir = paste0(folder,dailyfolder),start_year = start_year,end_year = end_year)
daily_plots <- fasstr_daily_stats_plots(flowdata=timeseries,
                                        #HYDAT = stn_number,
                                        write_plot = T,report_dir = paste0(folder,dailyfolder),log_discharge = T,start_year = start_year,end_year = end_year)

cumulative_table <- fasstr_daily_cumulative_stats(flowdata=timeseries,
                                                  #HYDAT = stn_number,
                                                  write_table = T,report_dir = paste0(folder,dailyfolder),start_year = start_year,end_year = end_year)
cumulative_plots <- fasstr_daily_cumulative_plots(flowdata=timeseries,
                                                  #HYDAT = stn_number,
                                                  write_plot = T,report_dir = paste0(folder,dailyfolder),start_year = start_year,end_year = end_year)



### Low Flows
##########################
lowflowfolder <- "6-LowFlows/"
dir.create(path=paste0(folder,lowflowfolder))

# LOW FLOWS


### Low Flow Frequency
##########################
freqfolder <- "7-LowFlowFrequencies/"
dir.create(path=paste0(folder,freqfolder))

lowflow_results <- fasstr_annual_freq_analysis(flowdata=timeseries,
                                               #HYDAT = stn_number,
                                               report_dir = paste0(folder,freqfolder),start_year = start_year,end_year = end_year)


### Annual Trending
##########################
trendingfolder <- "8-Trending/"
dir.create(path=paste0(folder,trendingfolder))

trends <- fasstr_annual_trends_analysis(flowdata=timeseries,
                               #HYDAT = stn_number,
                               write_trends_data = T,write_trends_results = T,report_dir = paste0(folder,trendingfolder),
                               zyp_method = "yuepilon",start_year = start_year,end_year = end_year)
trends_plots <- fasstr_annual_trends_plots(flowdata=timeseries,
                                           #HYDAT = stn_number,
                                           write_plots = T, report_dir = paste0(folder,trendingfolder),
                                           zyp_method = "yuepilon",start_year = start_year,end_year = end_year)




