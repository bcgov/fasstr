
#devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr",repos = NULL, type = "source")


library(fasstr)
# Parameters
stn_number <- "08NM116"
start_year=1981 #NULL
end_year=2000 #NULL


folder <- "testing/MissionCreek/"
dir.create(path=folder)


### Time series
##########################
timeseriesfolder <- "1-TimeSeries/"
dir.create(path=paste0(folder,timeseriesfolder))

timeseries <- fasstr_add_missing_dates(HYDAT = stn_number)
timeseries <- fasstr_add_date_vars(timeseries)
timeseries <- fasstr_add_rolling_means(timeseries)
fasstr_write_daily_flows(timeseries,report_dir = paste0(folder,timeseriesfolder),na="")
timeseries_plot <- fasstr_timeseries_plot(HYDAT = stn_number,
                                          write_plot = T,
                                          report_dir = paste0(folder,timeseriesfolder),
                                          start_year = start_year,end_year = end_year)
timeseries_annual_plot <- fasstr_timeseries_plot(HYDAT = stn_number,
                                          write_plot = T,
                                          report_dir = paste0(folder,timeseriesfolder),
                                          plot_by_year = T,start_year = start_year,end_year = end_year)

flow_summary <- fasstr_data_screening(flowdata=timeseries,
                                      #HYDAT = stn_number,
                                      write_table = T,report_dir = paste0(folder,timeseriesfolder),start_year = start_year,end_year = end_year)
summary_plot <- fasstr_data_screening_plots(flowdata=timeseries,
                                            #HYDAT = stn_number,
                                            write_plot = T,report_dir = paste0(folder,timeseriesfolder),start_year = start_year,end_year = end_year)
missing_plot <- fasstr_data_screening_plots(flowdata=timeseries,
                                            #HYDAT = stn_number,
                                            write_plot = T,report_dir = paste0(folder,timeseriesfolder),start_year = start_year,end_year = end_year)



### Long-term Stats
##########################
longtermfolder <- "2-Longterm/"
dir.create(path=paste0(folder,longtermfolder))

longterm_table <- fasstr_longterm_stats(flowdata=timeseries,
                                        #HYDAT = stn_number,
                                        write_table = T,report_dir = paste0(folder,longtermfolder),start_year = start_year,end_year = end_year)
longterm_plot <- fasstr_longterm_stats_plot(flowdata=timeseries,
                                            #HYDAT = stn_number,
                                            log_discharge = T,
                                            write_plot =T,report_dir = paste0(folder,longtermfolder),start_year = start_year,end_year = end_year)

longterm_ptiles <- fasstr_longterm_ptiles(flowdata=timeseries,
                                         # HYDAT = stn_number,
                                          write_table = T,report_dir = paste0(folder,longtermfolder), transpose = T,start_year = start_year,end_year = end_year)
flow_curves <- fasstr_flow_duration_plots(flowdata=timeseries,
                                        #  HYDAT = stn_number,
                                          write_plot = T,report_dir = paste0(folder,longtermfolder),start_year = start_year,end_year = end_year)



### Annual Stats
##########################
annualfolder <- "3-Annual/"
dir.create(path=paste0(folder,annualfolder))

annual_stats <- fasstr_annual_stats(flowdata=timeseries,
                                  #HYDAT = stn_number,
                                  write_table = T,report_dir = paste0(folder,annualfolder),start_year = start_year,end_year = end_year)
annual_total <- fasstr_annual_total_flows(flowdata=timeseries,
                                          #HYDAT = stn_number,
                                          write_table = T,report_dir = paste0(folder,annualfolder),start_year = start_year,end_year = end_year)
annual_timing <- fasstr_annual_flow_timing(flowdata=timeseries,
                                          #HYDAT = stn_number,
                                          write_table = T,report_dir = paste0(folder,annualfolder),start_year = start_year,end_year = end_year)# DAYS OUTSIDE NORMAL
annual_normal <- fasstr_annual_days_outside_normal(flowdata=timeseries,
                                          #HYDAT = stn_number,
                                          write_table = T,report_dir = paste0(folder,annualfolder),start_year = start_year,end_year = end_year)


### Monthly Stats
##########################
monthfolder <- "4-Month/"
dir.create(path=paste0(folder,monthfolder))

# MONTHLY STATS



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




