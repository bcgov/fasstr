
#devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr",repos = NULL, type = "source")


# Writes all data and plots


library(fasstr)
# Parameters
stn_number <- "08NM116"
start_year = 1981 #NULL
end_year = 2000 #NULL


folder <- "testing/MissionCreek/"
dir.create(path = folder, showWarnings = FALSE)


### Time series
##########################
timeseriesfolder <- "1-TimeSeries/"
dir.create(path=paste0(folder,timeseriesfolder), showWarnings = FALSE)

timeseries <- fill_missing_dates(station_number = stn_number)
timeseries <- add_date_variables(data = timeseries)
timeseries <- add_rolling_means(data = timeseries)


write_flow_data(data = timeseries, 
                file = paste0(folder, timeseriesfolder, "daily_record.xlsx"))

write_results(data = screen_flow_data(data = timeseries, start_year = start_year, end_year = end_year), 
              file = paste0(folder, timeseriesfolder, "flow_summary.xlsx"))

write_plots(plots = c(plot_flow_data(data = timeseries, start_year = start_year,end_year = end_year,plot_by_year = TRUE),
                      plot_flow_data(data = timeseries, start_year = start_year,end_year = end_year),
                      plot_data_screening(data = timeseries, start_year = start_year, end_year = end_year),
                      plot_missing_dates(data = timeseries, start_year = start_year, end_year = end_year)),
            foldername = paste0(folder, timeseriesfolder),
            type = "pdf",
            width = 8.5,
            height = 4)


### Long-term Stats
##########################
longtermfolder <- "2-Longterm/"
dir.create(path=paste0(folder,longtermfolder), showWarnings = FALSE)

write_results(data = calc_longterm_stats(timeseries, start_year = start_year,end_year = end_year, percentiles = 1:99, transpose = T), 
              file = paste0(folder, longtermfolder, "longterm_stats.xlsx"))

write_plots(plots = c(plot_longterm_stats(timeseries,start_year = start_year,end_year = end_year),
                      plot_flow_duration(timeseries,start_year = start_year,end_year = end_year)), 
            foldername = paste0(folder, longtermfolder), 
            type = "pdf",
            width = 8.5,
            height = 4)

### Annual Stats
##########################
annualfolder <- "3-Annual/"
dir.create(path=paste0(folder,annualfolder), showWarnings = FALSE)

write_results(data = calc_annual_stats(timeseries,start_year = start_year,end_year = end_year), 
              file = paste0(folder, annualfolder, "annual_stats.xlsx"))
write_results(data = calc_annual_cumulative_stats(timeseries,start_year = start_year,end_year = end_year), 
              file = paste0(folder, annualfolder, "annual_cumulative_flows.xlsx"))
write_results(data = calc_annual_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, use_yield = T), 
              file = paste0(folder, annualfolder, "annual_yield.xlsx"))
write_results(data = calc_annual_flow_timing(timeseries,start_year = start_year,end_year = end_year),# DAYS OUTSIDE NORMAL, 
              file = paste0(folder, annualfolder, "annual_flow_timing.xlsx"))
write_results(data = calc_annual_outside_normal(timeseries,start_year = start_year,end_year = end_year), 
              file = paste0(folder, annualfolder, "annual_normal_days.xlsx"))
write_results(data = calc_annual_lowflows(timeseries,start_year = start_year,end_year = end_year), 
              file = paste0(folder, annualfolder, "annual_lowflows.xlsx"))

write_plots(plots = c(plot_annual_stats(timeseries, start_year = start_year, end_year = end_year),
                      plot_annual_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, incl_seasons = T),
                      plot_annual_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, incl_seasons = T, use_yield = T),
                      plot_annual_flow_timing(timeseries, start_year = start_year, end_year = end_year),
                      plot_annual_outside_normal(timeseries, start_year = start_year, end_year = end_year),
                      plot_annual_lowflows(timeseries, start_year = start_year, end_year = end_year)
                      ), 
            foldername = paste0(folder, annualfolder), 
            type = "pdf",
            width = 8.5,
            height = 4)

### Monthly Stats
##########################
monthfolder <- "4-Month/"
dir.create(path=paste0(folder,monthfolder), showWarnings = FALSE)

# MONTHLY STATS
write_results(data = calc_monthly_stats(timeseries,start_year = start_year,end_year = end_year), 
              file = paste0(folder, monthfolder, "monthly_stats.xlsx"))
write_results(data = calc_monthly_cumulative_stats(timeseries,start_year = start_year,end_year = end_year), 
              file = paste0(folder, monthfolder, "monthly_cumulative.xlsx"))
write_results(data =  calc_monthly_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, use_yield = T), 
              file = paste0(folder, monthfolder, "monthly_yield.xlsx"))


write_plots(plots = c(plot_monthly_stats(timeseries, start_year = start_year, end_year = end_year),
                      plot_monthly_cumulative_stats(timeseries, start_year = start_year, end_year = end_year),
                      plot_monthly_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, use_yield = T)), 
            foldername = paste0(folder, monthfolder), 
            type = "pdf",
            width = 8.5,
            height = 4)

### Daily Stats
##########################
dailyfolder <- "5-Daily/"
dir.create(path=paste0(folder,dailyfolder), showWarnings = FALSE)

write_results(data = calc_daily_stats(data = timeseries,start_year = start_year,end_year = end_year), 
              file = paste0(folder, dailyfolder, "daily_stats.xlsx"))
write_results(data = calc_daily_cumulative_stats(data=timeseries,start_year = start_year,end_year = end_year), 
              file = paste0(folder, dailyfolder, "daily_cumulative_volume_stats.xlsx"))
write_results(data =  calc_daily_cumulative_stats(data=timeseries,start_year = start_year,end_year = end_year, use_yield = T), 
              file = paste0(folder, dailyfolder, "daily_cumulative_yield_stats.xlsx"))

write_plots(plots = c(plot_daily_stats(data = timeseries,start_year = start_year,end_year = end_year),
                      plot_daily_cumulative_stats(data = timeseries,start_year = start_year,end_year = end_year),
                      plot_daily_cumulative_stats(data = timeseries,start_year = start_year,end_year = end_year, use_yield = T)), 
            foldername = paste0(folder, dailyfolder), 
            type = "pdf",
            width = 8.5,
            height = 4)


### Low Flow Frequency
##########################
# freqfolder <- "6-LowFlowFrequencies/"
# dir.create(path=paste0(folder,freqfolder), showWarnings = FALSE)
# 
# lowflow_results <- fasstr_annual_freq_analysis(flowdata=timeseries,
#                                                #HYDAT = stn_number,
#                                                report_dir = paste0(folder,freqfolder),start_year = start_year,end_year = end_year)
# 
# 
# ### Annual Trending
# ##########################
trendingfolder <- "7-Trending/"
dir.create(path=paste0(folder,trendingfolder), showWarnings = FALSE)


write_results(data = compute_annual_trends(data = timeseries, zyp_method = "yuepilon", start_year = start_year,end_year = end_year), 
              file = paste0(folder, trendingfolder, "annual_trends_results.xlsx"))

write_plots(plots = plot_annual_trends(trends_results = compute_annual_trends(data = timeseries, zyp_method = "yuepilon", start_year = start_year,end_year = end_year),
                                       zyp_alpha = 0.05),
            foldername = paste0(folder, trendingfolder , "Annual_Trends"), 
            type = "pdf",
            width = 8.5,
            height = 4,
            combined_pdf = TRUE)




