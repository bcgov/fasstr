
#devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr",repos = NULL, type = "source")


# Writes all data and plots


library(fasstr)


folder <- "testing/MissionCreek/"
# Parameters
stn_number <- "08NM116"
start_year = 1981 #NULL
end_year = 2000 #NULL



calc_everything("08HB048", water_year = TRUE, start_year=1980, end_year=2000, exclude_years = c(1995:1997, 1999),folder="testing/Carnation/")


calc_everything <- function(station_number = NULL,
                            water_year = FALSE,
                            Water_year_start = 10,
                            start_year = NULL,
                            end_year = NULL,
                            exclude_years = NULL,
                            folder = "everything_folder"){

  if (is.null(start_year)) {start_year <- 0}
  if (is.null(end_year)) {end_year <- 3000}
  
  
  dir.create(path = folder, showWarnings = FALSE)
  
  
  ### Time series
  ##########################
  timeseriesfolder <- "1-TimeSeries/"
  dir.create(path=paste0(folder,timeseriesfolder), showWarnings = FALSE)
  
  timeseries <- fill_missing_dates(station_number = station_number)
  timeseries <- add_date_variables(data = timeseries)
  timeseries <- add_rolling_means(data = timeseries)
  
  
  write_flow_data(data = timeseries, 
                  file = paste0(folder, timeseriesfolder, "daily_record.xlsx"))
  
  write_results(data = screen_flow_data(data = timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = Water_year_start), 
                file = paste0(folder, timeseriesfolder, "flow_summary.xlsx"))
  
  write_plots(plots = c(plot_flow_data(data = timeseries, start_year = start_year,end_year = end_year,plot_by_year = TRUE, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_flow_data(data = timeseries, start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_data_screening(data = timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = Water_year_start),
                        plot_missing_dates(data = timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = Water_year_start)),
              foldername = paste0(folder, timeseriesfolder),
              type = "pdf",
              width = 8.5,
              height = 4)
  
  
  ### Long-term Stats
  ##########################
  longtermfolder <- "2-Longterm/"
  dir.create(path=paste0(folder,longtermfolder), showWarnings = FALSE)
  
  write_results(data = calc_longterm_stats(timeseries, start_year = start_year,end_year = end_year, percentiles = 1:99, transpose = T, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, longtermfolder, "longterm_stats.xlsx"))
  
  write_plots(plots = c(plot_longterm_stats(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_flow_duration(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years)), 
              foldername = paste0(folder, longtermfolder), 
              type = "pdf",
              width = 8.5,
              height = 4)
  
  ### Annual Stats
  ##########################
  annualfolder <- "3-Annual/"
  dir.create(path=paste0(folder,annualfolder), showWarnings = FALSE)
  
  write_results(data = calc_annual_stats(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, annualfolder, "annual_stats.xlsx"))
  write_results(data = calc_annual_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, annualfolder, "annual_cumulative_flows.xlsx"))
  write_results(data = calc_annual_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, use_yield = T, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, annualfolder, "annual_yield.xlsx"))
  write_results(data = calc_annual_flow_timing(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),# DAYS OUTSIDE NORMAL, 
                file = paste0(folder, annualfolder, "annual_flow_timing.xlsx"))
  write_results(data = calc_annual_outside_normal(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, annualfolder, "annual_normal_days.xlsx"))
  write_results(data = calc_annual_lowflows(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, annualfolder, "annual_lowflows.xlsx"))
  
  write_plots(plots = c(plot_annual_stats(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_annual_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, incl_seasons = T, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_annual_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, incl_seasons = T, use_yield = T, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_annual_flow_timing(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_annual_outside_normal(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_annual_lowflows(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years)
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
  write_results(data = calc_monthly_stats(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, monthfolder, "monthly_stats.xlsx"))
  write_results(data = calc_monthly_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, monthfolder, "monthly_cumulative.xlsx"))
  write_results(data =  calc_monthly_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, use_yield = T, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, monthfolder, "monthly_yield.xlsx"))
  
  
  write_plots(plots = c(plot_monthly_stats(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_monthly_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_monthly_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, use_yield = T, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years)), 
              foldername = paste0(folder, monthfolder), 
              type = "pdf",
              width = 8.5,
              height = 4)
  
  ### Daily Stats
  ##########################
  dailyfolder <- "5-Daily/"
  dir.create(path=paste0(folder,dailyfolder), showWarnings = FALSE)
  
  write_results(data = calc_daily_stats(data = timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, dailyfolder, "daily_stats.xlsx"))
  write_results(data = calc_daily_cumulative_stats(data=timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, dailyfolder, "daily_cumulative_volume_stats.xlsx"))
  write_results(data =  calc_daily_cumulative_stats(data=timeseries,start_year = start_year,end_year = end_year, use_yield = T, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, dailyfolder, "daily_cumulative_yield_stats.xlsx"))
  
  write_plots(plots = c(plot_daily_stats(data = timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_daily_cumulative_stats(data = timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                        plot_daily_cumulative_stats(data = timeseries,start_year = start_year,end_year = end_year, use_yield = T, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years)), 
              foldername = paste0(folder, dailyfolder), 
              type = "pdf",
              width = 8.5,
              height = 4)
  
 ### LOOP THROUGH ALL YEARS AND PLOT
 
#   write_plots(plots = c( 
#     for(year in seq(from = start_year, to = end_year, by = 1)[!(seq(from = start_year, to = end_year, by = 1) %in% exclude_years)]) {
#       plot_daily_stats(data = timeseries,  
#                        start_year = start_year ,
#                        end_year = end_year, 
#                        water_year = water_year, 
#                        water_year_start = water_year_start, 
#                        exclude_years = exclude_years, 
#                        include_year = year)
#     }
#   ), 
#   foldername = paste0(folder, dailyfolder), 
#   type = "pdf",
#   width = 8.5,
#   height = 4,
#   combined_pdf = T)
# }
    
  
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
  
  
  write_results(data = compute_annual_trends(data = timeseries, zyp_method = "yuepilon", start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, trendingfolder, "annual_trends_results.xlsx"))
  
  write_plots(plots = plot_annual_trends(trends_results = compute_annual_trends(data = timeseries, zyp_method = "yuepilon", start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = Water_year_start, exclude_years = exclude_years),
                                         zyp_alpha = 0.05),
              foldername = paste0(folder, trendingfolder , "Annual_Trends"), 
              type = "pdf",
              width = 8.5,
              height = 4,
              combined_pdf = TRUE)
  
  
  ### remove anything created!!!
  rm(timeseries)
  
  
  
  print("Huzzah! Go look at your data")
}
  
  
  
  