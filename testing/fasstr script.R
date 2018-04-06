
#devtools::document()
#install.packages("/Users/jongoetz/Documents/R/fasstr",repos = NULL, type = "source")
#install.packages("C:/Users/jgoetz/R/fasstr",repos = NULL, type = "source")


# Writes all data and plots


library(fasstr)


data = timeseries
start_year = 1980
end_year = 2010
water_year = FALSE
water_year_start = 9
exclude_years = 1990:1993
include_year = 2000

# 
# folder <- "testing/MissionCreek/"
# # Parameters
# stn_number <- "08NM116"
# start_year = 1981 #NULL
# end_year = 2000 #NULL



calc_everything("08HB048", 
                water_year = TRUE, 
                start_year=1980, 
                end_year=2000, 
                exclude_years = c(1995:1997, 1999),
                folder="testing/Carnation/")


calc_everything <- function(station_number = NULL,
                            water_year = FALSE,
                            water_year_start = 10,
                            start_year = NULL,
                            end_year = NULL,
                            exclude_years = NULL,
                            folder = "everything_folder"){

  # Argument checks
  if (is.null(start_year)) {start_year <- 0}
  if (is.null(end_year)) {end_year <- 3000}
  
  
  # Create the folder
  dir.create(path = folder, showWarnings = FALSE)
  
  # Create the table of contents and meta data files
  metadata <- data.frame(col1 = c("fasstr Complete Analyses","","Station:","Water Year","","Folder","1-TimeSeries","2-Longterm"),
                         col2 = c("","",paste(station_number),water_year,"","","MissingDate.pdf","FLowduration.pdf"))
  table_contents <- data.frame(Folder = c(rep("1-TimeSeries",4),rep("2-Longterm",3)),
                               File = c(rep("MissingDate.pdf",4),rep("FLowduration.pdf",3)),
                               Type = c("table","plot","table","plot","table","plot","table"),
                               Description = c(rep("...",7)))
  write_results(metadata,paste0(folder, "fasstr Analysis Metadata.xlsx"))
  write_results(table_contents,paste0(folder, "fasstr Table of Contents.xlsx"))
  rm(metadata)
  rm(table_contents)
  
  
  ### Time series
  ##########################
  timeseriesfolder <- "1-TimeSeries/"
  dir.create(path=paste0(folder,timeseriesfolder), showWarnings = FALSE)
  
  timeseries <- fill_missing_dates(station_number = station_number)
  timeseries <- add_date_variables(data = timeseries)
  timeseries <- add_rolling_means(data = timeseries)
  
  
  write_flow_data(data = timeseries, 
                  file = paste0(folder, timeseriesfolder, "Daily_Flows.xlsx"))
  
  write_results(data = screen_flow_data(data = timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = water_year_start), 
                file = paste0(folder, timeseriesfolder, "Flow_Summary.xlsx"))
  
  invisible(write_plots(plots = c(plot_flow_data(data = timeseries, start_year = start_year,end_year = end_year,plot_by_year = TRUE, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_flow_data(data = timeseries, start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_data_screening(data = timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = water_year_start),
                        plot_missing_dates(data = timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = water_year_start)),
              foldername = paste0(folder, timeseriesfolder),
              type = "pdf",
              width = 8.5,
              height = 4))
  
  
  ### Long-term Stats
  ##########################
  longtermfolder <- "2-Longterm/"
  dir.create(path=paste0(folder,longtermfolder), showWarnings = FALSE)
  
  write_results(data = calc_longterm_stats(timeseries, start_year = start_year,end_year = end_year, percentiles = 1:99, transpose = T, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, longtermfolder, "Longterm_Stats.xlsx"))
  
  invisible( write_plots(plots = c(plot_longterm_stats(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_flow_duration(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years)), 
              foldername = paste0(folder, longtermfolder), 
              type = "pdf",
              width = 8.5,
              height = 4))
  
  ### Annual Stats
  ##########################
  annualfolder <- "3-Annual/"
  dir.create(path=paste0(folder,annualfolder), showWarnings = FALSE)
  
  write_results(data = calc_annual_stats(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, annualfolder, "Annual_Stats.xlsx"))
  write_results(data = calc_annual_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, annualfolder, "Annual_Cumulative_Volumes.xlsx"))
  write_results(data = calc_annual_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, use_yield = T, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, annualfolder, "Annual_Cumualtive_Yield.xlsx"))
  write_results(data = calc_annual_flow_timing(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),# DAYS OUTSIDE NORMAL, 
                file = paste0(folder, annualfolder, "Annual_Flow_Timing.xlsx"))
  write_results(data = calc_annual_outside_normal(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, annualfolder, "Annual_Days_Outside_Normal.xlsx"))
  write_results(data = calc_annual_lowflows(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, annualfolder, "Annual_Lowflows.xlsx"))
  
  invisible(write_plots(plots = c(plot_annual_stats(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_annual_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, incl_seasons = T, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_annual_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, incl_seasons = T, use_yield = T, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_annual_flow_timing(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_annual_outside_normal(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_annual_lowflows(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years)
  ), 
  foldername = paste0(folder, annualfolder), 
  type = "pdf",
  width = 8.5,
  height = 4))
  
  ### Monthly Stats
  ##########################
  monthfolder <- "4-Month/"
  dir.create(path=paste0(folder,monthfolder), showWarnings = FALSE)
  
  # MONTHLY STATS
  write_results(data = calc_monthly_stats(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, monthfolder, "Monthly_Stats.xlsx"))
  write_results(data = calc_monthly_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, monthfolder, "Monthly_Cumulative_Volumes.xlsx"))
  write_results(data =  calc_monthly_cumulative_stats(timeseries,start_year = start_year,end_year = end_year, use_yield = T, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, monthfolder, "Monthly_Cumulative_Yield.xlsx"))
  
  
  invisible(write_plots(plots = c(plot_monthly_stats(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_monthly_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_monthly_cumulative_stats(timeseries, start_year = start_year, end_year = end_year, use_yield = T, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years)), 
              foldername = paste0(folder, monthfolder), 
              type = "pdf",
              width = 8.5,
              height = 4))
  
  ### Daily Stats
  ##########################
  dailyfolder <- "5-Daily/"
  dir.create(path=paste0(folder,dailyfolder), showWarnings = FALSE)
  
  write_results(data = calc_daily_stats(data = timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, dailyfolder, "Daily_Stats.xlsx"))
  write_results(data = calc_daily_cumulative_stats(data=timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, dailyfolder, "Daily_Cumulative_Volumes.xlsx"))
  write_results(data =  calc_daily_cumulative_stats(data=timeseries,start_year = start_year,end_year = end_year, use_yield = T, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, dailyfolder, "Daily_Cumulative_Yield.xlsx"))
  
  invisible(write_plots(plots = c(plot_daily_stats(data = timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_daily_cumulative_stats(data = timeseries,start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                        plot_daily_cumulative_stats(data = timeseries,start_year = start_year,end_year = end_year, use_yield = T, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years)), 
              foldername = paste0(folder, dailyfolder), 
              type = "pdf",
              width = 8.5,
              height = 4))
  
 ### LOOP THROUGH ALL YEARS AND PLOT
 
  plots_list <- list()
  for(year in seq(from = start_year, to = end_year, by = 1)[!(seq(from = start_year, to = end_year, by = 1) %in% exclude_years)]) {
    plot <- plot_daily_stats(data = timeseries,
                             start_year = start_year,
                             end_year = end_year,
                             water_year = water_year,
                             water_year_start = water_year_start,
                             exclude_years = exclude_years,
                             include_year = year)
    plots_list[[ paste0(names(plot), "_", year) ]] <- plot$Daily_Stats
    
  }
  write_plots(plots = plots_list, foldername = paste0(folder, dailyfolder, "Daily_Flows"), 
              type = "pdf",
              width = 8.5,
              height = 4,
              combined_pdf = TRUE)
  rm(plots_list)  
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
  
  
  write_results(data = compute_annual_trends(data = timeseries, zyp_method = "yuepilon", start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years), 
                file = paste0(folder, trendingfolder, "Annual_Trends_Results.xlsx"))
  
  invisible(write_plots(plots = plot_annual_trends(trends_results = compute_annual_trends(data = timeseries, zyp_method = "yuepilon", start_year = start_year,end_year = end_year, water_year = water_year, water_year_start = water_year_start, exclude_years = exclude_years),
                                         zyp_alpha = 0.05),
              foldername = paste0(folder, trendingfolder , "Annual_Trends_Results"), 
              type = "pdf",
              width = 8.5,
              height = 4,
              combined_pdf = TRUE))
  
  
  ### remove anything created!!!
  rm(timeseries)
  
  
  
  message("Huzzah! Go look at your data")
}
  
  
  
  