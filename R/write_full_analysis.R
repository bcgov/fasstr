# Copyright 2018 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' @title Calculate and write 
#'
#' @description Calculates all statiscs and writes them ina folder
#'    
#' @inheritParams compute_annual_trends
#' @inheritParams plot_annual_trends
#' @param zyp_method Character string identifying the prewhitened trend method to use from 'zyp', either "zhang' or "yuepilon". 
#'    Default \code{"yuepilon"}.
#' @param folder Name of folder to save everything
#' @param table_filetype Excel or csv filetype
#' @param plot_filetype Type of image to save
#' @param sections Section of analysis to run
#' \itemize{
#'  \item{1: Timeseries}
#'  \item{2: Long-term}
#'  \item{3: Annual}
#'  \item{4: Monthly}
#'  \item{5: Daily}
#'  \item{6: Annual Trends}
#'  \item{7: Frequency Analysis}
#'  }
#' 
#' @return Folders upon folders
#'
#' @examples
#' \dontrun{
#' 
#' write_full_analysis(station_number = "08NM116")
#'
#' }
#' @export



write_full_analysis <- function(data = NULL,
                                dates = Date,
                                values = Value,
                                groups = STATION_NUMBER,
                                station_number = NULL,
                                basin_area = NA,
                                water_year = FALSE,
                                water_year_start = 10,
                                start_year = NULL,
                                end_year = NULL,
                                exclude_years = NULL,
                                ignore_missing = FALSE,
                                foldername = NULL,
                                sections = 1:7,
                                table_filetype = "xlsx",
                                plot_filetype = "png",
                                zyp_method = 'yuepilon',
                                zyp_alpha = NA){
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  water_year_checks(water_year, water_year_start)
  years_checks(start_year, end_year, exclude_years)
  ignore_missing_checks(ignore_missing)
  
  if (is.null(foldername))
    stop("A folder name is required with the foldername argument to write all results tables and plots.", call. = FALSE)
  if (!substr(foldername, nchar(foldername), nchar(foldername)) == "/") {
    foldername <- paste0(foldername, "/")
  }
  
  if (!is.numeric(sections)) 
    stop("sections argument must be numbers between 1 and 7. See ?write_full_analysis for analysis section numbers.", call. = FALSE)
  if (!all(sections %in% 1:7)) 
    stop("sections argument must be numbers between 1 and 7. See ?write_full_analysis for analysis section numbers.", call. = FALSE)
  
  if (6 %in% sections) {
    zyp_alpha_checks(zyp_alpha)
    zyp_method_checks(zyp_method)
  }
  
  
  # Do this for now, until looping of include_year plots is sorted out
  if (length(station_number) > 1) stop("Only one station_number can be listed.", call. = FALSE)
  
  # Argument checks
  if (is.null(start_year)) {start_year <- 0}
  if (is.null(end_year)) {end_year <- 3000}
  
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, 
                               station_number = station_number)
  
  # Save the original columns (to check for STATION_NUMBER col at end) and ungroup if necessary
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)),
                               rm_other_cols = TRUE)
  
  
  # Data setup
  flow_data <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
  flow_data <- add_rolling_means(data = flow_data)
  
  
  
  
  
  ### Folders Setup
  ###########################

  # Create the main directory folder
  main_dir <- foldername
  dir.create(path = main_dir, showWarnings = FALSE)
  
  # Create the table of contents and meta data files
  metadata <- data.frame(col1 = c("fasstr Complete Analyses","","Station:","Water Year","","Folder","1-TimeSeries","2-Longterm"),
                         col2 = c("","","STTIONS",water_year,"","","MissingDate.pdf","FLowduration.pdf"))
  table_contents <- data.frame(Folder = c(rep("1-TimeSeries",4),rep("2-Longterm",3)),
                               File = c(rep("MissingDate.pdf",4),rep("FLowduration.pdf",3)),
                               Type = c("table","plot","table","plot","table","plot","table"),
                               Description = c(rep("...",7)))
  write_results(metadata,paste0(main_dir, "fasstr Analysis Metadata.", table_filetype))
  write_results(table_contents,paste0(main_dir, "fasstr Table of Contents.", table_filetype))

  

  
  
  
  ### Time series
  ##########################
  
  if (1 %in% sections) {
    
    # Create the folder
    timeseries_dir <- "1 - Timeseries/"
    dir.create(path = paste0(main_dir, timeseries_dir), showWarnings = FALSE)

    # Write the flow data
    write_flow_data(data = flow_data, 
                    start_year = start_year, end_year = end_year,
                    water_year = water_year, water_year_start = water_year_start,
                    file = paste0(main_dir, timeseries_dir, "Daily_Discharge_Timeseries.", table_filetype))
    
    # Write the data screening
    write_results(data = screen_flow_data(data = flow_data, start_year = start_year, end_year = end_year, 
                                          water_year = water_year, water_year_start = water_year_start), 
                  file = paste0(main_dir, timeseries_dir, "Flow_Screening.", table_filetype))
    
    # Write flow data by year plots
    ts_annual_plot <- plot_flow_data(data = flow_data, 
                                        start_year = start_year,end_year = end_year, exclude_years = exclude_years,
                                        water_year = water_year, water_year_start = water_year_start,
                                        plot_by_year = TRUE)
    invisible(write_plots(plots = ts_annual_plot,
                          foldername = paste0(main_dir, timeseries_dir),
                          type = plot_filetype,
                          width = 14,
                          height = 8.5))

    # Write flow data plot
    ts_full_plot <- plot_flow_data(data = flow_data, 
                                      start_year = start_year,end_year = end_year, exclude_years = exclude_years,
                                      water_year = water_year, water_year_start = water_year_start)
    invisible(write_plots(plots = ts_full_plot,
                          foldername = paste0(main_dir, timeseries_dir),
                          type = plot_filetype,
                          width = 14,
                          height = 5))
    
    # Write screening plots
    ts_screen_plot <- plot_data_screening(data = flow_data, 
                                          start_year = start_year, end_year = end_year, 
                                          water_year = water_year, water_year_start = water_year_start)
    invisible(write_plots(plots = ts_screen_plot,
                          foldername = paste0(main_dir, timeseries_dir),
                          type = plot_filetype,
                          width = 8.5,
                          height = 5))
    
    # Write missing dates plots
    ts_missing_plot <- plot_missing_dates(data = flow_data, 
                                          start_year = start_year, end_year = end_year, 
                                          water_year = water_year, water_year_start = water_year_start)
    invisible(write_plots(plots = ts_missing_plot,
                          foldername = paste0(main_dir, timeseries_dir),
                          type = plot_filetype,
                          width = 8.5,
                          height = 5))
    
    
  }  
  
  
  ### Long-term Stats
  ##########################
  
  if (2 %in% sections) {
    
    # Create the folder
    longterm_dir <- "2 - Long-term/"
    dir.create(path = paste0(main_dir, longterm_dir), showWarnings = FALSE)
    
    # Write the long-term stats with percentiles
    write_results(data = calc_longterm_stats(data = flow_data, 
                                             start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                             water_year = water_year, water_year_start = water_year_start,
                                             percentiles = 1:99, 
                                             transpose = TRUE), 
                  file = paste0(main_dir, longterm_dir, "Longterm_Statistics_and_Percentiles.", table_filetype))
    
    # Write the long-term stats plot
    lt_stats_plot <- plot_longterm_stats(data = flow_data,
                                         start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                         water_year = water_year, water_year_start = water_year_start)
    invisible(write_plots(plots = lt_stats_plot, 
                          foldername = paste0(main_dir, longterm_dir), 
                          type = plot_filetype,
                          width = 11,
                          height = 5))
    
    # Write the flow duration plot
    lt_flowduration_plot <- plot_flow_duration(data = flow_data, 
                                               start_year = start_year, end_year = end_year, exclude_years = exclude_years, 
                                               water_year = water_year, water_year_start = water_year_start)
    
    invisible(write_plots(plots = lt_flowduration_plot, 
                          foldername = paste0(main_dir, longterm_dir), 
                          type = plot_filetype,
                          width = 11,
                          height = 7))
  }
  
  
  ### Annual Stats
  ##########################
  
  if (3 %in% sections) {
    
    # Create the folder
    annual_dir <- "3 - Annual/"
    dir.create(path = paste0(main_dir, annual_dir), showWarnings = FALSE)
    
    # Write each of the annual stats
    write_results(data = calc_annual_stats(data = flow_data, 
                                           start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                           water_year = water_year, water_year_start = water_year_start), 
                  file = paste0(main_dir, annual_dir, "Annual_Statistics.", table_filetype))
    write_results(data = calc_annual_cumulative_stats(data = flow_data, 
                                                      start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                      water_year = water_year, water_year_start = water_year_start,
                                                      incl_seasons = TRUE), 
                  file = paste0(main_dir, annual_dir, "Annual_Cumulative_Volumes.", table_filetype))
    write_results(data = calc_annual_cumulative_stats(data = flow_data, 
                                                      start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                      water_year = water_year, water_year_start = water_year_start,
                                                      include_seasons = TRUE, use_yield = TRUE),
                  digits = 1,
                  file = paste0(main_dir, annual_dir, "Annual_Cumualtive_Yield.", table_filetype))
    write_results(data = calc_annual_flow_timing(data = flow_data, 
                                                 start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                 water_year = water_year, water_year_start = water_year_start),# DAYS OUTSIDE NORMAL, 
                  file = paste0(main_dir, annual_dir, "Annual_Flow_Timing.xlsx"))
    write_results(data = calc_annual_outside_normal(data = flow_data, 
                                                    start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                    water_year = water_year, water_year_start = water_year_start), 
                  file = paste0(main_dir, annual_dir, "Annual_Days_Outside_Normal.", table_filetype))
    write_results(data = calc_annual_lowflows(data = flow_data, 
                                              start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                              water_year = water_year, water_year_start = water_year_start), 
                  file = paste0(main_dir, annual_dir, "Annual_Lowflows.", table_filetype))
    
    
    # Write each of the annual stats plots
    ann_stat_plot <- plot_annual_stats(data = flow_data, 
                                       start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                       water_year = water_year, water_year_start = water_year_start)
    ann_vol_plot <- plot_annual_cumulative_stats(data = flow_data, 
                                                 start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                 water_year = water_year, water_year_start = water_year_start,
                                                 incl_seasons = TRUE)
    ann_yield_plot <- plot_annual_cumulative_stats(data = flow_data, 
                                                   start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                   water_year = water_year, water_year_start = water_year_start,
                                                   incl_seasons = TRUE, use_yield = TRUE)
    ann_timing_plot <- plot_annual_flow_timing(data = flow_data, 
                                               start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                               water_year = water_year, water_year_start = water_year_start)
    ann_norm_plot <- plot_annual_outside_normal(data = flow_data, 
                                                start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                water_year = water_year, water_year_start = water_year_start)
    ann_lowflow_plot <- plot_annual_lowflows(data = flow_data, 
                                             start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                             water_year = water_year, water_year_start = water_year_start)
    
    
    invisible(write_plots(plots = c(ann_stat_plot, ann_vol_plot, ann_yield_plot, ann_timing_plot, ann_norm_plot, ann_lowflow_plot), 
                          foldername = paste0(main_dir, annual_dir), 
                          type = plot_filetype,
                          width = 10,
                          height = 5.5))
  }
  
  
  ### Monthly Stats
  ##########################
  
  if (4 %in% sections) {
    
    # Create the folder
    month_dir <- "4 - Monthly/"
    dir.create(path = paste0(main_dir, month_dir), showWarnings = FALSE)
    
    # Write all the monthly stats
    write_results(data = calc_monthly_stats(data = flow_data, 
                                            start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                            water_year = water_year, water_year_start = water_year_start), 
                  file = paste0(main_dir, month_dir, "Monthly_Statistics.", table_filetype))
    write_results(data = calc_monthly_cumulative_stats(data = flow_data, 
                                                       start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                       water_year = water_year, water_year_start = water_year_start), 
                  file = paste0(main_dir, month_dir, "Monthly_Cumulative_Volumes.", table_filetype))
    write_results(data =  calc_monthly_cumulative_stats(data = flow_data, 
                                                        start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                        water_year = water_year, water_year_start = water_year_start,
                                                        use_yield = TRUE), 
                  digits = 1,
                  file = paste0(main_dir, month_dir, "Monthly_Cumulative_Yield.", table_filetype))
    
    
    # Write the monthly stats plots
    mon_stat_plot <- plot_monthly_stats(data = flow_data, 
                                        start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                        water_year = water_year, water_year_start = water_year_start)
    invisible(write_plots(plots = mon_stat_plot, 
                          foldername = paste0(main_dir, month_dir), 
                          type = plot_filetype,
                          width = 11,
                          height = 5))
    
    # Write the monthly cumulative plots
    mon_vol_plot <- plot_monthly_cumulative_stats(data = flow_data, 
                                                  start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                  water_year = water_year, water_year_start = water_year_start)
    mon_yield_plot <- plot_monthly_cumulative_stats(data = flow_data, 
                                                    start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                    water_year = water_year, water_year_start = water_year_start,
                                                    use_yield = TRUE)
    invisible(write_plots(plots = c(mon_vol_plot, mon_yield_plot), 
                          foldername = paste0(main_dir, month_dir), 
                          type = plot_filetype,
                          width = 8.5,
                          height = 4))
    
    
    # Write the monthly cumulative volumetric plots with years
    mon_vol_years_plot <- list()
    for(year in seq(from = start_year, to = end_year, by = 1)[!(seq(from = start_year, to = end_year, by = 1) %in% exclude_years)]) {
      plot <- suppressMessages(plot_monthly_cumulative_stats(data = flow_data,
                                                             start_year = start_year,
                                                             end_year = end_year,
                                                             water_year = water_year,
                                                             water_year_start = water_year_start,
                                                             exclude_years = exclude_years,
                                                             include_year = year))
      mon_vol_years_plot[[ paste0(names(plot), "_", year) ]] <- suppressMessages(plot$Monthly_Cumulative_Volumetric_Stats)

    }
    write_plots(plots = mon_vol_years_plot, 
                foldername = paste0(main_dir, month_dir, "Monthly_Cumulative_Volumetric_Stats_with_Years"),
                type = plot_filetype,
                width = 8.5,
                height = 4,
                combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))

    # Write the monthly cumulative yield plots with years
    mon_yield_years_plot <- list()
    for(year in seq(from = start_year, to = end_year, by = 1)[!(seq(from = start_year, to = end_year, by = 1) %in% exclude_years)]) {
      plot <- suppressMessages(plot_monthly_cumulative_stats(data = flow_data,
                                                             start_year = start_year,
                                                             end_year = end_year,
                                                             water_year = water_year,
                                                             water_year_start = water_year_start,
                                                             exclude_years = exclude_years,
                                                             include_year = year,
                                                             use_yield = TRUE))
      mon_yield_years_plot[[ paste0(names(plot), "_", year) ]] <- suppressMessages(plot$Monthly_Cumulative_Yield_Stats)

    }
    write_plots(plots = mon_yield_years_plot, 
                foldername = paste0(main_dir, month_dir, "Monthly_Cumulative_Yield_Stats_with_Years"),
                type = plot_filetype,
                width = 8.5,
                height = 4,
                combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
  }
  
  
  ### Daily Stats
  ##########################
  
  if (5 %in% sections) {
    
    # Create the folder
    daily_dir <- "5 - Daily/"
    dir.create(path = paste0(main_dir, daily_dir), showWarnings = FALSE)
    
    # Write all daily stats
    write_results(data = calc_daily_stats(data = flow_data, 
                                          start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                          water_year = water_year, water_year_start = water_year_start), 
                  file = paste0(main_dir, daily_dir, "Daily_Statistics.", table_filetype))
    write_results(data = calc_daily_cumulative_stats(data = flow_data, 
                                                     start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                     water_year = water_year, water_year_start = water_year_start), 
                  file = paste0(main_dir, daily_dir, "Daily_Cumulative_Volumes.", table_filetype))
    write_results(data =  calc_daily_cumulative_stats(data = flow_data, 
                                                      start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                      water_year = water_year, water_year_start = water_year_start,
                                                      use_yield = TRUE), 
                  digits = 1,
                  file = paste0(main_dir, daily_dir, "Daily_Cumulative_Yield.", table_filetype))
    
    
    
    # Write the daily stats plots
    day_stat_plot <- plot_daily_stats(data = flow_data, 
                                      start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                      water_year = water_year, water_year_start = water_year_start)
    day_vol_plot <- plot_daily_cumulative_stats(data = flow_data, 
                                                start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                water_year = water_year, water_year_start = water_year_start)
    day_yield_plot <- plot_daily_cumulative_stats(data = flow_data, 
                                                  start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                                  water_year = water_year, water_year_start = water_year_start)
    
    # Write the daily cumulative plots
    invisible(write_plots(plots = c(day_stat_plot, day_vol_plot, day_yield_plot), 
                          foldername = paste0(main_dir, daily_dir), 
                          type = plot_filetype,
                          width = 8.5,
                          height = 4))
    
    # Write the daily statistics plots with years
    day_stats_year_plots <- list()
    for(year in seq(from = start_year, to = end_year, by = 1)[!(seq(from = start_year, to = end_year, by = 1) %in% exclude_years)]) {
      plot <- suppressMessages(plot_daily_stats(data = flow_data,
                                                start_year = start_year,
                                                end_year = end_year,
                                                water_year = water_year,
                                                water_year_start = water_year_start,
                                                exclude_years = exclude_years,
                                                include_year = year))
      day_stats_year_plots[[ paste0(names(plot), "_", year) ]] <- suppressMessages(plot$Daily_Stats)
      
    }
    write_plots(plots = day_stats_year_plots, foldername = paste0(main_dir, daily_dir, "Daily_Statistics_with_Years"), 
                type = plot_filetype,
                width = 8.5,
                height = 4,
                combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
    
    # Write the daily cumulative volumetric plots with years
    day_vol_year_plots <- list()
    for(year in seq(from = start_year, to = end_year, by = 1)[!(seq(from = start_year, to = end_year, by = 1) %in% exclude_years)]) {
      plot <- suppressMessages(plot_daily_cumulative_stats(data = flow_data,
                                                           start_year = start_year,
                                                           end_year = end_year,
                                                           water_year = water_year,
                                                           water_year_start = water_year_start,
                                                           exclude_years = exclude_years,
                                                           include_year = year))
      day_vol_year_plots[[ paste0(names(plot), "_", year) ]] <- suppressMessages(plot$Daily_Cumulative_Volumetric_Stats)
      
    }
    write_plots(plots = day_vol_year_plots, foldername = paste0(main_dir, daily_dir, "Daily_Cumulative_Volumes_with_Years"), 
                type = plot_filetype,
                width = 8.5,
                height = 4,
                combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
    
    # Write the daily cumulative yield plots with years
    day_yield_year_plots <- list()
    for(year in seq(from = start_year, to = end_year, by = 1)[!(seq(from = start_year, to = end_year, by = 1) %in% exclude_years)]) {
      plot <- suppressMessages(plot_daily_cumulative_stats(data = flow_data,
                                                           start_year = start_year,
                                                           end_year = end_year,
                                                           water_year = water_year,
                                                           water_year_start = water_year_start,
                                                           exclude_years = exclude_years,
                                                           include_year = year,
                                                           use_yield = TRUE))
      day_yield_year_plots[[ paste0(names(plot), "_", year) ]] <- suppressMessages(plot$Daily_Cumulative_Yield_Stats)
      
    }
    write_plots(plots = day_yield_year_plots, foldername = paste0(main_dir, daily_dir, "Daily_Cumulative_Yield_with_Years"), 
                type = plot_filetype,
                width = 8.5,
                height = 4,
                combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
    
  }
  
  
  # ### Annual Trending
  # ##########################
  
  if (6 %in% sections) {
    
    # Create the folder
    trending_dir <- "6 - Trending/"
    dir.create(path = paste0(main_dir, trending_dir), showWarnings = FALSE)
    
    # Write the trends results
    ann_trends <- compute_annual_trends(data = flow_data, 
                                        start_year = start_year,end_year = end_year, exclude_years = exclude_years, 
                                        water_year = water_year, water_year_start = water_year_start, 
                                        zyp_method = "yuepilon")
    write_results(data = ann_trends, 
                  file = paste0(main_dir, trending_dir, "Annual_Trends_Results.", table_filetype))
    
    # Write the trends plots
    ann_trends_plots <-  plot_annual_trends(trends_results = ann_trends,
                                            zyp_alpha = 0.05)
    invisible(write_plots(plots = ann_trends_plots,
                          foldername = paste0(main_dir, trending_dir , "Annual_Trends_Results"), 
                          type = plot_filetype,
                          width = 8.5,
                          height = 3,
                          combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE)))
  }
  
  
  ### Low Flow Frequency
  ##########################
  
  #if (7 %in% sections) {
  #  
  # Create the folder
  # freq_dir <- "7 - LowFlowFrequencies/"
  # dir.create(path = paste0(main_dir, freq_dir), showWarnings = FALSE)
  # 
  # lowflow_results <- fasstr_annual_freq_analysis(flowdata=flow_data,
  #                                                #HYDAT = stn_number,
  #                                                report_dir = paste0(main_dir,freq_dir),start_year = start_year,end_year = end_year)
  # }
  # 
  
  
  
  message(paste0("calc_everything() analysis complete. Go to ", main_dir, " folder for results."))
  
}



