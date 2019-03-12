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

#' @title Computes a suite of tables and plots from various fasstr functions
#'
#' @description Calculates tables and plots of a suite of statistics from various fasstr functions. The statistics are grouped into
#'    7 sections (see below) which are stored in lists in the created object. Due to the number of tables and plots to be made, this 
#'    function may take several minutes to complete. Can also directly write all plots and tables directly into a directory
#'    ('foldername' argument) using the 'write_to_dir' argument.
#'    If using \code{ignore_missing = FALSE} (default) and there is missing data, some tables and plots may be empty
#'    and produce warnings. Use \code{ignore_missing = TRUE} to ignore the missing values or filter your data to complete years.
#'    
#' @inheritParams compute_annual_trends
#' @param sections Sections of analysis to run (default is all (\code{1:7})):
#' \itemize{
#'  \item{1: Screening}
#'  \item{2: Long-term}
#'  \item{3: Annual}
#'  \item{4: Monthly}
#'  \item{5: Daily}
#'  \item{6: Annual Trends}
#'  \item{7: Low-flow Frequencies}
#'  }
#' @param zyp_method Character string identifying the prewhitened trend method to use from 'zyp', either "zhang' or "yuepilon". 
#'    Only required if section 7 is included. Default \code{"yuepilon"}.
#' @param write_to_dir Logical value indicating if all results are to also be written into  a directory. Default \code{FALSE}.
#' @param foldername Name of folder to create on disk (if it does not exist) to create all folders and save tables and plots. 
#' @param table_filetype Table type to write. One of "csv", "xls", or "xlsx". Default \code{"xlsx"}.
#' @param plot_filetype Image type to write. One of "png", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "bmp", or "svg".
#'    Deafult \code{"png"}.
#' 
#' @return A list of lists of tibble data frames and ggplot2 objects from various fasstr functions
#'    organized by the sections as listed above.
#'
#' @examples
#' \dontrun{
#' 
#' compute_full_analysis(station_number = "08NM116")
#'
#' }
#' @export



compute_full_analysis <- function(data = NULL,
                                  dates = Date,
                                  values = Value,
                                  groups = STATION_NUMBER,
                                  station_number = NULL,
                                  sections = 1:7,
                                  basin_area = NA,
                                  water_year_start = 1,
                                  start_year = 0,
                                  end_year = 3000,
                                  exclude_years = NULL,
                                  ignore_missing = FALSE,
                                  zyp_method = 'yuepilon',
                                  zyp_alpha = NA,
                                  write_to_dir = FALSE,
                                  foldername = NULL,
                                  table_filetype = "xlsx",
                                  plot_filetype = "png"){
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  ignore_missing_checks(ignore_missing)
  
  if (write_to_dir) {
    if (is.null(foldername))
      stop("A folder name is required with the foldername argument to write all results tables and plots.", call. = FALSE)
    if (!substr(foldername, nchar(foldername), nchar(foldername)) == "/") {
      foldername <- paste0(foldername, "/")
    }
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
  
  if (all(flow_data$STATION_NUMBER == "XXXXXXX")) {
    flow_data <- dplyr::select(flow_data, -STATION_NUMBER)
  }
  
  # Data setup
  flow_data <- fill_missing_dates(data = flow_data, water_year_start = water_year_start)
  flow_data <- add_date_variables(data = flow_data, water_year_start = water_year_start)
  flow_data <- add_rolling_means(data = flow_data)
  
  # Set up basin_area
  flow_data <- add_basin_area(flow_data, basin_area = basin_area)
  basin_area <- unique(flow_data$Basin_Area_sqkm)[1]
  
  
  # Get the start and end years of the data to make a list of all included years
  flow_data <- analysis_prep(data = flow_data,
                             water_year_start = water_year_start)
  if (start_year < min(flow_data$WaterYear)) {
    start_year <- min(flow_data$WaterYear)
  }
  if (end_year > max(flow_data$WaterYear)) {
    end_year <- max(flow_data$WaterYear)
  }
  years_list <- seq(from = start_year, to = end_year, by = 1)[!(seq(from = start_year, to = end_year, by = 1) %in% exclude_years)]
  
  flow_data_plus <- dplyr::filter(flow_data, WaterYear >= start_year-1 & WaterYear <= end_year+1)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  
  
  ### Folders Setup
  ##########################
  
  # Create the main directory folder
  if (write_to_dir) {
    main_dir <- foldername
    dir.create(path = main_dir, showWarnings = FALSE)
    
    # Create the excel document
    output_excel <- openxlsx::createWorkbook()
    
    openxlsx::addWorksheet(wb = output_excel, 
                           sheetName = "Analysis Information",
                           tabColour = "#003e1f") # 73fbd3 44e5e7 59d2fe 4a8fe7 5c7aff
    
    
    
    add_table <- function(wb, sheet, data, title, col, row) {
      openxlsx::writeData(wb = wb,
                          sheet = sheet, 
                          x = title, 
                          startCol = col, startRow = row)
      openxlsx::writeData(wb = wb,
                          sheet = sheet, 
                          x = data, 
                          startCol = col, startRow = row + 1,
                          headerStyle = openxlsx::createStyle(fontSize = 11,
                                                              textDecoration = "bold",
                                                              border = "TopBottom",
                                                              fgFill = "#add8e6",
                                                              halign = "left"))
      openxlsx::addStyle(wb = wb,
                         sheet = sheet, 
                         cols =  col, 
                         rows =  row,
                         style = openxlsx::createStyle(fontSize = 11,
                                                       textDecoration = "bold")) 
    }
    
    add_plot <- function(wb, sheet, plot, title, col, row, height, width) {
      openxlsx::writeData(wb = wb,
                          sheet = sheet, 
                          x = title, 
                          startCol = col, startRow = row)
      print(plot)
      openxlsx::insertPlot(wb = wb,
                           sheet = sheet, 
                           startCol = col, startRow = row + 1,
                           height = height, 
                           width = width)
      openxlsx::addStyle(wb = wb,
                         sheet = sheet, 
                         cols =  col, 
                         rows =  row,
                         style = openxlsx::createStyle(fontSize = 11,
                                                       textDecoration = "bold")) 
    }
    
    
  }  
  
  # Create list of all objects
  
  all_objects <- list()
  
  
  
  ## Time series
  ##########################
  
  if (1 %in% sections) {
    
    # Write the data screening
    flow_screening = screen_flow_data(data = flow_data,
                                      water_year_start = water_year_start)
    
    # Write flow data plot
    ts_full_plot <- plot_flow_data(data = flow_data,
                                   exclude_years = exclude_years,
                                   water_year_start = water_year_start)
    
    # Write flow data by year plots
    ts_annual_plot <- plot_flow_data(data = flow_data,
                                     exclude_years = exclude_years,
                                     water_year_start = water_year_start,
                                     plot_by_year = TRUE)
    
    # Write screening plots
    ts_screen_plot <- plot_data_screening(data = flow_data,
                                          start_year = start_year, end_year = end_year,
                                          water_year_start = water_year_start)
    
    # Write missing dates plots
    ts_missing_plot <- plot_missing_dates(data = flow_data,
                                          start_year = start_year, end_year = end_year,
                                          water_year_start = water_year_start)
    
    
    all_objects <- append(all_objects,
                          list("Screening" = list("Daily_Flows" = flow_data,
                                                  "Daily_Flows_Plot" = ts_full_plot,
                                                  "Daily_Flows_by_Year_Plot" = ts_annual_plot,
                                                  "Flow_Screening" = flow_screening,
                                                  "Flow_Screening_Plot" = ts_screen_plot,
                                                  "Missing_Dates_Plot" = ts_missing_plot)))
    
    # Create the folder
    if (write_to_dir) {
      
      # timeseries_dir <- "1 - Screening/"
      # dir.create(path = paste0(main_dir, timeseries_dir), showWarnings = FALSE)
      #
      # Write the flow data
      # write_flow_data(data = flow_data,
      #                 water_year_start = water_year_start,
      #                 file = paste0(main_dir, timeseries_dir, "Daily_Flows.", table_filetype))
      # 
      # write_results(data = flow_screening,
      #               file = paste0(main_dir, timeseries_dir, "Flow_Screening.", table_filetype))
      # 
      # invisible(write_plots(plots = ts_full_plot,
      #                       foldername = paste0(main_dir, timeseries_dir),
      #                       plot_filetype = plot_filetype,
      #                       width = 14,
      #                       height = 5))
      # 
      # invisible(write_plots(plots = ts_annual_plot,
      #                       foldername = paste0(main_dir, timeseries_dir),
      #                       plot_filetype = plot_filetype,
      #                       width = 14,
      #                       height = 8.5))
      # 
      # invisible(write_plots(plots = ts_screen_plot,
      #                       foldername = paste0(main_dir, timeseries_dir),
      #                       plot_filetype = plot_filetype,
      #                       width = 8.5,
      #                       height = 5))
      # 
      # invisible(write_plots(plots = ts_missing_plot,
      #                       foldername = paste0(main_dir, timeseries_dir),
      #                       plot_filetype = plot_filetype,
      #                       width = 8.5,
      #                       height = 5))
      
      # Write to the Excel Workbook
      openxlsx::addWorksheet(wb = output_excel, 
                             sheetName = "Data Timeseries",
                             tabColour = "#73ba9b")
      
      # Add data table and title
      flow_data_out <- flow_data[,!colnames(flow_data) %in% "STATION_NUMBER"]
      add_table(wb = output_excel,
                sheet = "Data Timeseries",
                data = flow_data_out, 
                title = "Daily Data Timeseries",
                col = 1,
                row = 1)
      
      # Add plots and titles
      add_plot(wb = output_excel, 
               sheet = "Data Timeseries", 
               plot = ts_full_plot[[1]], 
               title = "Daily Data Timeseries", 
               col = ncol(flow_data_out) + 2, 
               row = 2, 
               height = 5,
               width = 20)
      
      
      # Write to the Excel Workbook
      openxlsx::addWorksheet(wb = output_excel, 
                             sheetName = "Data Screening",
                             tabColour = "#73ba9b")
      
      # Add data table and title
      flow_screening_out <- flow_screening[,!colnames(flow_screening) %in% "STATION_NUMBER"]
      add_table(wb = output_excel,
                sheet = "Data Screening", 
                data = flow_screening_out, 
                title = "Data Screening: Annual Summary Statistics and Data Availability",
                col = 1,
                row = 1)
      
      # Add plots and titles
      add_plot(wb = output_excel, 
               sheet = "Data Screening", 
               plot = ts_screen_plot[[1]], 
               title = "Annual Summary Statistics for Screening", 
               col = ncol(flow_screening_out) + 2, 
               row = 2, 
               height = 5,
               width = 8.5)
      add_plot(wb = output_excel, 
               sheet = "Data Screening", 
               plot = ts_missing_plot[[1]], 
               title = "Number of Missing Dates Per Month", 
               col = ncol(flow_screening_out) + 2, 
               row = 28, 
               height = 6,
               width = 8.5)
    }
  }  
  
  
  ### Long-term Stats
  ##########################
  
  if (2 %in% sections) {
    
    
    
    # Write the long-term stats with percentiles
    lt_stats <- calc_longterm_stats(data = flow_data,
                                    exclude_years = exclude_years,
                                    water_year_start = water_year_start,
                                    percentiles = 1:99,
                                    transpose = TRUE,
                                    ignore_missing = ignore_missing)
    
    
    # Write the long-term stats plot
    lt_stats_plot <- plot_longterm_stats(data = flow_data,
                                         exclude_years = exclude_years,
                                         water_year_start = water_year_start,
                                         ignore_missing = ignore_missing)
    
    
    # Write the flow duration plot
    lt_flowduration_plot <- plot_flow_duration(data = flow_data,
                                               exclude_years = exclude_years,
                                               water_year_start = water_year_start,
                                               ignore_missing = ignore_missing)
    
    all_objects <- append(all_objects,    
                          list("Longterm" = list("Longterm_Summary_Stats_Percentiles" = lt_stats,
                                                 "Longterm_Summary_Stats_Plot" = lt_stats_plot,
                                                 "Flow_Duration_Curves" = lt_flowduration_plot)))
    
    if (write_to_dir) {
      # # Create the folder
      # longterm_dir <- "2 - Long-term/"
      # dir.create(path = paste0(main_dir, longterm_dir), showWarnings = FALSE)
      # 
      # write_results(data = lt_stats,
      #               file = paste0(main_dir, longterm_dir, "Longterm_Statistics_and_Percentiles.", table_filetype))
      # 
      # invisible(write_plots(plots = lt_stats_plot,
      #                       foldername = paste0(main_dir, longterm_dir),
      #                       plot_filetype = plot_filetype,
      #                       width = 11,
      #                       height = 5))
      # 
      # invisible(write_plots(plots = lt_flowduration_plot,
      #                       foldername = paste0(main_dir, longterm_dir),
      #                       plot_filetype = plot_filetype,
      #                       width = 11,
      #                       height = 7))
      
      # Write to the Excel Workbook
      openxlsx::addWorksheet(wb = output_excel, 
                             sheetName = "Long-term Stats",
                             tabColour = "#73fbd3")
      
      # Add data table and title
      lt_stats_out <- lt_stats[,!colnames(lt_stats) %in% "STATION_NUMBER"]
      add_table(wb = output_excel,
                sheet = "Long-term Stats", 
                data = lt_stats_out, 
                title = paste0("Long-term Summary Statistics from ", start_year, "-", end_year),
                col = 1,
                row = 1)
      
      # Add plots and titles
      add_plot(wb = output_excel, 
               sheet = "Long-term Stats", 
               plot = lt_stats_plot[[1]], 
               title = paste0("Long-term Summary Statistics from ", start_year, "-", end_year), 
               col = ncol(lt_stats_out) + 2, 
               row = 2, 
               height = 4,
               width = 10)
      add_plot(wb = output_excel, 
               sheet = "Long-term Stats", 
               plot = lt_flowduration_plot[[1]], 
               title = paste0("Flow Duration Curves from ", start_year, "-", end_year), 
               col = ncol(lt_stats_out) + 2, 
               row = 23, 
               height = 5,
               width = 10)
      
      
    }
  }
  
  
  ### Annual Stats
  ##########################
  
  if (3 %in% sections) {
    
    
    # Annual stats
    ann_stats <- calc_annual_stats(data = flow_data,
                                   exclude_years = exclude_years,
                                   water_year_start = water_year_start,
                                   ignore_missing = ignore_missing)
    
    
    # Annual volume
    ann_vol <- calc_annual_cumulative_stats(data = flow_data,
                                            exclude_years = exclude_years,
                                            water_year_start = water_year_start,
                                            include_seasons = TRUE)
    
    
    # Annual yield
    ann_yield <- calc_annual_cumulative_stats(data = flow_data,
                                              exclude_years = exclude_years,
                                              water_year_start = water_year_start,
                                              include_seasons = TRUE, 
                                              use_yield = TRUE, 
                                              basin_area = basin_area)
    
    
    # Annual flow timing
    ann_timing <- calc_annual_flow_timing(data = flow_data,
                                          exclude_years = exclude_years,
                                          water_year_start = water_year_start)
    
    
    # Annual days outside normal
    ann_norm <- calc_annual_outside_normal(data = flow_data,
                                           exclude_years = exclude_years,
                                           water_year_start = water_year_start)
    
    
    # Annual lowflows
    ann_lowflow <- calc_annual_lowflows(data = flow_data_plus,
                                        start_year = start_year,
                                        end_year = end_year,
                                        exclude_years = exclude_years,
                                        water_year_start = water_year_start,
                                        ignore_missing = ignore_missing)
    
    
    
    # Write each of the annual stats plots
    ann_stats_plot <- plot_annual_stats(data = flow_data,
                                        exclude_years = exclude_years,
                                        water_year_start = water_year_start,
                                        ignore_missing = ignore_missing)
    ann_vol_plot <- plot_annual_cumulative_stats(data = flow_data,
                                                 exclude_years = exclude_years,
                                                 water_year_start = water_year_start,
                                                 include_seasons = TRUE)
    ann_yield_plot <- plot_annual_cumulative_stats(data = flow_data,
                                                   exclude_years = exclude_years,
                                                   water_year_start = water_year_start,
                                                   include_seasons = TRUE, 
                                                   use_yield = TRUE,
                                                   basin_area = basin_area)
    ann_timing_plot <- plot_annual_flow_timing(data = flow_data,
                                               exclude_years = exclude_years,
                                               water_year_start = water_year_start)
    ann_norm_plot <- plot_annual_outside_normal(data = flow_data,
                                                exclude_years = exclude_years,
                                                water_year_start = water_year_start)
    ann_lowflow_plot <- plot_annual_lowflows(data = flow_data_plus,
                                             start_year = start_year,
                                             end_year = end_year,
                                             exclude_years = exclude_years,
                                             water_year_start = water_year_start,
                                             ignore_missing = ignore_missing)
    ann_means_plot <- plot_annual_means(data = flow_data,
                                        exclude_years = exclude_years,
                                        water_year_start = water_year_start,
                                        ignore_missing = ignore_missing)
    
    
    
    all_objects <- append(all_objects,    
                          list("Annual" = list("Annual_Summary_Stats" = ann_stats,
                                               "Annual_Summary_Stats_Plot" = ann_stats_plot,
                                               "Annual_Cumul_Volume_Stats_m3" = ann_vol,
                                               "Annual_Cumul_Volume_Stats_m3_Plot" = ann_vol_plot,
                                               "Annual_Cumul_Yield_Stats_mm" = ann_yield,
                                               "Annual_Cumul_Yield_Stats_mm_Plot" = ann_yield_plot,
                                               "Annual_Flow_Timing" = ann_timing,
                                               "Annual_Flow_Timing_Plot" = ann_timing_plot,
                                               "Annual_Days_Outside_Normal" = ann_norm,
                                               "Annual_Days_Outside_Normal_Plot" = ann_norm_plot,
                                               "Annual_Low_Flows" = ann_lowflow,
                                               "Annual_Low_Flows_Plot" = ann_lowflow_plot,
                                               "Annual_Means_plot" =  ann_means_plot)))
    
    if (write_to_dir) {
      # # Create the folder
      # annual_dir <- "3 - Annual/"
      # dir.create(path = paste0(main_dir, annual_dir), showWarnings = FALSE)
      # 
      # write_results(data = ann_stats,
      #               file = paste0(main_dir, annual_dir, "Annual_Summary_Statistics.", table_filetype))
      # 
      # write_results(data = ann_vol,
      #               file = paste0(main_dir, annual_dir, "Annual_Cumulative_Volume.", table_filetype))
      # 
      # write_results(data = ann_yield,
      #               digits = 1,
      #               file = paste0(main_dir, annual_dir, "Annual_Cumulative_Yield.", table_filetype))
      # 
      # write_results(data = ann_timing,
      #               file = paste0(main_dir, annual_dir, "Annual_Flow_Timing.xlsx"))
      # 
      # write_results(data = ann_norm,
      #               file = paste0(main_dir, annual_dir, "Annual_Days_Outside_Normal.", table_filetype))
      # 
      # write_results(data = ann_lowflow,
      #               file = paste0(main_dir, annual_dir, "Annual_Low_Flows.", table_filetype))
      # 
      # 
      # invisible(write_plots(plots = c(ann_stats_plot, ann_vol_plot, ann_yield_plot, 
      #                                 ann_timing_plot, ann_norm_plot, ann_lowflow_plot,
      #                                 ann_means_plot),
      #                       foldername = paste0(main_dir, annual_dir),
      #                       plot_filetype = plot_filetype,
      #                       width = 10,
      #                       height = 5.5))
      
      # Write to the Excel Workbook
      openxlsx::addWorksheet(wb = output_excel, 
                             sheetName = "Annual Stats",
                             tabColour = "#44e5e7")
      
      # Add data table and title
      ann_stats_out <- ann_stats[,!colnames(ann_stats) %in% "STATION_NUMBER"]
      
      add_table(wb = output_excel,
                sheet = "Annual Stats", 
                data = ann_stats_out, 
                title = paste0("Annual Summary Statistics from ", start_year, "-", end_year),
                col = 1,
                row = 1)
      
      # Add plots and titles
      add_plot(wb = output_excel, 
               sheet = "Annual Stats", 
               plot = ann_stats_plot[[1]], 
               title = paste0("Annual Summary Statistics from ", start_year, "-", end_year), 
               col = ncol(ann_stats_out) + 2, 
               row = 2, 
               height = 3,
               width = 8.5)
      add_plot(wb = output_excel, 
               sheet = "Annual Stats", 
               plot = ann_means_plot[[1]], 
               title = paste0("Annual Means from ", start_year, "-", end_year), 
               col = ncol(ann_stats_out) + 2, 
               row = 18, 
               height = 3,
               width = 8.5)
      
      
      # Write to the Excel Workbook
      openxlsx::addWorksheet(wb = output_excel, 
                             sheetName = "Annual Cumulative Stats",
                             tabColour = "#44e5e7")
      
      # Add data table and title
      
      ann_cumul <- dplyr::left_join(ann_vol, ann_yield, by = c("STATION_NUMBER", "Year"))
      ann_cumul_out <- ann_cumul[,!colnames(ann_cumul) %in% "STATION_NUMBER"]

      add_table(wb = output_excel,
                sheet = "Annual Cumulative Stats", 
                data = ann_cumul_out, 
                title = paste0("Annual Cumulative Summary Statistics from ", start_year, "-", end_year),
                col = 1,
                row = 1)
      
      # Add plots and titles
      add_plot(wb = output_excel, 
               sheet = "Annual Cumulative Stats", 
               plot = ann_vol_plot[[1]], 
               title = paste0("Annual Total Volume from ", start_year, "-", end_year), 
               col = ncol(ann_cumul_out) + 2, 
               row = 2, 
               height = 2,
               width = 6)
      add_plot(wb = output_excel, 
               sheet = "Annual Cumulative Stats", 
               plot = ann_vol_plot[[2]], 
               title = paste0("Seasonal (Two Seasons) Total Volume from ", start_year, "-", end_year), 
               col = ncol(ann_cumul_out) + 2, 
               row = 13, 
               height = 2.5,
               width = 6)
      add_plot(wb = output_excel, 
               sheet = "Annual Cumulative Stats", 
               plot = ann_vol_plot[[3]], 
               title = paste0("Seasonal (Four Seasons) Total Volume from ", start_year, "-", end_year), 
               col = ncol(ann_cumul_out) + 2, 
               row = 27, 
               height = 4,
               width = 6)
      
      add_plot(wb = output_excel, 
               sheet = "Annual Cumulative Stats", 
               plot = ann_yield_plot[[1]], 
               title = paste0("Annual Total Yield from ", start_year, "-", end_year), 
               col = ncol(ann_cumul_out) + 2 + 10, 
               row = 2, 
               height = 2,
               width = 6)
      add_plot(wb = output_excel, 
               sheet = "Annual Cumulative Stats", 
               plot = ann_yield_plot[[2]], 
               title = paste0("Seasonal (Two Seasons) Total Yield from ", start_year, "-", end_year), 
               col = ncol(ann_cumul_out) + 2 + 10, 
               row = 13, 
               height = 2.5,
               width = 6)
      add_plot(wb = output_excel, 
               sheet = "Annual Cumulative Stats", 
               plot = ann_yield_plot[[3]], 
               title = paste0("Seasonal (Four Seasons) Total Yield from ", start_year, "-", end_year), 
               col = ncol(ann_cumul_out) + 2 + 10, 
               row = 27, 
               height = 4,
               width = 6)
      
      
      # Write to the Excel Workbook
      openxlsx::addWorksheet(wb = output_excel, 
                             sheetName = "Annual Stats Other",
                             tabColour = "#44e5e7")
      
      # Add data table and title
      ann_other <- dplyr::left_join(ann_lowflow, ann_timing, by = c("STATION_NUMBER", "Year"))
      ann_other <- dplyr::left_join(ann_other, ann_norm, by = c("STATION_NUMBER", "Year"))
      ann_other_out <- ann_other[,!colnames(ann_other) %in% "STATION_NUMBER"]
      add_table(wb = output_excel,
                sheet = "Annual Stats Other", 
                data = ann_other_out, 
                title = paste0("Other Annual Statistics (lowflows, etc) from ", start_year, "-", end_year),
                col = 1,
                row = 1)
      
      # Add plots and titles
      add_plot(wb = output_excel, 
               sheet = "Annual Stats Other",
               plot = ann_lowflow_plot[[1]], 
               title = paste0("Annual Low-flows from ", start_year, "-", end_year), 
               col = ncol(ann_other_out) + 2, 
               row = 2, 
               height = 5,
               width = 6)
      add_plot(wb = output_excel, 
               sheet = "Annual Stats Other",
               plot = ann_lowflow_plot[[2]], 
               title = paste0("Day of Annual Low-flows from ", start_year, "-", end_year), 
               col = ncol(ann_other_out) + 2, 
               row = 28, 
               height = 5,
               width = 6)
      add_plot(wb = output_excel,
               sheet = "Annual Stats Other",
               plot = ann_timing_plot[[1]],
               title = paste0("Annual Flow Timing from ", start_year, "-", end_year),
               col = ncol(ann_other_out) + 2 + 10,
               row = 2, 
               height = 4,
               width = 6)
      add_plot(wb = output_excel,
               sheet = "Annual Stats Other",
               plot = ann_norm_plot[[1]],
               title = paste0("Annual Days Per Year Above, Below, and Outside Normal from ", start_year, "-", end_year),
               col = ncol(ann_other_out) + 2 + 10,
               row = 23, 
               height = 4,
               width = 6)

      
    }
    
  }
  
  
  ### Monthly Stats
  ##########################
  
  if (4 %in% sections) {
    
    
    # Write all the monthly stats
    mon_stats <- calc_monthly_stats(data = flow_data,
                                    exclude_years = exclude_years,
                                    water_year_start = water_year_start,
                                    ignore_missing = ignore_missing)
    mon_stats_spread <- calc_monthly_stats(data = flow_data,
                                    exclude_years = exclude_years,
                                    water_year_start = water_year_start,
                                    ignore_missing = ignore_missing,
                                    spread = TRUE)
    
    
    mon_vol <- calc_monthly_cumulative_stats(data = flow_data,
                                             exclude_years = exclude_years,
                                             water_year_start = water_year_start)
    
    
    mon_yield <- calc_monthly_cumulative_stats(data = flow_data,
                                               exclude_years = exclude_years,
                                               water_year_start = water_year_start,
                                               use_yield = TRUE, 
                                               basin_area = basin_area)
    
    
    
    # Write the monthly stats plots
    mon_stats_plot <- plot_monthly_stats(data = flow_data,
                                         exclude_years = exclude_years,
                                         water_year_start = water_year_start,
                                         ignore_missing = ignore_missing)
    
    
    # Write the monthly cumulative plots
    mon_vol_plot <- plot_monthly_cumulative_stats(data = flow_data,
                                                  exclude_years = exclude_years,
                                                  water_year_start = water_year_start)
    mon_yield_plot <- plot_monthly_cumulative_stats(data = flow_data,
                                                    exclude_years = exclude_years,
                                                    water_year_start = water_year_start,
                                                    use_yield = TRUE,
                                                    basin_area = basin_area)
    
    
    all_objects <- append(all_objects,    
                          list("Monthly" = list("Monthly_Summary_Stats" = mon_stats,
                                                "Monthly_Summary_Stats_Plot" = mon_stats_plot,
                                                "Monthly_Total_Cumul_Volume_m3" = mon_vol,
                                                "Monthly_Total_Cumul_Volume_m3_Plot" = mon_vol_plot,
                                                "Monthly_Total_Cumul_Yield_mm" = mon_yield,
                                                "Monthly_Total_Cumul_Yield_mm_Plot" = mon_yield_plot)))
    
    if (write_to_dir) {
      # Create the folder
      # month_dir <- "4 - Monthly/"
      # dir.create(path = paste0(main_dir, month_dir), showWarnings = FALSE)
      # 
      # write_results(data = mon_stats,
      #               file = paste0(main_dir, month_dir, "Monthly_Summary_Statistics.", table_filetype))
      # 
      # write_results(data = mon_vol,
      #               file = paste0(main_dir, month_dir, "Monthly_Cumulative_Volume.", table_filetype))
      # 
      # write_results(data =  mon_yield,
      #               digits = 1,
      #               file = paste0(main_dir, month_dir, "Monthly_Cumulative_Yield.", table_filetype))
      # 
      # invisible(write_plots(plots = mon_stats_plot,
      #                       foldername = paste0(main_dir, month_dir),
      #                       plot_filetype = plot_filetype,
      #                       width = 11,
      #                       height = 5))
      # 
      # invisible(write_plots(plots = c(mon_vol_plot, mon_yield_plot),
      #                       foldername = paste0(main_dir, month_dir),
      #                       plot_filetype = plot_filetype,
      #                       width = 8.5,
      #                       height = 4))
      
      # Write to the Excel Workbook
      openxlsx::addWorksheet(wb = output_excel, 
                             sheetName = "Monthly Stats",
                             tabColour = "#59d2fe")
      
      # Add data table and title
      mon_stats_out <- mon_stats_spread[,!colnames(mon_stats_spread) %in% "STATION_NUMBER"]
      add_table(wb = output_excel,
                sheet = "Monthly Stats",
                data = mon_stats_out, 
                title = paste0("Monthly Sumamry Statistics from ", start_year, "-", end_year),
                col = 1,
                row = 1)
      
      # Add plots and titles
      add_plot(wb = output_excel, 
               sheet = "Monthly Stats",
               plot = mon_stats_plot[[1]], 
               title = paste0("Monthly Mean Flows from ", start_year, "-", end_year), 
               col = ncol(mon_stats_out) + 2, 
               row = 2, 
               height = 5,
               width = 9)
      add_plot(wb = output_excel, 
               sheet = "Monthly Stats",
               plot = mon_stats_plot[[2]], 
               title = paste0("Monthly Median Flows from ", start_year, "-", end_year), 
               col = ncol(mon_stats_out) + 2, 
               row = 28, 
               height = 5,
               width = 9)
      add_plot(wb = output_excel, 
               sheet = "Monthly Stats",
               plot = mon_stats_plot[[3]], 
               title = paste0("Monthly Maximum Flows from ", start_year, "-", end_year), 
               col = ncol(mon_stats_out) + 2 + 14, 
               row = 2, 
               height = 5,
               width = 9)
      add_plot(wb = output_excel, 
               sheet = "Monthly Stats",
               plot = mon_stats_plot[[4]], 
               title = paste0("Monthly Minimum Flows from ", start_year, "-", end_year), 
               col = ncol(mon_stats_out) + 2 + 14, 
               row = 28, 
               height = 5,
               width = 9)
      
      # Write to the Excel Workbook
      openxlsx::addWorksheet(wb = output_excel, 
                             sheetName = "Monthly Cumulative Stats",
                             tabColour = "#59d2fe")
      
    }
  }
  
  
  ### Daily Stats
  ##########################
  
  if (5 %in% sections) {
    
    
    # Write all daily stats
    day_stats <- calc_daily_stats(data = flow_data,
                                  exclude_years = exclude_years,
                                  water_year_start = water_year_start,
                                  ignore_missing = ignore_missing)
    
    
    day_vol <- calc_daily_cumulative_stats(data = flow_data,
                                           exclude_years = exclude_years,
                                           water_year_start = water_year_start)
    
    
    day_yield <- calc_daily_cumulative_stats(data = flow_data,
                                             exclude_years = exclude_years,
                                             water_year_start = water_year_start,
                                             use_yield = TRUE,
                                             basin_area = basin_area)
    
    
    
    
    # Write the daily stats plots
    day_stats_plot <- plot_daily_stats(data = flow_data,
                                       exclude_years = exclude_years,
                                       water_year_start = water_year_start,
                                       ignore_missing = ignore_missing)
    day_vol_plot <- plot_daily_cumulative_stats(data = flow_data,
                                                exclude_years = exclude_years,
                                                water_year_start = water_year_start)
    day_yield_plot <- plot_daily_cumulative_stats(data = flow_data,
                                                  exclude_years = exclude_years,
                                                  water_year_start = water_year_start,
                                                  use_yield = TRUE,
                                                  basin_area = basin_area)
    
    # Write the daily cumulative plots
    
    
    # Write the daily statistics plots with years
    day_stats_year_plots <- list()
    for(year in years_list) {
      plot <- suppressMessages(plot_daily_stats(data = flow_data,
                                                start_year = start_year,
                                                end_year = end_year,
                                                water_year_start = water_year_start,
                                                exclude_years = exclude_years,
                                                include_year = year,
                                                ignore_missing = ignore_missing))
      day_stats_year_plots[[ paste0(names(plot), "_", year) ]] <- suppressMessages(plot$Daily_Statistics)
      
    }
    
    
    # Write the daily cumulative volumetric plots with years
    day_vol_year_plots <- list()
    for(year in years_list) {
      plot <- suppressMessages(plot_daily_cumulative_stats(data = flow_data,
                                                           start_year = start_year,
                                                           end_year = end_year,
                                                           water_year_start = water_year_start,
                                                           exclude_years = exclude_years,
                                                           include_year = year))
      day_vol_year_plots[[ paste0(names(plot), "_", year) ]] <- suppressMessages(plot$Daily_Cumulative_Volumetric_Stats)
      
    }
    
    
    # Write the daily cumulative yield plots with years
    day_yield_year_plots <- list()
    for(year in years_list) {
      plot <- suppressMessages(plot_daily_cumulative_stats(data = flow_data,
                                                           start_year = start_year,
                                                           end_year = end_year,
                                                           water_year_start = water_year_start,
                                                           exclude_years = exclude_years,
                                                           include_year = year,
                                                           use_yield = TRUE, basin_area = basin_area))
      day_yield_year_plots[[ paste0(names(plot), "_", year) ]] <- suppressMessages(plot$Daily_Cumulative_Yield_Stats)
      
    }
    
    
    all_objects <- append(all_objects,    
                          list("Daily" = list("Daily_Summary_Stats" = day_stats,
                                              "Daily_Summary_Stats_Plot" = day_stats_plot,
                                              "Daily_Summary_Stats_with_Years" = day_stats_year_plots,
                                              "Daily_Total_Cumul_Volume_m3" = day_vol,
                                              "Daily_Total_Cumul_Volume_m3_Plot" = day_vol_plot,
                                              "Daily_Total_Cumul_Yield_mm" = day_yield,
                                              "Daily_Total_Cumul_Yield_mm_Plot" = day_yield_plot,
                                              "Daily_Total_Cumul_Volume_m3_with_Years" = day_vol_year_plots,
                                              "Daily_Total_Cumul_Yield_mm_with_Years" = day_yield_year_plots)))
    
    if (write_to_dir) {
      # Create the folder
      daily_dir <- "5 - Daily/"
      dir.create(path = paste0(main_dir, daily_dir), showWarnings = FALSE)
      
      write_results(data = day_stats,
                    file = paste0(main_dir, daily_dir, "Daily_Summary_Statistics.", table_filetype))
      
      write_results(data = day_vol,
                    file = paste0(main_dir, daily_dir, "Daily_Cumulative_Volume.", table_filetype))
      
      write_results(data =  day_yield,
                    digits = 1,
                    file = paste0(main_dir, daily_dir, "Daily_Cumulative_Yield.", table_filetype))
      
      invisible(write_plots(plots = c(day_stats_plot, day_vol_plot, day_yield_plot),
                            foldername = paste0(main_dir, daily_dir),
                            plot_filetype = plot_filetype,
                            width = 8.5,
                            height = 4))
      
      write_plots(plots = day_stats_year_plots, foldername = paste0(main_dir, daily_dir, "Daily_Statistics_with_Years"),
                  plot_filetype = plot_filetype,
                  width = 8.5,
                  height = 4,
                  combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
      
      write_plots(plots = day_vol_year_plots, foldername = paste0(main_dir, daily_dir, "Daily_Cumulative_Volume_with_Years"),
                  plot_filetype = plot_filetype,
                  width = 8.5,
                  height = 4,
                  combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
      
      write_plots(plots = day_yield_year_plots, foldername = paste0(main_dir, daily_dir, "Daily_Cumulative_Yield_with_Years"),
                  plot_filetype = plot_filetype,
                  width = 8.5,
                  height = 4,
                  combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
      
      
    }
    
  }
  
  
  # ### Annual Trending
  ##########################
  
  if (6 %in% sections) {
    
    
    
    # Write the trends results
    ann_trends <- compute_annual_trends(data = flow_data,
                                        exclude_years = exclude_years,
                                        water_year_start = water_year_start,
                                        ignore_missing = ignore_missing,
                                        zyp_method = "yuepilon",
                                        zyp_alpha = zyp_alpha)
    ann_data <- ann_trends$Annual_Trends_Data
    ann_results <- ann_trends$Annual_Trends_Results
    ann_trends_plots <-  ann_trends[c(3:length(names(ann_trends)))]
    
    all_objects <- append(all_objects,    
                          list("Trending" = list("Annual_Trends_Data" = ann_data,
                                                 "Annual_Trends_Results" = ann_results,
                                                 "Annual_Trends_Plots" = ann_trends_plots)))
    
    if (write_to_dir) {
      # Create the folder
      trending_dir <- "6 - Trending/"
      dir.create(path = paste0(main_dir, trending_dir), showWarnings = FALSE)
      
      write_results(data = ann_data,
                    file = paste0(main_dir, trending_dir, "Annual_Trends_Data.", table_filetype))
      
      write_results(data = ann_results,
                    file = paste0(main_dir, trending_dir, "Annual_Trends_Results.", table_filetype))
      
      invisible(write_plots(plots = ann_trends_plots,
                            foldername = paste0(main_dir, trending_dir , "Annual_Trends_Results_Plots"),
                            plot_filetype = plot_filetype,
                            width = 8.5,
                            height = 3,
                            combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE)))
    }
  }
  
  
  ### Low Flow Frequency
  ##########################
  
  if (7 %in% sections) {
    
    data_check <- calc_annual_lowflows(data = flow_data,
                                       start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                       water_year_start = water_year_start,
                                       ignore_missing = ignore_missing)
    data_check <- dplyr::select(data_check, Min_1_Day, Min_3_Day, Min_7_Day, Min_30_Day)
    
    if (any(as.numeric(colSums(!is.na(data_check))) < 3)) {
      warning("Not enough annual data (3 years) for frequency analysis. Consider filtering for appropriate years or use ignore_missing = TRUE,",
              call. = FALSE)
      
    } else {
      
      
      freq_results <- compute_annual_frequencies(data = flow_data,
                                                 exclude_years = exclude_years,
                                                 water_year_start = water_year_start,
                                                 ignore_missing = ignore_missing)
      
      # freq_ann_data <- tidyr::spread(freq_results$Freq_Analysis_Data, Measure, value)
      freq_ann_data <- freq_results$Freq_Analysis_Data
      
      freq_plot_data <- freq_results$Freq_Plot_Data
      
      freq_plots <- list("Frequency_Plot" = freq_results$Freq_Plot)
      
      #freq_fitting <- freq_results$fit
      
      freq_quantiles <- freq_results$Freq_Fitted_Quantiles
      
      
      all_objects <- append(all_objects,    
                            list("Lowflow_Frequencies" = freq_results))
      
      
      if (write_to_dir) {
        # Create the folder
        freq_dir <- "7 - Low-flow Frequencies/"
        dir.create(path = paste0(main_dir, freq_dir), showWarnings = FALSE)
        
        write_results(data = freq_ann_data,
                      file = paste0(main_dir, freq_dir, "Annual_Lowflows.", table_filetype))
        
        write_results(data = freq_plot_data,
                      file = paste0(main_dir, freq_dir, "Plotting_Data.", table_filetype))
        
        invisible(write_plots(plots = freq_plots,
                              foldername = paste0(main_dir, freq_dir),
                              plot_filetype = "png",
                              width = 8.5,
                              height = 5))
        
        write_results(data = freq_quantiles,
                      file = paste0(main_dir, freq_dir, "Fitted_Quantiles.", table_filetype),
                      digits = 4)
      }
      
      
    }
  }
  
  
  # Write the metadata and table of contents
  ##########################
  
  if (write_to_dir) {
    
    
    ## Create a table of contents file
    
    # Get all folders in directory with appropriate names
    dirs <- list.files(path = main_dir)
    dirs <- dirs[dirs %in% c("1 - Screening", "2 - Long-term", "3 - Annual", "4 - Monthly", "5 - Daily", "6 - Trending", "7 - Low-flow Frequencies")]
    
    # Create a dataframe and cycle through each subfolder
    tbl_contents <- data.frame()
    for (i in dirs) {
      # Get the list of files in each subfolder
      subdirs <- list.files(path = paste0(main_dir, i))
      
      # Create a table with each file name, paste in the subdirectory name and attached to dataframe
      dir_data <- data.frame("File" = subdirs)
      dir_data$Directory <- i
      tbl_contents <- rbind(tbl_contents, dir_data)
    }
    
    # Create a FileType columns listing if file is a plot or table
    tbl_contents$fileExt <- sub('.*\\.', '', tbl_contents$File)
    tbl_contents$Type <- ifelse(tbl_contents$fileExt %in% c("xlsx", "xls", "csv"), "Table", 
                                ifelse(tbl_contents$fileExt %in% c("png", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "bmp", "svg"), "Plot",
                                       "Folder with Plots"))
    
    # Write the file into the main directory
    write_results(data = tbl_contents[,c(2,1,4)],
                  file = paste0(main_dir, "Table of Contents.", table_filetype))
    
    
    
    ## Create a meta data file of the analysis arguments
    
    # Make a list of all options used in function, plus dates and covnert to a dataframe
    metadata <- list(data = ifelse(!is.null(data), as.character(substitute(data)), ""),
                     dates = as.character(substitute(Date)),
                     values = as.character(substitute(Value)),
                     groups = as.character(substitute(STATION_NUMBER)),
                     station_number = ifelse(!is.null(station_number), station_number, ""),
                     basin_area = basin_area,
                     water_year_start = water_year_start,
                     start_year = start_year,
                     end_year = end_year,
                     exclude_years = exclude_years,
                     ignore_missing = ignore_missing,
                     zyp_method = zyp_method,
                     zyp_alpha = ifelse(!is.na(zyp_alpha), zyp_alpha, ""),
                     sections = sections,
                     foldername = foldername,
                     table_filetype = table_filetype,
                     plot_filetype = plot_filetype,
                     analysis_date = as.character(Sys.time()))
    metadata <- data.frame("Argument" = names(metadata),
                           "Option" = as.character(unname(metadata)))
    
    # Write the table
    write_results(data = metadata[c(1,5,2:4,6:nrow(metadata)),],
                  file = paste0(main_dir, "Analysis Metadata.", table_filetype))
    
    
    ## Success message
    message(paste0("Success! compute_full_analysis() has saved all tables and plots. Go to ", main_dir, " folder for files."))
    
  }
  
  if (write_to_dir){
    
    # Create the file with all the sheets
    
    openxlsx::saveWorkbook(wb = output_excel, 
                           file = paste0(foldername, "_fasstr_analysis.xlsx"),
                           overwrite = T)
    
    
    
    
  }
  
  return(all_objects)
  
}



