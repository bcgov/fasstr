# Copyright 2019 Province of British Columbia
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

#' @title Write a suite of tables and plots from various fasstr functions into a directory
#'
#' @description Calculates and writes tables and plots from a suite of statistics from fasstr functions into 
#'    an Excel workbook, and accompanying plot files for certain analyses. Due to the number of tables and plots to be made, this 
#'    function may take several minutes to complete. If using \code{ignore_missing = FALSE} (default) and there is missing data, 
#'    some tables and plots may be empty and produce warnings. Use \code{ignore_missing = TRUE} to ignore the missing values or 
#'    filter your data to complete years.
#'    
#' @inheritParams compute_full_analysis
#' @param file_name Character string of the name of the Excel Workbook (and folder for plots if necessary) to create on drive to 
#'    write all results. 
#' @param plot_filetype Image type to write. One of "png", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "bmp", or "svg".
#'    If not "pdf" then individual plots will be created instead of a combined PDF. Default \code{"pdf"}.
#' 
#' @seealso \code{\link{compute_full_analysis}},
#'          \code{\link{screen_flow_data}},
#'          \code{\link{plot_data_screening}},
#'          \code{\link{plot_missing_dates}},
#'          \code{\link{calc_longterm_monthly_stats}},
#'          \code{\link{plot_longterm_monthly_stats}},
#'          \code{\link{calc_longterm_daily_stats}},
#'          \code{\link{plot_longterm_daily_stats}},
#'          \code{\link{plot_flow_duration}},
#'          \code{\link{calc_annual_stats}},
#'          \code{\link{plot_annual_stats}},
#'          \code{\link{calc_annual_cumulative_stats}},
#'          \code{\link{plot_annual_cumulative_stats}},
#'          \code{\link{calc_annual_flow_timing}},
#'          \code{\link{plot_annual_flow_timing}},
#'          \code{\link{calc_annual_outside_normal}},
#'          \code{\link{plot_annual_outside_normal}},
#'          \code{\link{calc_annual_lowflows}},
#'          \code{\link{plot_annual_lowflows}},
#'          \code{\link{plot_annual_means}},
#'          \code{\link{calc_monthly_stats}},
#'          \code{\link{plot_monthly_stats}},
#'          \code{\link{calc_monthly_cumulative_stats}},
#'          \code{\link{plot_monthly_cumulative_stats}},
#'          \code{\link{calc_daily_stats}},
#'          \code{\link{plot_daily_stats}},
#'          \code{\link{calc_daily_cumulative_stats}},
#'          \code{\link{plot_daily_cumulative_stats}},
#'          \code{\link{compute_annual_trends}},
#'          \code{\link{compute_annual_frequencies}},
#'          \code{\link{write_flow_data}},
#'          \code{\link{write_plots}}
#' 
#' @examples
#' \dontrun{
#' 
#' # Save a full analysis will all the analyses
#' write_full_analysis(station_number = "08NM116",
#'                     file_name = "Mission Creek",
#'                     start_year = 1980,
#'                     end_year = 2010)
#' 
#' # Save a full analysis with only Annual and Daily analyses
#' write_full_analysis(station_number = "08NM116",
#'                     file_name = "Mission Creek",
#'                     start_year = 1980,
#'                     end_year = 2010,
#'                     analyses = c(3,5))
#' }
#' @export



write_full_analysis <- function(data,
                                dates = Date,
                                values = Value,
                                groups = STATION_NUMBER,
                                station_number,
                                analyses = 1:7,
                                basin_area,
                                water_year_start = 1,
                                start_year,
                                end_year,
                                exclude_years,
                                ignore_missing = FALSE,
                                zyp_method = 'yuepilon',
                                zyp_alpha,
                                file_name,
                                plot_filetype = 'pdf'){
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data = NULL
  }
  if (missing(station_number)) {
    station_number = NULL
  }
  if (missing(start_year)) {
    start_year = 0
  }
  if (missing(end_year)) {
    end_year = 9999
  }
  if (missing(exclude_years)) {
    exclude_years = NULL
  }
  if (missing(basin_area)) {
    basin_area = NA
  }
  if (missing(zyp_alpha)) {
    zyp_alpha = NA
  }
  
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  ignore_missing_checks(ignore_missing)
  
  if (missing(file_name))     stop("A file name is required with the file_name argument to write all results.", call. = FALSE)
  
  if (!is.numeric(analyses))
    stop("analyses argument must be numbers between 1 and 7. See ?write_full_analysis for analysis group numbers.", call. = FALSE)
  if (!all(analyses %in% 1:7))
    stop("analyses argument must be numbers between 1 and 7. See ?write_full_analysis for analysis group numbers.", call. = FALSE)
  
  if (6 %in% analyses) {
    zyp_alpha_checks(zyp_alpha)
    zyp_method_checks(zyp_method)
  }
  
  
  # Do this for now, until looping of include_year plots is sorted out
  if (length(station_number) > 1) stop("Only one station_number can be listed.", call. = FALSE)
  
  message("* this may take a few moments...")
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data_raw <- flowdata_import(data = data, 
                                   station_number = station_number)
  
  # Save the original columns (to check for STATION_NUMBER col at end) and ungroup if necessary
  orig_cols <- names(flow_data_raw)
  flow_data_raw <- dplyr::ungroup(flow_data_raw)
  
  # Check and rename columns
  flow_data_raw <- format_all_cols(data = flow_data_raw,
                                   dates = as.character(substitute(dates)),
                                   values = as.character(substitute(values)),
                                   groups = as.character(substitute(groups)),
                                   rm_other_cols = TRUE)
  
  # # Data setup
  flow_data_unfiltered <- fill_missing_dates(data = flow_data_raw, water_year_start = water_year_start)
  flow_data_unfiltered <- add_date_variables(data = flow_data_unfiltered, water_year_start = water_year_start)
  flow_data_unfiltered <- add_rolling_means(data = flow_data_unfiltered)
  # 
  # # Set up basin_area
  flow_data_unfiltered <- add_basin_area(flow_data_unfiltered, basin_area = basin_area)
  basin_area_stn <- unique(flow_data_unfiltered$Basin_Area_sqkm)[1]
  
  # Get the start and end years of the data to make a list of all included years
  flow_data_filtered <- flow_data_unfiltered
  
  flow_data_filtered <- analysis_prep(data = flow_data_filtered,
                                      water_year_start = water_year_start)
  if (start_year < min(flow_data_filtered$WaterYear)) {
    start_year <- min(flow_data_filtered$WaterYear)
  }
  if (end_year > max(flow_data_filtered$WaterYear)) {
    end_year <- max(flow_data_filtered$WaterYear)
  }
  
  # flow_data_plus <- dplyr::filter(flow_data, WaterYear >= start_year - 1 & WaterYear <= end_year + 1)
  flow_data_filtered <- dplyr::filter(flow_data_filtered, 
                                      WaterYear >= start_year & WaterYear <= end_year,
                                      !(WaterYear %in% exclude_years))
  
  if (any(is.na(flow_data_filtered$Value)) & !ignore_missing) {
    message("** warning: selected data contains dates with missing values; some NAs in tables and gaps in plots may be produced")
  }
  
  
  # Create list of all objects
  
  message("** creating data frames and plots")
  
  ### Compute results
  ##########################
  
  results <- suppressMessages(
    compute_full_analysis(data = flow_data_raw,
                          dates = "Date",
                          values = "Value",
                          groups = "STATION_NUMBER",
                          analyses = analyses,
                          basin_area = basin_area,
                          water_year_start = water_year_start,
                          start_year = start_year,
                          end_year = end_year,
                          exclude_years = exclude_years,
                          ignore_missing = ignore_missing,
                          zyp_method = zyp_method,
                          zyp_alpha = zyp_alpha)
  )
  
  ### Writing Functions
  ##########################
  
  # Create add table function
  add_table <- function(wb, sheet, data, title, col, row, comment = NA) {
    openxlsx::writeData(wb = wb, sheet = sheet, x = title, startCol = col, startRow = row)
    openxlsx::writeData(wb = wb, sheet = sheet, x = data, startCol = col, startRow = row + 1,
                        headerStyle = openxlsx::createStyle(fontSize = 11,
                                                            textDecoration = "bold",
                                                            border = "TopBottom",
                                                            fgFill = "#add8e6",
                                                            halign = "left"))
    openxlsx::addStyle(wb = wb, sheet = sheet, cols =  col, rows =  row,
                       style = openxlsx::createStyle(fontSize = 11,
                                                     textDecoration = "bold")) 
    if (!is.na(comment)) {
      openxlsx::writeComment(wb = wb, sheet = sheet, col = col, row = row,
                             comment = openxlsx::createComment(comment = comment,
                                                               visible = FALSE,
                                                               width = 5,
                                                               height = 4))
    }
  }
  
  # Create add plot function
  add_plot <- function(wb, sheet, plot, title, col, row, height, width, comment = NA) {
    openxlsx::writeData(wb = wb, sheet = sheet, x = title, startCol = col, startRow = row)
    print(plot)
    openxlsx::insertPlot(wb = wb, sheet = sheet, startCol = col, startRow = row + 1, height = height, width = width)
    openxlsx::addStyle(wb = wb, sheet = sheet, cols =  col, rows =  row,
                       style = openxlsx::createStyle(fontSize = 11,
                                                     textDecoration = "bold")) 
    if (!is.na(comment)) {
      openxlsx::writeComment(wb = wb, sheet = sheet, col = col, row = row,
                             comment = openxlsx::createComment(comment = comment,
                                                               visible = FALSE,
                                                               width = 5,
                                                               height = 4))
    }
  }
  
  
  ### Setup dataframe and values for writing the fasstr functions
  ##########################
  
  fasstr_functions <- data.frame("Worksheet" = character(), "Output" = character(), "Function" = character())
  
  # Create some values for creating character versions of fasstr functions
  fn_data <- paste0(ifelse(!is.null(data), 
                           paste0("data = ", as.character(substitute(data)), 
                                  ifelse(as.character(substitute(dates)) != "Date", 
                                         paste0(", dates = '", as.character(substitute(dates)), "'"), ""),
                                  ifelse(as.character(substitute(values)) != "Value",
                                         paste0(", values = '", as.character(substitute(values)), "'"), ""),
                                  ifelse(as.character(substitute(groups)) != "STATION_NUMBER",
                                         paste0(", groups = '", as.character(substitute(groups)), "'"), "")),
                           paste0("station_number = '", station_number, "'")))
  fn_area <- paste0(ifelse(!is.na(basin_area),
                           paste0(", basin_area = ", basin_area),
                           ""))
  fn_wys <- ifelse(water_year_start != 1, paste0(", water_year_start = ", water_year_start), "")
  fn_startend <- paste0(ifelse(start_year != min(flow_data_unfiltered$WaterYear),
                               paste0(", start_year = ", start_year), ""), 
                        ifelse(end_year != max(flow_data_unfiltered$WaterYear),
                               paste0(", end_year = ", end_year), ""))
  fn_exclude <- paste0(ifelse(!is.null(exclude_years),
                              paste0(", exclude_years = ", ifelse(length(exclude_years) == 1, 
                                                                  paste(exclude_years),
                                                                  paste0("c(",paste(exclude_years, collapse = ","),")"))),
                              ""))
  fn_missing <- ifelse(ignore_missing, paste0(", ignore_missing = TRUE"), "")
  fn_zyp <- paste0(", zyp_method = '", zyp_method, "'",
                   ", zyp_alpha = ", ifelse(!is.na(zyp_alpha), zyp_alpha, "NA"))
  
  # Create fasstr function strings for output
  analysis_function <- paste0("compute_full_analysis(",
                              fn_data,
                              fn_area,
                              fn_wys,
                              fn_startend,
                              fn_exclude,
                              fn_missing,
                              ifelse(6 %in% analyses, fn_zyp, ""),
                              paste0(", analyses = ", 
                                     ifelse(length(analyses) == 1, 
                                            paste(analyses),
                                            paste0("c(",paste(analyses, collapse = ","),")"))),
                              ")")
  
  # Add fasstr functions to table
  fasstr_functions <- dplyr::add_row(fasstr_functions, 
                                     "Worksheet" = file_name,
                                     "Output" = "Full Analysis Function",
                                     "Function" = analysis_function)
  
  ### Excel workbook Setup
  ##########################
  
  # Create the excel document and first worksheet
  output_excel <- openxlsx::createWorkbook()
  
  overview_sheet <- "Analysis Overview"
  openxlsx::addWorksheet(wb = output_excel, 
                         sheetName = overview_sheet,
                         tabColour = "#003e1f") # 73fbd3 44e5e7 59d2fe 4a8fe7 5c7aff
  
  # Add raw data to first worksheet
  rawdata_sheet <- "Data Input"
  openxlsx::addWorksheet(wb = output_excel, 
                         sheetName = rawdata_sheet,
                         tabColour = "#003e1f")
  add_table(wb = output_excel,
            sheet = rawdata_sheet,
            data = flow_data_raw, 
            title = "Provided Data",
            col = 1,
            row = 1,
            comment = NA)
  
  
  ## Screening
  ##########################
  
  if (1 %in% analyses) {
    
    # Add worksheet
    timeseries_sheet <- "Data Timeseries"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = timeseries_sheet,
                           tabColour = "#73ba9b")
    
    # Create fasstr function strings for output
    data_function <- paste0("add_date_variables(",
                            fn_data,
                            fn_wys,
                            ") %>% add_rolling_means() %>% add_basin_area(", 
                            paste0(ifelse(!is.na(basin_area),
                                          paste0("basin_area = ", basin_area),
                                          "")),
                            ")")
    data_plot_function <- paste0("plot_flow_data(",
                                 fn_data,
                                 fn_wys,
                                 fn_exclude,
                                 ")")
    
    # Add data tables
    flow_data_out <- flow_data_filtered[,!colnames(flow_data_filtered) %in% "STATION_NUMBER"]
    add_table(wb = output_excel,
              sheet = timeseries_sheet,
              data = flow_data_out,
              title = "Daily Data Timeseries",
              col = 1,
              row = 1,
              comment = data_function)
    
    # Add plots
    add_plot(wb = output_excel,
             sheet = timeseries_sheet,
             plot = results$Screening$Daily_Flows_Plot[[1]],
             title = "Daily Data Timeseries",
             col = ncol(flow_data_out) + 2,
             row = 2,
             height = 5,
             width = 20,
             comment = data_plot_function)
    
    
    # Add worksheet
    screening_sheet <- "Data Screening"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = screening_sheet,
                           tabColour = "#73ba9b")
    
    # Create fasstr function strings for output
    screening_function <- paste0("screen_flow_data(",
                                 fn_data,
                                 fn_wys,
                                 fn_startend,
                                 ")")
    screeningplot_function <- paste0("plot_data_screening(",
                                     fn_data,
                                     fn_wys,
                                     fn_startend,
                                     ")")
    missingingplot_function <- paste0("plot_missing_dates(",
                                      fn_data,
                                      fn_wys,
                                      fn_startend,
                                      ")")
    
    # Add data tables
    flow_screening_out <- results$Screening$Flow_Screening
    flow_screening_out <- flow_screening_out[,!colnames(flow_screening_out) %in% "STATION_NUMBER"]
    add_table(wb = output_excel,
              sheet = screening_sheet,
              data = flow_screening_out,
              title = "Data Screening: Annual Summary Statistics and Data Availability",
              col = 1,
              row = 1,
              comment = screening_function)
    
    # Add plots
    add_plot(wb = output_excel,
             sheet = screening_sheet,
             plot = results$Screening$Flow_Screening_Plot[[1]],
             title = "Annual Summary Statistics for Screening",
             col = ncol(flow_screening_out) + 2,
             row = 2,
             height = 5,
             width = 8.5,
             comment = screeningplot_function)
    add_plot(wb = output_excel,
             sheet = screening_sheet,
             plot = results$Screening$Missing_Dates_Plot[[1]],
             title = "Number of Missing Dates Per Month",
             col = ncol(flow_screening_out) + 2,
             row = 29,
             height = 6,
             width = 8.5,
             comment = missingingplot_function)
    
    # Add fasstr functions to table
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = timeseries_sheet,
                                       "Output" = "Daily Data Timeseries Table",
                                       "Function" = data_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = timeseries_sheet,
                                       "Output" = "Daily Data Timeseries Plot",
                                       "Function" = data_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = screening_sheet,
                                       "Output" = "Data Screening Table",
                                       "Function" = screening_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = screening_sheet,
                                       "Output" = "Annual Statistics for Screening Plot",
                                       "Function" = screeningplot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = screening_sheet,
                                       "Output" = "Missing Data Plot",
                                       "Function" = missingingplot_function)
    
  }
  
  
  ### Long-term Stats
  ##########################
  
  if (2 %in% analyses) {
    
    # Add worksheet
    lt_sheet <- "Long-term Daily Stats"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = lt_sheet,
                           tabColour = "#73fbd3")
    lt_mon_sheet <- "Long-term Monthly Stats"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = lt_mon_sheet,
                           tabColour = "#73fbd3")
    
    # Create fasstr function strings for output
    longterm_mon_function <- paste0("calc_longterm_monthly_stats(",
                                    fn_data,
                                    fn_wys,
                                    fn_startend,
                                    fn_exclude,
                                    fn_missing,
                                    ")")
    longtermplot_mon_function <- paste0("plot_longterm_monthly_stats(",
                                        fn_data,
                                        fn_wys,
                                        fn_startend,
                                        fn_exclude,
                                        fn_missing,
                                        ")")
    longterm_function <- paste0("calc_longterm_daily_stats(",
                                fn_data,
                                fn_wys,
                                fn_startend,
                                fn_exclude,
                                fn_missing,
                                ")")
    longtermplot_function <- paste0("plot_longterm_daily_stats(",
                                    fn_data,
                                    fn_wys,
                                    fn_startend,
                                    fn_exclude,
                                    fn_missing,
                                    ")")
    durationplot_function <- paste0("plot_flow_duration(",
                                    fn_data,
                                    fn_wys,
                                    fn_startend,
                                    fn_exclude,
                                    fn_missing,
                                    ")")
    
    # Add data tables
    lt_stats_out <- results$Longterm$Longterm_Daily_Summary_Stats_Percentiles
    lt_stats_out <- lt_stats_out[,!colnames(lt_stats_out) %in% "STATION_NUMBER"]
    add_table(wb = output_excel,
              sheet = lt_sheet,
              data = lt_stats_out,
              title = paste0("Long-term Daily Summary Statistics from ", start_year, "-", end_year),
              col = 1,
              row = 1,
              comment = longterm_function)
    
    # Add plots
    add_plot(wb = output_excel,
             sheet = lt_sheet,
             plot = results$Longterm$Longterm_Daily_Summary_Stats_Plot[[1]],
             title = paste0("Long-term Daily Summary Statistics from ", start_year, "-", end_year),
             col = ncol(lt_stats_out) + 2,
             row = 2,
             height = 4,
             width = 10,
             comment = longtermplot_function)
    add_plot(wb = output_excel,
             sheet = lt_sheet,
             plot = results$Longterm$Flow_Duration_Curves[[1]],
             title = paste0("Flow Duration Curves from ", start_year, "-", end_year),
             col = ncol(lt_stats_out) + 2,
             row = 24,
             height = 5,
             width = 10,
             comment = durationplot_function)
    
    
    
    # Add data tables
    lt_mon_stats_out <- results$Longterm$Longterm_Monthly_Summary_Stats_Percentiles
    lt_mon_stats_out <- lt_mon_stats_out[,!colnames(lt_mon_stats_out) %in% "STATION_NUMBER"]
    add_table(wb = output_excel,
              sheet = lt_mon_sheet,
              data = lt_mon_stats_out,
              title = paste0("Long-term Monthly Summary Statistics from ", start_year, "-", end_year),
              col = 1,
              row = 1,
              comment = longterm_function)
    
    # Add plots
    add_plot(wb = output_excel,
             sheet = lt_mon_sheet,
             plot = results$Longterm$Longterm_Monthly_Summary_Stats_Plot[[1]],
             title = paste0("Long-term Monthly Summary Statistics from ", start_year, "-", end_year),
             col = ncol(lt_mon_stats_out) + 2,
             row = 2,
             height = 4,
             width = 10,
             comment = longtermplot_function)
    
    
    # Add fasstr functions to table
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = lt_mon_sheet,
                                       "Output" = "Long-term Monthly Summary Statistics Table",
                                       "Function" = longterm_mon_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = lt_mon_sheet,
                                       "Output" = "Long-term Monthly Summary Statistics Plot",
                                       "Function" = longtermplot_mon_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = lt_sheet,
                                       "Output" = "Long-term Daily Summary Statistics Table",
                                       "Function" = longterm_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = lt_sheet,
                                       "Output" = "Long-term Daily Summary Statistics Plot",
                                       "Function" = longtermplot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = lt_sheet,
                                       "Output" = "Flow Duration Curves",
                                       "Function" = durationplot_function)
    
  }
  
  
  ### Annual Stats
  ##########################
  
  if (3 %in% analyses) {
    
    # Add worksheet
    ann_stat_sheet <- "Annual Stats"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = ann_stat_sheet,
                           tabColour = "#44e5e7")
    
    # Create fasstr function strings for output
    annual_stats_function <- paste0("calc_annual_stats(",
                                    fn_data,
                                    fn_wys,
                                    fn_startend,
                                    fn_exclude,
                                    fn_missing,
                                    ")")
    annual_plot_function <- paste0("plot_annual_stats(",
                                   fn_data,
                                   fn_wys,
                                   fn_startend,
                                   fn_exclude,
                                   fn_missing,
                                   ")")
    annual_mean_plot_function <- paste0("plot_annual_means(",
                                        fn_data,
                                        fn_wys,
                                        fn_startend,
                                        fn_exclude,
                                        fn_missing,
                                        ")")
    
    # Add data tables
    ann_stats_out <- results$Annual$Annual_Summary_Stats[,!colnames(results$Annual$Annual_Summary_Stats) %in% "STATION_NUMBER"]
    add_table(wb = output_excel,
              sheet = ann_stat_sheet,
              data = ann_stats_out,
              title = paste0("Annual Summary Statistics from ", start_year, "-", end_year),
              col = 1,
              row = 1,
              comment = annual_stats_function)
    
    # Add plots
    add_plot(wb = output_excel,
             sheet = ann_stat_sheet,
             plot = results$Annual$Annual_Summary_Stats_Plot[[1]],
             title = paste0("Annual Summary Statistics from ", start_year, "-", end_year),
             col = ncol(ann_stats_out) + 2,
             row = 2,
             height = 3,
             width = 8.5,
             comment = annual_plot_function)
    add_plot(wb = output_excel,
             sheet = ann_stat_sheet,
             plot = results$Annual$Annual_Means_Plot[[1]],
             title = paste0("Annual Means from ", start_year, "-", end_year),
             col = ncol(ann_stats_out) + 2,
             row = 19,
             height = 3,
             width = 8.5,
             comment = annual_mean_plot_function)
    
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_stat_sheet,
                                       "Output" = "Annual Summary Statistics Table",
                                       "Function" = annual_stats_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_stat_sheet,
                                       "Output" = "Annual Summary Statistics Plot",
                                       "Function" = annual_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_stat_sheet,
                                       "Output" = "Annual Mean Flows Plot",
                                       "Function" = annual_mean_plot_function)
    
    # Add worksheet
    ann_cumul_sheet <- "Annual Cumulative Stats"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = ann_cumul_sheet,
                           tabColour = "#44e5e7")
    
    # Create fasstr function strings for output
    annual_vol_function <- paste0("calc_annual_cumulative_stats(",
                                  fn_data,
                                  fn_wys,
                                  fn_startend,
                                  fn_exclude,
                                  ", include_seasons = TRUE)")
    annual_yield_function <- paste0("calc_annual_cumulative_stats(",
                                    fn_data,
                                    fn_wys,
                                    fn_startend,
                                    fn_exclude,
                                    ", use_yield = TRUE",
                                    fn_area,
                                    ", include_seasons = TRUE)")
    annual_vol_plot_function <- paste0("plot_annual_cumulative_stats(",
                                       fn_data,
                                       fn_wys,
                                       fn_startend,
                                       fn_exclude,
                                       ", include_seasons = TRUE)")
    annual_yield_plot_function <- paste0("plot_annual_cumulative_stats(",
                                         fn_data,
                                         fn_wys,
                                         fn_startend,
                                         fn_exclude,
                                         ", use_yield = TRUE",
                                         fn_area,
                                         ", include_seasons = TRUE)")
    
    # Add data tables
    ann_cumul_out <- dplyr::left_join(results$Annual$Annual_Cumul_Volume_Stats_m3, 
                                      results$Annual$Annual_Cumul_Yield_Stats_mm, 
                                      by = c("STATION_NUMBER", "Year"))
    ann_cumul_out <- ann_cumul_out[,!colnames(ann_cumul_out) %in% "STATION_NUMBER"]
    add_table(wb = output_excel,
              sheet = ann_cumul_sheet,
              data = ann_cumul_out,
              title = paste0("Annual Cumulative Summary Statistics from ", start_year, "-", end_year),
              col = 1,
              row = 1,
              comment = paste0(annual_vol_function, "              ", annual_yield_function))
    
    # Add plots
    add_plot(wb = output_excel,
             sheet = ann_cumul_sheet,
             plot = results$Annual$Annual_Cumul_Volume_Stats_m3_Plot[[1]],
             title = paste0("Annual Total Volume from ", start_year, "-", end_year),
             col = ncol(ann_cumul_out) + 2,
             row = 2,
             height = 2,
             width = 6,
             comment = annual_vol_plot_function)
    add_plot(wb = output_excel,
             sheet = ann_cumul_sheet,
             plot = results$Annual$Annual_Cumul_Volume_Stats_m3_Plot[[2]],
             title = paste0("Seasonal (Two Seasons) Total Volume from ", start_year, "-", end_year),
             col = ncol(ann_cumul_out) + 2,
             row = 14,
             height = 3.5,
             width = 6,
             comment = annual_vol_plot_function)
    add_plot(wb = output_excel,
             sheet = ann_cumul_sheet,
             plot = results$Annual$Annual_Cumul_Volume_Stats_m3_Plot[[3]],
             title = paste0("Seasonal (Four Seasons) Total Volume from ", start_year, "-", end_year),
             col = ncol(ann_cumul_out) + 2,
             row = 33,
             height = 5,
             width = 6,
             comment = annual_vol_plot_function)
    add_plot(wb = output_excel,
             sheet = ann_cumul_sheet,
             plot = results$Annual$Annual_Cumul_Yield_Stats_mm_Plot[[1]],
             title = paste0("Annual Total Yield from ", start_year, "-", end_year),
             col = ncol(ann_cumul_out) + 2 + 10,
             row = 2,
             height = 2,
             width = 6,
             comment = annual_yield_plot_function)
    add_plot(wb = output_excel,
             sheet = ann_cumul_sheet,
             plot = results$Annual$Annual_Cumul_Yield_Stats_mm_Plot[[2]],
             title = paste0("Seasonal (Two Seasons) Total Yield from ", start_year, "-", end_year),
             col = ncol(ann_cumul_out) + 2 + 10,
             row = 14,
             height = 3.5,
             width = 6,
             comment = annual_yield_plot_function)
    add_plot(wb = output_excel,
             sheet = ann_cumul_sheet,
             plot = results$Annual$Annual_Cumul_Yield_Stats_mm_Plot[[3]],
             title = paste0("Seasonal (Four Seasons) Total Yield from ", start_year, "-", end_year),
             col = ncol(ann_cumul_out) + 2 + 10,
             row = 33,
             height = 5,
             width = 6,
             comment = annual_yield_plot_function)
    
    # Add worksheet
    ann_oth_sheet <- "Annual Stats Other"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = ann_oth_sheet,
                           tabColour = "#44e5e7")
    
    # Create fasstr function strings for output
    annual_lows_function <- paste0("calc_annual_lowflows(",
                                   fn_data,
                                   fn_wys,
                                   fn_startend,
                                   fn_exclude,
                                   fn_missing,
                                   ")")
    annual_lows_plot_function <- paste0("plot_annual_lowflows(",
                                        fn_data,
                                        fn_wys,
                                        fn_startend,
                                        fn_exclude,
                                        fn_missing,
                                        ")")
    annual_timing_function <- paste0("calc_annual_flow_timing(",
                                     fn_data,
                                     fn_wys,
                                     fn_startend,
                                     fn_exclude,
                                     ")")
    annual_timing_plot_function <- paste0("plot_annual_flow_timing(",
                                          fn_data,
                                          fn_wys,
                                          fn_startend,
                                          fn_exclude,
                                          ")")
    annual_norm_function <- paste0("calc_annual_outside_normal(",
                                   fn_data,
                                   fn_wys,
                                   fn_startend,
                                   fn_exclude,
                                   ")")
    annual_norm_plot_function <- paste0("plot_annual_outside_normal(",
                                        fn_data,
                                        fn_wys,
                                        fn_startend,
                                        fn_exclude,
                                        ")")
    
    # Add data tables
    ann_other_out <- dplyr::left_join(results$Annual$Annual_Low_Flows, 
                                      results$Annual$Annual_Flow_Timing, 
                                      by = c("STATION_NUMBER", "Year"))
    ann_other_out <- dplyr::left_join(ann_other_out, 
                                      results$Annual$Annual_Days_Outside_Normal, 
                                      by = c("STATION_NUMBER", "Year"))
    ann_other_out <- ann_other_out[,!colnames(ann_other_out) %in% "STATION_NUMBER"]
    add_table(wb = output_excel,
              sheet = ann_oth_sheet,
              data = ann_other_out,
              title = paste0("Other Annual Statistics (lowflows, etc) from ", start_year, "-", end_year),
              col = 1,
              row = 1,
              comment = paste0(annual_lows_function, "              ", annual_timing_function,
                               "              ", annual_norm_function))
    
    # Add plots
    add_plot(wb = output_excel,
             sheet = ann_oth_sheet,
             plot = results$Annual$Annual_Low_Flows_Plot[[1]],
             title = paste0("Annual Low-flows from ", start_year, "-", end_year),
             col = ncol(ann_other_out) + 2,
             row = 2,
             height = 5.5,
             width = 6,
             comment = annual_lows_plot_function)
    add_plot(wb = output_excel,
             sheet = ann_oth_sheet,
             plot = results$Annual$Annual_Low_Flows_Plot[[2]],
             title = paste0("Day of Annual Low-flows from ", start_year, "-", end_year),
             col = ncol(ann_other_out) + 2,
             row = 31,
             height = 5.5,
             width = 6,
             comment = annual_lows_plot_function)
    add_plot(wb = output_excel,
             sheet = ann_oth_sheet,
             plot = results$Annual$Annual_Flow_Timing_Plot[[1]],
             title = paste0("Annual Flow Timing from ", start_year, "-", end_year),
             col = ncol(ann_other_out) + 2 + 10,
             row = 2,
             height = 5.5,
             width = 6,
             comment = annual_timing_plot_function)
    add_plot(wb = output_excel,
             sheet = ann_oth_sheet,
             plot = results$Annual$Annual_Days_Outside_Normal_Plot[[1]],
             title = paste0("Annual Days Per Year Above, Below, and Outside Normal from ", start_year, "-", end_year),
             col = ncol(ann_other_out) + 2 + 10,
             row = 31,
             height = 4.5,
             width = 6,
             comment = annual_norm_plot_function)
    
    # Add fasstr functions to table
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_cumul_sheet,
                                       "Output" = "Annual Cumulative Volumes Table",
                                       "Function" = annual_vol_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_cumul_sheet,
                                       "Output" = "Annual Cumulative Yields Table",
                                       "Function" = annual_yield_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_cumul_sheet,
                                       "Output" = "Annual Cumulative Volumes Plots",
                                       "Function" = annual_vol_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_cumul_sheet,
                                       "Output" = "Annual Cumulative Yields Plots",
                                       "Function" = annual_yield_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_oth_sheet,
                                       "Output" = "Annual Low-Flows Table",
                                       "Function" = annual_lows_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_oth_sheet,
                                       "Output" = "Annual Low-Flows Plot",
                                       "Function" = annual_lows_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_oth_sheet,
                                       "Output" = "Annual Timing-of-Flows Table",
                                       "Function" = annual_timing_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_oth_sheet,
                                       "Output" = "Annual Timing-of-Flows Plot",
                                       "Function" = annual_timing_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_oth_sheet,
                                       "Output" = "Annual Days Outside Normal Table",
                                       "Function" = annual_norm_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = ann_oth_sheet,
                                       "Output" = "Annual Days Outside Normal Plot",
                                       "Function" = annual_norm_plot_function)
    
  }
  
  ### Monthly Stats
  ##########################
  
  if (4 %in% analyses) {
    
    # Add worksheet
    month_stat_sheet <- "Monthly Stats"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = month_stat_sheet,
                           tabColour = "#59d2fe")
    
    # Create fasstr function strings for output
    month_stats_function <- paste0("calc_monthly_stats(",
                                   fn_data,
                                   fn_wys,
                                   fn_startend,
                                   fn_exclude,
                                   fn_missing,
                                   ")")
    month_stats_plot_function <- paste0("plot_monthly_stats(",
                                        fn_data,
                                        fn_wys,
                                        fn_startend,
                                        fn_exclude,
                                        fn_missing,
                                        ")")
    
    
    # Add data tables
    mon_stats_out <- suppressWarnings(calc_monthly_stats(data = flow_data_raw,
                                                         start_year = start_year,
                                                         end_year = end_year,
                                                         exclude_years = exclude_years,
                                                         water_year_start = water_year_start,
                                                         ignore_missing = ignore_missing,
                                                         spread = TRUE))
    mon_stats_out <- mon_stats_out[,!colnames(mon_stats_out) %in% "STATION_NUMBER"]
    add_table(wb = output_excel,
              sheet = month_stat_sheet,
              data = mon_stats_out,
              title = paste0("Monthly Summary Statistics from ", start_year, "-", end_year),
              col = 1,
              row = 1,
              comment = month_stats_function)
    
    # Add plots
    add_plot(wb = output_excel,
             sheet = month_stat_sheet,
             plot = results$Monthly$Monthly_Summary_Stats_Plot[[1]],
             title = paste0("Monthly Mean Flows from ", start_year, "-", end_year),
             col = ncol(mon_stats_out) + 2,
             row = 2,
             height = 5,
             width = 9,
             comment = month_stats_plot_function)
    add_plot(wb = output_excel,
             sheet = month_stat_sheet,
             plot = results$Monthly$Monthly_Summary_Stats_Plot[[2]],
             title = paste0("Monthly Median Flows from ", start_year, "-", end_year),
             col = ncol(mon_stats_out) + 2,
             row = 29,
             height = 5,
             width = 9,
             comment = month_stats_plot_function)
    add_plot(wb = output_excel,
             sheet = month_stat_sheet,
             plot = results$Monthly$Monthly_Summary_Stats_Plot[[3]],
             title = paste0("Monthly Maximum Flows from ", start_year, "-", end_year),
             col = ncol(mon_stats_out) + 2 + 14,
             row = 2,
             height = 5,
             width = 9,
             comment = month_stats_plot_function)
    add_plot(wb = output_excel,
             sheet = month_stat_sheet,
             plot = results$Monthly$Monthly_Summary_Stats_Plot[[4]],
             title = paste0("Monthly Minimum Flows from ", start_year, "-", end_year),
             col = ncol(mon_stats_out) + 2 + 14,
             row = 29,
             height = 5,
             width = 9,
             comment = month_stats_plot_function)
    
    
    # Add worksheet
    month_cumul_sheet <- "Monthly Cumulative Stats"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = month_cumul_sheet,
                           tabColour = "#59d2fe")
    
    # Create fasstr function strings for output
    month_vol_function <- paste0("calc_monthly_cumulative_stats(",
                                 fn_data,
                                 fn_wys,
                                 fn_startend,
                                 fn_exclude,
                                 ")")
    month_vol_plot_function <- paste0("plot_monthly_cumulative_stats(",
                                      fn_data,
                                      fn_wys,
                                      fn_startend,
                                      fn_exclude,
                                      ")")
    month_yield_function <- paste0("calc_monthly_cumulative_stats(",
                                   fn_data,
                                   fn_wys,
                                   fn_startend,
                                   fn_exclude,
                                   ", use_yield = TRUE",
                                   fn_area,
                                   ")")
    month_yield_plot_function <- paste0("plot_monthly_cumulative_stats(",
                                        fn_data,
                                        fn_wys,
                                        fn_startend,
                                        fn_exclude,
                                        ", use_yield = TRUE",
                                        fn_area,
                                        ")")
    
    # Add data tables
    mon_vol_out <- results$Monthly$Monthly_Total_Cumul_Volume_m3[,!colnames(results$Monthly$Monthly_Total_Cumul_Volume_m3) %in% "STATION_NUMBER"]
    mon_vol_out <- mon_vol_out[,!colnames(mon_vol_out) %in% "STATION_NUMBER"]
    mon_vol_out <- tidyr::gather(mon_vol_out, Statistic, Value, -1)
    mon_vol_out <- dplyr::mutate(mon_vol_out,
                                 Statistic = paste0(Statistic, "_Volume_m3"),
                                 Statistic = factor(Statistic, levels = unique(Statistic)))
    mon_vol_out <- tidyr::spread(mon_vol_out, Month, Value)
    
    mon_yield_out <- results$Monthly$Monthly_Total_Cumul_Yield_mm[,!colnames(results$Monthly$Monthly_Total_Cumul_Yield_mm) %in% "STATION_NUMBER"]
    mon_yield_out <- mon_yield_out[,!colnames(mon_yield_out) %in% "STATION_NUMBER"]
    mon_yield_out <- tidyr::gather(mon_yield_out, Statistic, Value, -1)
    mon_yield_out <- dplyr::mutate(mon_yield_out,
                                   Statistic = paste0(Statistic, "_Yield_mm"),
                                   Statistic = factor(Statistic, levels = unique(Statistic)))
    mon_yield_out <- tidyr::spread(mon_yield_out, Month, Value)
    
    mon_cumul_out <- suppressWarnings(dplyr::bind_rows(mon_vol_out, mon_yield_out))
    
    add_table(wb = output_excel,
              sheet = month_cumul_sheet,
              data = mon_cumul_out,
              title = paste0("Monthly Cumulative Summary Statistics from ", start_year, "-", end_year),
              col = 1,
              row = 1,
              comment = paste0(month_vol_function, "              ", month_yield_function))
    
    # Add plots 
    add_plot(wb = output_excel,
             sheet = month_cumul_sheet,
             plot = results$Monthly$Monthly_Total_Cumul_Volume_m3_Plot[[1]],
             title = paste0("Cumulative Monthly Volumetric Flows from ", start_year, "-", end_year),
             col = ncol(mon_cumul_out) + 2,
             row = 2,
             height = 4,
             width = 9,
             comment = month_vol_plot_function)
    add_plot(wb = output_excel,
             sheet = month_cumul_sheet,
             plot = results$Monthly$Monthly_Total_Cumul_Yield_mm_Plot[[1]],
             title = paste0("Cumulative Monthly Yield from ", start_year, "-", end_year),
             col = ncol(mon_cumul_out) + 2,
             row = 24,
             height = 4,
             width = 9,
             comment = month_yield_plot_function)
    
    
    # Add fasstr functions to table
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = month_stat_sheet,
                                       "Output" = "Monthly Summary Statistics Table",
                                       "Function" = month_stats_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = month_stat_sheet,
                                       "Output" = "Monthly Summary Statistics Plot",
                                       "Function" = month_stats_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = month_cumul_sheet,
                                       "Output" = "Monthly Cumulative Volumes Table",
                                       "Function" = month_vol_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = month_cumul_sheet,
                                       "Output" = "Monthly Cumulative Volumes Plot",
                                       "Function" = month_vol_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = month_cumul_sheet,
                                       "Output" = "Monthly Cumulative Yields Table",
                                       "Function" = month_yield_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = month_cumul_sheet,
                                       "Output" = "Monthly Cumulative Yields Plot",
                                       "Function" = month_yield_plot_function)
  }
  
  ### Daily Stats
  ##########################
  
  if (5 %in% analyses) {
    
    # Create folder for plots if it doesn't exist
    plot_dir <- paste0(file_name, " Plots/")
    if (!dir.exists(plot_dir)) {
      message(paste0("** creating folder '", plot_dir ,"' for additional plots"))
    }
    dir.create(path = plot_dir, showWarnings = FALSE)
    
    # Add worksheet
    day_stats_sheet <- "Daily Stats"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = day_stats_sheet,
                           tabColour = "#4a8fe7")
    
    # Create fasstr function strings for output
    daily_stats_function <- paste0("calc_daily_stats(",
                                   fn_data,
                                   fn_wys,
                                   fn_startend,
                                   fn_exclude,
                                   fn_missing,
                                   ")")
    daily_stats_plot_function <- paste0("plot_daily_stats(",
                                        fn_data,
                                        fn_wys,
                                        fn_startend,
                                        fn_exclude,
                                        fn_missing,
                                        ")")
    
    # Add data tables
    day_stats_out <- results$Daily$Daily_Summary_Stats[,!colnames(results$Daily$Daily_Summary_Stats) %in% "STATION_NUMBER"]
    add_table(wb = output_excel,
              sheet = day_stats_sheet,
              data = day_stats_out,
              title = paste0("Daily Summary Statistics from ", start_year, "-", end_year),
              col = 1,
              row = 1,
              comment = daily_stats_function)
    
    # Add plots 
    add_plot(wb = output_excel,
             sheet = day_stats_sheet,
             plot = results$Daily$Daily_Summary_Stats_Plot[[1]],
             title = paste0("Daily Summary Statistics from ", start_year, "-", end_year),
             col = ncol(day_stats_out) + 2,
             row = 2,
             height = 5,
             width = 10,
             comment = daily_stats_plot_function)
    
    # Write plots
    if (plot_filetype == "pdf") {
      message("** writing 'Daily_Statistics_with_Years.pdf'")
    } else {
      message(paste0("** writing .", plot_filetype, " plots in 'Daily_Statistics_with_Years' folder"))
    }
    
    suppressWarnings(
      suppressMessages(
        write_plots(plots = results$Daily$Daily_Summary_Stats_with_Years,
                    folder_name = paste0(plot_dir, "Daily_Statistics_with_Years"),
                    plot_filetype = plot_filetype,
                    width = 8.5,
                    height = 4,
                    combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
      ))
    
    
    # Add worksheet
    day_cumul_sheet <- "Daily Cumulative Stats"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = day_cumul_sheet,
                           tabColour = "#4a8fe7")
    
    # Create fasstr function strings for output
    daily_vol_function <- paste0("calc_daily_cumulative_stats(",
                                 fn_data,
                                 fn_wys,
                                 fn_startend,
                                 fn_exclude,
                                 ")")
    daily_vol_plot_function <- paste0("plot_daily_cumulative_stats(",
                                      fn_data,
                                      fn_wys,
                                      fn_startend,
                                      fn_exclude,
                                      ")")
    daily_yield_function <- paste0("calc_daily_cumulative_stats(",
                                   fn_data,
                                   fn_wys,
                                   fn_startend,
                                   fn_exclude,
                                   ", use_yield = TRUE",
                                   fn_area,
                                   ")")
    daily_yield_plot_function <- paste0("plot_daily_cumulative_stats(",
                                        fn_data,
                                        fn_wys,
                                        fn_startend,
                                        fn_exclude,
                                        ", use_yield = TRUE",
                                        fn_area,
                                        ")")
    
    # Add data tables
    day_vol_out <- results$Daily$Daily_Total_Cumul_Volume_m3[,!colnames(results$Daily$Daily_Total_Cumul_Volume_m3) %in% "STATION_NUMBER"]
    day_vol_out <- day_vol_out[,!colnames(day_vol_out) %in% "STATION_NUMBER"]
    day_vol_out <- tidyr::gather(day_vol_out, Statistic, Value, -(1:2))
    day_vol_out <- dplyr::mutate(day_vol_out, Statistic = paste0(Statistic, "_Volume_m3"))
    order <- unique(day_vol_out$Statistic)
    day_vol_out <- tidyr::spread(day_vol_out, Statistic, Value)
    day_vol_out <- dplyr::select(day_vol_out, Date, DayofYear, order)
    day_vol_out <- dplyr::arrange(day_vol_out, DayofYear)
    
    day_yield_out <- results$Daily$Daily_Total_Cumul_Yield_mm[,!colnames(results$Daily$Daily_Total_Cumul_Yield_mm) %in% "STATION_NUMBER"]
    day_yield_out <- day_yield_out[,!colnames(day_yield_out) %in% "STATION_NUMBER"]
    day_yield_out <- tidyr::gather(day_yield_out, Statistic, Value, -(1:2))
    day_yield_out <- dplyr::mutate(day_yield_out, Statistic = paste0(Statistic, "_Yield_mm"))
    order <- unique(day_yield_out$Statistic)
    day_yield_out <- tidyr::spread(day_yield_out, Statistic, Value)
    day_yield_out <- dplyr::select(day_yield_out, Date, DayofYear, order)
    day_yield_out <- dplyr::arrange(day_yield_out, DayofYear)
    
    day_cumul_out <- dplyr::left_join(day_vol_out, day_yield_out, by = c("Date", "DayofYear"))
    
    add_table(wb = output_excel,
              sheet = day_cumul_sheet,
              data = day_cumul_out,
              title = paste0("Daily Cumulative Summary Statistics from ", start_year, "-", end_year),
              col = 1,
              row = 1,
              comment = paste0(daily_vol_function, "              ", daily_yield_function))
    
    # Add plots 
    add_plot(wb = output_excel,
             sheet = day_cumul_sheet,
             plot = results$Daily$Daily_Total_Cumul_Volume_m3_Plot[[1]],
             title = paste0("Cumulative Daily Volumetric Flows from ", start_year, "-", end_year),
             col = ncol(day_cumul_out) + 2,
             row = 2,
             height = 4,
             width = 9,
             comment = daily_vol_plot_function)
    add_plot(wb = output_excel,
             sheet = day_cumul_sheet,
             plot = results$Daily$Daily_Total_Cumul_Yield_mm_Plot[[1]],
             title = paste0("Cumulative Daily Yield from ", start_year, "-", end_year),
             col = ncol(day_cumul_out) + 2,
             row = 24,
             height = 4,
             width = 9,
             comment = daily_yield_plot_function)
    
    # Write plots
    if (plot_filetype == "pdf") {
      message("** writing 'Daily_Cumulative_Volume_with_Years.pdf'")
    } else {
      message(paste0("** writing .", plot_filetype, " plots in 'Daily_Cumulative_Volume_with_Years' folder"))
    }
    
    suppressWarnings(
      suppressMessages(
        write_plots(plots = results$Daily$Daily_Total_Cumul_Volume_m3_with_Years,
                    folder_name = paste0(plot_dir, "Daily_Cumulative_Volume_with_Years"),
                    plot_filetype = plot_filetype,
                    width = 8.5,
                    height = 4,
                    combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
      ))
    
    if (plot_filetype == "pdf") {
      message("** writing 'Daily_Cumulative_Yield_with_Years.pdf'")
    } else {
      message(paste0("** writing .", plot_filetype, " plots in 'Daily_Cumulative_Yield_with_Years' folder"))
    }
    
    suppressWarnings(
      suppressMessages(
        write_plots(plots = results$Daily$Daily_Total_Cumul_Yield_mm_with_Years,
                    folder_name = paste0(plot_dir, "Daily_Cumulative_Yield_with_Years"),
                    plot_filetype = plot_filetype,
                    width = 8.5,
                    height = 4,
                    combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
      ))
    
    # Add fasstr functions to table
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = day_stats_sheet,
                                       "Output" = "Daily Summary Statistics Table",
                                       "Function" = daily_stats_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = day_stats_sheet,
                                       "Output" = "Daily Summary Statistics Plot",
                                       "Function" = daily_stats_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = day_stats_sheet,
                                       "Output" = "Daily Summary Statistics Plot with Year",
                                       "Function" =  paste0("plot_daily_stats(",
                                                            fn_data,
                                                            fn_wys,
                                                            fn_startend,
                                                            fn_exclude,
                                                            fn_missing,
                                                            ", include_year = ", start_year,
                                                            ")"))
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = day_cumul_sheet,
                                       "Output" = "Daily Cumulative Volumes Table",
                                       "Function" = daily_vol_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = day_cumul_sheet,
                                       "Output" = "Daily Cumulative Volumes Plot",
                                       "Function" = daily_vol_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = day_cumul_sheet,
                                       "Output" = "Daily Cumulative Volumes Plot with Year",
                                       "Function" = paste0("plot_daily_cumulative_stats(",
                                                           fn_data,
                                                           fn_wys,
                                                           fn_startend,
                                                           fn_exclude,
                                                           ", include_year = ", start_year,
                                                           ")"))
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = day_cumul_sheet,
                                       "Output" = "Daily Cumulative Yields Table",
                                       "Function" = daily_yield_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = day_cumul_sheet,
                                       "Output" = "Daily Cumulative Yields Plot",
                                       "Function" = daily_yield_plot_function)
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = day_cumul_sheet,
                                       "Output" = "Daily Cumulative Yields Plot with Year",
                                       "Function" = paste0("plot_daily_cumulative_stats(",
                                                           fn_data,
                                                           fn_wys,
                                                           fn_startend,
                                                           fn_exclude,
                                                           ", use_yield = TRUE",
                                                           fn_area,
                                                           ", include_year = ", start_year,
                                                           ")"))
  }
  
  # ### Annual Trending
  ##########################
  
  if (6 %in% analyses) {
    
    # Create folder for plots if it doesn't exist
    plot_dir <- paste0(file_name, " Plots/")
    if (!dir.exists(plot_dir)) {
      message(paste0("** creating folder '", plot_dir ,"' for additional plots"))
    }
    dir.create(path = plot_dir, showWarnings = FALSE)
    
    # Add worksheet
    trends_sheet <- "Annual Trends"
    openxlsx::addWorksheet(wb = output_excel,
                           sheetName = trends_sheet,
                           tabColour = "#5c7aff")
    
    # Create fasstr function strings for output
    trends_function <- paste0("compute_annual_trends(",
                              fn_data,
                              fn_wys,
                              fn_startend,
                              fn_exclude,
                              fn_area,
                              ", zyp_method = '", zyp_method, "'",
                              ", zyp_alpha = ", zyp_alpha,
                              fn_missing,
                              ")")
    
    # Add data tables
    trends_out <- dplyr::left_join(results$Trending$Annual_Trends_Results,
                                   results$Trending$Annual_Trends_Data, 
                                   by = c("STATION_NUMBER", "Statistic"))
    trends_out <- trends_out[,!colnames(trends_out) %in% "STATION_NUMBER"]
    add_table(wb = output_excel,
              sheet = trends_sheet,
              data = trends_out,
              title = paste0("Annual Trending Statistics from ", start_year, "-", end_year),
              col = 1,
              row = 1,
              comment = trends_function)
    
    # Write plots
    if (plot_filetype == "pdf") {
      message("** writing 'Annual_Trends_Results_Plots.pdf'")
    } else {
      message(paste0("** writing .", plot_filetype, " plots in 'Annual_Trends_Results_Plots' folder"))
    }
    
    suppressMessages(
      write_plots(plots = results$Trending$Annual_Trends_Plots,
                  folder_name = paste0(plot_dir , "Annual_Trends_Results_Plots"),
                  plot_filetype = plot_filetype,
                  width = 8.5,
                  height = 3,
                  combined_pdf = ifelse(plot_filetype == "pdf", TRUE, FALSE))
    )
    
    # Add fasstr functions to table
    fasstr_functions <- dplyr::add_row(fasstr_functions,
                                       "Worksheet" = trends_sheet,
                                       "Output" = "Annual Trends Tables and Plots",
                                       "Function" = trends_function)
    
  }
  
  ### Low Flow Frequency
  ##########################
  
  if (7 %in% analyses) {
    
    # Check if there is sufficient data for frequency analysis
    data_check <- suppressWarnings(
      calc_annual_lowflows(data = flow_data_raw,
                           start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                           water_year_start = water_year_start,
                           ignore_missing = ignore_missing)
    )
    data_check <- dplyr::select(data_check, Min_1_Day, Min_3_Day, Min_7_Day, Min_30_Day)
    
    if (any(as.numeric(colSums(!is.na(data_check))) < 3)) {
      warning("Not enough annual data (3 years) for frequency analysis. Consider filtering for appropriate years or use ignore_missing = TRUE,",
              call. = FALSE)
      
    } else {
      
      # Add worksheet
      freq_sheet <- "Low-flow Frequencies"
      openxlsx::addWorksheet(wb = output_excel,
                             sheetName = freq_sheet,
                             tabColour = "#5c7aff")
      
      # Create fasstr function strings for output
      frequency_function <- paste0("compute_annual_frequencies(",
                                   fn_data,
                                   fn_wys,
                                   fn_startend,
                                   fn_exclude,
                                   ", roll_days = c(1,3,7,30,60)",
                                   ", prob_plot_position = 'weibull'",
                                   ", fit_distr = 'PIII'",
                                   ", fit_distr_method = 'MOM'",
                                   fn_missing,
                                   ")")
      
      # Add data tables
      freq_ann_data_out <- tidyr::spread(results$Lowflow_Frequencies$Freq_Analysis_Data, Measure, Value)
      freq_ann_data_out <- freq_ann_data_out[,!colnames(freq_ann_data_out) %in% "STATION_NUMBER"]
      add_table(wb = output_excel,
                sheet = freq_sheet,
                data = freq_ann_data_out,
                title = paste0("Annual Low-flow Values from ", start_year, "-", end_year),
                col = 1,
                row = 1,
                comment = paste0(frequency_function, "[[1]]"))
      freq_plot_data_out <- results$Lowflow_Frequencies$Freq_Plot_Data[,!colnames(results$Lowflow_Frequencies$Freq_Plot_Data) %in% "STATION_NUMBER"]
      add_table(wb = output_excel,
                sheet = freq_sheet,
                data = freq_plot_data_out,
                title = paste0("Plotting Data"),
                col = ncol(freq_ann_data_out) + 2,
                row = 1,
                comment = paste0(frequency_function, "[[2]]"))
      freq_quantiles_out <- results$Lowflow_Frequencies$Freq_Fitted_Quantiles[,!colnames(results$Lowflow_Frequencies$Freq_Fitted_Quantiles) %in% "STATION_NUMBER"]
      add_table(wb = output_excel,
                sheet = freq_sheet,
                data = freq_quantiles_out,
                title = paste0("Fitted Quantiles"),
                col = ncol(freq_ann_data_out) + 2 + ncol(freq_plot_data_out) + 1 + 11,
                row = 1,
                comment = paste0(frequency_function, "[[5]]"))
      
      # Add plots
      add_plot(wb = output_excel,
               sheet = freq_sheet,
               plot = results$Lowflow_Frequencies$Freq_Plot,
               title = paste0("Frequency Plot ", start_year, "-", end_year),
               col = ncol(freq_ann_data_out) + 2 + ncol(freq_plot_data_out) + 1,
               row = 1,
               height = 5,
               width = 7,
               comment = paste0(frequency_function, "[[3]]"))
      
      # Add fasstr functions to table
      fasstr_functions <- dplyr::add_row(fasstr_functions,
                                         "Worksheet" = freq_sheet,
                                         "Output" = "Low-Flow Frequency Analysis Tables and Plots",
                                         "Function" = frequency_function)
    }
  }
  
  # Write the analysis information
  ##########################
  
  ## Create a meta data file of the analysis arguments
  
  metadata <- list(data = ifelse(!is.null(data), as.character(substitute(data)), ""),
                   dates = as.character(substitute(Date)),
                   values = as.character(substitute(Value)),
                   groups = as.character(substitute(STATION_NUMBER)),
                   station_number = ifelse(!is.null(station_number), toupper(station_number), ""),
                   basin_area = basin_area_stn,
                   water_year_start = water_year_start,
                   start_year = start_year,
                   end_year = end_year,
                   exclude_years = paste0(ifelse(!is.null(exclude_years),
                                                 paste0(ifelse(length(exclude_years) == 1, 
                                                               paste(exclude_years),
                                                               paste(exclude_years, collapse = ", "))),
                                                 "")),
                   ignore_missing = ignore_missing,
                   zyp_method = zyp_method,
                   zyp_alpha = ifelse(!is.na(zyp_alpha), zyp_alpha, ""),
                   analyses = analyses,
                   file_name = paste0(file_name, ".xlsx"),
                   plot_filetype = plot_filetype,
                   analysis_function = analysis_function,
                   analysis_date = as.character(Sys.time()))
  metadata <- data.frame("Argument" = names(metadata),
                         "Option" = as.character(unname(metadata)))
  metadata <- metadata[c(1,5,2:4,6:nrow(metadata)),]
  
  add_table(wb = output_excel,
            sheet = overview_sheet,
            data = metadata,
            title = paste0("Analysis Overview"),
            col = 1,
            row = 1)
  
  
  
  
  if (!is.null(station_number)) {
    
    hydat_info <- dplyr::left_join(tidyhydat::hy_stations(station_number),
                                   tidyhydat::hy_stn_regulation(station_number),
                                   by = "STATION_NUMBER")
    hydat_info <- dplyr::select(hydat_info, STATION_NUMBER, STATION_NAME, PROV_TERR_STATE_LOC, HYD_STATUS,
                                LATITUDE, LONGITUDE, DRAINAGE_AREA_GROSS, REGULATED, RHBN, REAL_TIME)
    hydat_info <- dplyr::mutate(hydat_info, HYDAT_VERSION =  as.character(dplyr::pull(tidyhydat::hy_version()[,2])))
    hydat_info <- tidyr::gather(hydat_info, Info, Value)
    
    add_table(wb = output_excel,
              sheet = overview_sheet,
              data = hydat_info,
              title = paste0("HYDAT Station Information"),
              col = 4,
              row = 1)
  }
  
  
  # Write the fasstr functions
  ##########################
  
  fasstr_sheet <- "fasstr Functions"
  openxlsx::addWorksheet(wb = output_excel, 
                         sheetName = fasstr_sheet,
                         tabColour = "#003e1f")
  add_table(wb = output_excel,
            sheet = fasstr_sheet,
            data = fasstr_functions,
            title = paste0("fasstr Functions"),
            col = 1,
            row = 1,
            comment = "Copy and paste a function into R to reproduce or further customize  results")
  openxlsx::setColWidths(wb = output_excel, sheet = fasstr_sheet, cols = 1:3, widths = "auto")
  
  
  
  
  # Save the workbook
  ##########################
  
  message(paste0("** writing analysis results in '", file_name, ".xlsx'"))
  
  openxlsx::saveWorkbook(wb = output_excel, 
                         file = paste0(file_name, ".xlsx"),
                         overwrite = TRUE)
  
  if (5 %in% analyses | 6 %in% analyses) {
    message(paste0("* DONE. For analysis results go to: '", normalizePath(paste0(file_name, ".xlsx")),
                   "' and '", normalizePath(plot_dir), "' folder"))
  } else {
    message(paste0("* DONE. For analysis results go to: '", normalizePath(paste0(file_name, ".xlsx"))))
  }
  
  
}