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

#' @title Compute a suite of tables and plots from various fasstr functions
#'
#' @description Calculates tables and plots from a suite of statistics from fasstr functions. The statistics are grouped into
#'    7 analysis groups (see 'analyses' argument) which are stored in lists in the object. Due to the number of tables and plots to 
#'    be made, this function may take several minutes to complete. If using \code{ignore_missing = FALSE} (default) and there is
#'    missing data, some tables and plots may be empty and produce warnings. Use \code{ignore_missing = TRUE} to ignore the missing
#'    values or filter your data to complete years.
#'    
#' @inheritParams compute_annual_trends
#' @param analyses Numeric vector of analyses to run (default is all (\code{1:7})):
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
#'    Only required if analysis group 6 is included. Default \code{"yuepilon"}.
#' 
#' @return A list of lists of tibble data frames and ggplot2 objects from various fasstr functions
#'    organized by the analysis groups as listed above.
#'    
#' @seealso \code{\link{plot_flow_data}},
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
#' # Compute a full analysis will all the analyses
#' compute_full_analysis(station_number = "08NM116",
#'                      start_year = 1980,
#'                      end_year = 2010)
#' 
#' # Compute a full analysis with only Annual and Daily analyses
#' compute_full_analysis(station_number = "08NM116",
#'                      start_year = 1980,
#'                      end_year = 2010,
#'                      analyses = c(3,5))
#' }
#' @export


compute_full_analysis <- function(data,
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
                                  zyp_alpha){
  
  
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
  
  
  if (!is.numeric(analyses)) 
    stop("analyses argument must be numbers between 1 and 7. See ?compute_full_analysis for analysis group numbers.", call. = FALSE)
  if (!all(analyses %in% 1:7)) 
    stop("analyses argument must be numbers between 1 and 7. See ?compute_full_analysis for analysis group numbers.", call. = FALSE)
  
  if (6 %in% analyses) {
    zyp_alpha_checks(zyp_alpha)
    zyp_method_checks(zyp_method)
  }
  
  # Do this for now, until looping of include_year plots is sorted out
  if (length(station_number) > 1) stop("Only one station_number can be listed.", call. = FALSE)
  
  message("* this may take a few moments...")
  
  ## FLOW DATA CHECKS AND FORMATTING
  ##########################
  
  # Check if data is provided and import it
  flow_data_raw <- flowdata_import(data = data, 
                                   station_number = station_number)
  
  # Save the original columns (to check for STATION_NUMBER col at end) and ungroup if necessary
  orig_cols <- names(flow_data_raw)
  flow_data_raw <- dplyr::ungroup(flow_data_raw)
  
  # Check and rename columns
  flow_data_unfiltered <- format_all_cols(data = flow_data_raw,
                                          dates = as.character(substitute(dates)),
                                          values = as.character(substitute(values)),
                                          groups = as.character(substitute(groups)),
                                          rm_other_cols = TRUE)
  
  if (all(flow_data_unfiltered$STATION_NUMBER == "XXXXXXX")) {
    flow_data_unfiltered <- dplyr::select(flow_data_unfiltered, -STATION_NUMBER)
  }
  
  # Data setup
  flow_data_unfiltered <- fill_missing_dates(data = flow_data_unfiltered, water_year_start = water_year_start)
  flow_data_unfiltered <- add_date_variables(data = flow_data_unfiltered, water_year_start = water_year_start)
  flow_data_unfiltered <- add_rolling_means(data = flow_data_unfiltered)
  
  # Set up basin_area
  flow_data_unfiltered <- add_basin_area(flow_data_unfiltered, basin_area = basin_area)
  basin_area_stn <- unique(flow_data_unfiltered$Basin_Area_sqkm)[1]
  
  # Get the start and end years of the data to make a list of all included years
  flow_data <- flow_data_unfiltered
  
  flow_data <- analysis_prep(data = flow_data,
                             water_year_start = water_year_start)
  if (start_year < min(flow_data$WaterYear)) {
    start_year <- min(flow_data$WaterYear)
  }
  if (end_year > max(flow_data$WaterYear)) {
    end_year <- max(flow_data$WaterYear)
  }
  years_list <- seq(from = start_year, to = end_year, by = 1)[!(seq(from = start_year, to = end_year, by = 1) %in% exclude_years)]
  
  flow_data_plus <- dplyr::filter(flow_data, WaterYear >= start_year - 1 & WaterYear <= end_year + 1)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  
  ### Results Setup
  ##########################
  
  # Create list of all objects
  
  all_objects <- list()
  
  message("** creating data frames and plots")
  
  ## Screening
  ##########################
  
  if (1 %in% analyses) {
    
    # Time series plot
    ts_full_plot <- suppressWarnings(plot_flow_data(data = flow_data,
                                                    exclude_years = exclude_years,
                                                    water_year_start = water_year_start))
    
    # Data screening
    flow_screening = screen_flow_data(data = flow_data,
                                      water_year_start = water_year_start)
    
    
    # Screening plots
    ts_screen_plot <- plot_data_screening(data = flow_data,
                                          start_year = start_year, end_year = end_year,
                                          water_year_start = water_year_start)
    
    # Missing dates plots
    ts_missing_plot <- plot_missing_dates(data = flow_data,
                                          start_year = start_year, end_year = end_year,
                                          water_year_start = water_year_start)
    
    # Add to objects list
    all_objects <- append(all_objects,
                          list("Screening" = list("Daily_Flows" = flow_data,
                                                  "Daily_Flows_Plot" = ts_full_plot,
                                                  "Flow_Screening" = flow_screening,
                                                  "Flow_Screening_Plot" = ts_screen_plot,
                                                  "Missing_Dates_Plot" = ts_missing_plot)))
    
    
  }  
  
  
  ### Long-term Stats
  ##########################
  
  if (2 %in% analyses) {
    
    # Long-term stats with percentiles
    lt_mon_stats <- calc_longterm_monthly_stats(data = flow_data,
                                                exclude_years = exclude_years,
                                                water_year_start = water_year_start,
                                                percentiles = seq(5, 95, by = 5),
                                                transpose = TRUE,
                                                ignore_missing = ignore_missing)
    
    
    # Long-term stats plot
    lt_mon_stats_plot <- plot_longterm_monthly_stats(data = flow_data,
                                                     exclude_years = exclude_years,
                                                     water_year_start = water_year_start,
                                                     ignore_missing = ignore_missing)
    
    # Long-term stats with percentiles
    lt_stats <- calc_longterm_daily_stats(data = flow_data,
                                          exclude_years = exclude_years,
                                          water_year_start = water_year_start,
                                          percentiles = 1:99,
                                          transpose = TRUE,
                                          ignore_missing = ignore_missing)
    
    
    # Long-term stats plot
    lt_stats_plot <- plot_longterm_daily_stats(data = flow_data,
                                               exclude_years = exclude_years,
                                               water_year_start = water_year_start,
                                               ignore_missing = ignore_missing)
    
    
    # Flow duration plot
    lt_flowduration_plot <- plot_flow_duration(data = flow_data,
                                               exclude_years = exclude_years,
                                               water_year_start = water_year_start,
                                               ignore_missing = ignore_missing)
    
    # Add to objects list
    all_objects <- append(all_objects,    
                          list("Longterm" = list("Longterm_Monthly_Summary_Stats_Percentiles" = lt_mon_stats,
                                                 "Longterm_Monthly_Summary_Stats_Plot" = lt_mon_stats_plot,
                                                 "Longterm_Daily_Summary_Stats_Percentiles" = lt_stats,
                                                 "Longterm_Daily_Summary_Stats_Plot" = lt_stats_plot,
                                                 "Flow_Duration_Curves" = lt_flowduration_plot)))
    
  }
  
  
  ### Annual Stats
  ##########################
  
  if (3 %in% analyses) {
    
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
                                              basin_area = basin_area_stn)
    
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
    
    # Create each of the annual stats plots
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
                                                   basin_area = basin_area_stn)
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
    
    # Add to objects list
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
                                               "Annual_Means_Plot" =  ann_means_plot)))
    
  }
  
  ### Monthly Stats
  ##########################
  
  if (4 %in% analyses) {
    
    # Create all the monthly stats
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
                                               basin_area = basin_area_stn)
    
    # Monthly stats plots
    mon_stats_plot <- plot_monthly_stats(data = flow_data,
                                         exclude_years = exclude_years,
                                         water_year_start = water_year_start,
                                         ignore_missing = ignore_missing)
    
    # Monthly cumulative plots
    mon_vol_plot <- plot_monthly_cumulative_stats(data = flow_data,
                                                  exclude_years = exclude_years,
                                                  water_year_start = water_year_start)
    mon_yield_plot <- plot_monthly_cumulative_stats(data = flow_data,
                                                    exclude_years = exclude_years,
                                                    water_year_start = water_year_start,
                                                    use_yield = TRUE,
                                                    basin_area = basin_area_stn)
    
    # Add to objects list
    all_objects <- append(all_objects,    
                          list("Monthly" = list("Monthly_Summary_Stats" = mon_stats,
                                                "Monthly_Summary_Stats_Plot" = mon_stats_plot,
                                                "Monthly_Total_Cumul_Volume_m3" = mon_vol,
                                                "Monthly_Total_Cumul_Volume_m3_Plot" = mon_vol_plot,
                                                "Monthly_Total_Cumul_Yield_mm" = mon_yield,
                                                "Monthly_Total_Cumul_Yield_mm_Plot" = mon_yield_plot)))
    
  }
  
  ### Daily Stats
  ##########################
  
  if (5 %in% analyses) {
    
    # Daily stats
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
                                             basin_area = basin_area_stn)
    
    # Daily stats plots
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
                                                  basin_area = basin_area_stn)
    
    # Create the daily stats plots
    daily_plots_table <- dplyr::group_by(flow_data, WaterYear)
    daily_plots_table <- tidyr::nest(daily_plots_table)
    daily_plots_table <- dplyr::filter(daily_plots_table, !(WaterYear %in% exclude_years))
    daily_plots_table <- dplyr::mutate(daily_plots_table,
                                       plot = purrr::map(WaterYear, 
                                                         ~suppressWarnings(
                                                           suppressMessages(
                                                             plot_daily_stats(
                                                               data = flow_data,
                                                               start_year = start_year,
                                                               end_year = end_year,
                                                               water_year_start = water_year_start,
                                                               exclude_years = exclude_years,
                                                               ignore_missing = ignore_missing,
                                                               include_year = .)[[1]]))))
    day_stats_year_plots <- daily_plots_table$plot
    names(day_stats_year_plots) <- paste0(daily_plots_table$WaterYear, "_Daily_Statistics")
    
    daily_vol_table <- dplyr::group_by(flow_data, WaterYear)
    daily_vol_table <- tidyr::nest(daily_vol_table)
    daily_vol_table <- dplyr::filter(daily_vol_table, !(WaterYear %in% exclude_years))
    daily_vol_table <- dplyr::mutate(daily_vol_table,
                                     plot = purrr::map(WaterYear, 
                                                       ~suppressWarnings(
                                                         suppressMessages(
                                                           plot_daily_cumulative_stats(
                                                             data = flow_data,
                                                             start_year = start_year,
                                                             end_year = end_year,
                                                             water_year_start = water_year_start,
                                                             exclude_years = exclude_years,
                                                             include_year = .)[[1]]))))
    day_vol_year_plots <- daily_vol_table$plot
    names(day_vol_year_plots) <- paste0(daily_vol_table$WaterYear, "_Daily_Cumulative_Volumetric_Stats")
    
    daily_yield_table <- dplyr::group_by(flow_data, WaterYear)
    daily_yield_table <- tidyr::nest(daily_yield_table)
    daily_yield_table <- dplyr::filter(daily_yield_table, !(WaterYear %in% exclude_years))
    daily_yield_table <- dplyr::mutate(daily_yield_table,
                                       plot = purrr::map(WaterYear, 
                                                         ~suppressWarnings(
                                                           suppressMessages(
                                                             plot_daily_cumulative_stats(
                                                               data = flow_data,
                                                               start_year = start_year,
                                                               end_year = end_year,
                                                               water_year_start = water_year_start,
                                                               exclude_years = exclude_years,
                                                               use_yield = TRUE, 
                                                               basin_area = basin_area_stn,
                                                               include_year = .)[[1]]))))
    day_yield_year_plots <- daily_yield_table$plot
    names(day_yield_year_plots) <- paste0(daily_yield_table$WaterYear, "_Daily_Cumulative_Yield_Stats")
    
    # Add to objects list
    all_objects <- append(all_objects,    
                          list("Daily" = list("Daily_Summary_Stats" = day_stats,
                                              "Daily_Summary_Stats_Plot" = day_stats_plot,
                                              "Daily_Summary_Stats_with_Years" = day_stats_year_plots,
                                              "Daily_Total_Cumul_Volume_m3" = day_vol,
                                              "Daily_Total_Cumul_Volume_m3_Plot" = day_vol_plot,
                                              "Daily_Total_Cumul_Yield_mm" = day_yield,
                                              "Daily_Total_Cumul_Yield_mm_Plot" = day_yield_plot,
                                              "Daily_Total_Cumul_Volume_m3_with_Years" = day_vol_year_plots,
                                              "Daily_Total_Cumul_Yield_mm_with_Years" = day_yield_year_plots
                          )))
    
  }
  
  # ### Annual Trending
  ##########################
  
  if (6 %in% analyses) {
    
    # Trends results
    ann_trends <- compute_annual_trends(data = flow_data_plus,
                                        start_year = start_year, end_year = end_year,
                                        exclude_years = exclude_years,
                                        water_year_start = water_year_start,
                                        ignore_missing = ignore_missing,
                                        basin_area = basin_area_stn,
                                        zyp_method = zyp_method,
                                        zyp_alpha = zyp_alpha)
    ann_data <- ann_trends$Annual_Trends_Data
    ann_results <- ann_trends$Annual_Trends_Results
    ann_trends_plots <- ann_trends[c(3:length(names(ann_trends)))]
    
    # Add to objects list
    all_objects <- append(all_objects,    
                          list("Trending" = list("Annual_Trends_Data" = ann_data,
                                                 "Annual_Trends_Results" = ann_results,
                                                 "Annual_Trends_Plots" = ann_trends_plots)))
    
  }
  
  ### Low Flow Frequency
  ##########################
  
  if (7 %in% analyses) {
    
    data_check <- calc_annual_lowflows(data = flow_data,
                                       start_year = start_year, end_year = end_year, exclude_years = exclude_years,
                                       water_year_start = water_year_start,
                                       ignore_missing = ignore_missing)
    data_check <- dplyr::select(data_check, Min_1_Day, Min_3_Day, Min_7_Day, Min_30_Day)
    
    if (any(as.numeric(colSums(!is.na(data_check))) < 3)) {
      warning("Not enough annual data (3 years) for frequency analysis. Consider filtering for appropriate years or use ignore_missing = TRUE,",
              call. = FALSE)
      
    } else {
      freq_results <- compute_annual_frequencies(data = flow_data_plus,
                                                 start_year = start_year,
                                                 end_year = end_year,
                                                 exclude_years = exclude_years,
                                                 water_year_start = water_year_start,
                                                 ignore_missing = ignore_missing,
                                                 roll_days = c(1, 3, 7, 30, 60))
      
      # freq_ann_data <- tidyr::spread(freq_results$Freq_Analysis_Data, Measure, value)
      freq_ann_data <- freq_results$Freq_Analysis_Data
      freq_plot_data <- freq_results$Freq_Plot_Data
      freq_plots <- list("Frequency_Plot" = freq_results$Freq_Plot)
      #freq_fitting <- freq_results$fit
      freq_quantiles <- freq_results$Freq_Fitted_Quantiles
      
      # Add to objects list
      all_objects <- append(all_objects,    
                            list("Lowflow_Frequencies" = freq_results))
      
    }
  }
  
  message("* DONE")
  
  return(all_objects)
  
}