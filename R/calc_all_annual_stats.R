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



#' @title Calculate all fasstr annual statistics
#'
#' @description Calculates all annual statistics of daily flow values from a streamflow dataset from all annual fasstr functions. 
#'    Calculates the statistics from all daily discharge values from all years, unless specified. Data is ideally long-term and continuous 
#'    with minimal missing/seasonal data as annual statistics are calculated. Data calculated using the folling functions:
#' \itemize{
#'  \item{calc_annual_stats()}
#'  \item{calc_annual_lowflows()}
#'  \item{calc_annual_cumulative_stats()}
#'  \item{calc_annual_flow_timing()}
#'  \item{calc_monthly_stats()}
#'  \item{calc_annual_outside_normal()}
#'  }
#'    
#' @inheritParams calc_annual_stats
#' @inheritParams calc_annual_cumulative_stats
#' @inheritParams calc_annual_outside_normal
#' @param annual_percentiles Numeric vector of percentiles to calculate annually. Set to NA if none required. Used for calc_annual_stats()
#'    function. Default \code{c(10,90)}.
#' @param monthly_percentiles Numeric vector of percentiles to calculate monthly for each year. Set to NA if none required. Used for 
#'    calc_monthly_stats() function. Default \code{c(10,20)}.
#' @param stats_days Numeric vector of the number of days to apply a rolling mean on basic stats. Default \code{c(1)}.
#'    Used for calc_annual_stats() and calc_monthly_stats() functions.
#' @param stats_align Character string identifying the direction of the rolling mean on basic stats from the specified date, either by 
#'    the first ('left'), last ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#'    Used for calc_annual_stats(), calc_monthly_stats(), and calc_annual_outside_normal() functions.
#' @param lowflow_days Numeric vector of the number of days to apply a rolling mean on lowflow stats. Default \code{c(1,3,7,30)}.
#'    Used for calc_lowflow_stats() function.
#' @param lowflow_align Character string identifying the direction of the rolling mean on lowflow stats from the specified date, either by 
#'    the first ('left'), last ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#'    Used for calc_lowflow_stats() function.
#' @param timing_percent Numeric vector of percents of annual total flows to determine dates. Used for calc_annual_flow_timing() function. 
#'    Default \code{c(25,33.3,50,75)}.
#' 
#' @return A tibble data frame with column "Year" and then 107 (default) variables from the fasstr annual functions.
#'    See listed functions above for default variables. Transposing data creates a column of "Statistics" and subsequent
#'    columns for each year selected.
#' 
#' @seealso \code{\link{calc_annual_stats}},
#'          \code{\link{calc_annual_lowflows}}, 
#'          \code{\link{calc_annual_cumulative_stats}}, 
#'          \code{\link{calc_annual_flow_timing}}, 
#'          \code{\link{calc_monthly_stats}}, 
#'          \code{\link{calc_annual_outside_normal}}
#'
#' @examples
#' \dontrun{
#' 
#' # Calculate statistics with default arguments
#' calc_all_annual_stats(station_number = "08NM116") 
#' }
#' @export


calc_all_annual_stats <- function(data,
                                  dates = Date,
                                  values = Value,
                                  groups = STATION_NUMBER,
                                  station_number,
                                  basin_area, 
                                  water_year_start = 1,
                                  start_year,
                                  end_year,
                                  exclude_years,
                                  annual_percentiles = c(10,90),
                                  monthly_percentiles = c(10,20),
                                  stats_days = 1,
                                  stats_align = "right",
                                  lowflow_days = c(1,3,7,30),
                                  lowflow_align = "right",
                                  timing_percent = c(25,33,50,75),
                                  normal_percentiles = c(25,75),
                                  transpose = FALSE,
                                  ignore_missing = FALSE){
  
  
  
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
  
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  transpose_checks(transpose)
  ignore_missing_checks(ignore_missing)
  ann_percentiles_checks(annual_percentiles)
  mon_percentiles_checks(monthly_percentiles)
  stats_days_checks(stats_days, stats_align)
  lowflow_days_checks(lowflow_days, lowflow_align)
  timing_pct_checks(timing_percent)
  normal_percentiles_checks(normal_percentiles)
  sort(normal_percentiles)
  
  
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
  
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  annual_stats <- suppressWarnings(calc_annual_stats(data = flow_data,
                                                     percentiles = annual_percentiles,
                                                     roll_days = stats_days,
                                                     roll_align = stats_align,
                                                     water_year_start = water_year_start,
                                                     start_year = start_year,
                                                     end_year = end_year,
                                                     exclude_years = exclude_years, 
                                                     ignore_missing = ignore_missing))
  
  # Gather to name all columns with CY or WY for calendar or water year
  annual_stats <- tidyr::gather(annual_stats, Stat, Value, 3:ncol(annual_stats))
  annual_stats <- dplyr::mutate(annual_stats, Stat = paste0("Annual_", Stat))
  annual_stats <- tidyr::spread(annual_stats, Stat, Value)
  
  
  lowflow_stats <- suppressWarnings(calc_annual_lowflows(data = flow_data,
                                                         roll_days = lowflow_days,
                                                         roll_align = lowflow_align,
                                                         water_year_start = water_year_start,
                                                         start_year = start_year,
                                                         end_year = end_year,
                                                         exclude_years = exclude_years,
                                                         ignore_missing = ignore_missing))
  lowflow_stats <- dplyr::select(lowflow_stats, -dplyr::contains("Date"))
  
  
  totalQ_stats <- suppressWarnings(calc_annual_cumulative_stats(data = flow_data,
                                                                use_yield = FALSE,
                                                                basin_area = NA,
                                                                water_year_start = water_year_start,
                                                                start_year = start_year,
                                                                end_year = end_year,
                                                                exclude_years = exclude_years,
                                                                include_seasons = TRUE))
  
  totalyield_stats <- suppressWarnings(calc_annual_cumulative_stats(data = flow_data,
                                                                    use_yield = TRUE,
                                                                    basin_area = basin_area,
                                                                    water_year_start = water_year_start,
                                                                    start_year = start_year,
                                                                    end_year = end_year,
                                                                    exclude_years = exclude_years,
                                                                    include_seasons = TRUE))
  
  
  timing_stats <- suppressWarnings(calc_annual_flow_timing(data = flow_data,
                                                           percent_total = timing_percent,
                                                           water_year_start = water_year_start,
                                                           start_year = start_year,
                                                           end_year = end_year,
                                                           exclude_years = exclude_years))
  timing_stats <- dplyr::select(timing_stats, STATION_NUMBER, Year, dplyr::contains("DoY"))
  
  
  month_stats <- suppressWarnings(calc_monthly_stats(data = flow_data,
                                                     percentiles = monthly_percentiles,
                                                     roll_days = stats_days,
                                                     roll_align = stats_align,
                                                     water_year_start = water_year_start,
                                                     start_year = start_year,
                                                     end_year = end_year,
                                                     exclude_years = exclude_years,
                                                     spread = TRUE,
                                                     ignore_missing = ignore_missing))
  
  
  normals_stats <- suppressWarnings(calc_annual_outside_normal(data = flow_data,
                                                               normal_percentiles = normal_percentiles,
                                                               roll_days = stats_days,
                                                               roll_align = stats_align,
                                                               water_year_start = water_year_start,
                                                               start_year = start_year,
                                                               end_year = end_year,
                                                               exclude_years = exclude_years))
  
  ## COMBINE ALL STATS
  ## -----------------
  
  # Create the megazord
  all_stats <- merge(annual_stats, lowflow_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  all_stats <- merge(all_stats, totalQ_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  all_stats <- merge(all_stats, totalyield_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  all_stats <- merge(all_stats, timing_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  all_stats <- merge(all_stats, normals_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  all_stats <- merge(all_stats, month_stats, by = c("STATION_NUMBER", "Year"), all = TRUE)
  
  # Gather to name all columns with CY or WY for calendar or water year
  all_stats <- tidyr::gather(all_stats, Stat, Value, 3:ncol(all_stats))
  #all_stats <- dplyr::mutate(all_stats, Stat = paste0(ifelse(water_year, paste("WY_"), paste("CY_")), Stat))
  
  # Spread back using the same order
  col_order <- c("STATION_NUMBER", "Year", unique(all_stats$Stat))
  all_stats <- tidyr::spread(all_stats, Stat, Value)
  all_stats <- all_stats[, col_order]
  
  # Remove excluded years
  all_stats <- dplyr::filter(all_stats, Year >= start_year & Year <= end_year)
  all_stats[all_stats$Year %in% exclude_years, -(1:2)] <- NA
  
  
  # Give warning if any NA values or no basin areas
  missing_test <- dplyr::filter(all_stats, !(Year %in% exclude_years))
  
  if ( anyNA(dplyr::select(missing_test, -dplyr::contains("Yield"))) & 
       all(is.na(dplyr::select(missing_test, dplyr::contains("Yield"))))) 
    warning("No basin area values provided or extracted from HYDAT, and one or more calculations included missing values and NA's were produced. Provide a basin_area if desired and/or filter data for complete years or months, or use to ignore_missing = TRUE to ignore missing values.", call. = FALSE)
  
  if ( !anyNA(dplyr::select(missing_test, -dplyr::contains("Yield"))) & 
       all(is.na(dplyr::select(missing_test, dplyr::contains("Yield"))))) 
    warning("No basin area values provided or extracted from HYDAT and NA's were produced for all 'Yield' calculations. Use basin_area argument to provide one if desired.", call. = FALSE)
  
  if ( anyNA(missing_test[,3:ncol(missing_test)]) & 
       !all(is.na(dplyr::select(missing_test, dplyr::contains("Yield"))))) 
    warning("One or more calculations included missing values and NA's were produced. Filter data for complete years or months, or use to ignore_missing = TRUE to ignore missing values.", call. = FALSE)
  
  
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    options(scipen = 999)
    
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(all_stats[-(1:2)])
    
    # Transpose the columns for rows
    all_stats <- tidyr::gather(all_stats, Statistic, Value, -STATION_NUMBER, -Year)
    all_stats <- tidyr::spread(all_stats, Year, Value)
    
    # Order the columns
    all_stats$Statistic <- factor(all_stats$Statistic, levels = stat_levels)
    all_stats <- dplyr::arrange(all_stats, STATION_NUMBER, Statistic)
  }
  
  
  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(all_stats)[names(all_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    all_stats <- dplyr::select(all_stats, -STATION_NUMBER)
  }
  
  dplyr::as_tibble(all_stats)
  
  
} 

