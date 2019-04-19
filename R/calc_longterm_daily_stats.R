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

#' @title Calculate the long-term and long-term monthly summary statistics
#'
#' @description Calculates the long-term and long-term monthly mean, median, maximum, minimum, and percentiles of daily flow values 
#'    from a streamflow dataset. Calculates the statistics from all daily values from all years, unless specified.
#'
#' @inheritParams calc_daily_stats
#' @param percentiles Numeric vector of percentiles to calculate. Set to NA if none required. Default \code{c(10,90)}.
#' @param include_longterm Logical value indicating whether to include longterm calculation of all data. Default \code{TRUE}.
#' @param custom_months Numeric vector of months to combine to summarize (ex. \code{6:8} for Jun-Aug). Adds results to the end of table.
#'    If wanting months that overlap calendar years (ex. Oct-Mar), choose water_year_start that begins before the first 
#'    month listed. Leave blank for no custom month summary.
#' @param custom_months_label Character string to label custom months. For example, if choosing months 7:9  you may choose 
#'    "Summer" or "Jul-Sep". Default \code{"Custom-Months"}.
#' 
#' @return A tibble data frame with the following columns:
#'   \item{Month}{month of the year, included 'Long-term' for all months, and 'Custom-Months' if selected}
#'   \item{Mean}{mean of all daily data for a given month and long-term over all years}
#'   \item{Median}{median of all daily data for a given month and long-term over all years}
#'   \item{Maximum}{maximum of all daily data for a given month and long-term over all years}
#'   \item{Minimum}{minimum of all daily data for a given month and long-term over all years}
#'   \item{P'n'}{each  n-th percentile selected for a given month and long-term over all years}
#'   Default percentile columns:
#'   \item{P10}{annual 10th percentile selected for a given month and long-term over all years}
#'   \item{P90}{annual 90th percentile selected for a given month and long-term over all years}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'   
#' @examples
#' \dontrun{
#' 
#' # Calculate statistics using data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' calc_longterm_daily_stats(data = flow_data,
#'                           start_year = 1980)
#' 
#' # Calculate statistics using station_number argument with defaults
#' calc_longterm_daily_stats(station_number = "08NM116",
#'                           start_year = 1980)
#' 
#' # Calculate statistics regardless if there is missing data for a given year
#' calc_longterm_daily_stats(station_number = "08NM116",
#'                           ignore_missing = TRUE)
#'                   
#' # Calculate statistics for water years starting in October
#' calc_longterm_daily_stats(station_number = "08NM116",
#'                           start_year = 1980,
#'                           water_year_start = 10)
#'                   
#' # Calculate statistics with custom years
#' calc_longterm_daily_stats(station_number = "08NM116",
#'                           start_year = 1981,
#'                           end_year = 2010,
#'                           exclude_years = c(1991,1993:1995))
#'                   
#' # Calculate statistics for 7-day flows for July-September months only, with 25 and 75th percentiles
#' calc_longterm_daily_stats(station_number = "08NM116",
#'                           roll_days = 7,
#'                           months = 7:9,
#'                           percentiles = c(25,75),
#'                           ignore_missing = TRUE,
#'                           include_longterm = FALSE) # removes the Long-term numbers 
#'                     
#' # Calculate statistics and add custom stats for July-September
#' calc_longterm_daily_stats(station_number = "08NM116",
#'                           start_year = 1980,
#'                           custom_months = 7:9,
#'                           custom_months_label = "Summer")                  
#' }
#' @export


calc_longterm_daily_stats <- function(data,
                                      dates = Date,
                                      values = Value,
                                      groups = STATION_NUMBER,
                                      station_number,
                                      percentiles = c(10,90),
                                      roll_days = 1,
                                      roll_align = "right",
                                      water_year_start = 1,
                                      start_year,
                                      end_year,
                                      exclude_years,
                                      months = 1:12,
                                      complete_years = FALSE,
                                      include_longterm = TRUE,
                                      custom_months,
                                      custom_months_label,
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
  if (missing(custom_months)) {
    custom_months = NULL
  }
  if (missing(custom_months_label)) {
    custom_months_label = "Custom-Months"
  }
  
  rolling_days_checks(roll_days, roll_align)
  percentiles_checks(percentiles)
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  months_checks(months = months)
  transpose_checks(transpose)
  ignore_missing_checks(ignore_missing)
  complete_yrs_checks(complete_years)
  custom_months_checks(custom_months, custom_months_label)
  include_longterm_checks(include_longterm)
  
  
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
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill missing dates, add date variables, and add WaterYear
  flow_data <- analysis_prep(data = flow_data, 
                             water_year_start = water_year_start)
  
  # Add rolling means to end of dataframe
  flow_data <- add_rolling_means(data = flow_data, roll_days = roll_days, roll_align = roll_align)
  colnames(flow_data)[ncol(flow_data)] <- "RollingValue"
  
  # Filter for the selected years
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  flow_data <- dplyr::filter(flow_data, !(WaterYear %in% exclude_years))
  flow_data <- dplyr::filter(flow_data, Month %in% months)
  
  
  # Remove incomplete years if selected
  flow_data <- filter_complete_yrs(complete_years = complete_years, 
                                   flow_data)
  
  
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Calculate the monthly and longterm stats
  Q_months <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, MonthName),
                               Mean = mean(RollingValue, na.rm = ignore_missing),
                               Median = stats::median(RollingValue, na.rm = ignore_missing),
                               Maximum = max(RollingValue, na.rm = ignore_missing),
                               Minimum = min(RollingValue, na.rm = ignore_missing))
  Q_months <- dplyr::ungroup(Q_months)
  
  if (include_longterm) {
    longterm_stats   <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER),
                                         Mean = mean(RollingValue, na.rm = ignore_missing),
                                         Median = stats::median(RollingValue, na.rm = ignore_missing),
                                         Maximum = max(RollingValue, na.rm = ignore_missing),
                                         Minimum = min(RollingValue, na.rm = ignore_missing))
    longterm_stats <- dplyr::ungroup(longterm_stats)
    longterm_stats <- dplyr::mutate(longterm_stats, MonthName = as.factor("Long-term"))
    
    longterm_stats <- rbind(Q_months, longterm_stats)  #dplyr::bindrows gives unnecessary warnings
  } else {
    longterm_stats <- Q_months
  }
  
  
  # Calculate the monthly and longterm percentiles
  if(!all(is.na(percentiles))) {
    for (ptile in percentiles) {
      
      Q_months_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, MonthName),
                                         Percentile = ifelse(!is.na(mean(RollingValue, na.rm = FALSE)) | ignore_missing, 
                                                             stats::quantile(RollingValue, ptile / 100, na.rm = TRUE), NA))
      names(Q_months_ptile)[names(Q_months_ptile) == "Percentile"] <- paste0("P", ptile)
      Q_months_ptile <- dplyr::ungroup(Q_months_ptile)
      
      
      if (include_longterm) {
        longterm_stats_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER),
                                                 Percentile = ifelse(!is.na(mean(RollingValue, na.rm = FALSE)) | ignore_missing, 
                                                                     stats::quantile(RollingValue, ptile / 100, na.rm = TRUE), NA))
        longterm_stats_ptile <- dplyr::mutate(longterm_stats_ptile, MonthName = "Long-term")
        
        names(longterm_stats_ptile)[names(longterm_stats_ptile) == "Percentile"] <- paste0("P", ptile)
        longterm_stats_ptile <- dplyr::ungroup(longterm_stats_ptile)
        
        longterm_stats_ptile <- rbind(dplyr::ungroup(Q_months_ptile), dplyr::ungroup(longterm_stats_ptile))  #dplyr::bindrows gives unnecessary warnings
      } else {
        longterm_stats_ptile <- Q_months_ptile
      }
      # Merge with longterm_stats
      longterm_stats <- merge(longterm_stats,longterm_stats_ptile, by = c("STATION_NUMBER", "MonthName"))
    }
  }
  
  # Calculate custom_months is selected, append data to end
  if(is.numeric(custom_months) & all(custom_months %in% c(1:12))) {
    
    # Filter months for those selected and calculate stats
    flow_data_temp <- dplyr::filter(flow_data, Month %in% custom_months)
    Q_months_custom <-   dplyr::summarize(dplyr::group_by(flow_data_temp, STATION_NUMBER),
                                          Mean = mean(RollingValue, na.rm = ignore_missing),
                                          Median = stats::median(RollingValue, na.rm = ignore_missing),
                                          Maximum = max(RollingValue,na.rm = ignore_missing),
                                          Minimum = min(RollingValue,na.rm = ignore_missing))
    Q_months_custom <- dplyr::mutate(Q_months_custom, MonthName = paste0(custom_months_label))
    
    # Calculate percentiles
    if (!all(is.na(percentiles))){
      for (ptile in percentiles) {
        Q_ptile_custom <- dplyr::summarize(dplyr::group_by(flow_data_temp, STATION_NUMBER),
                                           Percentile = ifelse(!is.na(mean(RollingValue, na.rm = FALSE)) | ignore_missing, 
                                                               stats::quantile(RollingValue, ptile / 100, na.rm = TRUE), NA))
        Q_ptile_custom <- dplyr::mutate(Q_ptile_custom, MonthName = paste0(custom_months_label))
        names(Q_ptile_custom)[names(Q_ptile_custom) == "Percentile"] <- paste0("P", ptile)
        
        # Merge with custom stats
        Q_months_custom <- merge(dplyr::ungroup(Q_months_custom), dplyr::ungroup(Q_ptile_custom), by = c("STATION_NUMBER", "MonthName"))
      }
    }
    # Merge with longterm_stats
    longterm_stats <- rbind(longterm_stats, Q_months_custom)
  }
  
  # Rename Month column and reorder to proper levels (set in add_date_vars)
  longterm_stats <- dplyr::rename(longterm_stats, Month = MonthName)
  longterm_stats <- with(longterm_stats, longterm_stats[order(STATION_NUMBER, Month),])
  #  row.names(longterm_stats) <- c(1:nrow(longterm_stats))
  
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(longterm_stats[-(1:2)])
    
    # Transpose the columns for rows
    longterm_stats <- tidyr::gather(longterm_stats, Statistic, Value, -STATION_NUMBER, -Month)
    longterm_stats <- tidyr::spread(longterm_stats, Month, Value)
    
    # Order the columns
    longterm_stats$Statistic <- factor(longterm_stats$Statistic, levels = stat_levels)
    longterm_stats <- dplyr::arrange(longterm_stats, STATION_NUMBER, Statistic)
  }
  
  # Give warning if any NA values
  missing_values_warning(longterm_stats[, 3:ncol(longterm_stats)])
  
  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(longterm_stats)[names(longterm_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    longterm_stats <- dplyr::select(longterm_stats, -STATION_NUMBER)
  }
  
  
  
  dplyr::as_tibble(longterm_stats)
  
  
}
