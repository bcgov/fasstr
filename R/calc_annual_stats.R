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

#' @title Calculate annual summary statistics
#'
#' @description Calculates means, medians, maximums, minimums, and percentiles for each year from all years of a daily streamflow 
#'    data set. Calculates statistics from all values, unless specified. Returns a tibble with statistics.
#'
#' @param data Data frame of daily data that contains columns of dates, flow values, and (optional) groups (e.g. station numbers).
#'    Leave blank or set to \code{NULL} if using \code{station_number} argument.
#' @param dates Name of column in \code{data} that contains dates formatted YYYY-MM-DD. Only required if dates column name is not 
#'    'Date' (default). Leave blank or set to \code{NULL} if using \code{station_number} argument.
#' @param values Name of column in \code{data} that contains numeric flow values, in units of cubic metres per second.
#'    Only required if values column name is not 'Value' (default). Leave blank if using \code{station_number} argument.
#' @param groups Name of column in \code{data} that contains unique identifiers for different data sets, if applicable. Only required 
#'    if groups column name is not 'STATION_NUMBER'. Function will automatically group by a column named 'STATION_NUMBER' if 
#'    present. Remove the 'STATION_NUMBER' column beforehand to remove this grouping. Leave blank if using \code{station_number} 
#'    argument.
#' @param station_number Character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of
#'    which to extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.
#'    Leave blank if using \code{data} argument.
#' @param roll_days Numeric value of the number of days to apply a rolling mean. Default \code{1}.
#' @param roll_align Character string identifying the direction of the rolling mean from the specified date, either by the first 
#'    (\code{'left'}), last (\code{'right'}), or middle (\code{'center'}) day of the rolling n-day group of observations. 
#'    Default \code{'right'}.
#' @param percentiles Numeric vector of percentiles to calculate. Set to \code{NA} if none required. Default \code{c(10,90)}.
#' @param water_year_start Numeric value indicating the month (\code{1} through \code{12}) of the start of water year for
#'    analysis. Default \code{1}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank or set well before start date (i.e.
#'    \code{1800}) to use from the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank or set well after end date (i.e.
#'    \code{2100}) to use up to the last year of the source data.
#' @param exclude_years Numeric vector of years to exclude from analysis. Leave blank or set to \code{NULL} to include all years.             
#' @param months Numeric vector of months to include in analysis. For example, \code{3} for March, \code{6:8} for Jun-Aug or 
#'    \code{c(10:12,1)} for first four months (Oct-Jan) when \code{water_year_start = 10} (Oct). Default summarizes all 
#'    months (\code{1:12}).
#' @param transpose Logical value indicating whether to transpose rows and columns of results. Default \code{FALSE}.
#' @param complete_years Logical values indicating whether to include only years with complete data in analysis. Default \code{FALSE}. 
#' @param ignore_missing Logical value indicating whether dates with missing values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only those statistics from
#'    time periods with no missing dates will be returned. Default \code{FALSE}.
#' @param allowed_missing Numeric value between 0 and 100 indicating the \strong{percentage} of missing dates allowed to be
#'    included to calculate a statistic (0 to 100 percent). If \code{'ignore_missing = FALSE'} then it defaults to \code{0} (zero missing dates allowed),
#'    if \code{'ignore_missing = TRUE'} then it defaults to \code{100} (any missing dates allowed); consistent with 
#'    \code{ignore_missing} usage. Supersedes \code{ignore_missing} when used.
#'    
#' @return A tibble data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{Mean}{annual mean of all daily flows for a given year}
#'   \item{Median}{annual median of all daily flows for a given year}
#'   \item{Maximum}{annual maximum of all daily flows for a given year}
#'   \item{Minimum}{annual minimum of all daily flows for a given year}
#'   \item{P'n'}{each annual n-th percentile selected of all daily flows}
#'   Default percentile columns:
#'   \item{P10}{annual 10th percentile of all daily flows for a given year}
#'   \item{P90}{annual 90th percentile of all daily flows for a given year}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Calculate annual statistics from a data frame using the data argument
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' calc_annual_stats(data = flow_data)
#' 
#' # Calculate annual statistics using station_number argument
#' calc_annual_stats(station_number = "08NM116")
#' 
#' # Calculate annual statistics regardless if there 
#' # is missing data for a given year
#' calc_annual_stats(station_number = "08NM116",
#'                   ignore_missing = TRUE)
#'                   
#' # Calculate annual statistics for water years starting in October
#' calc_annual_stats(station_number = "08NM116",
#'                   water_year_start = 10)
#'                   
#' # Calculate annual statistics for 7-day flows for July-September 
#' # months only, with 25 and 75th percentiles
#' calc_annual_stats(station_number = "08NM116",
#'                   roll_days = 7,
#'                   months = 7:9,
#'                   percentiles = c(25,75))
#'                   
#' }
#' @export


calc_annual_stats <- function(data,
                              dates = Date,
                              values = Value,
                              groups = STATION_NUMBER,
                              station_number,
                              roll_days = 1,
                              roll_align = "right",
                              percentiles = c(10,90),
                              water_year_start = 1,
                              start_year,
                              end_year,
                              exclude_years, 
                              months = 1:12,
                              transpose = FALSE, 
                              complete_years = FALSE,
                              ignore_missing = FALSE,
                              allowed_missing = ifelse(ignore_missing,100,0)){
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data <- NULL
  }
  if (missing(station_number)) {
    station_number <- NULL
  }
  if (missing(start_year)) {
    start_year <- 0
  }
  if (missing(end_year)) {
    end_year <- 9999
  }
  if (missing(exclude_years)) {
    exclude_years <- NULL
  }
  
  rolling_days_checks(roll_days, roll_align)
  numeric_range_checks(percentiles)
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  months_checks(months)
  logical_arg_check(transpose)
  logical_arg_check(ignore_missing)
  allowed_missing_checks(allowed_missing, ignore_missing)
  
  logical_arg_check(complete_years)
  if (complete_years) {
    if (ignore_missing | allowed_missing > 0) {
      ignore_missing <- FALSE
      allowed_missing <- 0
      message("complete_years argument overrides ignore_missing and allowed_missing arguments.")
    }
  }
  
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
  
  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  flow_data <- dplyr::filter(flow_data, Month %in% months)
  
  # Stop if all data is NA
  no_values_error(flow_data$RollingValue)
  
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Calculate basic stats
  annual_stats <-   suppressWarnings(
    dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear),
                     Mean = mean(RollingValue, na.rm = allowed_narm(RollingValue, allowed_missing)),
                     Median = stats::median(RollingValue, na.rm = allowed_narm(RollingValue, allowed_missing)),
                     Maximum = max (RollingValue, na.rm = allowed_narm(RollingValue, allowed_missing)),
                     Minimum = min (RollingValue, na.rm = allowed_narm(RollingValue, allowed_missing))))
  annual_stats <- dplyr::ungroup(annual_stats)
  
  #Remove Nans and Infs
  annual_stats$Mean[is.nan(annual_stats$Mean)] <- NA
  annual_stats$Maximum[is.infinite(annual_stats$Maximum)] <- NA
  annual_stats$Minimum[is.infinite(annual_stats$Minimum)] <- NA
  
  # Calculate annual percentiles
  if(!all(is.na(percentiles))) {
    for (ptile in unique(percentiles)) {
      # Calculate percentiles
      annual_stats_ptile <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear),
                                             Percentile = stats::quantile(RollingValue, ptile / 100, na.rm = TRUE))
      annual_stats_ptile <- dplyr::ungroup(annual_stats_ptile)
      names(annual_stats_ptile)[names(annual_stats_ptile) == "Percentile"] <- paste0("P", ptile)
      
      # Merge with stats
      annual_stats <- merge(annual_stats, annual_stats_ptile, by = c("STATION_NUMBER", "WaterYear"))
      
      # Remove percentile if mean is NA (workaround for na.rm=FALSE in quantile)
      annual_stats[, ncol(annual_stats)] <- ifelse(is.na(annual_stats$Mean), NA, annual_stats[, ncol(annual_stats)])
    }
  }
  
  
  ## Final formatting
  ## ----------------
  
  # Rename year column
  annual_stats <- dplyr::rename(annual_stats, Year = WaterYear)
  
  # Remove selected excluded years
  annual_stats[annual_stats$Year %in% exclude_years, -(1:2)] <- NA
  
  
  # If transpose if selected
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(annual_stats[-(1:2)])
    
    # Transpose the columns for rows
    annual_stats <- tidyr::gather(annual_stats, Statistic, Value, -STATION_NUMBER, -Year)
    annual_stats <- tidyr::spread(annual_stats, Year, Value)
    
    # Order the columns
    annual_stats$Statistic <- factor(annual_stats$Statistic, levels = stat_levels)
    annual_stats <- dplyr::arrange(annual_stats, STATION_NUMBER, Statistic)
  } 
  
  # Give warning if any NA values
  if (!transpose) {
    missing_test <- dplyr::filter(annual_stats, !(Year %in% exclude_years))
    missing_values_warning(missing_test[, 3:ncol(missing_test)])
  } else {
    missing_test <- dplyr::select(annual_stats, -dplyr::one_of(as.character(exclude_years)))
    missing_values_warning(missing_test[, 3:ncol(missing_test)])
  }
  
  
  # Recheck if station_number/grouping was in original data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(annual_stats)[names(annual_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    annual_stats <- dplyr::select(annual_stats, -STATION_NUMBER)
  }
  
  
  dplyr::as_tibble(annual_stats)
}

