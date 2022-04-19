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

#' @title Calculate daily summary statistics
#'
#' @description Calculates means, medians, maximums, minimums, and percentiles for each day of the year of flow values 
#'    from a daily streamflow data set. Can determine statistics of rolling mean days (e.g. 7-day flows) using the \code{roll_days} 
#'    argument. Note that statistics are based on the numeric days of year (1-365) and not the date of year (Jan 1 - Dec 31).
#'    Calculates statistics from all values, unless specified. Returns a tibble with statistics.
#'    
#' @inheritParams calc_annual_stats
#' @param percentiles Numeric vector of percentiles to calculate. Set to \code{NA} if none required. Default \code{c(5,25,75,95)}.
#'    
#' @return A tibble data frame with the following columns:
#'   \item{Date}{date (MMM-DD) of daily statistics}
#'   \item{DayofYear}{day of year of daily statistics}
#'   \item{Mean}{daily mean of all flows for a given day of the year}
#'   \item{Median}{daily mean of all flows for a given day of the year}
#'   \item{Maximum}{daily mean of all flows for a given day of the year}
#'   \item{Minimum}{daily mean of all flows for a given day of the year}
#'   \item{P'n'}{each daily n-th percentile selected of all flows for a given day of the year}
#'   Default percentile columns:
#'   \item{P5}{daily 5th percentile of all flows for a given day of the year}
#'   \item{P25}{daily 25th percentile of all flows for a given day of the year}
#'   \item{P75}{daily 75th percentile of all flows for a given day of the year}
#'   \item{P95}{daily 95th percentile of all flows for a given day of the year}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Calculate daily statistics using station_number argument with defaults
#' calc_daily_stats(station_number = "08NM116",
#'                  start_year = 1980)
#' 
#' # Calculate daily statistics regardless if there is missing data for a given day of year
#' calc_daily_stats(station_number = "08NM116",
#'                  ignore_missing = TRUE)
#'                   
#' # Calculate daily statistics using only years with no missing data
#' calc_daily_stats(station_number = "08NM116",
#'                  complete_years = TRUE)
#' 
#' # Calculate daily statistics for water years starting in October between 1980 and 2010
#' calc_daily_stats(station_number = "08NM116",
#'                  start_year = 1980,
#'                  end_year = 2010,
#'                  water_year_start = 10)
#'                  
#' }
#' @export



calc_daily_stats <- function(data,
                             dates = Date,
                             values = Value,
                             groups = STATION_NUMBER,
                             station_number,
                             percentiles = c(5,25,75,95),
                             roll_days = 1,
                             roll_align = "right",
                             water_year_start = 1,
                             start_year,
                             end_year,
                             exclude_years,
                             months = 1:12,
                             transpose = FALSE, 
                             complete_years = FALSE,
                             ignore_missing = FALSE){
  
  
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
  percentiles_checks(percentiles)
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  months_checks(months)
  logical_arg_check(transpose)
  logical_arg_check(ignore_missing)
  logical_arg_check(complete_years)
    
  
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
                             water_year_start = water_year_start,
                             date = TRUE)
  
  # Add rolling means to end of dataframe
  flow_data <- add_rolling_means(data = flow_data, roll_days = roll_days, roll_align = roll_align)
  colnames(flow_data)[ncol(flow_data)] <- "RollingValue"
  
  # Filter for the selected and excluded years and leap year values (last day)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  flow_data <- dplyr::filter(flow_data, !(WaterYear %in% exclude_years))
  flow_data <- dplyr::filter(flow_data, DayofYear < 366)
  
  # Stop if all data is NA
  no_values_error(flow_data$RollingValue)
  
  # Remove incomplete years if selected
  flow_data <- filter_complete_yrs(complete_years = complete_years, 
                                   flow_data)
  
  # Stop if all data is NA
  no_values_error(flow_data$RollingValue)
  

  ## CALCULATE STATISTICS
  ## --------------------

  # Calculate basic stats
  daily_stats <- suppressWarnings(dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisDate, DayofYear),
                              Mean = mean(RollingValue, na.rm = ignore_missing),
                              Median = stats::median(RollingValue, na.rm = ignore_missing),
                              Minimum = min(RollingValue, na.rm = ignore_missing),
                              Maximum = max(RollingValue, na.rm = ignore_missing)))
  
  #Remove Nans and Infs
  daily_stats$Mean[is.nan(daily_stats$Mean)] <- NA
  daily_stats$Maximum[is.infinite(daily_stats$Maximum)] <- NA
  daily_stats$Minimum[is.infinite(daily_stats$Minimum)] <- NA

  # Compute daily percentiles (if 10 or more years of data)
  if (!all(is.na(percentiles))){
    for (ptile in unique(percentiles)) {
      daily_stats_ptile <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisDate, DayofYear),
                                        Percentile = stats::quantile(RollingValue, ptile / 100, na.rm = TRUE))
      names(daily_stats_ptile)[names(daily_stats_ptile) == "Percentile"] <- paste0("P", ptile)

      # Merge with daily_stats
      daily_stats <- merge(daily_stats, daily_stats_ptile, by = c("STATION_NUMBER", "AnalysisDate", "DayofYear"))

      # Remove percentile if mean is NA (workaround for na.rm=FALSE in quantile)
      daily_stats[, ncol(daily_stats)] <- ifelse(is.na(daily_stats$Mean), NA, daily_stats[, ncol(daily_stats)])
    }
  }
  
  # Filter for months
  daily_stats$Month <- as.numeric(format(as.Date(daily_stats$AnalysisDate), format = "%m"))
  daily_stats <- dplyr::filter(daily_stats, Month %in% months)
  daily_stats <- dplyr::select(daily_stats, -Month)
  

  # Final formatting
  daily_stats <- dplyr::rename(daily_stats, DayofYear = DayofYear, Date = AnalysisDate)
  daily_stats$Date <- format(as.Date(daily_stats$Date), format = "%b-%d")
  col_order <- daily_stats$Date


  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(daily_stats[-(1:2)])

    # Transpose the columns for rows
    daily_stats <- tidyr::gather(daily_stats, Statistic, Value, -(1:2))
    daily_stats <- tidyr::spread(daily_stats, Date, Value)

    # Order the columns
    daily_stats <-  daily_stats[,c("STATION_NUMBER", "Statistic", col_order)]
    daily_stats$Statistic <- factor(daily_stats$Statistic, levels = stat_levels)
    daily_stats <- dplyr::arrange(daily_stats, STATION_NUMBER, Statistic)
  }

  # Give warning if any NA values
  missing_values_warning(daily_stats[, 4:ncol(daily_stats)])
  

  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(daily_stats)[names(daily_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    daily_stats <- dplyr::select(daily_stats, -STATION_NUMBER)
  }



  dplyr::as_tibble(daily_stats)


}
