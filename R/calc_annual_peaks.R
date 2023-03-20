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


#' @title Calculate annual high and low flows
#'
#' @description 
#'    
#'    This function has been superseded by the \code{calc_annual_extremes()} function.
#'    
#'    Calculates annual n-day minimum and maximum values, and the day of year and date of occurrence of daily flow values
#'    from a daily streamflow data set. Calculates statistics from all values, unless specified. Returns a tibble with statistics.
#'
#' @inheritParams calc_annual_stats
#' @param roll_days_low Numeric value of the number of days to apply a rolling mean for low flows.  Will override 'roll_days' argument 
#'     for low flows. Default \code{NA}.
#' @param roll_days_high Numeric value of the number of days to apply a rolling mean for high flows.  Will override 'roll_days' argument 
#'     for high flows. Default \code{NA}.
#' @param months_low Numeric vector of specified months for window of low flows (3 for March, 6:8 for Jun-Aug). Will override 'months' 
#'    argument for low flows. Default \code{NA}.
#' @param months_high Numeric vector of specified months for window of high flows (3 for March, 6:8 for Jun-Aug). Will override 'months' 
#'    argument for high flows. Default \code{NA}.
#'    
#' @return A tibble data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{Min_'n'_Day}{annual minimum for selected n-day rolling mean, direction of mean specified by roll_align}
#'   \item{Min_'n'_Day_DoY}{day of year for selected annual minimum of n-day rolling mean}
#'   \item{Min_'n'_Day_Date}{date (YYYY-MM-DD) for selected annual minimum of n-day rolling mean}
#'   \item{Max_'n'_Day}{annual maximum for selected n-day rolling mean, direction of mean specified by roll_align}
#'   \item{Max_'n'_Day_DoY}{day of year for selected annual maximum of n-day rolling mean}
#'   \item{Max_'n'_Day_Date}{date (YYYY-MM-DD) for selected annual maximum of n-day rolling mean}
#'   Default columns:
#'   \item{Min_1_Day}{annual 1-day mean minimum (roll_align = right)}
#'   \item{Min_1_Day_DoY}{day of year of annual 1-day mean minimum}
#'   \item{Min_1_Day_Date}{date (YYYY-MM-DD) of annual 1-day mean minimum}
#'   \item{Max_1_Day}{annual 1-day mean maximum (roll_align = right)}
#'   \item{Max_1_Day_DoY}{day of year of annual 1-day mean maximum}
#'   \item{Max_1_Day_Date}{date (YYYY-MM-DD) of annual 1-day mean maximum}   
#'   Transposing data creates a column of 'Statistics' and subsequent columns for each year selected. 'Date' statistics
#'   not transposed.
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Calculate annual 1-day (default) peak flow data with 
#' # default alignment ('right')
#' calc_annual_peaks(station_number = "08NM116") 
#' 
#' # Calculate custom 3-day peak flow data with 'center' alignment
#' calc_annual_peaks(station_number = "08NM116",
#'                   roll_days = 3,
#'                   roll_align = "center")
#'                      
#' }
#' @export



calc_annual_peaks <- function(data,
                              dates = Date,
                              values = Value,
                              groups = STATION_NUMBER,
                              station_number,
                              roll_days = 1,
                              roll_days_low = NA,
                              roll_days_high = NA,
                              roll_align = "right",
                              water_year_start = 1,
                              start_year,
                              end_year,
                              exclude_years, 
                              months = 1:12,
                              months_low = NA,
                              months_high = NA,
                              transpose = FALSE,
                              complete_years = FALSE,
                              ignore_missing = FALSE,
                              allowed_missing = ifelse(ignore_missing,100,0)){
  
  message("Note: this function has been superseded by the 'calc_annual_extremes()' function. ", 
          "This function is still supported but no longer receives active development, ",
          "as better solutions now exist.")
  
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
  if (is.na(roll_days_low)) {
    roll_days_low <- roll_days
  }
  if (is.na(roll_days_high)) {
    roll_days_high <- roll_days
  }
  if (is.na(months_low[1])) {
    months_low <- months
  }
  if (is.na(months_high[1])) {
    months_high <- months
  }
  
  rolling_days_checks(roll_days, roll_align, multiple = FALSE)
  rolling_days_checks(roll_days_low, roll_align, multiple = FALSE)
  rolling_days_checks(roll_days_high, roll_align, multiple = FALSE)
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
  
  # Fill in the missing dates and the add the date variables again
  # Fill missing dates, add date variables, and add WaterYear and DOY
  flow_data <- analysis_prep(data = flow_data, 
                             water_year_start = water_year_start)
  
  # Filter data for one year prior and one year after the selected data for proper rolling means
  flow_data <- dplyr::filter(flow_data, CalendarYear >= start_year - 1 & CalendarYear <= end_year + 1)
  
  # Stop if all data is NA
  no_values_error(flow_data$Value)
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  # Loop through each rolling_day and compute annual min values and their dates
  peak_stats <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear))
  
  # Add specified rolling mean
  flow_data_temp <- fasstr::add_rolling_means(data = flow_data, roll_days = roll_days_low, roll_align = roll_align)
  names(flow_data_temp)[names(flow_data_temp) == paste0("Q", roll_days_low, "Day")] <- "RollingValue_Low"
  
  flow_data_temp <- fasstr::add_rolling_means(data = flow_data_temp, roll_days = roll_days_high, roll_align = roll_align)
  names(flow_data_temp)[names(flow_data_temp) == paste0("Q", roll_days_high, "Day")] <- "RollingValue_High"
  
  # Filter for selected months
  peak_stats_temp_min <- dplyr::filter(flow_data_temp, Month %in% months_low)
  # Calculate the mins and dates
  peak_stats_temp_min <- dplyr::summarize(dplyr::group_by(peak_stats_temp_min, STATION_NUMBER, WaterYear),
                                          MIN_VALUE = min(RollingValue_Low, na.rm = allowed_narm(RollingValue_Low, allowed_missing)),	     
                                          MIN_DAY = ifelse(is.na(MIN_VALUE), NA, DayofYear[which(RollingValue_Low == MIN_VALUE)]),
                                          MIN_DATE = ifelse(is.na(MIN_VALUE), NA, Date[which(RollingValue_Low == MIN_VALUE)]))
  # Calculate the mins and dates
  peak_stats_temp_max <- dplyr::filter(flow_data_temp, Month %in% months_high)
  # Calculate the mins and dates
  peak_stats_temp_max <- dplyr::summarize(dplyr::group_by(peak_stats_temp_max, STATION_NUMBER, WaterYear),
                                          MAX_VALUE = max(RollingValue_High, na.rm = allowed_narm(RollingValue_High, allowed_missing)),	     
                                          MAX_DAY = ifelse(is.na(MAX_VALUE), NA, DayofYear[which(RollingValue_High == MAX_VALUE)]),
                                          MAX_DATE = ifelse(is.na(MAX_VALUE), NA, Date[which(RollingValue_High == MAX_VALUE)]))
  peak_stats_temp <- dplyr::left_join(peak_stats_temp_min, peak_stats_temp_max, by = c("STATION_NUMBER", "WaterYear"))


  peak_stats_temp$MIN_DATE <- as.Date(peak_stats_temp$MIN_DATE, origin = "1970-01-01")
  peak_stats_temp$MAX_DATE <- as.Date(peak_stats_temp$MAX_DATE, origin = "1970-01-01")
  #class(peak_stats_temp$MIN_DATE) <- "Date" # fixes ifelse and date issue
  #class(peak_stats_temp$MAX_DATE) <- "Date" # fixes ifelse and date issue
  names(peak_stats_temp)[names(peak_stats_temp) == "MIN_VALUE"] <- paste0("Min_", roll_days_low, "_Day")
  names(peak_stats_temp)[names(peak_stats_temp) == "MIN_DAY"] <- paste0("Min_", roll_days_low, "_Day_DoY")
  names(peak_stats_temp)[names(peak_stats_temp) == "MIN_DATE"] <- paste0("Min_", roll_days_low, "_Day_Date")
  names(peak_stats_temp)[names(peak_stats_temp) == "MAX_VALUE"] <- paste0("Max_", roll_days_high, "_Day")
  names(peak_stats_temp)[names(peak_stats_temp) == "MAX_DAY"] <- paste0("Max_", roll_days_high, "_Day_DoY")
  names(peak_stats_temp)[names(peak_stats_temp) == "MAX_DATE"] <- paste0("Max_", roll_days_high, "_Day_Date")

  peak_stats <- merge(peak_stats, peak_stats_temp, by = c("STATION_NUMBER", "WaterYear"), all = TRUE)

  peak_stats <-   dplyr::rename(peak_stats, Year = WaterYear)

  # Filter for start and end years and make excluded years data NA
  peak_stats <- subset(peak_stats, Year >= start_year & Year <= end_year)
  peak_stats[peak_stats$Year %in% exclude_years, -(1:2)] <- NA


  # If transpose if selected, switch columns and rows
  if (transpose) {

    # Remove the dates
    peak_stats <- dplyr::select(peak_stats, STATION_NUMBER, Year, dplyr::contains("Day"), -dplyr::contains("Date"))

    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(peak_stats[-(1:2)])
    peak_stats <- tidyr::gather(peak_stats, Statistic, Value, -STATION_NUMBER, -Year)
    peak_stats <- tidyr::spread(peak_stats, Year, Value)

    # Order the columns
    peak_stats$Statistic <- factor(peak_stats$Statistic, levels = stat_levels)
    peak_stats <- dplyr::arrange(peak_stats, STATION_NUMBER, Statistic)
  }

  # Give warning if any NA values
  if (!transpose) {
    missing_test <- dplyr::filter(peak_stats, !(Year %in% exclude_years))
    missing_values_warning(missing_test[, 3:ncol(missing_test)])
  } else {
    missing_test <- dplyr::select(peak_stats, -dplyr::one_of(as.character(exclude_years)))
    missing_values_warning(missing_test[, 3:ncol(missing_test)])
  }


  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(peak_stats)[names(peak_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    peak_stats <- dplyr::select(peak_stats, -STATION_NUMBER)
  }

  dplyr::as_tibble(peak_stats)

}

