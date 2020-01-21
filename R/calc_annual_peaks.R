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


#' @title Calculate annual peaks
#'
#' @description Calculates annual n-day minimum and maximum values, and the day of year and date of occurrence of daily flow values from a 
#'    streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
#'
#' @inheritParams calc_annual_stats
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
                              roll_align = "right",
                              water_year_start = 1,
                              start_year,
                              end_year,
                              exclude_years, 
                              months = 1:12,
                              transpose = FALSE,
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
  
  rolling_days_checks(roll_days, roll_align, multiple = FALSE)
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  months_checks(months)
  transpose_checks(transpose)
  ignore_missing_checks(ignore_missing)
  
  
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
  flow_data_temp <- fasstr::add_rolling_means(data = flow_data, roll_days = roll_days, roll_align = roll_align)
  names(flow_data_temp)[names(flow_data_temp) == paste0("Q", roll_days, "Day")] <- "RollingValue"
  
  # Filter for selected months
  flow_data_temp <- dplyr::filter(flow_data_temp, Month %in% months)
  
  # Calculate the mins and dates
  peak_stats_temp <- dplyr::summarize(dplyr::group_by(flow_data_temp, STATION_NUMBER, WaterYear),
                                      MIN_VALUE = min(RollingValue, na.rm = ignore_missing),	     
                                      MIN_DAY = ifelse(is.na(MIN_VALUE), NA, DayofYear[which(RollingValue == MIN_VALUE)]),
                                      MIN_DATE = ifelse(is.na(MIN_VALUE), NA, Date[which(RollingValue == MIN_VALUE)]),
                                      MAX_VALUE = max(RollingValue, na.rm = ignore_missing),	     
                                      MAX_DAY = ifelse(is.na(MAX_VALUE), NA, DayofYear[which(RollingValue == MAX_VALUE)]),
                                      MAX_DATE = ifelse(is.na(MAX_VALUE), NA, Date[which(RollingValue == MAX_VALUE)]))
  class(peak_stats_temp$MIN_DATE) <- "Date" # fixes ifelse and date issue
  class(peak_stats_temp$MAX_DATE) <- "Date" # fixes ifelse and date issue
  names(peak_stats_temp)[names(peak_stats_temp) == "MIN_VALUE"] <- paste0("Min_", roll_days, "_Day")
  names(peak_stats_temp)[names(peak_stats_temp) == "MIN_DAY"] <- paste0("Min_", roll_days, "_Day_DoY")
  names(peak_stats_temp)[names(peak_stats_temp) == "MIN_DATE"] <- paste0("Min_", roll_days, "_Day_Date")
  names(peak_stats_temp)[names(peak_stats_temp) == "MAX_VALUE"] <- paste0("Max_", roll_days, "_Day")
  names(peak_stats_temp)[names(peak_stats_temp) == "MAX_DAY"] <- paste0("Max_", roll_days, "_Day_DoY")
  names(peak_stats_temp)[names(peak_stats_temp) == "MAX_DATE"] <- paste0("Max_", roll_days, "_Day_Date")
  
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

