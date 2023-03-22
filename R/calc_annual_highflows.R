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


#' @title Calculate annual high flows and dates
#'
#' @description Calculates annual n-day maximum values, and the day of year and date of occurrence of daily flow values from a daily 
#'    streamflow data set. Calculates statistics from all values, unless specified. Returns a tibble with statistics.
#'
#' @inheritParams calc_annual_stats
#'    
#' @return A tibble data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{Max_'n'_Day}{annual maximum for each n-day rolling mean, direction of mean specified by roll_align}
#'   \item{Max_'n'_Day_DoY}{day of year for each annual maximum of n-day rolling mean}
#'   \item{Max_'n'_Day_Date}{date (YYYY-MM-DD) for each annual maximum of n-day rolling mean}
#'   Default columns:
#'   \item{Max_1_Day}{annual 1-day mean maximum (roll_align = right)}
#'   \item{Max_1_Day_DoY}{day of year of annual 1-day mean maximum}
#'   \item{Max_1_Day_Date}{date (YYYY-MM-DD) of annual 1-day mean maximum}
#'   \item{Max_3_Day}{annual 3-day mean maximum (roll_align = right)}
#'   \item{Max_3_Day_DoY}{day of year of annual 3-day mean maximum}
#'   \item{Max_3_Day_Date}{date (YYYY-MM-DD) of annual 3-day mean maximum}   
#'   \item{Max_7_Day}{annual 7-day mean maximum (roll_align = right)}
#'   \item{Max_7_Day_DoY}{day of year of annual 7-day mean maximum}
#'   \item{Max_7_Day_Date}{date (YYYY-MM-DD) of annual 7-day mean maximum}
#'   \item{Max_30_Day}{annual 30-day mean maximum (roll_align = right)}
#'   \item{Max_30_Day_DoY}{day of year of annual 30-day mean maximum}
#'   \item{Max_30_Day_Date}{date (YYYY-MM-DD) of annual 30-day mean maximum}
#'   Transposing data creates a column of 'Statistics' and subsequent columns for each year selected. 'Date' statistics
#'   not transposed.
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Calculate annual 1, 3, 7, and 30-day (default) high flows with 
#' # default alignment ('right')
#' calc_annual_highflows(station_number = "08NM116") 
#' 
#' # Calculate custom 3 and 7-day annual high flows with 'center' alignment
#' calc_annual_highflows(station_number = "08NM116",
#'                       roll_days = c(3,7),
#'                       roll_align = "center",
#'                       start_year = 1980)
#'                      
#' }
#' @export



calc_annual_highflows <- function(data,
                                  dates = Date,
                                  values = Value,
                                  groups = STATION_NUMBER,
                                  station_number,
                                  roll_days = c(1,3,7,30),
                                  roll_align = "right",
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
  
  rolling_days_checks(roll_days, roll_align, multiple = TRUE)
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
  
  # Loop through each rolling_day and compute annual max values and their dates
  highflow_stats <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear))
  for (day in unique(roll_days)) {
    # Add specified rolling mean
    flow_data_temp <- fasstr::add_rolling_means(data = flow_data, roll_days = day, roll_align = roll_align)
    names(flow_data_temp)[names(flow_data_temp) == paste0("Q", day, "Day")] <- "RollingValue"
    
    # Filter for selected months
    flow_data_temp <- dplyr::filter(flow_data_temp, Month %in% months)
    
    # Calculate the maxs and dates
    highflow_stats_temp <- dplyr::summarize(dplyr::group_by(flow_data_temp, STATION_NUMBER, WaterYear),
                                            MAX_VALUE = max(RollingValue, na.rm = allowed_narm(RollingValue, allowed_missing)),	     
                                            MAX_DAY = ifelse(is.na(MAX_VALUE), NA, DayofYear[which(RollingValue == MAX_VALUE)]),
                                            MAX_DATE= ifelse(is.na(MAX_VALUE), NA, Date[which(RollingValue == MAX_VALUE)]))
    highflow_stats_temp$MAX_DATE <- as.Date(highflow_stats_temp$MAX_DATE, origin = "1970-01-01")
    #class(highflow_stats_temp$MAX_DATE) <- "Date" # fixes ifelse and date issue
    names(highflow_stats_temp)[names(highflow_stats_temp) == "MAX_VALUE"] <- paste0("Max_", day, "_Day")
    names(highflow_stats_temp)[names(highflow_stats_temp) == "MAX_DAY"] <- paste0("Max_", day, "_Day_DoY")
    names(highflow_stats_temp)[names(highflow_stats_temp) == "MAX_DATE"] <- paste0("Max_", day, "_Day_Date")
    
    highflow_stats <- merge(highflow_stats, highflow_stats_temp, by = c("STATION_NUMBER", "WaterYear"), all = TRUE)
  }
  highflow_stats <-   dplyr::rename(highflow_stats, Year = WaterYear)
  
  # Filter for start and end years and make excluded years data NA
  highflow_stats <- subset(highflow_stats, Year >= start_year & Year <= end_year)
  highflow_stats[highflow_stats$Year %in% exclude_years, -(1:2)] <- NA
  
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    
    # Remove the dates
    highflow_stats <- dplyr::select(highflow_stats, STATION_NUMBER, Year, dplyr::contains("Day"), -dplyr::contains("Date"))
    
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(highflow_stats[-(1:2)])
    highflow_stats <- tidyr::gather(highflow_stats, Statistic, Value, -STATION_NUMBER, -Year)
    highflow_stats <- tidyr::spread(highflow_stats, Year, Value)
    
    # Order the columns
    highflow_stats$Statistic <- factor(highflow_stats$Statistic, levels = stat_levels)
    highflow_stats <- dplyr::arrange(highflow_stats, STATION_NUMBER, Statistic)
  }
  
  # Give warning if any NA values
  if (!transpose) {
    missing_test <- dplyr::filter(highflow_stats, !(Year %in% exclude_years))
    missing_values_warning(missing_test[, 3:ncol(missing_test)])
  } else {
    missing_test <- dplyr::select(highflow_stats, -dplyr::one_of(as.character(exclude_years)))
    missing_values_warning(missing_test[, 3:ncol(missing_test)])
  }
  
  
  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(highflow_stats)[names(highflow_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    highflow_stats <- dplyr::select(highflow_stats, -STATION_NUMBER)
  }
  
  dplyr::as_tibble(highflow_stats)
  
}

