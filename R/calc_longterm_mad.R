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

#' @title Calculate the long-term mean annual discharge
#'
#' @description Calculates the long-term mean annual discharge of a streamflow dataset. Averages all daily discharge values from all years,
#'   unless specified.
#'
#' @inheritParams calc_daily_stats
#' @inheritParams calc_monthly_stats
#' @param percent_MAD Numeric vector of percents of long-term mean annual discharge to add to the table (ex. 20 for 20 percent MAD).
#'    Leave blank for no values to be calculated.
#'
#' @return A tibble data frame of numeric values of a long-term mean (and percent of long-term mean if selected) of selected years and months.
#' 
#' @examples
#' \dontrun{
#' 
#' # Calculate the long-term mean annual discharge (MAD) using only years with no missing data
#' calc_longterm_mad(station_number = "08NM116", 
#'                   complete_years = TRUE)
#' 
#' # Calculate the long-term MAD and 5, 10 and 20-percent MADs using only years with no missing data
#' calc_longterm_mad(station_number = "08NM116", 
#'                   complete_years = TRUE,
#'                   percent_MAD = c(5,10,20))
#' }
#' @export


calc_longterm_mad <- function(data,
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
                              complete_years = FALSE,
                              months = 1:12,
                              percent_MAD,
                              transpose = FALSE){
  
  
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
  if (missing(percent_MAD)) {
    percent_MAD = NA
  }
  
  rolling_days_checks(roll_days, roll_align)
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  complete_yrs_checks(complete_years)
  transpose_checks(transpose)
  
  if(!all(is.na(percent_MAD)) & all(percent_MAD <= 0))  
    stop("Numbers in percent_MAD argument must > 0.", call. = FALSE)
  
  
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
  
  # Filter for the selected year
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  flow_data <- dplyr::filter(flow_data, !(WaterYear %in% exclude_years))
  flow_data <- dplyr::filter(flow_data, Month %in% months)
  
  # Remove incomplete years if selected
  flow_data <- filter_complete_yrs(complete_years = complete_years, 
                                   flow_data)
  
  # Give warning if any NA values
  missing_values_warning_noNA(flow_data$RollingValue)
  
  ## CALCULATE STATISTICS
  ## --------------------
  
  ltmad_stats <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER),
                                  LTMAD = mean(RollingValue, na.rm = TRUE))
  
  
  # Calculate the monthly and longterm percentiles
  if(!all(is.na(percent_MAD))) {
    for (pcnt in percent_MAD) {
      ltmad_stats <- dplyr::mutate(ltmad_stats, Percent = LTMAD * pcnt / 100)
      names(ltmad_stats)[names(ltmad_stats) == "Percent"] <- paste0(pcnt, "%MAD")
      
    }
  }
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(ltmad_stats[-(1)])
    
    # Transpose the columns for rows
    ltmad_stats <- tidyr::gather(ltmad_stats, Statistic, Value, -STATION_NUMBER)
    
    # Order the columns
    ltmad_stats$Statistic <- factor(ltmad_stats$Statistic, levels = stat_levels)
    ltmad_stats <- dplyr::arrange(ltmad_stats, STATION_NUMBER, Statistic)
  }
  
  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(ltmad_stats)[names(ltmad_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    ltmad_stats <- dplyr::select(ltmad_stats, -STATION_NUMBER)
  }
  
  
  
  # If just one value is in the table, return is as a value, otherwise return it as a tibble
  if(nrow(ltmad_stats) == 1 & ncol(ltmad_stats) == 1){
    dplyr::pull(dplyr::as_tibble(ltmad_stats)[1,1])
  } else {
    dplyr::as_tibble(ltmad_stats)
  }
  
}
