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

#' @title Calculate the long-term percentiles
#'
#' @description Calculates the long-term percentiles discharge of a streamflow dataset. Averages all daily discharge values from all years,
#'   unless specified.
#'   
#' @inheritParams calc_daily_stats
#' @inheritParams calc_monthly_stats
#' @param percentiles Numeric vector of percentiles to calculate. Required.
#'
#' @return A tibble data frame of a long-term percentile of selected years and months.
#' 
#' @examples
#' \dontrun{
#' 
#' # Calculate the 20th percentile
#' calc_longterm_percentile(station_number = "08NM116",
#'                          percentile = 20)
#'                          
#' # Calculate the 90th percentile value with custom years
#' calc_longterm_percentile(station_number = "08NM116", 
#'                          start_year = 1980, 
#'                          end_year = 2010, 
#'                          percentile = 90)
#' }
#' @export



calc_longterm_percentile <- function(data,
                                     dates = Date,
                                     values = Value,
                                     groups = STATION_NUMBER,
                                     station_number,
                                     percentiles,
                                     roll_days = 1,
                                     roll_align = "right",
                                     water_year_start = 1,
                                     start_year,
                                     end_year,
                                     exclude_years, 
                                     complete_years = FALSE,
                                     months = 1:12,
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
  if (missing(percentiles)) {
    percentiles = NA
  }
  
  rolling_days_checks(roll_days, roll_align)
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  complete_yrs_checks(complete_years)
  transpose_checks(transpose)
  
  if (all(is.na(percentiles))) stop("percentiles argument is required.", call. = FALSE)
  percentiles_checks(percentiles)
  
  
  
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
  
  
  #--------------------------------------------------------------
  # Complete the analysis
  
  ptile_stats <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER))
  # Calculate the long-term percentile
  for(ptile in percentiles){
    ptile_statss <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER),
                                     Percentile = stats::quantile(RollingValue, ptile / 100, na.rm = TRUE))
    names(ptile_statss)[names(ptile_statss) == "Percentile"] <- paste0("P", ptile)
    
    # Merge with ptile_statss
    ptile_stats <- merge(ptile_stats, ptile_statss, by = c("STATION_NUMBER"))
  }
  
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(ptile_stats[-(1)])
    
    # Transpose the columns for rows
    ptile_stats <- tidyr::gather(ptile_stats, Statistic, Value, -STATION_NUMBER)
    
    # Order the columns
    ptile_stats$Statistic <- factor(ptile_stats$Statistic, levels = stat_levels)
    ptile_stats <- dplyr::arrange(ptile_stats, STATION_NUMBER, Statistic)
  }
  
  
  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(ptile_stats)[names(ptile_stats) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    ptile_stats <- dplyr::select(ptile_stats, -STATION_NUMBER)
  }
  
  
  
  # If just one value is in the table, return is as a value, otherwise return it as a tibble
  if(nrow(ptile_stats) == 1 & ncol(ptile_stats) == 1){
    dplyr::pull(dplyr::as_tibble(ptile_stats)[1,1])
  } else {
    dplyr::as_tibble(ptile_stats)
  }
  
}
