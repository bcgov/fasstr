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

#' @title Calculate the percentile rank of a flow value
#'
#' @description Calculates the percentile, or percentile rank, of a discharge value compared to all flow values of a streamflow dataset. 
#'    Looks up the value in the distribution (stats::ecdf() function) of all daily discharge values from all years, unless specified.
#'
#' @inheritParams calc_daily_stats
#' @inheritParams calc_monthly_stats
#' @param flow_value A numeric flow value of which to determine the percentile rank. Required.
#'
#' @return A tibble data frame, or a single numeric value if no station number proivded, of the percentile rank of a given flow value.
#' 
#' @examples
#' \dontrun{
#' 
#' # Calculate the percentile rank of a 10-cms flow value from a full record
#' calc_flow_percentile(station_number = "08NM116", 
#'                      flow_value = 10)
#' 
#' # Calculate the percentile rank of a 10-cms flow value from years with no missing data
#' calc_flow_percentile(station_number = "08NM116", 
#'                      complete_years = TRUE,
#'                      flow_value = 10)
#'                      
#' # Calculate the percentile rank of a 10-cms flow value for June from years with no missing data
#' calc_flow_percentile(station_number = "08NM116", 
#'                      complete_years = TRUE,
#'                      months = 6,
#'                      flow_value = 10)
#' }
#' @export



calc_flow_percentile <- function(data,
                                 dates = Date,
                                 values = Value,
                                 groups = STATION_NUMBER,
                                 station_number,
                                 roll_days = 1,
                                 roll_align = "right",
                                 flow_value,
                                 water_year_start = 1,
                                 start_year,
                                 end_year,
                                 exclude_years,
                                 complete_years = FALSE,
                                 months = 1:12){
  
  
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

  rolling_days_checks(roll_days, roll_align)
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years)
  complete_yrs_checks(complete_years)
  
  if (missing(flow_value))        stop("A numeric flow_value is required.", call. = FALSE)
  if (!is.numeric(flow_value))    stop("A numeric flow_value is required.", call. = FALSE)
  if (length(flow_value) > 1)     stop("Only one numeric flow_value is required.", call. = FALSE)
  
  
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
  
  if (flow_value > max(flow_data$Value, na.rm = TRUE)) warning("flow_value was greater than the maximum daily flow value.", call. = FALSE)
  if (flow_value < min(flow_data$Value, na.rm = TRUE)) warning("flow_value was less than the minimum daily flow value.", call. = FALSE)
  
  # Give warning if any NA values
  missing_values_warning_noNA(flow_data$Value)

    ## CALCULATE PTILE RANK
  ## --------------------
  
  ptile_rank <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER),
                                 Percentile = round(stats::ecdf(Value)(flow_value), 5) * 100)
  
  
  # Recheck if station_number was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(ptile_rank)[names(ptile_rank) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    ptile_rank <- dplyr::select(ptile_rank, -STATION_NUMBER)
  }
  
  
  # If just one value is in the table, return is as a value, otherwise return it as a tibble
  if(nrow(ptile_rank) == 1 & ncol(ptile_rank) == 1){
    dplyr::pull(dplyr::as_tibble(ptile_rank)[1,1])
  } else {
    dplyr::as_tibble(ptile_rank)
  }
  
}
