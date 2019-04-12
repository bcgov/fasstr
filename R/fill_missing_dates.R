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


#' @title Fill dates of missing flow values with NA
#'
#' @description Adds rows of dates with missing flow values to a streamflow dataset with daily flow values of NA. Missing dates will 
#'    be filled in gaps between data and compeltely fill the first and last years (calendar or water year if selected).
#'
#' @inheritParams calc_annual_stats
#' 
#' @return A tibble data frame of the source data with additional rows of filled values of missing dates.
#'
#' @examples
#' \dontrun{
#' 
#' # Fill missing dates with NA using calendar years
#' fill_missing_dates(data = "08NM116")
#' 
#' # Fill missing dates with NA using water years starting in August
#' fill_missing_dates(data = "08NM116", 
#'                    water_year_start = 8)
#' }
#' @export


fill_missing_dates <- function(data,
                               dates = Date,
                               values = Value,
                               groups = STATION_NUMBER,
                               station_number,
                               water_year_start = 1){
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data = NULL
  }
  if (missing(station_number)) {
    station_number = NULL
  }
  
  water_year_checks(water_year_start)
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Save the original columns and groups to return at the end
  orig_cols <- names(flow_data)
  orig_groups <- dplyr::group_vars(flow_data)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)),
                               rm_other_cols = FALSE)
  
  
  ## FILL IN GAPS
  ## ------------
  
  # Loop through each station number, fill in gaps and append
  flow_data_new <- flow_data[0,]
  for (stn in unique(flow_data$STATION_NUMBER)) {
    
    # Filter for station number
    flow_data_stn <- dplyr::filter(flow_data, STATION_NUMBER == stn)
    flow_data_stn <- flow_data_stn[order(flow_data_stn$Date), ]
    
    # Fill if water year is TRUE and month is not January
    if (water_year_start > 1) {
      
      # Determine the min months and years to set the start_date
      # If the month in the data is less than the water_year_start, the water year will begin in the previous calendar year
      min_month_wy <- lubridate::month(min(flow_data_stn$Date))
      min_year_wy <- lubridate::year(min(flow_data_stn$Date))
      if (min_month_wy < water_year_start) {
        start_date <- as.Date(paste(min_year_wy - 1, water_year_start, '01', sep = '-'), "%Y-%m-%d")
      } else {
        start_date <- as.Date(paste(min_year_wy, water_year_start, '01', sep = '-'), "%Y-%m-%d")
      }
      
      # Determine the max months and years to set the start_date
      # If the month in the data is greater than the water_year_start, the water year will end in the next calendar year
      max_month_wy <- lubridate::month(max(flow_data_stn$Date))
      max_year_wy <- lubridate::year(max(flow_data_stn$Date))
      if (max_month_wy > water_year_start) {
        end_date <- as.Date(paste(max_year_wy + 1, water_year_start, '01', sep = '-'), "%Y-%m-%d") - 1
      } else {
        end_date <- as.Date(paste(max_year_wy, water_year_start, '01', sep = '-'), "%Y-%m-%d") - 1
      }
      
      # Fill in missing dates
      flow_data_stn <- merge(flow_data_stn, 
                             data.frame(Date = seq(start_date, end_date, 1)),
                             all.y = TRUE)
      
      
      # Fill not water year, or January is chosen as water year start  
    } else {
      min_year <- lubridate::year(min(flow_data_stn$Date))
      max_year <- lubridate::year(max(flow_data_stn$Date))
      
      # Fill in missing dates
      flow_data_stn <- merge(flow_data_stn, 
                             data.frame(Date = seq(as.Date(paste(min_year, '01-01', sep='-'), "%Y-%m-%d"),
                                                   as.Date(paste(max_year  ,'12-31',sep='-'), '%Y-%m-%d'), 
                                                   1)),
                             all.y = TRUE)
    }
    
    # Fill in station number and parameter gaps (removed if not originally there)
    flow_data_stn$STATION_NUMBER <- stn
    flow_data_stn$Parameter <- "Flow"
    
    # Append to flow_data
    flow_data_new <- dplyr::bind_rows(flow_data_new, flow_data_stn)
    
  }
  flow_data <- flow_data_new
  
  
  ## Reformat to original names and groups
  ## -------------------------------------
  
  # Return the original names of the columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  
  #Return columns to original order
  flow_data <-  flow_data[, orig_cols]
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data, dplyr::vars(orig_groups))
  
  
  dplyr::as_tibble(flow_data)
} 
