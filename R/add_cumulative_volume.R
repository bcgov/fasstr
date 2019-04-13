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

#' @title Add daily cumulative volumetric flows on an annual basis
#'
#' @description Add a column of rolling daily cumulative volumetric flows on an annual basis to a streamflow dataset. Adds the 
#'    volumetric discharge from each day with the previous day(s) for each year, in units of cubic metres. 
#'    The cumulative flows restart every year and are only calculated in years with complete data.
#'
#' @inheritParams calc_annual_stats
#' 
#' @return A tibble data frame of the source data with an additional column:
#'   \item{Cumul_Volume_m3}{cumulative volumetric flows for each day for each year, in units of cubic metres}
#'   
#' @examples
#' \dontrun{
#' 
#' # Add a column based on years starting in August
#' add_cumulative_volume(station_number = "08NM116", 
#'                       water_year_start = 8)
#' }
#' @export


add_cumulative_volume <- function(data,
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
  
  # Check if data is provided
  flow_data <- flowdata_import(data = data, 
                               station_number = station_number)
  
  # Save the original columns from the flow_data to remove added columns
  orig_cols <- names(flow_data)
  
  # Get groups of flow_data to return after
  orig_groups <- dplyr::group_vars(flow_data)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)))
  
  
  ## FLOW DATA PREP
  ## --------------
  
  # Fill missing dates, add date variables
  flow_data_temp <- analysis_prep(data = flow_data, 
                                  water_year_start = water_year_start)
  
  
  ## ADD VOLUME COLUMN
  ## -----------------
  
  # Create cumsum function to not create cumsum if any NA's in a given year
  cumsum_na <- function(x) {
    if (any(is.na(x))) {
      return(rep(NA, length(x)))
    } else {
      cumsum(x)
    }
  }
  
  # Add cumulative volume column and ungroup (remove analysisyear group)
  flow_data_temp <- dplyr::ungroup(flow_data_temp)
  flow_data_temp <- dplyr::mutate(dplyr::group_by(flow_data_temp, STATION_NUMBER, WaterYear), 
                                  Cumul_Volume_m3 = cumsum_na(Value) * 86400)
  flow_data_temp <- dplyr::ungroup(flow_data_temp)
  
  # Get new column and merge back with
  flow_data_temp <- dplyr::select(flow_data_temp, STATION_NUMBER, Date, Cumul_Volume_m3)
  
  # If column existed, replace it, otherwise add it
  if ("Cumul_Volume_m3" %in% orig_cols){
    flow_data <- merge(flow_data, flow_data_temp, by = c("STATION_NUMBER", "Date"), all.x = TRUE)
    flow_data$Cumul_Volume_m3 <- flow_data$Cumul_Volume_m3.y
    flow_data <- dplyr::select(flow_data, -Cumul_Volume_m3.y, -Cumul_Volume_m3.x)
  } else {
    flow_data <- merge(flow_data, flow_data_temp, by = c("STATION_NUMBER", "Date"), all.x = TRUE)
  }
  
  
  ## Reformat to original names and groups
  ## -------------------------------------
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(dates))
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  
  # Return columns to original order plus new column
  if ("Cumul_Volume_m3" %in% orig_cols){
    flow_data <-  flow_data[, c(orig_cols)]
  } else {
    flow_data <-  flow_data[, c(orig_cols, paste("Cumul_Volume_m3"))]
  }
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data, dplyr::vars(orig_groups))
  
  dplyr::as_tibble(flow_data)
  
}

