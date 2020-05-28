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

#' @title Add a daily volumetric water yield column to daily flows
#'
#' @description Add a column of daily water yields to a daily streamflow data set, in units of millimetres. Converts the discharge to a depth
#'   of water based on the upstream drainage basin area.
#'
#' @inheritParams calc_annual_stats
#' @inheritParams add_basin_area
#'    
#' @return A tibble data frame of the source data with an additional column:
#'   \item{Yield_mm}{daily water yield, in units of millimetres}
#'
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Add a column of yields based on HYDAT basin area
#' add_daily_yield(station_number = "08NM116")
#'                      
#' # Add a column of yields based on a custom basin area
#' add_daily_yield(station_number = "08NM116",
#'                 basin_area = 800)
#'                 
#' }
#' @export


add_daily_yield <- function(data,
                            values = Value,
                            groups = STATION_NUMBER,  
                            station_number,
                            basin_area){
  
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  if (missing(data)) {
    data <- NULL
  }
  if (missing(station_number)) {
    station_number <- NULL
  }
  if (missing(basin_area)) {
    basin_area <- NA
  }
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)

  # Save the original columns and groups from the flow_data to remove added columns
  orig_cols <- names(flow_data)
  
  # Check and rename columns
  flow_data <- format_groups_col(data = flow_data, groups = as.character(substitute(groups)))
  flow_data <- format_values_col(data = flow_data, values = as.character(substitute(values)))
  
  
  ## SET UP BASIN AREA
  ## -----------------

  suppressWarnings(flow_data <- add_basin_area(flow_data, basin_area = basin_area))
  flow_data$Basin_Area_sqkm_temp <- flow_data$Basin_Area_sqkm
  
  ## ADD YIELD COLUMN
  ## ----------------
  
  flow_data <- dplyr::mutate(flow_data, Yield_mm = Value * 86400 / (Basin_Area_sqkm_temp * 1000))
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  
  
  ## Reformat to original names and groups
  ## -------------------------------------
  
  # Return columns to original order plus new column
  if("Yield_mm" %in% orig_cols){
    flow_data <-  flow_data[, c(orig_cols)]
  } else {
    flow_data <-  flow_data[, c(orig_cols, paste("Yield_mm"))]
  }
  
  
  dplyr::as_tibble(flow_data)
  
}

