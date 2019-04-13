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

#' @title Add daily volumetric flows
#'
#' @description Add a column of daily volumetric flows to a streamflow dataset, in units of cubic metres. Converts the discharge to a volume.
#'
#' @inheritParams calc_annual_stats
#' 
#' @return A tibble data frame of the source data with an additional column:
#'   \item{Volume_m3}{daily total volumetric flow, in units of cubic metres}
#'
#' @examples
#' \dontrun{
#' 
#' # Add a column of daily volumes
#' add_daily_volume(station_number = "08NM116")
#' }
#' @export


add_daily_volume <- function(data,
                             values = Value,  
                             station_number){
  
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  if (missing(data)) {
    data = NULL
  }
  if (missing(station_number)) {
    station_number = NULL
  }
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Check and rename columns
  flow_data <-   format_values_col(data = flow_data, values = as.character(substitute(values)))
  
  
  ## ADD VOLUME COLUMN
  ## -----------------
  
  flow_data <- dplyr::mutate(flow_data, Volume_m3 = Value * 86400)
  
  
  ## Reformat to original names and groups
  ## -------------------------------------
  
  # Return the original names Value column
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(values))
  
  
  dplyr::as_tibble(flow_data)
  
}

