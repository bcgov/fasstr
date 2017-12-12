# Copyright 2017 Province of British Columbia
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
#' @param flow_data Data frame. A data frame of daily mean flow data. Not required if \code{HYDAT} argument is used.
#' @param flow_values A column in flow_data that contains numeric values of daily mean flow data, in units of cubic metres per second. 
#'    Default \code{Value}.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flow_data} argument is used.
#' 
#' @return A data frame of the original flow_data or HYDAT data with an additional column:
#'   \item{Volume_m3}{daily total volumetric flow, in units of cubic metres}
#'
#' @examples
#' \dontrun{
#' 
#'add_daily_volume(flow_data = flow_data)
#' 
#'add_daily_volume(HYDAT = "08NM116")
#'
#' }
#' @export

#--------------------------------------------------------------

add_daily_volume <- function(flow_data=NULL,
                             flow_values=Value,
                             HYDAT=NULL){
  
  
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
  # Check if data is provided
  if(is.null(flow_data) & is.null(HYDAT))   stop("No flow data provided, must use flow_data or HYDAT arguments.")
  if(!is.null(flow_data) & !is.null(HYDAT)) stop("Only one of flow_data or HYDAT arguments can be used.")
  
  # Get HYDAT data if selected and stations exist
  if(!is.null(HYDAT)) {
    if(!all(HYDAT %in% dplyr::pull(tidyhydat::allstations[1]))) stop("One or more stations listed in 'HYDAT' do not exist.")
    flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # This method allows the user to select the Value column if the column name is different
  if(!as.character(substitute(flow_values)) %in% names(flow_data)) 
    stop("Flow values not found. Rename flow values column to 'Value' or identify the column using 'flow_values' argument.")
  # Temporarily rename the Value column
  names(flow_data)[names(flow_data) == as.character(substitute(flow_values))] <- "Value"
  
  # Check columns are in proper formats
  if(!is.numeric(flow_data$Value))          stop("'Value' column in flow_data data frame does not contain numeric values.")
  
  
  
  ## ADD VOLUME COLUMN
  ## -----------------
  
  flow_data <- dplyr::mutate(flow_data, Volume_m3 = Value * 86400)
  
  
  # Return the original names Value column
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(flow_values))
  
  
  flow_data
  
}

