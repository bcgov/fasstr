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

#' @title Add daily volumetric runoff yields
#'
#' @description Add a column of daily runoff yields to a streamflow dataset, in units of millimetres. Converts the discharge to a depth
#'   of water based on the upstream drainge basin area.
#'
#' @param data Daily data to be analyzed. Options:
#' 
#'    A data frame of daily data that contains columns of dates, values, and (optional) groups (ex. station 
#'    names/numbers).
#'    
#'    A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param values Column in the \code{data} data frame that contains numeric flow values, in units of cubic metres per second.
#'    Only required if using the data frame option of \code{data} and values column is not named 'Value'. Default \code{Value}. 
#' @param basin_area Upstream drainage basin area to apply to daily observations. Options:
#'    
#'    Leave blank if \code{groups} is STATION_NUMBER with HYDAT station numbers to extract basin areas from HYDAT.
#'    
#'    Single numeric value to apply to all observations.
#'    
#'    List each basin area for each grouping factor (can override HYDAT value) as such \code{c("08NM116" = 795, "08NM242" = 10)}.
#'    Factors not listed will result in NA basin areas.
#'    
#' @return A data frame of the source data with an additional column:
#'   \item{Yield_mm}{daily runoff yield flow, in units of millimetres}
#'
#' @examples
#' \dontrun{
#' 
#'add_daily_yield(flow_data = flow_data, basin_area = 104.5)
#' 
#'add_daily_yield(HYDAT = "08NM116")
#'
#' }
#' @export


add_daily_yield <- function(data = NULL,
                            values = Value,
                            groups = STATION_NUMBER,
                            basin_area = NA){
  
  
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
  # Check if data is provided
  if(is.null(data))   stop("No data provided, must provide a data frame or HYDAT station number(s).")
  if(is.vector(data)) {
    if(!all(data %in% dplyr::pull(tidyhydat::allstations[1]))) 
      stop("One or more stations numbers listed in data argument do not exist in HYDAT. Re-check numbers or provide a data frame of data.")
    flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number = data))
  } else {
    flow_data <- data
  }
  if(!is.data.frame(flow_data)) stop("Incorrect selection for data argument, must provide a data frame or HYDAT station number(s).")
  flow_data <- as.data.frame(flow_data) # Getting random 'Unknown or uninitialised column:' warnings if using tibble
  
  # Save the original columns from the flow_data to remove added columns
  orig_cols <- names(flow_data)
  
  # If no STATION_NUMBER in flow_data, make it so (required for grouping)
  if(!as.character(substitute(groups)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(groups))] <- "XXXXXXX"
  }
  
  # This method allows the user to select the Value column if the column name is different
  if(!as.character(substitute(values)) %in% names(flow_data)) 
    stop("Flow values not found. Rename flow values column to 'Value' or identify the column using 'values' argument.")
  # Temporarily rename the Value column
  names(flow_data)[names(flow_data) == as.character(substitute(groups))] <- "STATION_NUMBER"
  names(flow_data)[names(flow_data) == as.character(substitute(values))] <- "Value"
  
  # Check columns are in proper formats
  if(!is.numeric(flow_data$Value))          stop("'Value' column in data frame does not contain numeric values.")
  
  
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
  
  
  # Return columns to original order plus new column
  if("Yield_mm" %in% orig_cols){
    flow_data <-  flow_data[, c(orig_cols)]
  } else {
    flow_data <-  flow_data[, c(orig_cols, paste("Yield_mm"))]
  }
  
  
  dplyr::as_tibble(flow_data)
  
}

