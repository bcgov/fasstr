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
#' @param flow_data Data frame. A data frame of daily mean flow data. Not required if \code{HYDAT} argument is used.
#' @param flow_values A column in flow_data that contains numeric values of daily mean flow data, in units of cubic metres per second. 
#'    Default \code{Value}.
#' @param flow_basin_areas A column in flow_data of upstream drainage basin areas used to calculate the daily yield. If left blank
#'    this function will use basin areas provided by the fasstr::add_basin_areas() function using the \code{basin_area} argument.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flow_data} argument is used.
#' @param basin_area Numeric. If no \code{flow_basin_area} provided in flow_data, used to determine basin areas from the
#'    fasstr::add_basin_areas() function. Leave blank if \code{HYDAT} is used or a column in \code{flow_data} called 'STATION_NUMBER' 
#'    contains a WSC station number, as the basin area will be extracted from HYDAT. Using \code{basin_area} will replace the HYDAT basin 
#'    area. If setting basin areas for multiple stations without HYDAT, set them using 
#'    \code{basin_area = c("08NM116" = 795, "08NM242" = 10)}; stations not listed will result in NA basin areas.
#' @return A data frame of the original flow_data or HYDAT data with an additional columns:
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


#--------------------------------------------------------------

add_daily_yield <- function(flow_data=NULL,
                            flow_values=Value,
                            flow_basin_areas=flow_basin_areas,
                            HYDAT=NULL,
                            basin_area=NA){
  
  
  
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
  
  # Save the original columns from the flow_data to remove added columns
  orig_cols <- names(flow_data)
  
  # If no STATION_NUMBER in flow_data, make it so (required for grouping)
  if(!"STATION_NUMBER" %in% colnames(flow_data)) {
    flow_data$STATION_NUMBER <- "XXXXXXX"
  }
  
  # This method allows the user to select the Value column if the column name is different
  if(!as.character(substitute(flow_values)) %in% names(flow_data)) 
    stop("Flow values not found. Rename flow values column to 'Value' or identify the column using 'flow_values' argument.")
  
  # Temporarily rename the value area column
  names(flow_data)[names(flow_data) == as.character(substitute(flow_values))] <- "Value"
  
  # Check columns are in proper formats
  if(!is.numeric(flow_data$Value))          stop("'Value' column in flow_data data frame does not contain numeric values.")
  
  
  ## SET UP BASIN AREA
  ## -----------------
  
  # Check methods
  if(as.character(substitute(flow_basin_areas)) != "flow_basin_areas" & !is.na(basin_area)) 
    stop("Both flow_basin_areas and basin_area arguments cannot be used. Select one.")
    
  # If there is basin area column provided use it, otherwise create one
  if(as.character(substitute(flow_basin_areas)) != "flow_basin_areas"){
    
    #Check if the column is a name in flow_data
    if(!as.character(substitute(flow_basin_areas)) %in% names(flow_data)) 
      stop("'",paste(as.character(substitute(flow_basin_areas))),"' is not a column in the flow_data data frame.")
    
    # Create the column name
    flow_data$Basin_Area_sqkm_temp <- flow_data[,as.character(substitute(flow_basin_areas))]
    
    # Check if the basin areas are numeric
    if(!is.numeric(flow_data$Basin_Area_sqkm_temp)) stop("Basin area column in flow_data data frame does not contain numeric values.")
    
  } else {
    # Get the basin areas using fasstr::add_basin_area if no column provided
    suppressWarnings(flow_data <- fasstr::add_basin_area(flow_data, basin_area = basin_area))
    flow_data$Basin_Area_sqkm_temp <- flow_data$Basin_Area_sqkm
  }
  
  ## ADD YIELD COLUMN
  ## ----------------
  
  flow_data <- dplyr::mutate(flow_data, Yield_mm = Value * 86400 / (Basin_Area_sqkm_temp * 1000))
  
  # Return the original names of the Date and Value columns
  names(flow_data)[names(flow_data) == "Value"] <- as.character(substitute(flow_values))
  
  
  # Return columns to original order plus new column
  if("Yield_mm" %in% orig_cols){
    flow_data <-  flow_data[,c(orig_cols)]
  } else {
    flow_data <-  flow_data[,c(orig_cols, paste("Yield_mm"))]
  }
  
  
  flow_data
  
}

