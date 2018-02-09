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

#' @title Add a basin area column to daily flows
#'
#' @description Add a column of basin areas to a streamflow dataset, in units of square kilometres.
#'
#' @param data Daily data to be analyzed. Options:
#' 
#'    A data frame of daily data that contains columns of dates, values, and (optional) groups (ex. station 
#'    names/numbers).
#'    
#'    A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param groups Column in the \code{data} data frame that contains unique identifiers for different data sets. 
#'    Only required if using the data frame option of \code{data} and groups column is not named 'STATION_NUMBER'.
#'    Function will automatically group by a column named 'STATION_NUMBER' if present. Remove the 'STATION_NUMBER' column or identify 
#'    another non-existing column name to remove this grouping. Identify another column if desired. Default \code{STATION_NUMBER}. 
#' @param basin_area Upstream drainage basin area to apply to daily observations. Options:
#'    
#'    Leave blank if \code{groups} is STATION_NUMBER with HYDAT station numbers to extract basin areas from HYDAT.
#'    
#'    Single numeric value to apply to all observations.
#'    
#'    List each basin area for each groups factor (can override HYDAT value) as such \code{c("08NM116" = 795, "08NM242" = 10)}.
#'    Factors not listed will result in NA basin areas.
#'    
#' @return A data frame of the original source data with an additional column:
#'   \item{Basin_Area_sqkm}{area of upstream drainage basin area, in square kilometres}
#'
#' @examples
#' \dontrun{
#' 
#'add_basin_area(data = flow_data, area = 104.5)
#' 
#'add_basin_area(data = "08NM116")
#'
#' }
#' @export


add_basin_area <- function(data = NULL,
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
  
  # Get groups of flow_data to return after
  flow_data_groups <- dplyr::group_vars(flow_data)
  
  # If no STATION_NUMBER in flow_data, make it so (required for station groups)
  if(!as.character(substitute(groups)) %in% colnames(flow_data)) {
    flow_data[, as.character(substitute(groups))] <- "XXXXXXX"
  }
  names(flow_data)[names(flow_data) == as.character(substitute(groups))] <- "STATION_NUMBER"
  
  
  ## CHECKS ON BASIN AREA
  ## --------------------
  
  # Extract basin_area for HYDAT stations if basin_area=NA 
  if(all(is.na(basin_area))){
    basin_stations <- data.frame(STATION_NUMBER = unique(flow_data$STATION_NUMBER), stringsAsFactors = FALSE)
    basin_HYDAT <- suppressMessages(tidyhydat::hy_stations(station_number = basin_stations$STATION_NUMBER))
    basin_HYDAT <- dplyr::select(basin_HYDAT, STATION_NUMBER, Basin_Area_sqkm = DRAINAGE_AREA_GROSS)
    basin_area_table <- dplyr::right_join(basin_HYDAT, basin_stations, by = "STATION_NUMBER")
  }
  
  # Apply basin_areas to matching STATION_NUMBERS
  if(!all(is.na(basin_area))){
    if(!is.numeric(basin_area)) stop("basin_area argument must be numeric.", call. = FALSE)
    if(is.null(names(basin_area)) & length(basin_area) == 1) {
      if(length(unique(flow_data$STATION_NUMBER)) > 1) warning("Just one basin_area applied without a corresponding STATION_NUMBER, the basin_area will be applied to all stations.", call. = FALSE)
      basin_area_table <- data.frame(STATION_NUMBER = unique(flow_data$STATION_NUMBER), Basin_Area_sqkm = basin_area)
    } else {
      if(length(basin_area)!=length(unique(flow_data$STATION_NUMBER)) | !all(names(basin_area) %in% unique(flow_data$STATION_NUMBER))) 
        warning("The number/names of STATION_NUMBERS and basin_area values provided do not match the number/names of STATION_NUMBERS in the flow data. Only those that match will be applied.", call. = FALSE)
      #if(!all(names(basin_area) %in% unique(flow_data$STATION_NUMBER))) warning("All STATION_NUMBERS listed in basin_area do not match those in the flow data. Only those that match will be applied.")
      basin_area_table <- data.frame(STATION_NUMBER = names(basin_area), Basin_Area_sqkm = basin_area, stringsAsFactors = FALSE)
    }
  }
  if(all(is.na(basin_area_table$Basin_Area_sqkm))) warning("No basin_area values provided or extracted from HYDAT. All values will be NA.", call. = FALSE)
  
  
  ## ADD BASIN AREA COLUMN
  ## ---------------------
  
  # Get new column and merge back with
  if("Basin_Area_sqkm" %in% orig_cols){
    flow_data <- dplyr::ungroup(flow_data)
    flow_data <- dplyr::left_join(flow_data, basin_area_table, by = "STATION_NUMBER")
    flow_data$Basin_Area_sqkm <- flow_data$Basin_Area_sqkm.y
    flow_data <- dplyr::select(flow_data, -Basin_Area_sqkm.y, -Basin_Area_sqkm.x)
  } else {
    flow_data <- suppressWarnings(dplyr::left_join(flow_data, basin_area_table, by = "STATION_NUMBER"))
  }
  
  # Return the original names of the columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  
  # Return columns to original order plus new column
  if("Basin_Area_sqkm" %in% orig_cols){
    flow_data <-  flow_data[,c(orig_cols)]
  } else {
    flow_data <-  flow_data[,c(orig_cols, paste("Basin_Area_sqkm"))]
  }

  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data,dplyr::vars(flow_data_groups))
  
  
  
  dplyr::as_tibble(flow_data)
  
}

