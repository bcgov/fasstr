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

#' @title Add a basin area column to daily flows
#'
#' @description Add a column of basin areas to a streamflow dataset, in units of square kilometres.
#'
#' @inheritParams calc_annual_stats
#' @param basin_area Upstream drainage basin area, in square kilometres, to apply to observations. Three options:
#'    
#'    (1) Leave blank if \code{groups} is STATION_NUMBER with HYDAT station numbers to extract basin areas from HYDAT.
#'    
#'    (2) A single numeric value to apply to all observations.
#'    
#'    (3) List each basin area for each group/station in groups (can override HYDAT value if listed) as such \code{c("08NM116" = 795, 
#'    "08NM242" = 10)}. If group is not listed the HYDAT area will be applied if it exists, otherwise it will be NA.
#'    
#' @return A tibble data frame of the original source data with an additional column:
#'   \item{Basin_Area_sqkm}{area of upstream drainage basin area, in square kilometres}
#'
#' @examples
#' \dontrun{
#' 
#' # HYDAT basin area
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' add_basin_area(data = flow_data)
#' 
#' # HYDAT basin area
#' add_basin_area(station_number = "08NM116")
#' 
#' # Set the basin area
#' add_basin_area(station_number = "08NM116",
#'                basin_area = 800)
#'                
#' # Set multiple basin areas for multiple stations
#' add_basin_area(station_number = c("08NM116", "08NM242"),
#'                basin_area = c("08NM116" = 800, "08NM242" = 10))
#' }
#' @export


add_basin_area <- function(data,
                           groups = STATION_NUMBER,
                           station_number,
                           basin_area){
  
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  if (missing(data)) {
    data = NULL
  }
  if (missing(station_number)) {
    station_number = NULL
  }
  if (missing(basin_area)) {
    basin_area = NA
  }
  
  # Check if data is provided
  flow_data <- flowdata_import(data = data, 
                               station_number = station_number)
  
  # Save the original columns and groups to return at the end
  orig_cols <- names(flow_data)
  orig_groups <- dplyr::group_vars(flow_data)
  
  # Format the groups column
  flow_data <- format_groups_col(data = flow_data,
                                 groups = as.character(substitute(groups)))
  
  
  ## CHECKS ON BASIN AREA
  ## --------------------
  
  # Extract basin_area for HYDAT stations if basin_area=NA 
  if(all(is.na(basin_area))){
    
    # Create a dataframe with a column of stations
    basin_stations <- data.frame(STATION_NUMBER = unique(flow_data$STATION_NUMBER), stringsAsFactors = FALSE)
    
    # Extract the basin areas and merge with the stations
    basin_HYDAT <- suppressMessages(tidyhydat::hy_stations(station_number = toupper(basin_stations$STATION_NUMBER)))
    basin_HYDAT <- dplyr::select(basin_HYDAT, STATION_NUMBER, Basin_Area_sqkm = DRAINAGE_AREA_GROSS)
    basin_area_table <- dplyr::right_join(basin_HYDAT, basin_stations, by = "STATION_NUMBER")
  }
  
  # Apply basin_areas to matching STATION_NUMBERS
  if(!all(is.na(basin_area))){
    if(!is.numeric(basin_area)) stop("Areas in basin_area argument must be numeric.", call. = FALSE)
    
    if(is.null(names(basin_area)) & length(basin_area) == 1) {
      
      # Give message that 1 supplied basin area will apply to all stations
      if(length(unique(flow_data$STATION_NUMBER)) > 1) {
        warning("One basin_area supplied with multiple groups/stations, the basin_area supplied will apply to all stations.", call. = FALSE)
      }
      
      # Make a dataframe and apply single basin area to all groups/stations
      basin_area_table <- data.frame(STATION_NUMBER = unique(flow_data$STATION_NUMBER), Basin_Area_sqkm = basin_area)
      
    } else {
      
      # Warning if number of station.groups dont match the number of supplied basin areas
      if(length(basin_area) != length(unique(flow_data$STATION_NUMBER)) | !all(toupper(names(basin_area)) %in% unique(flow_data$STATION_NUMBER))) 
        warning("The supplied groups/stations and basin_area values do not match the groups/stations of STATION_NUMBERS in the flow data. Only those that match will be applied.", call. = FALSE)
      #if(!all(names(basin_area) %in% unique(flow_data$STATION_NUMBER))) warning("All STATION_NUMBERS listed in basin_area do not match those in the flow data. Only those that match will be applied.")
      
      # Create a dataframe with a column of stations
      basin_stations <- data.frame(STATION_NUMBER = toupper(unique(flow_data$STATION_NUMBER)), stringsAsFactors = FALSE)
      
      # Extract the basin areas and merge with the stations
      basin_HYDAT <- suppressMessages(tidyhydat::hy_stations(station_number = toupper(basin_stations$STATION_NUMBER)))
      basin_HYDAT <- dplyr::select(basin_HYDAT, STATION_NUMBER, Basin_Area_sqkm = DRAINAGE_AREA_GROSS)
      basin_area_table <- dplyr::right_join(basin_HYDAT, basin_stations, by = "STATION_NUMBER")
      
      # Make a dataframe and apply basin areas to all groups/stations
      basin_area_table2 <- data.frame(STATION_NUMBER = toupper(names(basin_area)), Basin_Area_sqkm2 = basin_area, stringsAsFactors = FALSE)
      basin_area_table <- suppressMessages(dplyr::left_join(basin_area_table, basin_area_table2))
      
      basin_area_table <- dplyr::mutate(basin_area_table,
                                         Basin_Area_sqkm = ifelse(STATION_NUMBER %in% toupper(names(basin_area)), Basin_Area_sqkm2, Basin_Area_sqkm))
      basin_area_table <- dplyr::select(basin_area_table, -Basin_Area_sqkm2)
                                        
      
      
    }
  }
  
  # If the process resulted in no basin areas, make a message
  if(all(is.na(basin_area_table$Basin_Area_sqkm))) 
    warning("No basin_area values provided or extracted from HYDAT. All basin area values will be NA.", call. = FALSE)
  
  
  ## ADD BASIN AREA COLUMN
  ## ---------------------
  
  # Get new column and merge back with
  if("Basin_Area_sqkm" %in% orig_cols){
    flow_data <- dplyr::ungroup(flow_data)
    flow_data <- suppressWarnings(dplyr::left_join(flow_data, basin_area_table, by = "STATION_NUMBER"))
    flow_data$Basin_Area_sqkm <- flow_data$Basin_Area_sqkm.y
    flow_data <- dplyr::select(flow_data, -Basin_Area_sqkm.y, -Basin_Area_sqkm.x)
  } else {
    flow_data <- suppressWarnings(dplyr::left_join(flow_data, basin_area_table, by = "STATION_NUMBER"))
  }
  
  
  ## Reformat to original names and groups
  ## -------------------------------------
  
    # Return the original names of the columns
  names(flow_data)[names(flow_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  
  # Return columns to original order plus new column
  if("Basin_Area_sqkm" %in% orig_cols){
    flow_data <-  flow_data[,c(orig_cols)]
  } else {
    flow_data <-  flow_data[,c(orig_cols, paste("Basin_Area_sqkm"))]
  }
  
  # Regroup by the original groups
  flow_data <- dplyr::group_by_at(flow_data,dplyr::vars(orig_groups))
  
  
  
  dplyr::as_tibble(flow_data)
}

