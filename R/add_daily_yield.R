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
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param basin_area Numeric. Upstream drainage basin area of the hydrometric station, in sq. km. Leave blank if \code{HYDAT} is used or 
#'    a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the basin area will be extracted from HYDAT. 
#'    Setting the basin area will replace the HYDAT basin area. 
#'    
#' @return A data frame of the original flowdata or HYDAT data with an additional column:
#'   \item{Yield_MM}{daily runoff yield flow, in units of millimetres}
#'
#' @examples
#' \dontrun{
#' 
#'add_daily_yield(flowdata = flowdata, basin_area = 104.5)
#' 
#'add_daily_yield(HYDAT = "08NM116")
#'
#' }
#' @export


#--------------------------------------------------------------

add_daily_yield <- function(flowdata=NULL,
                                   HYDAT=NULL,
                                   basin_area=NA){
  
  
  #--------------------------------------------------------------
  #  Some basic error checking on the input parameters
  
  if( !is.null(HYDAT) & !is.null(flowdata))           {stop("must select either flowdata or HYDAT arguments, not both")}
  if( is.null(HYDAT)) {
    if( is.null(flowdata))                            {stop("one of flowdata or HYDAT arguments must be set")}
    if( !is.data.frame(flowdata))                     {stop("flowdata arguments is not a data frame")}
    if( !all(c("Date","Value") %in% names(flowdata))) {stop("flowdata data frame doesn't contain the variables 'Date' and 'Value'")}
    if( !inherits(flowdata$Date[1], "Date"))          {stop("'Date' column in flowdata data frame is not a date")}
    if( !is.numeric(flowdata$Value))                  {stop("'Value' column in flowdata data frame is not numeric")}
    if( any(flowdata$Value <0, na.rm=TRUE))           {warning('flowdata cannot have negative values - check your data')}
  }
  
  if( !is.na(basin_area) & !is.numeric(basin_area)) {stop("basin_area argument must be numeric")}
  if( length(basin_area)>1)                         {stop("basin_area argument cannot have length > 1")}
  
  # If HYDAT station is listed, check if it exists and extract the flowdata and basin_area
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 )                                  {stop("Only one HYDAT station can be selected.")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
    if (is.na(basin_area)) {basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = HYDAT)$DRAINAGE_AREA_GROSS)}
  }
  
  # If STATION_NUMBER column is in flowdata, extract the basin_area
  if ( is.null(HYDAT) & is.na(basin_area) & "STATION_NUMBER" %in% names(flowdata) ){
    basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = flowdata$STATION_NUMBER[1])$DRAINAGE_AREA_GROSS)
  }
  if( is.na(basin_area) )  {stop("no basin_area provided")}
  
  
  #--------------------------------------------------------------
  # Add column to flowdata
  
  flowdata <- dplyr::mutate(flowdata,Yield_mm=Value*86400 /(basin_area*1000))
  
  
  
  return(flowdata)
}

