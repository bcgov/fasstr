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

#' @title Add daily cumulative runoff yield flows on an annual basis
#'
#' @description Add a column of daily cumulative runoff yield flows on an annual basis to a streamflow dataset. Adds the runoff yield 
#'    discharge from each day with the previous day(s) for each year, in units of millimetres. The cumulative flows restart every year.
#'    Converts cumulative discharge to a depth of water based on the upstream drainge basin area.
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
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' 
#' @return A column of cumulative runoff yield flows for each day for each year specific called 'Cumul_Yield_mm' added to the flowdata 
#'    data frame input or HYDAT dataset, in units of millimetres.
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_add_cumulative_yield(flowdata = flowdata, basin_area = 105.6)
#' 
#'fasstr_add_cumulative_yield(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------

fasstr_add_cumulative_yield <- function(flowdata=NULL,
                                        HYDAT=NULL,
                                        basin_area=NA,
                                        water_year=FALSE,
                                        water_year_start=10){
  
  
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
  
  if( !is.logical(water_year))         {stop("water_year argument must be logical (TRUE/FALSE)")}
  if( !is.numeric(water_year_start) )  {stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec)")}
  if( length(water_year_start)>1)      {stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec)")}
  if( !water_year_start %in% c(1:12) ) {stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec)")}
  
  if( !is.na(basin_area) & !is.numeric(basin_area)) {stop("basin_area argument must be numeric")}
  if( length(basin_area)>1)                         {stop("basin_area argument cannot have length > 1")}
  
  # If HYDAT station is listed, check if it exists and extract the flowdata and basin_area
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 )                                  {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' arguement does not exist.")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
    if (is.na(basin_area)) {basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = HYDAT)$DRAINAGE_AREA_GROSS)}
  }
  
  # If STATION_NUMBER column is in flowdata, extract the basin_area
  if ( is.null(HYDAT) & is.na(basin_area) & "STATION_NUMBER" %in% names(flowdata) ){
    basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = flowdata$STATION_NUMBER[1])$DRAINAGE_AREA_GROSS)
  }
  if( is.na(basin_area) )  {stop("no basin_area provided")}
  
  #--------------------------------------------------------------
  # Set the flowdata for analysis
  
  # Get the list/order of dates to eventually return to the same order
  flowdata <- flowdata[ order(flowdata$Date),]
  col_ord <- names(flowdata)
  
  # Fill in missing dates to ensure all years are covered
  flowdata_temp <- fasstr::fasstr_add_missing_dates(flowdata=flowdata)
  flowdata_temp <- fasstr::fasstr_add_date_vars(flowdata=flowdata_temp,water_year = T,water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata_temp$AnalysisYear <- flowdata_temp$WaterYear
  }  else {
    flowdata_temp$AnalysisYear <- flowdata_temp$Year
  }
  
  #--------------------------------------------------------------
  # Add column to flowdata
  
  # Add cumulative yields
  flowdata_temp <- dplyr::mutate(dplyr::group_by(flowdata_temp,AnalysisYear),Cumul_Yield_mm=cumsum(Value)*86400/(basin_area*1000))
  
  # Return flowdata to original dates
  flowdata_temp <- dplyr::select(dplyr::ungroup(flowdata_temp),Date,Cumul_Yield_mm)
  
  flowdata <- merge(flowdata,flowdata_temp,by="Date",all.x = T)
  flowdata <-  flowdata[,c(col_ord,paste("Cumul_Yield_mm"))]
  
  
  return(flowdata)
}

