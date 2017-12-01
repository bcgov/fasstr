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

#' @title Calculate the long-term mean annual discharge
#'
#' @description Calculates the long-term mean annual discharge of a streamflow dataset. Averages all daily discharge values from all years,
#'   unless specified.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param excluded_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.    
#' @param percent_MAD Numeric. Percent of long-term mean annual discharge to output. Default \code{100} (i.e. 100pct. MAD).
#'
#' @return A numeric value of the long-term mean annual discharge.
#' 
#' @examples
#' \dontrun{
#' 
#' fasstr_LTMAD(flowdata = data, start_year = 1980, end_year = 2010)
#' 
#' fasstr_LTMAD(HYDAT = "08NM116",water_year = TRUE, excluded_years = (1990, 1992:1994))
#' 
#' fasstr_LTMAD(HYDAT = "08NM116", percent_MAD = 20)
#' 
#' }
#' @export

#--------------------------------------------------------------

fasstr_LTMAD <- function(flowdata=NULL,
                         HYDAT=NULL,
                         water_year=FALSE, 
                         water_year_start=10,
                         start_year=NULL,
                         end_year=NULL,
                         excluded_years=NULL,
                         percent_MAD=100){
  
  
  #--------------------------------------------------------------
  #  Error checking on the input parameters
  
  if( !is.null(HYDAT) & !is.null(flowdata))  {
    stop("must select either flowdata or HYDAT arguments, not both")}
  if( is.null(HYDAT)) {
    if( is.null(flowdata)) {stop("one of flowdata or HYDAT arguments must be set")}
    if( !is.data.frame(flowdata)) {stop("flowdata arguments is not a data frame")}
    if( !all(c("Date","Value") %in% names(flowdata))){stop("flowdata data frame doesn't contain the variables 'Date' and 'Value'")}
    if( !inherits(flowdata$Date[1], "Date")){stop("'Date' column in flowdata data frame is not a date")}
    if( !is.numeric(flowdata$Value)) {stop("'Value' column in flowdata data frame is not numeric")}
    if( any(flowdata$Value <0, na.rm=TRUE)) {stop('flowdata cannot have negative values - check your data')}
  }
  
  if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}
  if( !is.numeric(water_year_start) ) {stop("water_year_start must be a number between 1 and 12 (Jan-Dec)")}
  if( length(water_year_start)>1) {stop("water_year_start must be a number between 1 and 12 (Jan-Dec)")}
  if( !water_year_start %in% c(1:12) ) {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  
  if( length(start_year)>1) {stop("only one start_year integer can be selected")}
  if( !is.null(start_year) ) {if( !start_year %in% c(0:5000) )  {stop("start_year must be an integer")}}
  if( length(end_year)>1) {stop("only one end_year integer can be selected")}
  if( !is.null(end_year) ) {if( !end_year %in% c(0:5000) )  {stop("end_year must be an integer")}}
  
  if( !is.null(excluded_years) ) {if( !all(excluded_years %in% c(0:5000)) )  {stop("excluded_years must be integers (ex. 1999 or c(1999,2000))")}}
  if ( (is.null(start_year) & is.null(end_year) & is.null(excluded_years)) & water_year ) {
    message("water_year=TRUE ignored; no start_year, end_year, or excluded_years selected to filter the dates")}
  
  if( length(percent_MAD)>1) {stop("percent_MAD must be a single number > 0")}
  if( percent_MAD <=0 ) {stop("percent_MAD must be a single number > 0")}
  
  
  #--------------------------------------------------------------
  # If HYDAT station is listed, check if it exists and make it the flowdata
  
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  
  #--------------------------------------------------------------
  # Set the flowdata for analysis
  
  # add date variables to determine the min/max cal/water years
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  min_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))
  max_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))
  
  # If start/end years are not select, set them as the min/max dates
  if (is.null(start_year)) {start_year <- min_year}
  if (is.null(end_year)) {end_year <- max_year}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  #  Fill in the missing dates and the add the date variables again
  flowdata <- fasstr::fasstr_add_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  
  # Filter for the selected year
  flowdata <- dplyr::filter(flowdata,AnalysisYear>=start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% excluded_years))
  
  
  #--------------------------------------------------------------
  # Complete the analysis
  
  # Calculate the long-term mean annual discharge
  LTMAD <- mean(flowdata$Value,na.rm =T)*(percent_MAD/100)
  
  
  
  return(LTMAD)
  
}
