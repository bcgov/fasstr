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

#' @title Compute long-term mean annual discharge.
#'
#' @description Computes long-term statistics of streamflow data.
#' Streamflow data can be supplied through the \code{flowdata} parameter or extracted from a 
#' HYDAT database using the tidyhydat package and \code{HYDAT} parameter.
#'
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Value' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flowdata} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param water_year Logical. Set to \code{TRUE} if data should be summarized by water year (Oct-Sep) instead of the
#'    default calendar year (Jan-Dec) (\code{water_year=FALSE}). Water years are designated by the year which they end in
#'    (e.g. water year 2000 start on 1 Oct 1999 and ends on 30 Sep 2000).
#' @param water_year_start Numeric. Month to start water year (1 to 12 for Jan to Dec).
#' @param start_year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start_year} is the first
#'    year of the data provided.
#' @param end_year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end_year} is the last
#'    year of the data provided.
#' @param excluded_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param percent_MAD Numeric. Factor to mulitply the MAD. Deafult 100 (i.e. 100%MAD)
#'
#' @examples
#' \dontrun{
#' 
#' coming soon :)
#' }
#' @export

#'


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

fasstr_LTMAD <- function(flowdata=NULL,
                         HYDAT=NULL,
                         water_year=FALSE, 
                         water_year_start=10,
                         start_year=NULL,
                         end_year=NULL,
                         exclude_years=NULL,
                         percent_MAD=100){
  
  
  #
  #############################################################
  #  Some basic error checking on the input parameters
  #
  if( is.null(flowdata) & is.null(HYDAT)) {
    stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {
    stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {
    stop("flowdata parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Value") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Value.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {
    stop("Value column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {
    stop('flowdata cannot have negative values - check your data')}
  
  if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}
  if( length(water_year_start)>1) {stop("water_year_start must be a number between 1 and 12 (Jan-Dec)")}
  if( water_year_start <1 | water_year_start >12 ) {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  if( !(water_year_start==floor(water_year_start)))  {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if ( (is.null(start_year) & is.null(end_year) & is.null(exclude_years)) & water_year ) {
    message("water_year=TRUE ignored; no start_year, end_year, or exclude_years selected to filter the dates")
  }
  
  if( length(percent_MAD)>1) {stop("percent_MAD must be a number >0")}
  if( percent_MAD <=0 ) {stop("percent_MAD must be a number >0")}
  
  
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
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
  flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% exclude_years))
  
  LTMAD <- mean(flowdata$Value,na.rm =T)*(percent_MAD/100)
  
  return(LTMAD)
  
  
}
