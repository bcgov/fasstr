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

#' @title Add calendar and water year date variables.
#'
#' @description Adds mulitple date variables to a dataframe from a column of dates, including
#'   year, month (numeric and text), day of years, and water years and day of water years.
#'
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Value' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flowdata} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param water_year_start Numeric. Month to start water year (1 to 12 for Jan to Dec).
#'
#' @examples
#' \dontrun{
#' 
#' set example :)
#' }
#' @export

#'
#--------------------------------------------------------------
# Compute the statistics on an (calendar and water) year basis

fasstr_add_date_vars <- function(
                         flowdata=NULL,
                         HYDAT=NULL,
                         water_year_start=10){  
  
  #  Compute statistics on an annual (calendar and water) year basis
  #
  #  See the man-roxygen director for definition of parameters
  #
  #  Output: List with elements given above.
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
  if( is.null(HYDAT) & !"Date" %in% names(flowdata)){
    stop("flowdata dataframe doesn't contain a Date variable.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( !is.numeric(water_year_start))  {
    stop("water_year_start parameter must be numeric between 1 and 12 (Jan-Dec)")}
  if( water_year_start<1 & water_year_start>12 )  {
    stop("water_year_start parameter must be numeric between 1 and 12 (Jan-Dec)")}

  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    flowdata <- tidyhydat::hy_daily_flows(station_number =  HYDAT)
  }
  
  # Create values used to calculate the water year day of year
  if (water_year_start==2) {doy.temp <- c(31,31)}
  if (water_year_start==3) {doy.temp <- c(61,62)}
  if (water_year_start==4) {doy.temp <- c(90,91)}
  if (water_year_start==5) {doy.temp <- c(120,121)}
  if (water_year_start==6) {doy.temp <- c(151,152)}
  if (water_year_start==7) {doy.temp <- c(181,182)}
  if (water_year_start==8) {doy.temp <- c(212,213)}
  if (water_year_start==9) {doy.temp <- c(243,244)}
  if (water_year_start==10) {doy.temp <- c(273,274)}
  if (water_year_start==11) {doy.temp <- c(304,305)}
  if (water_year_start==12) {doy.temp <- c(334,335)}

  # Calculate each date variable
  flowdata$Year  <- lubridate::year(flowdata$Date)
  flowdata$Month  <- lubridate::month(flowdata$Date)
  flowdata$MonthName <- month.abb[flowdata$Month]
  flowdata$DayofYear <- lubridate::yday(flowdata$Date)

  if (water_year_start==1) {
    flowdata$WaterYear <- flowdata$Year
    flowdata$WaterDayofYear <- flowdata$DayofYear
  } else {
    flowdata$WaterYear <- as.numeric(ifelse(flowdata$Month>=water_year_start,
                                            flowdata$Year+1,
                                            flowdata$Year))
    flowdata$WaterDayofYear <- ifelse(flowdata$Month<water_year_start,
                                      flowdata$DayofYear+(365-doy.temp[1]),
                                      ifelse((as.Date(with(flowdata, paste(Year+1,01,01,sep="-")),"%Y-%m-%d")
                                              -as.Date(with(flowdata, paste(Year,01,01,sep="-")),"%Y-%m-%d"))==366,
                                             flowdata$DayofYear-doy.temp[2],
                                             flowdata$DayofYear-doy.temp[1]))
  }
  
 

  
  # ADD SEASONS?

  
  return(flowdata)
} # end of function

