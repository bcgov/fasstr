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


#' @title Fill missing dates with NA.
#'
#' @description Fill missing dates with NA.
#'
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Value' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flowdata} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param water_year Logical (TRUE/FALSE). Choose to fill to the start of the first/last water years.
#' @param water_year_start Numeric. Month to start water year (1 to 12 for Jan to Dec). Default 10 (Oct).
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

fasstr_fill_missing_dates <- function(flowdata=NULL,
                                      HYDAT=NULL,
                                      water_year=FALSE,
                                      water_year_start=10){
  
  #  Some basic error checking on the input parameters
  if( is.null(flowdata) & is.null(HYDAT)) {
    stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {
    stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {
    stop("flowdata parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Value") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain date or flow columns (labeled 'Value')")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {
    stop("Flow data ('Value') column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {
    stop('flowdata cannot have negative values - check your data')}
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
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  #Get the station_number and Parameter from flowdata if used HYDAT in a previous fasstr function
  if ("STATION_NUMBER" %in% names(flowdata)){STATION_NUMBER <- flowdata$STATION_NUMBER[1]}
  if ("Parameter" %in% names(flowdata)){Parameter <- flowdata$Parameter[1]}
  
  
  flowdata <- flowdata[ order(flowdata$Date),]
  col_order <- names(flowdata)
  
  # If water year is TRUE and month is not January
  if (water_year & water_year_start>1) {
    
    #Create a temp file to determine the min/max water years (cant affect flowdata yet)
    flowdata_temp <- fasstr::fasstr_add_date_vars(dplyr::select(flowdata,Date,Value),
                                                  water_year = T,
                                                  water_year_start = water_year_start)
    min_wateryear <- ifelse(water_year,min(flowdata_temp$WaterYear),min(flowdata_temp$Year))
    max_wateryear <- ifelse(water_year,max(flowdata_temp$WaterYear),max(flowdata_temp$Year))
    
    
    # Extend the flowdata to well before the start and end dates (will filter to water years)
    flowdata_temp <- merge(flowdata_temp, 
                      data.frame(Date=seq(as.Date(paste(min(flowdata_temp$Year)-1,'01-01',sep='-'),
                                                  "%Y-%m-%d"),
                                          as.Date(paste(max(flowdata_temp$Year)+1,'12-31',sep='-'),
                                                  '%Y-%m-%d'), 1)),
                      all.y=TRUE)
    
    # Add Water year to be able to filter it
    flowdata_temp <- fasstr::fasstr_add_date_vars(flowdata_temp,
                                                  water_year = T,
                                                  water_year_start = water_year_start)
    
    # Filter flowdata for the min and max water years and remove date columns
    flowdata_temp <- dplyr::filter(flowdata_temp,WaterYear>=min_wateryear & WaterYear<=max_wateryear)
    flowdata_temp <- dplyr::select(flowdata_temp,Date,Value)
    
    flowdata <- merge(flowdata,flowdata_temp,all.y = T)
    
    
    # If not water year, or January is chosen as water year start  
  } else {
    min_year <- lubridate::year(min(flowdata$Date))
    max_year <- lubridate::year(max(flowdata$Date))
    
    flowdata <- merge(flowdata, 
                      data.frame(Date=seq(as.Date(paste(min_year,'01-01',sep='-'),
                                                  "%Y-%m-%d"),
                                          as.Date(paste(max_year  ,'12-31',sep='-'),
                                                  '%Y-%m-%d'), 1)),
                      all.y=TRUE)
  }
  
  # Return columns to original order
  flowdata <-  flowdata[,col_order]
  
  
  # Fill in STATION_NUMBER and Parameter if HYDAT selected
  if (!is.null(HYDAT)) {
    flowdata$STATION_NUMBER <- HYDAT
    flowdata$Parameter <- "FLOW"
  }

  # If flowdata was from HYDAT in a previous function
  if ("STATION_NUMBER" %in% names(flowdata)){flowdata$STATION_NUMBER <- STATION_NUMBER}
  if ("Parameter" %in% names(flowdata)){flowdata$Parameter <- Parameter}
  
  
  
  return(flowdata)
} # end of function
