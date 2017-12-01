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


#' @title Fill dates of missing flow values with NA
#'
#' @description Adds rows of dates with missing flow values to a streamflow dataset with daily flow values of NA. Missing dates will 
#'    be filled in gaps between data and compeltely fill the first and last years (calendar or water year if selected).
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' 
#' @return Additional rows of streamflow data to the flowdata data frame input or HYDAT dataset with flow values of NA where flow values 
#'    were missing.
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_add_missing_dates(flowdata = flowdata)
#' 
#'fasstr_add_missing_dates(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------


fasstr_add_missing_dates <- function(flowdata=NULL,
                                      HYDAT=NULL,
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
    if( any(flowdata$Value <0, na.rm=TRUE))           {stop('flowdata cannot have negative values - check your data')}
  }
  
  if( !is.logical(water_year))         {stop("water_year argument must be logical (TRUE/FALSE)")}
  if( !is.numeric(water_year_start) )  {stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec)")}
  if( length(water_year_start)>1)      {stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec)")}
  if( !water_year_start %in% c(1:12) ) {stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec)")}
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 )                                  {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' arguement does not exist.")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # If STATION_NUMBER and Parameter column is in flowdata, save the values for filling in the missing dates
  if ("STATION_NUMBER" %in% names(flowdata)){STATION_NUMBER <- flowdata$STATION_NUMBER[1]}
  if ("Parameter" %in% names(flowdata)){Parameter <- flowdata$Parameter[1]}
  
  
  #--------------------------------------------------------------
  # Set the flowdata for filling
  
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
} 
