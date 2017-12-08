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
#' @param HYDAT Character. Seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' 
#' @return A data frame of the original flowdata or HYDAT data with additional rows of filled values of missing dates with NA
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
    if( any(flowdata$Value <0, na.rm=TRUE))           {warning('flowdata cannot have negative values - check your data')}
  }
  
  if( !is.logical(water_year))         {stop("water_year argument must be logical (TRUE/FALSE)")}
  if( !is.numeric(water_year_start) )  {stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec)")}
  if( length(water_year_start)>1)      {stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec)")}
  if( !water_year_start %in% c(1:12) ) {stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec)")}
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( !all(HYDAT %in% dplyr::pull(tidyhydat::allstations[1])) ) {stop("one or more stations in 'HYDAT' argument do not exist")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  
  #--------------------------------------------------------------
  # Set the flowdata for filling
  
  # Save the original columns from the flowdata to remove added columns
  orig_cols <- names(flowdata)
  
  # Add a station number column if none
  if ( !"STATION_NUMBER" %in% names(flowdata) ){ flowdata$STATION_NUMBER <- "00AA000" }
  
  #--------------------------------------------------------------
  # Loop through each station number, fill in gaps and append
  
  flowdata_new <- flowdata[0,]
  for (stn in unique(flowdata$STATION_NUMBER)) {
    
    # Filter for station number
    flowdata_stn <- dplyr::filter(flowdata,STATION_NUMBER==stn)
    flowdata_stn <- flowdata_stn[ order(flowdata_stn$Date),]
    
    
    # Fill if water year is TRUE and month is not January
    if (water_year & water_year_start>1) {
      
      # Determine the min months and years to set the start_date
      # If the month in the data is less than the water_year_start, the water year will begin in the previous calendar year
      min_month_wy <- lubridate::month(min(flowdata_stn$Date))
      min_year_wy <- lubridate::year(min(flowdata_stn$Date))
      if (min_month_wy < water_year_start) {
        start_date=as.Date(paste(min_year_wy-1,water_year_start,'01',sep='-'),"%Y-%m-%d")
      } else {
        start_date=as.Date(paste(min_year_wy,water_year_start,'01',sep='-'),"%Y-%m-%d")
      }
      
      # Determine the max months and years to set the start_date
      # If the month in the data is greater than the water_year_start, the water year will end in the next calendar year
      max_month_wy <- lubridate::month(max(flowdata_stn$Date))
      max_year_wy <- lubridate::year(max(flowdata_stn$Date))
      if (max_month_wy > water_year_start) {
        end_date=as.Date(paste(max_year_wy+1,water_year_start,'01',sep='-'),"%Y-%m-%d")-1
      } else {
        end_date=as.Date(paste(max_year_wy,water_year_start,'01',sep='-'),"%Y-%m-%d")-1
      }
      
      # Fill in missing dates
      flowdata_stn <- merge(flowdata_stn, 
                            data.frame(Date=seq(start_date,end_date, 1)),
                            all.y=TRUE)

      
   # Fill not water year, or January is chosen as water year start  
    } else {
      min_year <- lubridate::year(min(flowdata_stn$Date))
      max_year <- lubridate::year(max(flowdata_stn$Date))
      
      # Fill in missing dates
      flowdata_stn <- merge(flowdata_stn, 
                            data.frame(Date=seq(as.Date(paste(min_year,'01-01',sep='-'),
                                                        "%Y-%m-%d"),
                                                as.Date(paste(max_year  ,'12-31',sep='-'),
                                                        '%Y-%m-%d'), 1)),
                            all.y=TRUE)
    }
    
    # Fill in station number and parameter gaps (removed if not originally there)
    flowdata_stn$STATION_NUMBER <- stn
    flowdata_stn$Parameter <- "FLOW"
    
    # Append to flowdata
    flowdata_new <- dplyr::bind_rows(flowdata_new,flowdata_stn)
    
  }
  flowdata <- flowdata_new
  
  #Return columns to original order
  flowdata <-  flowdata[,orig_cols]
  
  
  return(flowdata)
} 
