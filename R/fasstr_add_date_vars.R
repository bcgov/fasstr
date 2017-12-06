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

#' @title Add year, month, and day of year variables
#' 
#' @description Add columns of Year (YYYY), Month (MM), MonthName (e.g. 'Jan'), and DayofYear (1-365 or 366); and WaterYear (YYYY) and 
#'    WaterDayofYear (1-365 or 366) if selected; to a data frame with a column of dates called 'Date'. Water years are designated by 
#'    the year in which they end. For example, Water Year 1999 (starting Oct) is from 1 Oct 1998 (WaterDayofYear 1) to 30 Sep 1999
#'    (WaterDayofYear 365)).
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
#' @return A data frame of the original flowdata or HYDAT data with additional columns:
#'   \item{Year}{calendar year}
#'   \item{Month}{numeric month (1 to 12)}
#'   \item{MonthName}{month name (Jan-Dec)}
#'   \item{DayofYear}{day of the year (1-365 or 366)}
#'   \item{WaterYear}{(optional) water year, designated by the calendar year in which it ends}
#'   \item{WaterDayofYear}{(optional) day of the water year (1-365 or 366), starting in the first month of the water year}
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_add_date_vars(flowdata = flowdata)
#' 
#'fasstr_add_date_vars(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export



#--------------------------------------------------------------

fasstr_add_date_vars <- function(flowdata=NULL,
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
    if( length(HYDAT)>1 )                                  {stop("Only one HYDAT station can be selected.")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  #--------------------------------------------------------------
  #  Add dates and water_year dates if selected
  
  # Calculate each date variable
  flowdata$Year  <- lubridate::year(flowdata$Date)
  flowdata$Month  <- lubridate::month(flowdata$Date)
  flowdata$MonthName <- month.abb[flowdata$Month]
  flowdata$DayofYear <- lubridate::yday(flowdata$Date)
  
  if (water_year){
    # Create values used to calculate the water year day of year
    if (water_year_start==2) {doy_temp <- c(31,31)}
    if (water_year_start==3) {doy_temp <- c(61,62)}
    if (water_year_start==4) {doy_temp <- c(90,91)}
    if (water_year_start==5) {doy_temp <- c(120,121)}
    if (water_year_start==6) {doy_temp <- c(151,152)}
    if (water_year_start==7) {doy_temp <- c(181,182)}
    if (water_year_start==8) {doy_temp <- c(212,213)}
    if (water_year_start==9) {doy_temp <- c(243,244)}
    if (water_year_start==10) {doy_temp <- c(273,274)}
    if (water_year_start==11) {doy_temp <- c(304,305)}
    if (water_year_start==12) {doy_temp <- c(334,335)}
    
    if (water_year_start==1) {
      flowdata$WaterYear <- flowdata$Year
      flowdata$WaterDayofYear <- flowdata$DayofYear
    } else {
      flowdata$WaterYear <- as.numeric(ifelse(flowdata$Month>=water_year_start,
                                              flowdata$Year+1,
                                              flowdata$Year))
      flowdata$WaterDayofYear <- ifelse(flowdata$Month<water_year_start,
                                        flowdata$DayofYear+(365-doy_temp[1]),
                                        ifelse((as.Date(with(flowdata, paste(Year+1,01,01,sep="-")),"%Y-%m-%d")
                                                -as.Date(with(flowdata, paste(Year,01,01,sep="-")),"%Y-%m-%d"))==366,
                                               flowdata$DayofYear-doy_temp[2],
                                               flowdata$DayofYear-doy_temp[1]))
    }
  }
  
  # Set the levels of the months
  if (!water_year) {
    flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul",
                                                              "Aug","Sep","Oct","Nov","Dec","Long-term"))
  } else {
    if (water_year_start==1) {
      flowdata$MonthName <- factor(flowdata$MonthName,levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul",
                                                               "Aug","Sep","Oct","Nov","Dec","Long-term"))
    } else if (water_year_start==2) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Feb", "Mar", "Apr", "May","Jun","Jul","Aug",
                                                                "Sep","Oct","Nov","Dec","Jan","Long-term"))
    } else if (water_year_start==3) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Mar", "Apr", "May","Jun","Jul","Aug","Sep",
                                                                "Oct","Nov","Dec","Jan", "Feb", "Long-term"))
    } else if (water_year_start==4) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Apr", "May","Jun","Jul","Aug","Sep","Oct",
                                                                "Nov","Dec","Jan", "Feb", "Mar", "Long-term"))
    } else if (water_year_start==5) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("May","Jun","Jul","Aug","Sep","Oct","Nov",
                                                                "Dec","Jan", "Feb", "Mar", "Apr", "Long-term"))
    } else if (water_year_start==6) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Jun","Jul","Aug","Sep","Oct","Nov","Dec",
                                                                "Jan", "Feb", "Mar", "Apr", "May","Long-term"))
    } else if (water_year_start==7) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Jul","Aug","Sep","Oct","Nov","Dec","Jan",
                                                                "Feb", "Mar", "Apr", "May","Jun","Long-term"))
    } else if (water_year_start==8) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Aug","Sep","Oct","Nov","Dec","Jan", "Feb",
                                                                "Mar", "Apr", "May","Jun","Jul","Long-term"))
    } else if (water_year_start==9) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Sep","Oct","Nov","Dec","Jan", "Feb", "Mar",
                                                                "Apr", "May","Jun","Jul","Aug","Long-term"))
    } else if (water_year_start==10) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr",
                                                                "May","Jun","Jul","Aug","Sep","Long-term"))
    } else if (water_year_start==11) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Nov","Dec","Jan", "Feb", "Mar", "Apr", "May",
                                                                "Jun","Jul","Aug","Sep","Oct","Long-term"))
    } else if (water_year_start==12) {
      flowdata$MonthName <- factor(flowdata$MonthName, levels=c("Dec","Jan", "Feb", "Mar", "Apr", "May","Jun",
                                                                "Jul","Aug","Sep","Oct","Nov","Long-term"))
    }
  }
  
  return(flowdata)
}

