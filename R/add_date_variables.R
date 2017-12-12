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
#' @param flow_data Data frame. A data frame of daily mean flow data. Not required if \code{HYDAT} argument is used.
#' @param flow_dates A column in flow_data that contains dates of daily flow data formatted YYYY-MM-DD. Default \code{Date}.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flow_data} argument is used.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' 
#' @return A data frame of the original flow_data or HYDAT data with additional columns:
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
#'add_date_variables(flow_data = flow_data)
#' 
#'add_date_variables(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


add_date_variables <- function(flow_data=NULL,
                               flow_dates=Date,
                               HYDAT=NULL,
                               water_year=FALSE,
                               water_year_start=10){  
  
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
  # Check if data is provided
  if(is.null(flow_data) & is.null(HYDAT))   stop("No flow data provided, must use flow_data or HYDAT arguments.")
  if(!is.null(flow_data) & !is.null(HYDAT)) stop("Only one of flow_data or HYDAT arguments can be used.")
  
  # Get HYDAT data if selected and stations exist
  if(!is.null(HYDAT)) {
    if(!all(HYDAT %in% dplyr::pull(tidyhydat::allstations[1]))) stop("One or more stations listed in 'HYDAT' do not exist.")
    flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # Get the Date column set up
  # This method allows the user to select the Date column if the column names are different
  if(!as.character(substitute(flow_dates)) %in% names(flow_data))  
    stop("Flow dates not found. Rename flow dates column to 'Date' or identify the column using 'flow_dates' argument.")
  # Temporarily rename the Date column
  names(flow_data)[names(flow_data) == as.character(substitute(flow_dates))] <- "Date"
  
  # Check columns are in proper formats
  if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in flow_data data frame does not contain dates.")
  
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).")
  if(!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).")
  
  
  ## ADD CALENDAR YEAR VARIABLES
  ## ---------------------------
  
  # Calculate each date variable
  flow_data$Year  <- lubridate::year(flow_data$Date)
  flow_data$Month  <- lubridate::month(flow_data$Date)
  flow_data$MonthName <- month.abb[flow_data$Month]
  flow_data$DayofYear <- lubridate::yday(flow_data$Date)
  
  
  ## ADD WATER YEAR VARIABLES (if selected)
  ## --------------------------------------
  
  if (water_year){
    # Create values used to calculate the water year day of year
    if (water_year_start == 2) {doy_temp <- c(31, 31)}
    if (water_year_start == 3) {doy_temp <- c(61, 62)}
    if (water_year_start == 4) {doy_temp <- c(90, 91)}
    if (water_year_start == 5) {doy_temp <- c(120, 121)}
    if (water_year_start == 6) {doy_temp <- c(151, 152)}
    if (water_year_start == 7) {doy_temp <- c(181, 182)}
    if (water_year_start == 8) {doy_temp <- c(212, 213)}
    if (water_year_start == 9) {doy_temp <- c(243, 244)}
    if (water_year_start == 10) {doy_temp <- c(273, 274)}
    if (water_year_start == 11) {doy_temp <- c(304, 305)}
    if (water_year_start == 12) {doy_temp <- c(334, 335)}
    
    if (water_year_start==1) {
      flow_data$WaterYear <- flow_data$Year
      flow_data$WaterDayofYear <- flow_data$DayofYear
    } else {
      flow_data$WaterYear <- as.numeric(ifelse(flow_data$Month >= water_year_start,
                                               flow_data$Year + 1,
                                               flow_data$Year))
      flow_data$WaterDayofYear <- ifelse(flow_data$Month < water_year_start,
                                         flow_data$DayofYear + (365 - doy_temp[1]),
                                         ifelse((as.Date(with(flow_data, paste(Year + 1, 01, 01, sep="-")), "%Y-%m-%d")
                                                 -as.Date(with(flow_data, paste(Year, 01, 01, sep="-")), "%Y-%m-%d")) == 366,
                                                flow_data$DayofYear-doy_temp[2],
                                                flow_data$DayofYear-doy_temp[1]))
    }
  }
  
  ## SET THE MONTH LEVELS FOR ORDERING
  ## ---------------------------------
  
  if (!water_year) {
    flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                  "Aug", "Sep", "Oct", "Nov", "Dec"))
  } else {
    if (water_year_start==1) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                                    "Aug", "Sep", "Oct", "Nov", "Dec"))
    } else if (water_year_start==2) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                                                    "Sep", "Oct", "Nov", "Dec", "Jan"))
    } else if (water_year_start==3) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                                                    "Oct", "Nov", "Dec", "Jan", "Feb"))
    } else if (water_year_start==4) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                                                    "Nov", "Dec", "Jan", "Feb", "Mar"))
    } else if (water_year_start==5) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
                                                                    "Dec", "Jan", "Feb", "Mar", "Apr"))
    } else if (water_year_start==6) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                                                                    "Jan", "Feb", "Mar", "Apr", "May"))
    } else if (water_year_start==7) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan",
                                                                    "Feb", "Mar", "Apr", "May", "Jun"))
    } else if (water_year_start==8) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb",
                                                                    "Mar", "Apr", "May","Jun", "Jul"))
    } else if (water_year_start==9) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar",
                                                                    "Apr", "May", "Jun", "Jul", "Aug"))
    } else if (water_year_start==10) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr",
                                                                    "May", "Jun", "Jul", "Aug", "Sep"))
    } else if (water_year_start==11) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May",
                                                                    "Jun", "Jul", "Aug", "Sep", "Oct"))
    } else if (water_year_start==12) {
      flow_data$MonthName <- factor(flow_data$MonthName, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                                    "Jul", "Aug", "Sep", "Oct", "Nov"))
    }
  }
  
  # Return the original names of the Date column
  names(flow_data)[names(flow_data) == "Date"] <- as.character(substitute(flow_dates))
  
  flow_data
  
}

