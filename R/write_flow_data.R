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

#' @title Write a streamflow dataset as a .csv
#'
#' @description Write a streamflow dataset as a .csv file to a directory. Can add missing dates or filter data by years before writing
#'    using given arguments. Just list flowdata data frame or HYDAT station number to write its entirety.
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
#' @param start_year Integer. First year to consider for writing to csv. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for writing to csv. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from writing to csv. Leave blank if all years are required.  
#' @param exclude_rm Logical. Will remove the excluded data if \code{TRUE}, otherwise excluded data remain as NA. Default \code{FALSE}.
#' @param fill_missing_dates Logical. Fill dates with missing flow data with NA. Default \code{FALSE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na Character. String to use for missing values in the data. Default \code{""} (blank).
#' 
#' @return A .csv file of streamflow data in a selected directory.
#'
#' @examples
#' \dontrun{
#' 
#'write_flow_data(flowdata = flowdata, station_name = "MissionCreek", na="")
#' 
#'write_flow_data(HYDAT = "08NM116")
#' 
#' }
#' @export

#--------------------------------------------------------------

write_flow_data <- function(flowdata=NULL,
                            HYDAT=NULL,
                            water_year=FALSE,
                            water_year_start=10,
                            start_year=NULL,
                            end_year=NULL,
                            exclude_years=NULL,
                            exclude_rm=FALSE,
                            fill_missing_dates=FALSE,
                            station_name="fasstr",
                            write_dir=".",
                            #write_digits=4,
                            na=""){  
  
  #--------------------------------------------------------------
  #  Error checking on the input parameters
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
  
  if( length(start_year)>1)   {stop("only one start_year value can be selected")}
  if( !is.null(start_year) )  {if( !start_year %in% c(0:5000) )  {stop("start_year must be an integer")}}
  if( length(end_year)>1)     {stop("only one end_year value can be selected")}
  if( !is.null(end_year) )    {if( !end_year %in% c(0:5000) )  {stop("end_year must be an integer")}}
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("list of exclude_years must be numeric - ex. 1999 or c(1999,2000)")}
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.logical(fill_missing_dates))  {stop("fill_missing_dates parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(exclude_rm))  {stop("exclude_rm parameter must be logical (TRUE/FALSE)")}
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    if (station_name=="fasstr") {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # Get station name from "STATION_NUMBER" columns if available
  if (is.null(HYDAT) & "STATION_NUMBER" %in% names(flowdata)) {
    station_name <- flowdata$STATION_NUMBER[1]
  }
  
  # Fill missing dates
  if (fill_missing_dates) {
    flowdata <- fasstr::fill_missing_dates(flowdata=flowdata,
                                                 HYDAT=NULL,
                                                 water_year=water_year,
                                                 water_year_start=water_year_start)
  }
  
  # Filter if start_year is selected
  if (!is.null(start_year)) {
    if (water_year) {
      if (water_year_start==1)         {start_date <- as.Date(paste0(start_year,"-01-01"))
      } else if (water_year_start==2)  {start_date <- as.Date(paste0(start_year-1,"-02-01"))
      } else if (water_year_start==3)  {start_date <- as.Date(paste0(start_year-1,"-03-01"))
      } else if (water_year_start==4)  {start_date <- as.Date(paste0(start_year-1,"-04-01"))
      } else if (water_year_start==5)  {start_date <- as.Date(paste0(start_year-1,"-05-01"))
      } else if (water_year_start==6)  {start_date <- as.Date(paste0(start_year-1,"-06-01"))
      } else if (water_year_start==7)  {start_date <- as.Date(paste0(start_year-1,"-07-01"))
      } else if (water_year_start==8)  {start_date <- as.Date(paste0(start_year-1,"-08-01"))
      } else if (water_year_start==9)  {start_date <- as.Date(paste0(start_year-1,"-09-01"))
      } else if (water_year_start==10) {start_date <- as.Date(paste0(start_year-1,"-10-01"))
      } else if (water_year_start==11) {start_date <- as.Date(paste0(start_year-1,"-11-01"))
      } else if (water_year_start==12) {start_date <- as.Date(paste0(start_year-1,"-12-01"))
      }
    } else if (!water_year) {
      start_date <- as.Date(paste0(start_year,"-01-01"))
    }
    flowdata <- dplyr::filter(flowdata,Date>=start_date)
  }
  
  # Filter if end_year is selected
  if (!is.null(end_year)) {
    if (water_year) {
      if (water_year_start==1)         {end_date <- as.Date(paste0(end_year,"-12-31"))
      } else if (water_year_start==2)  {end_date <- as.Date(paste0(end_year,"-01-31"))
      } else if (water_year_start==3)  {end_date <- as.Date(paste0(end_year,"-03-01"))-1
      } else if (water_year_start==4)  {end_date <- as.Date(paste0(end_year,"-03-31"))
      } else if (water_year_start==5)  {end_date <- as.Date(paste0(end_year,"-04-30"))
      } else if (water_year_start==6)  {end_date <- as.Date(paste0(end_year,"-05-31"))
      } else if (water_year_start==7)  {end_date <- as.Date(paste0(end_year,"-06-30"))
      } else if (water_year_start==8)  {end_date <- as.Date(paste0(end_year,"-07-31"))
      } else if (water_year_start==9)  {end_date <- as.Date(paste0(end_year,"-08-31"))
      } else if (water_year_start==10) {end_date <- as.Date(paste0(end_year,"-09-30"))
      } else if (water_year_start==11) {end_date <- as.Date(paste0(end_year,"-10-31"))
      } else if (water_year_start==12) {end_date <- as.Date(paste0(end_year,"-11-30"))
      }
    } else if (!water_year) {
      end_date <- as.Date(paste0(end_year,"-12-31"))
    }
    flowdata <- dplyr::filter(flowdata,Date<=end_date)
  }
  
  # Remove excluded years
  if (!is.null(exclude_years)) {
    
    for (yr in exclude_years) {
      if (water_year) {
        if (water_year_start==1)         {start_date <- as.Date(paste0(yr,"-01-01"))
        } else if (water_year_start==2)  {start_date <- as.Date(paste0(yr-1,"-02-01"))
        } else if (water_year_start==3)  {start_date <- as.Date(paste0(yr-1,"-03-01"))
        } else if (water_year_start==4)  {start_date <- as.Date(paste0(yr-1,"-04-01"))
        } else if (water_year_start==5)  {start_date <- as.Date(paste0(yr-1,"-05-01"))
        } else if (water_year_start==6)  {start_date <- as.Date(paste0(yr-1,"-06-01"))
        } else if (water_year_start==7)  {start_date <- as.Date(paste0(yr-1,"-07-01"))
        } else if (water_year_start==8)  {start_date <- as.Date(paste0(yr-1,"-08-01"))
        } else if (water_year_start==9)  {start_date <- as.Date(paste0(yr-1,"-09-01"))
        } else if (water_year_start==10) {start_date <- as.Date(paste0(yr-1,"-10-01"))
        } else if (water_year_start==11) {start_date <- as.Date(paste0(yr-1,"-11-01"))
        } else if (water_year_start==12) {start_date <- as.Date(paste0(yr-1,"-12-01"))
        }
      } else if (!water_year) {
        start_date <- as.Date(paste0(yr,"-01-01"))
      }
      
      if (water_year) {
        if (water_year_start==1) {end_date <- as.Date(paste0(yr,"-12-31"))
        } else if (water_year_start==2) {end_date <- as.Date(paste0(yr,"-01-31"))
        } else if (water_year_start==3) {end_date <- as.Date(paste0(yr,"-03-01"))-1
        } else if (water_year_start==4) {end_date <- as.Date(paste0(yr,"-03-31"))
        } else if (water_year_start==5) {end_date <- as.Date(paste0(yr,"-04-30"))
        } else if (water_year_start==6) {end_date <- as.Date(paste0(yr,"-05-31"))
        } else if (water_year_start==7) {end_date <- as.Date(paste0(yr,"-06-30"))
        } else if (water_year_start==8) {end_date <- as.Date(paste0(yr,"-07-31"))
        } else if (water_year_start==9) {end_date <- as.Date(paste0(yr,"-08-31"))
        } else if (water_year_start==10) {end_date <- as.Date(paste0(yr,"-09-30"))
        } else if (water_year_start==11) {end_date <- as.Date(paste0(yr,"-10-31"))
        } else if (water_year_start==12) {end_date <- as.Date(paste0(yr,"-11-30"))
        }
      } else if (!water_year) {
        end_date <- as.Date(paste0(yr,"-12-31"))
      }
    }
    # Make the Values NA
    flowdata <- dplyr::mutate(flowdata,Value=replace(Value, Date >= start_date & Date <= end_date, NA))
    
    # Remove the rows if exclude_rm
    if (exclude_rm){
      flowdata <- dplyr::filter(flowdata,Date < start_date | Date > end_date)
    }
    
  }
  
  #flowdata <- dplyr::mutate(flowdata,Value=round(Value,table_nddigits))
  # Replace any NA's with na (NA default)
  flowdata[is.na(flowdata)] <- na
  
  #Write the file
  write.csv(flowdata,file = paste0(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-daily-flows.csv"),
            row.names = F)
}

