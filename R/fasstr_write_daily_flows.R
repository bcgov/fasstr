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
#' @param water_year Logical. Set to \code{TRUE} if data should be summarized by water year (Oct-Sep) instead of the
#'    default calendar year (Jan-Dec) (\code{water_year=FALSE}). Water years are designated by the year which they end in
#'    (e.g. water year 2000 start on 1 Oct 1999 and ends on 30 Sep 2000).
#' @param water_year_start Numeric. Month to start water year (1 to 12 for Jan to Dec).
#' @param fill_missing_dates Logical.
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

fasstr_write_daily_flows <- function(flowdata=NULL,
                                   HYDAT=NULL,
                                   station_name="fasstr",
                                   write_dir=".",
                                   fill_missing_dates=FALSE,
                                   water_year=FALSE,
                                   water_year_start=10,
                                   start_year=NULL,
                                   end_year=NULL,
                                   exclude_years=NULL,
                                   exclude_rm=FALSE,
                                   #write_digits=4,
                                   na=NA){  
  
  
  
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
  
  if( !is.logical(fill_missing_dates))  {stop("fill_missing_dates parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}
  if( !is.numeric(water_year_start))  {
    stop("water_year_start parameter must be numeric between 1 and 12 (Jan-Dec)")}
  if( water_year_start<1 & water_year_start>12 )  {
    stop("water_year_start parameter must be numeric between 1 and 12 (Jan-Dec)")}
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.logical(exclude_rm))  {stop("exclude_rm parameter must be logical (TRUE/FALSE)")}
  
  
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    if (station_name=="fasstr") {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  if (is.null(HYDAT) & "STATION_NUMBER" %in% names(flowdata)) {
    station_name <- flowdata$STATION_NUMBER[1]
  }
  
  
  if (fill_missing_dates) {
    flowdata <- fasstr::fasstr_add_missing_dates(flowdata=flowdata,
                                                  HYDAT=NULL,
                                                  water_year=water_year,
                                                  water_year_start=water_year_start)
  }
  
  # Filter if start_year is selected
  if (!is.null(start_year)) {
    if (water_year) {
      if (water_year_start==1) {
        start_date <- as.Date(paste0(start_year,"-01-01"))
      } else if (water_year_start==2) {
        start_date <- as.Date(paste0(start_year-1,"-02-01"))
      } else if (water_year_start==3) {
        start_date <- as.Date(paste0(start_year-1,"-03-01"))
      } else if (water_year_start==4) {
        start_date <- as.Date(paste0(start_year-1,"-04-01"))
      } else if (water_year_start==5) {
        start_date <- as.Date(paste0(start_year-1,"-05-01"))
      } else if (water_year_start==6) {
        start_date <- as.Date(paste0(start_year-1,"-06-01"))
      } else if (water_year_start==7) {
        start_date <- as.Date(paste0(start_year-1,"-07-01"))
      } else if (water_year_start==8) {
        start_date <- as.Date(paste0(start_year-1,"-08-01"))
      } else if (water_year_start==9) {
        start_date <- as.Date(paste0(start_year-1,"-09-01"))
      } else if (water_year_start==10) {
        start_date <- as.Date(paste0(start_year-1,"-10-01"))
      } else if (water_year_start==11) {
        start_date <- as.Date(paste0(start_year-1,"-11-01"))
      } else if (water_year_start==12) {
        start_date <- as.Date(paste0(start_year-1,"-12-01"))
      }
    } else if (!water_year) {
      start_date <- as.Date(paste0(start_year,"-01-01"))
    }
    flowdata <- dplyr::filter(flowdata,Date>=start_date)
  }
  
  # Filter if end_year is selected
  if (!is.null(end_year)) {
    if (water_year) {
      if (water_year_start==1) {
        end_date <- as.Date(paste0(end_year,"-12-31"))
      } else if (water_year_start==2) {
        end_date <- as.Date(paste0(end_year,"-01-31"))
      } else if (water_year_start==3) {
        end_date <- as.Date(paste0(end_year,"-03-01"))-1
      } else if (water_year_start==4) {
        end_date <- as.Date(paste0(end_year,"-03-31"))
      } else if (water_year_start==5) {
        end_date <- as.Date(paste0(end_year,"-04-30"))
      } else if (water_year_start==6) {
        end_date <- as.Date(paste0(end_year,"-05-31"))
      } else if (water_year_start==7) {
        end_date <- as.Date(paste0(end_year,"-06-30"))
      } else if (water_year_start==8) {
        end_date <- as.Date(paste0(end_year,"-07-31"))
      } else if (water_year_start==9) {
        end_date <- as.Date(paste0(end_year,"-08-31"))
      } else if (water_year_start==10) {
        end_date <- as.Date(paste0(end_year,"-09-30"))
      } else if (water_year_start==11) {
        end_date <- as.Date(paste0(end_year,"-10-31"))
      } else if (water_year_start==12) {
        end_date <- as.Date(paste0(end_year,"-11-30"))
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
        if (water_year_start==1) {
          start_date <- as.Date(paste0(yr,"-01-01"))
        } else if (water_year_start==2) {
          start_date <- as.Date(paste0(yr-1,"-02-01"))
        } else if (water_year_start==3) {
          start_date <- as.Date(paste0(yr-1,"-03-01"))
        } else if (water_year_start==4) {
          start_date <- as.Date(paste0(yr-1,"-04-01"))
        } else if (water_year_start==5) {
          start_date <- as.Date(paste0(yr-1,"-05-01"))
        } else if (water_year_start==6) {
          start_date <- as.Date(paste0(yr-1,"-06-01"))
        } else if (water_year_start==7) {
          start_date <- as.Date(paste0(yr-1,"-07-01"))
        } else if (water_year_start==8) {
          start_date <- as.Date(paste0(yr-1,"-08-01"))
        } else if (water_year_start==9) {
          start_date <- as.Date(paste0(yr-1,"-09-01"))
        } else if (water_year_start==10) {
          start_date <- as.Date(paste0(yr-1,"-10-01"))
        } else if (water_year_start==11) {
          start_date <- as.Date(paste0(yr-1,"-11-01"))
        } else if (water_year_start==12) {
          start_date <- as.Date(paste0(yr-1,"-12-01"))
        }
      } else if (!water_year) {
        start_date <- as.Date(paste0(yr,"-01-01"))
      }
      
      if (water_year) {
        if (water_year_start==1) {
          end_date <- as.Date(paste0(yr,"-12-31"))
        } else if (water_year_start==2) {
          end_date <- as.Date(paste0(yr,"-01-31"))
        } else if (water_year_start==3) {
          end_date <- as.Date(paste0(yr,"-03-01"))-1
        } else if (water_year_start==4) {
          end_date <- as.Date(paste0(yr,"-03-31"))
        } else if (water_year_start==5) {
          end_date <- as.Date(paste0(yr,"-04-30"))
        } else if (water_year_start==6) {
          end_date <- as.Date(paste0(yr,"-05-31"))
        } else if (water_year_start==7) {
          end_date <- as.Date(paste0(yr,"-06-30"))
        } else if (water_year_start==8) {
          end_date <- as.Date(paste0(yr,"-07-31"))
        } else if (water_year_start==9) {
          end_date <- as.Date(paste0(yr,"-08-31"))
        } else if (water_year_start==10) {
          end_date <- as.Date(paste0(yr,"-09-30"))
        } else if (water_year_start==11) {
          end_date <- as.Date(paste0(yr,"-10-31"))
        } else if (water_year_start==12) {
          end_date <- as.Date(paste0(yr,"-11-30"))
        }
      } else if (!water_year) {
        end_date <- as.Date(paste0(yr,"-12-31"))
      }
    }
    
    flowdata <- dplyr::mutate(flowdata,
                              Value=replace(Value, Date >= start_date & Date <= end_date, NA))
      
    if (exclude_rm){
    flowdata <- dplyr::filter(flowdata,Date < start_date | Date > end_date)
    }
    
  }
  
  #flowdata <- dplyr::mutate(flowdata,Value=round(Value,table_nddigits))
  flowdata[is.na(flowdata)] <- na
  
  write.csv(flowdata,file = paste0(write_dir,"/",paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-daily-flows.csv"),
            row.names = F)
}

