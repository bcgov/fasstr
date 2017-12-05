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

#' @title Add rolling n-day averages
#'
#' @description Adds selected n-day rolling means to a streamflow dataset. Based on selected n-days and alignment, the rolling mean for
#'   a given day is obtained by averaging the adjacent dates of daily mean values. For example, rolling days of '7' and 'right' alignment 
#'   would obtain a mean of the given and previous 6 days of daily mean flow. Rolling mean values will not be calculated if there is less 
#'   than the n-days provided.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param days  Numeric. The number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param align Character. Specifies whether the dates of the rolling mean should be specified by the first ('left'), last ('right),
#'    or middle ('center') of the rolling n-day group of observations. Default \code{'right'}.
#' 
#' @return Additional column(s) of rolling means for each n-day specific, each called 'QnDay', n being the n-day (ex. Q7Day), to the flowdata 
#'    data frame input or HYDAT dataset, in units of the Value.
#'    
#' @examples
#' \dontrun{
#' 
#'fasstr_add_rolling_means(flowdata = flowdata, days = 7, align = 'centre')
#' 
#'fasstr_add_rolling_means(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------



fasstr_add_rolling_means <- function(flowdata=NULL,
                                     HYDAT=NULL,
                                     days=c(3,7,30),
                                     align="right"){
  
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
  
  if( !is.numeric(days))                       {stop("days argument must be numeric")}
  if( !all(days %in% c(1:180)) )                    {stop("days argument must be integers > 0 and <= 180)")}
  if( !align %in% c("right","left","center"))  {stop("align argument must be 'right', 'left', or 'center'")}
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 )                                  {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' arguement does not exist.")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  #--------------------------------------------------------------
  # Set the flowdata for analysis
  
  # get list of dates in flowdata
  flowdata <- flowdata[ order(flowdata$Date),]
  dates_list <- c(flowdata$Date)
  
  # fill in missing dates to ensure means roll over consecutive days
  flowdata <- fasstr::fasstr_add_missing_dates(flowdata=flowdata)
  
  #--------------------------------------------------------------
  # Add rolling means
  for (x in days) {
    flowdata[, paste0("Q",x,"Day")] <- zoo::rollapply( flowdata$Value,  x, mean, fill=NA, align=align)
  }
  
  # Return flowdata to original dates
  flowdata <- dplyr::filter(flowdata,Date %in% dates_list)
  
  
  return(flowdata)
} 

