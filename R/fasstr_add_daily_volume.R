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

#' @title Add volume per day
#'
#' @description Add cumulative volumetric flows on an annual basis.
#'
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Value' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flowdata} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' 
#' 
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

fasstr_add_daily_volume <- function(flowdata=NULL,
                                  HYDAT=NULL){  # or left or centre
  
  
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
  
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' arguement does not exist.")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  
    flowdata <- dplyr::mutate(flowdata,Volume_m3=Value*86400)

  

  return(flowdata)
}

