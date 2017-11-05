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

#' @title Add rolling means.
#'
#' @description Adds n-day rolling means. Ensures rolling means 
#'
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Q' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flowdata} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param days  Numeric. The number of days to apply the rolling mean. Default c(3,7,30).
#' @param align Character. specifyies whether the index of the result should be left- or right-aligned or centered 
#'    (default) compared to the rolling window of observations
#' 
#'
#' @return A list with the following elements:
#'   \item{flowdata}
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

fasstr_add_rolling_means <- function(flowdata=NULL,
                                     HYDAT=NULL,
                                     days=c(3,7,30),
                                     align="right"){  # or left or centre
  
  
  #  Some basic error checking on the input parameters
  Value.Q=FALSE #used if flow is in a column called Value, not Q
  if ("Value" %in% names(flowdata)){
    Value.Q=TRUE
    flowdata <- dplyr::rename(flowdata,Q=Value)}
  if( is.null(flowdata) & is.null(HYDAT)) {
    stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {
    stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {
    stop("flowdata parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Q") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain date or flow columns (labeled Q or Value)")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Q))          {
    stop("Flow data (Q or Value) column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Q <0, na.rm=TRUE))   {
    stop('flowdata cannot have negative values - check your data')}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( !is.numeric(days))   {
    stop("days must be numeric")}
  if( !all(days>0 & days<=180))  {
    stop("days must be >0 and <=180)")}
  if( !all(days==floor(days)))  {
    stop("days must be integers")}
  if ( !align %in% c("right","left","center")){
    stop("align parameter must be 'right', 'left', or 'center'.")}
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    flowdata <- tidyhydat::DLY_FLOWS(STATION_NUMBER = HYDAT)
    flowdata <- dplyr::rename(flowdata,Q=Value)
  }
  
  # get list of dates in flowdata
  flowdata <- flowdata[ order(flowdata$Date),]
  dates.list <- c(flowdata$Date)
  
  # fill in missing dates to ensure means roll over consecutive days
  flowdata <- fasstr_fill_missing_dates(flowdata=flowdata)
  
  # Add rolling means
  for (x in days) {
    flowdata[, paste0("Q",x,"Day")] <- zoo::rollapply( flowdata$Q,  x, mean, fill=NA, align=align)
  }
  
  # Return flowdata to original dates
  flowdata <- dplyr::filter(flowdata,Date %in% dates.list)
  
  
  # Fill in STATION_NUMBER and Parameter if HYDAT selected
  if (!is.null(HYDAT)) {
    flowdata$STATION_NUMBER <- HYDAT
    flowdata$Parameter <- "FLOW"
    flowdata <- dplyr::rename(flowdata,Value=Q)
  }
  
  if (Value.Q) {flowdata <- dplyr::rename(flowdata,Value=Q)}
  
  return(flowdata)
} # end of function

