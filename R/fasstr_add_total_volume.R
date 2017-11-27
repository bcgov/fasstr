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

#' @title Add cumulative volumetric flows on an annual basis.
#'
#' @description Add cumulative volumetric flows on an annual basis.
#'
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Value' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flowdata} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param water_year Logical (TRUE/FALSE). Choose to fill to the start of the first/last water years.
#' @param water_year_start Numeric. Month to start water year (1 to 12 for Jan to Dec). Default 10 (Oct).
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

fasstr_add_total_volume <- function(flowdata=NULL,
                                  HYDAT=NULL,
                                  water_year=FALSE,
                                  water_year_start=10){  # or left or centre
  
  
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
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' arguement does not exist.")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # get list of dates in flowdata
  flowdata <- flowdata[ order(flowdata$Date),]
  col.ord <- names(flowdata)

  # fill in missing dates to ensure means roll over consecutive days
  flowdata.temp <- fasstr::fasstr_fill_missing_dates(flowdata=flowdata)
  flowdata.temp <- fasstr::fasstr_add_date_vars(flowdata=flowdata.temp,water_year = T,water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata.temp$AnalysisYear <- flowdata.temp$WaterYear
  }  else {
    flowdata.temp$AnalysisYear <- flowdata.temp$Year
  }
  
  # Add cumulative
  flowdata.temp <- dplyr::mutate(dplyr::group_by(flowdata.temp,AnalysisYear),Vtotal=cumsum(Value)*86400)

  
  # Return flowdata to original dates
  flowdata.temp <- dplyr::select(dplyr::ungroup(flowdata.temp),Date,Vtotal)
  
  flowdata <- merge(flowdata,flowdata.temp,by="Date",all.x = T)
  flowdata <-  flowdata[,c(col.ord,paste("Vtotal"))]
  
  # Fill in STATION_NUMBER and Parameter if HYDAT selected
  if (!is.null(HYDAT)) {
    flowdata$STATION_NUMBER <- HYDAT
    flowdata$Parameter <- "FLOW"
  }
  

  return(flowdata)
} # end of function

