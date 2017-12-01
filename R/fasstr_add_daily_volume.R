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

#' @title Add daily volumetric flows
#'
#' @description Add a column of daily volumetric flows to a streamflow dataset, in units of cubic metres. Converts the discharge to a volume.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' 
#' @return A column of volumetric flows for each day called 'Volume_m3' added to the flowdata data frame input or HYDAT dataset, 
#'    in units of cubic metres.
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_add_daily_volume(flowdata = flowdata)
#' 
#'fasstr_add_daily_volume(HYDAT = "08NM116")
#'
#' }
#' @export

#--------------------------------------------------------------

fasstr_add_daily_volume <- function(flowdata=NULL,
                                  HYDAT=NULL){  # or left or centre
  
  
  #--------------------------------------------------------------
  #  Some basic error checking on the input parameters
  
  if( !is.null(HYDAT) & !is.null(flowdata))  {
    stop("must select either flowdata or HYDAT arguments, not both")}
  if( is.null(HYDAT)) {
    if( is.null(flowdata)) {stop("one of flowdata or HYDAT arguments must be set")}
    if( !is.data.frame(flowdata)) {stop("flowdata arguments is not a data frame")}
    if( !all(c("Date","Value") %in% names(flowdata))){stop("flowdata data frame doesn't contain the variables 'Date' and 'Value'")}
    if( !inherits(flowdata$Date[1], "Date")){stop("'Date' column in flowdata data frame is not a date")}
    if( !is.numeric(flowdata$Value)) {stop("'Value' column in flowdata data frame is not numeric")}
    if( any(flowdata$Value <0, na.rm=TRUE)) {stop('flowdata cannot have negative values - check your data')}
  }
  
  
  #--------------------------------------------------------------
  # If HYDAT station is listed, check if it exists and make it the flowdata
  
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' arguement does not exist.")}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }

  #--------------------------------------------------------------
  # Add column to flowdata
  
    flowdata <- dplyr::mutate(flowdata,Volume_m3=Value*86400)

  

  return(flowdata)
}

