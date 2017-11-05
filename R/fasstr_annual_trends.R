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


#' @title Compute annual trends.
#'
#' @description Computes annual trends.
#'
#' @param trendsdata Dataframe. Annual data with column names of years and rows of annual statistics.
#'@param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Q' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flowdata} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param station_name Character. Identifier name of the stream or station. Required when supplying data through \code{flowdata}.
#'    The station name will be used in plots and filenames of exported tables and plot. If using \code{HYDAT} to supply
#'    data and no \code{station_name} is provided, the HYDAT station number will be the identifier.
#' @param water_year Logical. Set to \code{TRUE} if data should be summarized by water year (Oct-Sep) instead of the
#'    default calendar year (Jan-Dec) (\code{water_year=FALSE}). Water years are designated by the year which they end in
#'    (e.g. water year 2000 start on 1 Oct 1999 and ends on 30 Sep 2000).
#' @param start_year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start_year} is the first
#'    year of the data provided.
#' @param end_year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end_year} is the last
#'    year of the data provided.
#' @param excluded.years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param basin_area Numeric. The upstream drainage basin area (in sq. km) of the station. Used to calculate runoff yields (mm).
#'    If no value provided, yield calculations will result in NA values.
#' @param write_table Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param write_transposed_table Logical. Should a file be created with the transposed of the annual statistics
#'    (both calendar and water year)?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-summary-stat-trans.csv'))}.
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param table_nddigits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#' @param na.rm TBD
#'
#'
#' @examples
#' \dontrun{
#' 
#' Coming
#' 
#' }
#' @export

#'
#--------------------------------------------------------------
# Compute the trends analysis

fasstr_annual_trends <- function(trendsdata=NULL,
                                 flowdata=NULL,
                                 HYDAT=NULL,
                                 zyp_method=NA,
                                 station_name="fasstr",
                                 water_year=FALSE, #create another for own water year????
                                 start_year=NULL,
                                 end_year=NULL,
                                 exclude_years=NULL, # list of stations
                                 basin_area=NA, # if na, then all Yield values == NA
                                 write_trends_data=FALSE,        # write out statistics on calendar year
                                 write_trends_results=FALSE,  # write out statistics in transposed format (cy & wy)
                                 report_dir=".",
                                 na.rm=list(na.rm.global=FALSE),
                                 table_nddigits=3){              # decimal digits for csv files for statistics
  
  
  
  
  #  Some basic error checking on the input parameters
  
  # If flowdata was from HYDAT in a previous function
  if ("Value" %in% names(flowdata)){
    flowdata <- dplyr::rename(flowdata,Q=Value)
  }

  # if trendsdata is provided
  if( !is.null(trendsdata) ) {
    if( !is.data.frame(trendsdata))         {
      stop("trendsdata parameter is not a data frame.")}

  # if no trendsdata is provided, but flowdata is
  } else {
    if( is.null(flowdata) & is.null(HYDAT)) {
      stop("If no trendsdata provided, one of flowdata or HYDAT parameters must be set.")}
    if( !is.null(HYDAT) & !is.null(flowdata))  {
      stop("If no trendsdata provided, one of flowdata or HYDAT parameters must be set, not both.")}
    if( is.null(HYDAT) & !is.data.frame(flowdata))         {
      stop("flowdata parameter is not a data frame.")}
    if( is.null(HYDAT) & "Date" %in% names(flowdata) ){
      stop("flowdata dataframe doesn't contain 'Date' column.")}
    if( is.null(HYDAT) & "Q" %in% names(flowdata) ){
      stop("flowdata dataframe doesn't contain a flow column labeled Q or Value.")}
    if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
      stop("Date column in flowdata dataframe is not a date.")}
    if( is.null(HYDAT) & !is.numeric(flowdata$Q))          {
      stop("Flow data (Q or Value) column in flowdata dataframe is not numeric.")}
    if( is.null(HYDAT) & any(flowdata$Q <0, na.rm=TRUE))   {
      stop('flowdata cannot have negative values - check your data')}
    
    if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}
    if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
    
    if( !is.na(basin_area) & !is.numeric(basin_area))    {stop("basin_area parameter must be numeric")}
    if( length(basin_area)>1 )        {stop("basin_area parameter cannot have length > 1")}
  }
  
  if( !is.na(zyp_method) & !zyp_method %in% c("yuepilon","zhang"))   {
    stop('zyp_trending parameter must have either "yuepilon" or "zhang" listed')}
  
  if( !is.character(station_name) )  {stop("station_name parameter must be a character string.")}
  if( length(station_name)>1 )        {stop("station_name parameter cannot have length > 1")}

  if( !is.logical(write_trends_data))  {stop("write_trends_data parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_trends_results)){stop("write_trends_results parameter must be logical (TRUE/FALSE)")}
  
  if( !dir.exists(as.character(report_dir)))      {stop("directory for saved files does not exist")}
  if( !is.numeric(table_nddigits))  { stop("csv.ndddigits parameter needs to be numeric")}
  table_nddigits <- round(table_nddigits[1])  # number of decimal digits for rounding in csv files
  
  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  
  trends_data <- trendsdata
  
  
  if ( is.null(trendsdata) ) {
    
    # If HYDAT station is listed, check if it exists and make it the flowdata
    if (!is.null(HYDAT)) {
      if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
      if (is.null(station_name)) {station_name <- HYDAT}
      flowdata <- tidyhydat::DLY_FLOWS(STATION_NUMBER = HYDAT)
      flowdata <- dplyr::select(flowdata,Date,Q=Value)
    }
    
    annual_stats <- fasstr::fasstr_annual_stats(flowdata,
                                                station_name=station_name,
                                                water_year=water_year, #create another for own water year????
                                                start_year=start_year,
                                                end_year=end_year,
                                                exclude_years=exclude_years, # list of stations
                                                basin_area=basin_area,
                                                transpose=TRUE,
                                                na.rm=na.rm)
  }
  
  
  # Complete trends analysis
  trends_results <- zyp::zyp.trend.dataframe(indat = trends_data,
                                             metadata.cols = 1,
                                             method=zyp_method)
  
  
  if(write_trends_data){
    file_trends_data <-file.path(report_dir,paste(station_name,"-annual-trends-data.csv",sep=""))
    temp <- trends_data
    utils::write.csv(temp, file=file_trends_data, row.names=FALSE)
  }
  
  if(write_trends_results){
    file_trends_results <-file.path(report_dir,paste(station_name,"-annual-trends-results.csv",sep=""))
    utils::write.csv(trends_results, file=file_trends_results, row.names=FALSE)
  }
  
  
  
  
  
  
  return(trends_results)
} # end of function

