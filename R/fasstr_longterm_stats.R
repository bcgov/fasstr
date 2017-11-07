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

#' @title Compute long-term statistics.
#'
#' @description Computes long-term statistics of streamflow data.
#' Streamflow data can be supplied through the \code{flowdata} parameter or extracted from a 
#' HYDAT database using the tidyhydat package and \code{HYDAT} parameter.
#'
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
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
#' @param water_year_start Numeric. Month to start water year (1 to 12 for Jan to Dec).
#' @param start_year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start_year} is the first
#'    year of the data provided.
#' @param end_year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end_year} is the last
#'    year of the data provided.
#' @param excluded_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param basin_area Numeric. The upstream drainage basin area (in sq. km) of the station. Used to calculate runoff yields (mm).
#'    If no value provided, yield calculations will result in NA values.
#' @param transpose Logical. Switch the rows and columns of the results.
#' @param write_table Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param table_nddigits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#' @param na.rm TBD
#'
#'
#' @examples
#' \dontrun{
#' 
#' coming soon :)
#' }
#' @export

#'


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

fasstr_longterm_stats <- function(flowdata=NULL,
                                  HYDAT=NULL,
                                  station_name="fasstr",
                                  water_year=FALSE, #create another for own water year????
                                  water_year_start=10,
                                  start_year=NULL,
                                  end_year=NULL,
                                  exclude_years=NULL, # list of stations
                                  transpose=FALSE,
                                  write_table=FALSE,        # write out statistics on calendar year
                                  report_dir=".",
                                  na.rm=list(na.rm.global=FALSE),
                                  table_nddigits=3){
  

  #
  #############################################################
  #  Some basic error checking on the input parameters
  #
  if( is.null(flowdata) & is.null(HYDAT)) {
    stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {
    stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & !is.character(station_name))  {
    stop("station_name parameter must be a character string.")}
  if( is.null(HYDAT) & length(station_name)>1)        {
    stop("station_name parameter cannot have length > 1")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {
    stop("flowdata parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Q") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Q.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Q))          {
    stop("Q column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Q <0, na.rm=TRUE))   {
    stop('flowdata cannot have negative values - check your data')}
  
  if( !is.logical(water_year))  {
    stop("water_year parameter must be logical (TRUE/FALSE)")}
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {
    stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}

  if( !is.logical(transpose))  {
    stop("transpose parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_table))  {
    stop("write_table parameter must be logical (TRUE/FALSE)")}
  
  if( !dir.exists(as.character(report_dir)))      {
    stop("directory for saved files does not exist")}
  if( !is.numeric(table_nddigits))  { 
    stop("csv.ndddigits parameter needs to be numeric")}
  table_nddigits <- round(table_nddigits[1])  # number of decimal digits for rounding in csv files
  
  if( !is.list(na.rm))              {
    stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm))){   
    stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){
    stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    if (station_name=="fasstr") {station_name <- HYDAT}
    flowdata <- tidyhydat::hy_daily_flows(station_number =  HYDAT)
    flowdata <- dplyr::select(flowdata,Date,Q=Value)
  }
  
  # If start/end years are not select, set them as the min/max dates
  min.year <- lubridate::year(min(flowdata$Date))-water_year
  max.year <- lubridate::year(max(flowdata$Date))
  if (!is.numeric(start_year)) {start_year <- min.year}
  if (!is.numeric(end_year)) {end_year <- max.year}
  if(! (start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}

  #  create the year (annual ) and month variables
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year_start = water_year_start)
  

  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  
  # Remove flow values from Q column is the year is in excluded_years
  flowdata <- dplyr::mutate(flowdata,Q=replace(Q, AnalysisYear %in% exclude_years, NA))

  
  #  Compute calendar year long-term stats
  Q_month_longterm <-   dplyr::summarize(dplyr::group_by(flowdata,MonthName),
                                         Mean = mean(Q,na.rm=TRUE),
                                         Median = median(Q,na.rm=TRUE),
                                         Maximum = max(Q,na.rm=TRUE),
                                         Minimum = min(Q,na.rm=TRUE))
  Q_all_longterm <-   dplyr::summarize(flowdata,
                                       Mean = mean(Q,na.rm=TRUE),
                                       Median = median(Q,na.rm=TRUE),
                                       Maximum = max(Q,na.rm=TRUE),
                                       Minimum = min(Q,na.rm=TRUE))
  Q_all_longterm <- dplyr::mutate(Q_all_longterm,MonthName="Long-term")
  
  
  Q_longterm <- rbind(Q_month_longterm, Q_all_longterm)
  Q_longterm <- dplyr::rename(Q_longterm,Month=MonthName)
  if (water_year) {
    if (water_year_start==1) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term"))
    } else if (water_year_start==2) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Long-term"))
    } else if (water_year_start==3) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Long-term"))
    } else if (water_year_start==4) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Long-term"))
    } else if (water_year_start==5) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "Long-term"))
    } else if (water_year_start==6) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Long-term"))
    } else if (water_year_start==7) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Jul","Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Long-term"))
    } else if (water_year_start==8) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Long-term"))
    } else if (water_year_start==9) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Long-term"))
    } else if (water_year_start==10) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Long-term"))
    } else if (water_year_start==11) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Long-term"))
    } else if (water_year_start==12) {
      Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Long-term"))
    }
      
  } else {           
    Q_longterm$Month <- factor(Q_longterm$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term"))
  }
  Q_longterm <- with(Q_longterm, Q_longterm[order(Month),])
  
  
  
  if (transpose) {
    Q_longterm_tpose <- tidyr::gather(Q_longterm,Statistic,Value,-Month)
    Q_longterm <- tidyr::spread(Q_longterm_tpose,Month,Value)
  }
  
  
  
  #  Write out summary tables for calendar years
  if (write_table) {
    file.stat.csv <-file.path(report_dir, paste(station_name,"-longterm-summary-stat.csv", sep=""))
    temp <- Q_longterm
    temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], table_nddigits)  # round the output
    utils::write.csv(temp, file=file.stat.csv, row.names=FALSE)
  }
  
  
  return(Q_longterm)
  
  
} # end of function
