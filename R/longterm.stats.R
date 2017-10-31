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

#' @title Compute long-term monthly and annual (calendar or water year) summary statistics.
#'
#' @description Computes long-term summary statistics on \code{flow$Q} variable
#'    between \code{start.year} and \code{end.year} inclusive for calendar or water years.
#'    It (optionally) saves the results in *.csv and *.pdf files.
#'
#' @param station.name Character. Identifier name of the stream or station. Required when supplying data through \code{flow.data}.
#'    The station name will be used in plots and filenames of exported tables and plot. If using \code{HYDAT} to supply
#'    data and no \code{station.name} is provided, the HYDAT station number will be the identifier.
#' @param flow.data Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Q' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flow.data} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param water.year Logical. Set to \code{TRUE} if data should be summarized by water year (Oct-Sep) instead of the
#'    default calendar year (Jan-Dec) (\code{water.year=FALSE}). Water years are designated by the year which they end in
#'    (e.g. water year 2000 start on 1 Oct 1999 and ends on 30 Sep 2000).
#' @param start.year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start.year} is the first
#'    year of the data provided.
#' @param end.year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end.year} is the last
#'    year of the data provided.
#' @param excluded.years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param write.table Should a file be created with the computed statistics?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,'-longterm-summary-stat.csv'))}.
#' @param write.transposed.table Should a file be created with the transposed of the statistics report?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,'-longterm-summary-stat-trans.csv'))}.
#' @param report.dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param csv.nddigits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#' @param na.rm TBD
#'
#' @return A list with the following elements:
#'   \item{Q.stat.longterm}{Data frame with the long-term statistics of \code{flow.data$Q} by month
#'         and overall between \code{start.year} and \code{end.year}}
#'   \item{Q.stat.longterm.trans}{Data frame with the long-term statistics of \code{flow.data$Q} transposed.}
#'   \item{file.stat.csv}{Object with file name of *.csv file with long term summary statistics.}
#'    \item{file.stat.trans.csv}{Object with file name of *.csv file with transposed long-term summary statistics.}
#'    \item{na.rm}{Missing value flags.}
#'    \item{Version}{Version of this function.}
#'    \item{Date}{Date function was run.}
#' @examples
#' \dontrun{
#' stat.longterm <- longterm.stats(
#'                          station.name  ='ABCDE',
#'                          flow.data          =flow,
#'                          start.year    =1960,
#'                          end.year      =2015)
#' }
#'
#'@export
#'@import ggplot2
#'@import scales
#'@import utils

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

longterm.stats <- function(station.name=NULL,
                           flow.data=NULL,
                           HYDAT=NULL,
                           water.year= FALSE, #not required
                           start.year=NULL, #not required
                           end.year=NULL, #not required
                           exclude.years=NULL, # list of stations
                           write.table=TRUE,         # write out calendar year statistics
                           write.transposed.table=TRUE,   # write out statistics in transposed format
                           report.dir='.',
                           csv.nddigits=3,               # decimal digit for csv files.
                           na.rm=list(na.rm.global=TRUE) ## may not be req'd
){
  
  #  Output: List with elements as desribed above
  #
  #############################################################
  #  Some basic error checking on the input parameters
  #
  Version <- packageVersion("fasstr")
  
  if( !is.null(HYDAT) & !is.null(flow.data))  {stop("Must select either flow.data or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & is.null(station.name))  {stop("station.name required with flow.data parameter.")}
  if( is.null(HYDAT) & !is.character(station.name))  {stop("station.name must be a character string.")}
  if( is.null(HYDAT) & length(station.name)>1)        {stop("station.name cannot have length > 1")}
  if( is.null(flow.data) & is.null(HYDAT)){stop("Flow or HYDAT parameters must be set")}
  if( is.null(HYDAT) & !is.data.frame(flow.data))         {stop("Flow is not a data frame.")}
  if( is.null(HYDAT) &! all(c("Date","Q") %in% names(flow))){stop("Flow dataframe doesn't contain the variables Date and Q.")}
  if( is.null(HYDAT) & ! inherits(flow.data$Date[1], "Date")){stop("Date column in Flow data frame is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flow.data$Q))          {stop("Q column in flow.data dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flow.data$Q <0, na.rm=TRUE))   {stop('flow.data cannot have negative values - check your data')}
  
  if( !(is.numeric(start.year) | is.null(start.year)))   {stop("start.year must be numeric.")}
  if( !(is.numeric(end.year) | is.null(end.year)))   {stop("end.year must be numeric.")}
  if( !is.logical(water.year))  {stop("water.year must be logical (TRUE/FALSE")}
  if( !is.null(exclude.years) & !is.numeric(exclude.years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.logical(write.table))  {stop("write.table must be logical (TRUE/FALSE")}
  if( !is.logical(write.transposed.table)){stop("write.transposed.table must be logical (TRUE/FALSE")}
  if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exist")}
  
  if(!is.numeric(csv.nddigits)){ stop("csv.nddigits must be numeric")}
  csv.nddigits <- round(csv.nddigits)[1]
  
  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if( !is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  # If HYDAT station is listed, check if it exists and make it the flow.data
  if (!is.null(HYDAT)) {
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("HYDAT station does not exist.")}
    if (is.null(station.name)) {station.name <- HYDAT}
    flow.data <- tidyhydat::DLY_FLOWS(STATION_NUMBER = HYDAT)
    flow.data <- dplyr::select(flow.data,Date,Q=Value)
  }
  
  # If start/end years are not select, set them as the min/max dates
  min.year <- lubridate::year(min(flow.data$Date))-water.year
  max.year <- lubridate::year(max(flow.data$Date))
  if (!is.numeric(start.year)) {start.year <- min.year}
  if (!is.numeric(end.year)) {end.year <- max.year}
  if(! (start.year <= end.year))    {stop("start.year parameter must be less than end.year parameter")}

  #  create the year (annual ) and month variables
  flow.data$Year  <- lubridate::year(flow.data$Date)
  flow.data$MonthNum  <- lubridate::month(flow.data$Date)
  flow.data$Month <- month.abb[flow.data$MonthNum]
  flow.data$WaterYear <- as.numeric(ifelse(flow.data$MonthNum>=10,flow.data$Year+1,flow.data$Year))
  
  # Filter for start and end years, and if water year, and remove excluded years' data
  if (water.year) {
    flow.data <- dplyr::filter(flow.data, WaterYear >= start.year & WaterYear<=end.year)
    flow.data <- dplyr::mutate(flow.data,Q=replace(Q, WaterYear %in% exclude.years, NA))
    
  } else {
    flow.data <- dplyr::filter(flow.data, Year >= start.year & Year<=end.year)
    flow.data <- dplyr::mutate(flow.data,Q=replace(Q, Year %in% exclude.years, NA))
  }
  
  #  Compute calendar year long-term stats
  Q.month.longterm <-   dplyr::summarize(dplyr::group_by(flow.data,Month),
                                         Mean = mean(Q,na.rm=TRUE),
                                         Median = median(Q,na.rm=TRUE),
                                         Maximum = max(Q,na.rm=TRUE),
                                         Minimum = min(Q,na.rm=TRUE))
  Q.all.longterm <-   dplyr::summarize(flow.data,
                                       Mean = mean(Q,na.rm=TRUE),
                                       Median = median(Q,na.rm=TRUE),
                                       Maximum = max(Q,na.rm=TRUE),
                                       Minimum = min(Q,na.rm=TRUE))
  Q.all.longterm <- dplyr::mutate(Q.all.longterm,Month="Long-term")
  
  
  Q.longterm <- rbind(Q.month.longterm, Q.all.longterm)
  if (water.year) {Q.longterm$Month <- factor(Q.longterm$Month, levels=c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Long-term"))}
  else {           Q.longterm$Month <- factor(Q.longterm$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term"))}
  Q.longterm <- with(Q.longterm, Q.longterm[order(Month),])
  
  
  #  Write out summary tables for calendar years
  file.stat.csv <- NA
  if(write.table){
    file.stat.csv <-file.path(report.dir, paste(station.name,"-longterm-summary-stat.csv", sep=""))
    temp <- Q.longterm
    temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], csv.nddigits)  # round the output
    utils::write.csv(temp, file=file.stat.csv, row.names=FALSE)
  }
  
  #  Write out thesummary table in transposed format
  Q.longterm.trans <- tidyr::gather(Q.longterm,Statistic,Value,-Month)
  Q.longterm.trans.temp <- dplyr::mutate(Q.longterm.trans,Value=round(Value,csv.nddigits)) # for writing to csv
  Q.longterm.trans <- tidyr::spread(Q.longterm.trans,Month,Value)
  file.stat.trans.csv <- NA
  if(write.transposed.table){
    file.stat.trans.csv <-file.path(report.dir,paste(station.name,"-longterm-summary-stat-trans.csv", sep=""))
    Q.longterm.trans.temp <- tidyr::spread(Q.longterm.trans.temp,Month,Value)
    utils::write.csv(Q.longterm.trans.temp, file=file.stat.trans.csv, row.names=FALSE)
  }
  
  
  return(list("station name"= station.name,
              "year type"=ifelse(!water.year,"Calendar Year (Jan-Dec)","Water Year (Oct-Sep)"),
              "year range"=paste0(start.year," - ",end.year),
              Q.stat.longterm=Q.longterm,
              Q.stat.longterm.trans=Q.longterm.trans,
              file.stat.csv=file.stat.csv,
              file.stat.trans.csv=file.stat.trans.csv,
              na.rm=na.rm,
              Version=Version,
              Date=Sys.time()))
} # end of function
