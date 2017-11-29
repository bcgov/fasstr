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


#' @title Compute annual low flows and their dates.
#'
#' @description Compute annual low flows and their dates.
#' Streamflow data can be supplied through the \code{flowdata} parameter or extracted from a 
#' HYDAT database using the tidyhydat package and \code{HYDAT} parameter.
#' No transpose due to dates.
#'
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Value' column with the daily 
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
#' @param exclude_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param transpose Logical. Switch the rows and columns of the results. Dates excluded.
#' @param write_table Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param table_nddigits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#' @param na.rm TBD
#'
#'
#' @examples
#' \dontrun{
#' stat.annual <- annual_stats(HYDAT="08HB048",
#'                             basin_area    = 10.1)
#' 
#' stat.annual <- annual_stats(station_name  ='Mission Creek',
#'                                flowdata     = flow,
#'                                water_year    = TRUE,
#'                                start_year    = 1967,
#'                                end_year      = 2014)
#' }
#' @export

#'
#--------------------------------------------------------------
# Compute the statistics on an (calendar and water) year basis

fasstr_annual_lowflows <- function(flowdata=NULL,
                                   HYDAT=NULL,
                                   station_name="fasstr",
                                   water_year=FALSE,
                                   water_year_start=10,
                                   start_year=NULL,
                                   end_year=NULL,
                                   exclude_years=NULL,
                                   rolling_days=c(1,3,7,30),
                                   rolling_align="right",
                                   transpose=FALSE,
                                   write_table=FALSE,
                                   report_dir=".",
                                   na.rm=list(na.rm.global=FALSE),
                                   table_nddigits=3){
  
  #############################################################
  
  #  Some basic error checking on the input parameters
  #
  if( is.null(flowdata) & is.null(HYDAT)) {stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & !is.character(station_name))  {stop("station_name parameter must be a character string.")}
  if( is.null(HYDAT) & length(station_name)>1)        {stop("station_name parameter cannot have length > 1")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {stop("flowdata parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Value") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Value.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {stop("Value column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {stop('flowdata cannot have negative values - check your data')}
  
  if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}
  if( length(water_year_start)>1) {stop("water_year_start must be a number between 1 and 12 (Jan-Dec)")}
  if( water_year_start <1 | water_year_start >12 ) {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  if( !(water_year_start==floor(water_year_start)))  {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.numeric(rolling_days))   {
    stop("rolling_days must be numeric")}
  if( !all(rolling_days>0 & rolling_days<=180))  {
    stop("rolling_days must be >0 and <=180)")}
  if( !all(rolling_days==floor(rolling_days)))  {
    stop("rolling_days must be integers")}
  if ( !rolling_align %in% c("right","left","center")){
    stop("align rolling_align must be 'right', 'left', or 'center'.")}
  
  
  if( !is.logical(write_table))  {stop("write_table parameter must be logical (TRUE/FALSE)")}
  
  if( !dir.exists(as.character(report_dir)))      {stop("directory for saved files does not exist")}
  if( !is.numeric(table_nddigits))  { stop("csv.ndddigits parameter needs to be numeric")}
  table_nddigits <- round(table_nddigits[1])  # number of decimal digits for rounding in csv files
  
  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    if (station_name=="fasstr") {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  # add date variables to determine the min/max cal/water years
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  min_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))
  max_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))
  
  # If start/end years are not select, set them as the min/max dates
  if (is.null(start_year)) {start_year <- min_year}
  if (is.null(end_year)) {end_year <- max_year}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  #  Fill in the missing dates and the add the date variables again
  flowdata <- fasstr::fasstr_fill_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  
  
  
  if (water_year) {
    if (water_year_start==1) {
      origin_date <- "12-31"
    } else if (water_year_start==2) {
      origin_date <- "01-31"
    } else if (water_year_start==3) {
      origin_date <- "02-28"
    } else if (water_year_start==4) {
      origin_date <- "03-31"
    } else if (water_year_start==5) {
      origin_date <- "04-30"
    } else if (water_year_start==6) {
      origin_date <- "05-31"
    } else if (water_year_start==7) {
      origin_date <- "06-30"
    } else if (water_year_start==8) {
      origin_date <- "07-31"
    } else if (water_year_start==9) {
      origin_date <- "08-31"
    } else if (water_year_start==10) {
      origin_date <- "09-30"
    } else if (water_year_start==11) {
      origin_date <- "10-31"
    } else if (water_year_start==12) {
      origin_date <- "11-30"
    }
  } else if (!water_year) {
    origin_date <- "12-31"
  }
  
  # Loop through each rolling_day and compute annual min values and their dates
  Q_lowflow <- dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear))
  for (day in rolling_days) {
    flowdata_temp <- fasstr::fasstr_add_rolling_means(flowdata,days = day,align = rolling_align)
    names(flowdata_temp)[names(flowdata_temp) == paste0("Q",day,"Day")] <- "RollingValue"
    Q_lowflow_temp <- dplyr::summarize(dplyr::group_by(flowdata_temp,AnalysisYear),
                                       MIN_VALUE = min(RollingValue, na.rm=na.rm$na.rm.global),	     
                                       MIN_DAY = ifelse(is.na(MIN_VALUE),NA,
                                                        AnalysisDoY[which(RollingValue==MIN_VALUE)]),
                                       MIN_DATE= ifelse(is.na(MIN_VALUE),NA,
                                                        Date[which(RollingValue==MIN_VALUE)]))
    class(Q_lowflow_temp$MIN_DATE)<- "Date" # fixes ifelse and date issue
    names(Q_lowflow_temp)[names(Q_lowflow_temp) == "MIN_VALUE"] <- paste0("Min_",day,"_Day")
    names(Q_lowflow_temp)[names(Q_lowflow_temp) == "MIN_DAY"] <- paste0("Min_",day,"_Day_DoY")
    names(Q_lowflow_temp)[names(Q_lowflow_temp) == "MIN_DATE"] <- paste0("Min_",day,"_Day_Date")
    
    Q_lowflow <- merge(Q_lowflow,Q_lowflow_temp,by="AnalysisYear",all = T)
  }
  Q_lowflow <-   dplyr::rename(Q_lowflow,Year=AnalysisYear)
  
  # FILTER Q_lowflow FOR SELECTED YEARS FOR REMAINDER OF CALCS
  Q_lowflow <- dplyr::filter(Q_lowflow, Year >= start_year & Year <= end_year)
  Q_lowflow[Q_lowflow$Year %in% exclude_years,-1] <- NA
  
  
  
  if(transpose){
    Q_lowflow <- dplyr::select(Q_lowflow,Year,dplyr::contains("Day"),-dplyr::contains("Date"))
    Q_lowflow <- tidyr::gather(Q_lowflow,Statistic,Value,-Year)
    Q_lowflow <- tidyr::spread(Q_lowflow,Year,Value)
  }
  
  
  if(write_table){
    file_Qlowflows_table <- file.path(report_dir, paste(station_name,"-annual-lowflows.csv", sep=""))
    temp <- Q_lowflow
    # rounding just numeric columns
    if (transpose) {
      temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], table_nddigits)
    } else {
      numVars <- sapply(temp, is.numeric) 
      temp[numVars] <- lapply(temp[numVars], round, digits = table_nddigits) 
    }
    utils::write.csv(temp,file=file_Qlowflows_table, row.names=FALSE)
  }
  
  
  
  
  return(Q_lowflow)
}

