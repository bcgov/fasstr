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

#' @title Compute streamflow statistics for each day of the year.
#'
#' @description Compute streamflow statistics for each day of the year.
#' Streamflow data can be supplied through the \code{flowdata} parameter or extracted from a 
#' HYDAT database using the tidyhydat package and \code{HYDAT} parameter.
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
#' @param excluded_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).   
#' @param percentiles Numeric. List of numbers to calculate percentiles (5 = 5th percentile). Default c(5,25,75,95).
#' @param rolling_days Numeric. Rolling days. Default 1.
#' @param rolling_align Character. Specifies whether the index of the result should be left- or right-aligned or centered 
#'    (default) compared to the rolling window of observations#' @param transpose Logical. Switch the rows and columns of the results.
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

fasstr_daily_stats <- function(flowdata=NULL,
                               HYDAT=NULL,
                               station_name="fasstr",
                               water_year=FALSE, #create another for own water year????
                               water_year_start=10,
                               start_year=NULL,
                               end_year=NULL,
                               exclude_years=NULL, # list of stations
                               percentiles=c(5,25,75,95),
                               rolling_days=1,
                               rolling_align="right",
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
  if( is.null(HYDAT) & !all(c("Date","Value") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Value.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {
    stop("Value column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {
    stop('flowdata cannot have negative values - check your data')}
  
  if( !is.numeric(percentiles))   {
    stop("percentiles must be numeric")}
  if( !all(percentiles>0 & percentiles<=100))  {
    stop("percentiles must be >0 and <=100)")}
  
  if( !is.numeric(rolling_days))   {
    stop("rolling_days must be numeric")}
  if( !all(rolling_days>0 & rolling_days<=180))  {
    stop("rolling_days must be >0 and <=180)")}
  if( !all(rolling_days==floor(rolling_days)))  {
    stop("rolling_days must be integers")}
  if ( !rolling_align %in% c("right","left","center")){
    stop("rolling_align parameter must be 'right', 'left', or 'center'.")}
  
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
  }
  
  # add date variables to determine the min/max cal/water years
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year_start = water_year_start)
  min_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))
  max_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))
  
  # If start/end years are not select, set them as the min/max dates
  if (is.null(start_year)) {start_year <- min_year}
  if (is.null(end_year)) {end_year <- max_year}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  #  create the year (annual ) and month variables
  flowdata <- fasstr::fasstr_fill_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
    
    if (water_year_start==1) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1989-12-31")
    } else if (water_year_start==2) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-01-31")
    } else if (water_year_start==3) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-02-28")
    } else if (water_year_start==4) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-03-31")
    } else if (water_year_start==5) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-04-30")
    } else if (water_year_start==6) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-05-31")
    } else if (water_year_start==7) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-06-30")
    } else if (water_year_start==8) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-07-31")
    } else if (water_year_start==9) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-08-31")
    } else if (water_year_start==10) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-09-30")
    } else if (water_year_start==11) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-10-31")
    } else if (water_year_start==12) {
      flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-11-30")
    }
    
    
  }  else {
    
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
    flowdata$AnalysisDate <- as.Date(flowdata$DayofYear, origin = "1899-12-31")
    
  }
  
  # Apply rolling mean if designated, default of 1
  flowdata <- fasstr::fasstr_add_rolling_means(flowdata,days = rolling_days,align = rolling_align)
  colnames(flowdata)[ncol(flowdata)] <- "RollingValue"
  
  
  # Filter for the selected and excluded years
  flowdata <- dplyr::filter(flowdata,AnalysisYear>=start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% exclude_years))
  
  # remove leap year values
  flowdata <- dplyr::filter(flowdata,AnalysisDoY<366)
  
  #  Compute daily summary stats
  Q_daily <- dplyr::summarise(dplyr::group_by(flowdata,AnalysisDate,AnalysisDoY),
                              Mean=mean(RollingValue, na.rm=T),
                              Median=median(RollingValue, na.rm=T),
                              Minimum=min(RollingValue, na.rm=T),
                              Maximum=max(RollingValue, na.rm=T))
  
  # Compute daily percentiles (if 10 or more years of data)
  if (!all(is.na(percentiles))){
    for (ptile in percentiles) {
      Q_daily_ptile <- dplyr::summarise(dplyr::group_by(flowdata,AnalysisDate,AnalysisDoY),
                                        Percentile=ifelse(sum(!is.na(RollingValue))>=10,quantile(RollingValue,ptile/100, na.rm=TRUE),NA))
      colnames(Q_daily_ptile)[3] <- paste0("P",ptile)
      Q_daily <- merge(Q_daily,Q_daily_ptile,by=c("AnalysisDate","AnalysisDoY"))
    }
  }
  
  # Final formatting
  Q_daily <- dplyr::rename(Q_daily,"DayofYear"=AnalysisDoY,Date=AnalysisDate)
  Q_daily$Date <- format(as.Date(Q_daily$Date),format="%b-%d")
  col_order <- Q_daily$Date
  
  
  # If transpose==TRUE
  if (transpose) {
    Q_daily_tpose <- tidyr::gather(Q_daily,Statistic,Value,-Date)
    Q_daily <- tidyr::spread(Q_daily_tpose,Date,Value)
    Q_daily <-  Q_daily[,c("Statistic",col_order)]
  }
  
  #  Write the table
  if (write_table) {
    file.stat.csv <-file.path(report_dir, paste(station_name,"-daily-summary-stat.csv", sep=""))
    temp <- Q_daily
    temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], table_nddigits)  # round the output
    utils::write.csv(temp, file=file.stat.csv, row.names=FALSE)
  }
  
  return(Q_daily)
  
  
} # end of function
