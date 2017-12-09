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

#' @title Calculate the daily summary statistics
#'
#' @description Calculates the daily mean, median, maximum, minimum, and percentiles for each day of the year of daily flow values 
#'    from a streamflow dataset. Calculate the statistics from all daily discharge values from all years, unless specified. Can determine
#'    statistics of rolling mean days (e.g. 7-day flows) using the rolling_days argument.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param percentiles Numeric. Vector of percentiles to calculate. Set to NA if none required. Default \code{c(5,25,75,95)}
#' @param rolling_days  Numeric. The number of days to apply a rolling mean. Default \code{1}.
#' @param rolling_align Character. Specifies whether the dates of the rolling mean should be specified by the first ('left'), last ('right),
#'    or middle ('center') of the rolling n-day group of observations. Default \code{'right'}.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.  
#' @param transpose Logical. Switch the rows and columns of the results table. Default \code{FALSE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_table Logical. Write the table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_digits Numeric. Number of significant digits to round the results in the written table. Default \code{3}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#'    
#' @return A data frame with the following columns:
#'   \item{Date}{date (MMM-DD) of daily statistics}
#'   \item{DayofYear}{day of year of daily statistics}
#'   \item{Mean}{daily mean of all flows for a given day of the year}
#'   \item{Median}{daily mean of all flows for a given day of the year}
#'   \item{Maximum}{daily mean of all flows for a given day of the year}
#'   \item{Minimum}{daily mean of all flows for a given day of the year}
#'   \item{P'n'}{each daily n-th percentile selected of all flows for a given day of the year}
#'   Default percentile columns:
#'   \item{P5}{daily 5th percentile of all flows for a given day of the year}
#'   \item{P25}{daily 25th percentile of all flows for a given day of the year}
#'   \item{P75}{daily 75th percentile of all flows for a given day of the year}
#'   \item{P95}{daily 95th percentile of all flows for a given day of the year}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'   
#' @examples
#' \dontrun{
#' 
#'calc_daily_stats(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'calc_daily_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, percentiles = c(1:10))
#'
#' }
#' @export

#--------------------------------------------------------------

calc_daily_stats <- function(flowdata=NULL,
                               HYDAT=NULL,
                               percentiles=c(5,25,75,95),
                               rolling_days=1,
                               rolling_align="right",
                               water_year=FALSE,
                               water_year_start=10,
                               start_year=NULL,
                               end_year=NULL,
                               exclude_years=NULL, 
                               transpose=FALSE,
                               station_name=NA,
                               write_table=FALSE,     
                               write_digits=3,
                               write_dir=".",
                               na.rm=list(na.rm.global=FALSE)){
  
  
  #--------------------------------------------------------------
  #  Error checking on the input parameters
  
  if( !is.null(HYDAT) & !is.null(flowdata))           {stop("must select either flowdata or HYDAT arguments, not both")}
  if( is.null(HYDAT)) {
    if( is.null(flowdata))                            {stop("one of flowdata or HYDAT arguments must be set")}
    if( !is.data.frame(flowdata))                     {stop("flowdata arguments is not a data frame")}
    if( !all(c("Date","Value") %in% names(flowdata))) {stop("flowdata data frame doesn't contain the variables 'Date' and 'Value'")}
    if( !inherits(flowdata$Date[1], "Date"))          {stop("'Date' column in flowdata data frame is not a date")}
    if( !is.numeric(flowdata$Value))                  {stop("'Value' column in flowdata data frame is not numeric")}
    if( any(flowdata$Value <0, na.rm=TRUE))           {warning('flowdata cannot have negative values - check your data')}
  }
  
  if( !is.logical(water_year))         {stop("water_year argument must be logical (TRUE/FALSE)")}
  if( !is.numeric(water_year_start) )  {stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec)")}
  if( length(water_year_start)>1)      {stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec)")}
  if( !water_year_start %in% c(1:12) ) {stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec)")}
  
  if( length(start_year)>1)   {stop("only one start_year value can be selected")}
  if( !is.null(start_year) )  {if( !start_year %in% c(0:5000) )  {stop("start_year must be an integer")}}
  if( length(end_year)>1)     {stop("only one end_year value can be selected")}
  if( !is.null(end_year) )    {if( !end_year %in% c(0:5000) )  {stop("end_year must be an integer")}}
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("list of exclude_years must be numeric - ex. 1999 or c(1999,2000)")}
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.numeric(percentiles))                 {stop("percentiles argument must be numeric")}
  if( !all(percentiles>0 & percentiles<100))    {stop("percentiles must be >0 and <100)")}
  
  if( !is.numeric(rolling_days))                       {stop("rolling_days argument must be numeric")}
  if( !all(rolling_days %in% c(1:180)) )               {stop("rolling_days argument must be integers > 0 and <= 180)")}
  if( !rolling_align %in% c("right","left","center"))  {stop("rolling_align argument must be 'right', 'left', or 'center'")}
  
  if( !is.logical(transpose))    {stop("transpose parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_table))  {stop("write_table parameter must be logical (TRUE/FALSE)")}
  if( !is.numeric(write_digits))  {stop("csv.ndddigits parameter needs to be numeric")}
  write_digits <- round(write_digits[1])
  
  if( !dir.exists(as.character(write_dir))) {
    message("directory for saved files does not exist, new directory will be created")
    if( write_table & write_dir!="." ) {dir.create(write_dir)}
  }
  
  if( !is.list(na.rm))                        {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm)))             {stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  #--------------------------------------------------------------
  # Set the flowdata for analysis
  
  # Select just Date and Value for analysis
  flowdata <- dplyr::select(flowdata,Date,Value)
  
  # add date variables to determine the min/max cal/water years
  flowdata <- fasstr::add_date_variables(flowdata,water_year = T,water_year_start = water_year_start)
  if (is.null(start_year)) {start_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))}
  if (is.null(end_year)) {end_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  #  Fill in the missing dates and the add the date variables again
  flowdata <- fasstr::fill_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::add_date_variables(flowdata,water_year = T,water_year_start = water_year_start)
  
  # Set selected year-type and day of year, and date columns for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
    if        (water_year_start==1)  {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1989-12-31")
    } else if (water_year_start==2)  {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-01-31")
    } else if (water_year_start==3)  {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-02-28")
    } else if (water_year_start==4)  {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-03-31")
    } else if (water_year_start==5)  {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-04-30")
    } else if (water_year_start==6)  {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-05-31")
    } else if (water_year_start==7)  {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-06-30")
    } else if (water_year_start==8)  {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-07-31")
    } else if (water_year_start==9)  {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-08-31")
    } else if (water_year_start==10) {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-09-30")
    } else if (water_year_start==11) {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-10-31")
    } else if (water_year_start==12) {flowdata$AnalysisDate <- as.Date(flowdata$WaterDayofYear, origin = "1899-11-30")
    }
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
    flowdata$AnalysisDate <- as.Date(flowdata$DayofYear, origin = "1899-12-31")
  }
  
  # Apply rolling mean if designated, default of 1
  flowdata <- fasstr::add_rolling_means(flowdata,days = rolling_days,align = rolling_align)
  colnames(flowdata)[ncol(flowdata)] <- "RollingValue"
  
  # Filter for the selected and excluded years and leap year values (last day)
  flowdata <- dplyr::filter(flowdata,AnalysisYear>=start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% exclude_years))
  flowdata <- dplyr::filter(flowdata,AnalysisDoY<366)
  
  #--------------------------------------------------------------
  # Complete analysis
  
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
                                        Percentile=quantile(RollingValue,ptile/100, na.rm=TRUE))
      colnames(Q_daily_ptile)[3] <- paste0("P",ptile)
      Q_daily <- merge(Q_daily,Q_daily_ptile,by=c("AnalysisDate","AnalysisDoY"))
    }
  }
  
  # Final formatting
  Q_daily <- dplyr::rename(Q_daily,"DayofYear"=AnalysisDoY,Date=AnalysisDate)
  Q_daily$Date <- format(as.Date(Q_daily$Date),format="%b-%d")
  col_order <- Q_daily$Date
  col_names <- names(Q_daily[-1])
  
  
  
  # If transpose==TRUE
  if (transpose) {
    Q_daily <- tidyr::gather(Q_daily,Statistic,Value,-Date)
    Q_daily <- tidyr::spread(Q_daily,Date,Value)
    Q_daily <-  Q_daily[,c("Statistic",col_order)]
    Q_daily <- Q_daily[match(col_names, Q_daily$Statistic),]
    row.names(Q_daily) <- c(1:nrow(Q_daily))
  }
  
  #  Write the table
  if (write_table) {
    file.stat.csv <-file.path(write_dir, 
                              paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                                    "-daily-summary-stat.csv", sep=""))
    temp <- Q_daily
    temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], write_digits)  # round the output
    utils::write.csv(temp, file=file.stat.csv, row.names=FALSE)
  }
  
  return(Q_daily)
  
  
}
