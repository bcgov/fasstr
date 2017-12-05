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

#' @title Calculate annual summary and missing data statistics
#'
#' @description Calculates mean, median, maximum, minimum, standard deviation of annual flows and data availability and missing data
#'    statistics for each year and month of each year. Calculates the statistics from all daily discharge values from all years, 
#'    unless specified.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param rolling_days  Numeric. The number of days to apply a rolling mean. Default \code{1}.
#' @param rolling_align Character. Specifies whether the dates of the rolling mean should be specified by the first ('left'), last ('right),
#'    or middle ('center') of the rolling n-day group of observations. Default \code{'right'}.
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required. 
#' @param transpose Logical. Switch the rows and columns of the results table. Default \code{FALSE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_table Logical. Write the table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_digits Numeric. Number of significant digits to round the results in the written table. Default \code{3}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#'
#' @return A data frame of annual and annual-month summary missing data statistics
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_data_screening(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'fasstr_data_screening(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export


#--------------------------------------------------------------


fasstr_data_screening <- function(flowdata=NULL,
                                  HYDAT=NULL,
                                  rolling_days=1,
                                  rolling_align="right",
                                  water_year=FALSE,
                                  water_year_start=10,
                                  start_year=NULL,
                                  end_year=NULL,
                                  transpose=FALSE,
                                  station_name=NA,
                                  write_table=FALSE,
                                  write_digits=3,      
                                  write_dir="."){             
  
  
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
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.logical(transpose))    {stop("transpose parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_table))  {stop("write_table parameter must be logical (TRUE/FALSE)")}
  if( !is.numeric(write_digits))  {stop("csv.ndddigits parameter needs to be numeric")}
  write_digits <- round(write_digits[1])
  
  if( !dir.exists(as.character(write_dir))) {
    message("directory for saved files does not exist, new directory will be created")
    if( write_table & write_dir!="." ) {dir.create(write_dir)}
  }
  
  if( !is.numeric(rolling_days))                       {stop("rolling_days argument must be numeric")}
  if( !all(rolling_days %in% c(1:180)) )               {stop("rolling_days argument must be integers > 0 and <= 180)")}
  if( !rolling_align %in% c("right","left","center"))  {stop("rolling_align argument must be 'right', 'left', or 'center'")}
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("only one HYDAT station can be selected")}
    if( !HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  #--------------------------------------------------------------
  # Set the flowdata for analysis
  
  # add date variables to determine the min/max cal/water years
  flowdata <- dplyr::select(flowdata,Date,Value)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  min_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))
  max_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))
  
  # If start/end years are not select, set them as the min/max dates
  if (is.null(start_year)) {start_year <- min_year}
  if (is.null(end_year)) {end_year <- max_year}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  #  Fill in the missing dates and the add the date variables again
  flowdata <- fasstr::fasstr_add_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_rolling_means(flowdata,days = rolling_days,align = rolling_align)
  colnames(flowdata)[ncol(flowdata)] <- "RollingValue"
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
  }
  
  # Filter data for years selected
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  
  #--------------------------------------------------------------
  # Complete analysis
  
  Q_summary <-   dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear),
                                  n_days      = length(AnalysisYear),
                                  n_Q         = sum (!is.na(RollingValue)),
                                  n_missing_Q = sum ( is.na(RollingValue)),
                                  Minimum     = ifelse(n_Q==0,NA,min (RollingValue,na.rm=T)),
                                  Maximum     = ifelse(n_Q==0,NA,max (RollingValue,na.rm=T)),
                                  Mean        = ifelse(n_Q==0,NA,mean(RollingValue,na.rm=T)),
                                  Median      = stats::median(RollingValue, na.rm=T),
                                  StandardDeviation     = stats::sd  (RollingValue,   na.rm=T)
  )
  
  Q_summary_month <-   dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear,MonthName),
                                        n_missing_Q = sum ( is.na(RollingValue)))
  Q_summary_month <- dplyr::rename(Q_summary_month,Month=MonthName)
  Q_summary_month <- dplyr::mutate(Q_summary_month,Month=paste0(Month,"_missing_Q"))
  
  
  if (water_year) {
    if (water_year_start==1) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q",
                                                                      "Apr_missing_Q", "May_missing_Q","Jun_missing_Q",
                                                                      "Jul_missing_Q","Aug_missing_Q","Sep_missing_Q",
                                                                      "Oct_missing_Q","Nov_missing_Q","Dec_missing_Q"))
    } else if (water_year_start==2) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", 
                                                                      "May_missing_Q","Jun_missing_Q","Jul_missing_Q",
                                                                      "Aug_missing_Q","Sep_missing_Q","Oct_missing_Q",
                                                                      "Nov_missing_Q","Dec_missing_Q","Jan_missing_Q"))
    } else if (water_year_start==3) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                      "Jun_missing_Q","Jul_missing_Q","Aug_missing_Q",
                                                                      "Sep_missing_Q","Oct_missing_Q","Nov_missing_Q",
                                                                      "Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q"))
    } else if (water_year_start==4) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Apr_missing_Q", "May_missing_Q","Jun_missing_Q",
                                                                      "Jul_missing_Q","Aug_missing_Q","Sep_missing_Q",
                                                                      "Oct_missing_Q","Nov_missing_Q","Dec_missing_Q",
                                                                      "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q"))
    } else if (water_year_start==5) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("May_missing_Q","Jun_missing_Q","Jul_missing_Q",
                                                                      "Aug_missing_Q","Sep_missing_Q","Oct_missing_Q",
                                                                      "Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", 
                                                                      "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q"))
    } else if (water_year_start==6) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jun_missing_Q","Jul_missing_Q","Aug_missing_Q",
                                                                      "Sep_missing_Q","Oct_missing_Q","Nov_missing_Q",
                                                                      "Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", 
                                                                      "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q"))
    } else if (water_year_start==7) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jul_missing_Q","Aug_missing_Q","Sep_missing_Q",
                                                                      "Oct_missing_Q","Nov_missing_Q","Dec_missing_Q",
                                                                      "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q",
                                                                      "Apr_missing_Q", "May_missing_Q","Jun_missing_Q"))
    } else if (water_year_start==8) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Aug_missing_Q","Sep_missing_Q","Oct_missing_Q",
                                                                      "Nov_missing_Q","Dec_missing_Q","Jan_missing_Q",
                                                                      "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q",
                                                                      "May_missing_Q","Jun_missing_Q","Jul_missing_Q"))
    } else if (water_year_start==9) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Sep_missing_Q","Oct_missing_Q","Nov_missing_Q",
                                                                      "Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q",
                                                                      "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                      "Jun_missing_Q","Jul_missing_Q","Aug_missing_Q"))
    } else if (water_year_start==10) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Oct_missing_Q","Nov_missing_Q","Dec_missing_Q",
                                                                      "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", 
                                                                      "Apr_missing_Q", "May_missing_Q","Jun_missing_Q",
                                                                      "Jul_missing_Q","Aug_missing_Q","Sep_missing_Q"))
    } else if (water_year_start==11) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Nov_missing_Q","Dec_missing_Q","Jan_missing_Q",
                                                                      "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", 
                                                                      "May_missing_Q","Jun_missing_Q","Jul_missing_Q",
                                                                      "Aug_missing_Q","Sep_missing_Q","Oct_missing_Q"))
    } else if (water_year_start==12) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q",
                                                                      "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                      "Jun_missing_Q","Jul_missing_Q","Aug_missing_Q",
                                                                      "Sep_missing_Q","Oct_missing_Q","Nov_missing_Q"))
    }
  } else {           
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", 
                                                                    "Apr_missing_Q", "May_missing_Q","Jun_missing_Q",
                                                                    "Jul_missing_Q","Aug_missing_Q","Sep_missing_Q",
                                                                    "Oct_missing_Q","Nov_missing_Q","Dec_missing_Q"))
  }
  
  
  Q_summary_month <- tidyr::spread(Q_summary_month,Month,n_missing_Q)
  
  Q_summary <- merge(Q_summary,Q_summary_month,by="AnalysisYear",all = TRUE)
  Q_summary <- dplyr::rename(Q_summary,Year=AnalysisYear)
  row_order <- names(Q_summary[,-1])
  
  if(transpose){
    Q_summary_tpose <- tidyr::gather(Q_summary,Statistic,Value,-Year)
    Q_summary_tpose_temp <- dplyr::mutate(Q_summary_tpose,Value=round(Value,write_digits)) # for writing to csv
    Q_summary <- tidyr::spread(Q_summary_tpose,Year,Value)
    Q_summary <- Q_summary_[match(row_order, Q_summary_$Statistic),]
  }
  
  # See if you want to write out the summary tables?
  if(write_table){
    # Write out the summary table for comparison to excel spreadsheet for calendar year
    file_Q_summary_table <- file.path(write_dir,
                                      paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                                            "-annual-summary.csv", sep=""))
    temp <- Q_summary
    temp <- round(temp, write_digits)
    if(transpose){
      temp <- tidyr::spread(Q_summary_tpose_temp,Year,Value)
    }
    utils::write.csv(temp,file=file_Q_summary_table, row.names=FALSE)
  }
  
  return(Q_summary)
  
}

