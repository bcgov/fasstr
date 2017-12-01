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


#' @title fasstr_annual_screening
#'
#' @description Computes annual statistics of streamflow data.
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
#' @param start_year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start_year} is the first
#'    year of the data provided.
#' @param end_year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end_year} is the last
#'    year of the data provided.
#' @param rolling_days Numeric. Rolling days. Default 1.
#' @param rolling_align Character. Specifies whether the index of the result should be left- or right-aligned or centered 
#'    (default) compared to the rolling window of observations#'
#' @param transpose Logical. Switch the rows and columns of the results.
#' @param write_table Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(write_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param write_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param write_digits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#'
#'
#' @examples
#' \dontrun{
#' 
#' Coming soon :)
#' 
#' }
#' @export

#'
#--------------------------------------------------------------
# Compute the statistics on an (calendar and water) year basis

fasstr_data_screening <- function(flowdata=NULL,
                                  HYDAT=NULL,
                                  station_name=NA,
                                  water_year=FALSE, 
                                  start_year=NULL,
                                  end_year=NULL,
                                  water_year_start=10,
                                  rolling_days=1,
                                  rolling_align="right",
                                  transpose=FALSE,
                                  write_table=FALSE,      
                                  write_dir=".",
                                  write_digits=3){             

  #############################################################
  
  #  Some basic error checking on the input parameters
  #
  if( is.null(flowdata) & is.null(HYDAT)) {stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & length(station_name)>1)        {stop("station_name parameter cannot have length > 1")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {stop("flowdata parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Value") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Value.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Value))          {stop("Value column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {stop('flowdata cannot have negative values - check your data')}
  
  if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}

  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.numeric(rolling_days))   {
    stop("rolling_days must be numeric")}
  if( length(rolling_days)>1 ) {
    stop("rolling_days must be one number")  }
  if( !all(rolling_days>0 & rolling_days<=180))  {
    stop("rolling_days must be >0 and <=180)")}
  if( !all(rolling_days==floor(rolling_days)))  {
    stop("rolling_days must be integers")}
  if ( !rolling_align %in% c("right","left","center")){
    stop("rolling_align parameter must be 'right', 'left', or 'center'.")}
  
  if( !is.logical(transpose))  {stop("transpose parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_table))  {stop("write_table parameter must be logical (TRUE/FALSE)")}

  if( !dir.exists(as.character(write_dir)))      {stop("directory for saved files does not exist")}
  if( !is.numeric(write_digits))  { stop("csv.ndddigits parameter needs to be numeric")}
  write_digits <- round(write_digits[1])  # number of decimal digits for rounding in csv files
  
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    if( is.na(station_name) ) {station_name <- HYDAT}
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
  flowdata <- fasstr::fasstr_add_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_rolling_means(flowdata,days = rolling_days,align = rolling_align)
  colnames(flowdata)[ncol(flowdata)] <- "RollingValue"
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  
  # FILTER DATA FOR SELECTED YEARS FOR REMAINDER OF CALCS
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  
 
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
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q"))
    } else if (water_year_start==2) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q"))
    } else if (water_year_start==3) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q"))
    } else if (water_year_start==4) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q"))
    } else if (water_year_start==5) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q"))
    } else if (water_year_start==6) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q"))
    } else if (water_year_start==7) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q"))
    } else if (water_year_start==8) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q"))
    } else if (water_year_start==9) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q"))
    } else if (water_year_start==10) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Oct_missing_Q","Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q"))
    } else if (water_year_start==11) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Nov_missing_Q","Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q"))
    } else if (water_year_start==12) {
      Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Dec_missing_Q","Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q"))
    }
  } else {           
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels=c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q","Jun_missing_Q","Jul_missing_Q","Aug_missing_Q","Sep_missing_Q","Oct_missing_Q","Nov_missing_Q","Dec_missing_Q"))
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
    file_Q_summary_table <- file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-summary.csv", sep=""))
    temp <- Q_summary
    temp <- round(temp, write_digits)
    if(transpose){
      temp <- tidyr::spread(Q_summary_tpose_temp,Year,Value)
    }
    utils::write.csv(temp,file=file_Q_summary_table, row.names=FALSE)
  }
  
  return(Q_summary)
} # end of function

