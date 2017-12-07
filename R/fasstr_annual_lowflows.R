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


#' @title Calculate annual lowflows
#'
#' @description Calculates annual n-day minimum values, and the day of year and date of occurrence of daily flow values from a 
#'    streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
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
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param months Integer. Vector of months to consider for analysis (ex. \code{6:8} for Jun-Aug). Leave blank if all months
#'    are required. Default \code{1:12}.
#' @param transpose Logical. Switch the rows and columns of the results table. Dates not transposed. Default \code{FALSE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_table Logical. Write the table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_digits Numeric. Number of significant digits to round the results in the written table. Default \code{3}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na.rm TBD
#'    
#' @return A data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{Min_'n'_Day}{annual minimum for each n-day rolling mean, direction of mean specified by rolling_align}
#'   \item{Min_'n'_Day_DoY}{day of year for each annual minimum of n-day rolling mean}
#'   \item{Min_'n'_Day_Date}{date (YYYY-MM-DD) for each annual minimum of n-day rolling mean}
#'   Default columns:
#'   \item{Min_1_Day}{annual 1-day mean minimum (rolling_align=right)}
#'   \item{Min_1_Day_DoY}{day of year of annual 1-day mean minimum}
#'   \item{Min_1_Day_Date}{date (YYYY-MM-DD) of annual 1-day mean minimum}
#'   \item{Min_3_Day}{annual 3-day mean minimum (rolling_align=right)}
#'   \item{Min_3_Day_DoY}{day of year of annual 3-day mean minimum}
#'   \item{Min_3_Day_Date}{date (YYYY-MM-DD) of annual 3-day mean minimum}   
#'   \item{Min_7_Day}{annual 7-day mean minimum (rolling_align=right)}
#'   \item{Min_7_Day_DoY}{day of year of annual 7-day mean minimum}
#'   \item{Min_7_Day_Date}{date (YYYY-MM-DD) of annual 7-day mean minimum}
#'   \item{Min_30_Day}{annual 30-day mean minimum (rolling_align=right)}
#'   \item{Min_30_Day_DoY}{day of year of annual 30-day mean minimum}
#'   \item{Min_30_Day_Date}{date (YYYY-MM-DD) of annual 30-day mean minimum}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected. "Date" statistics
#'   not transposed.
#'   
#' @examples
#' \dontrun{
#' 
#'fasstr_annual_lowflows(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'fasstr_annual_lowflows(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, rolling_days = c(3,7))
#'
#' }
#' @export

#--------------------------------------------------------------

fasstr_annual_lowflows <- function(flowdata=NULL,
                                   HYDAT=NULL,
                                   rolling_days=c(1,3,7,30),
                                   rolling_align="right",
                                   water_year=FALSE,
                                   water_year_start=10,
                                   start_year=NULL,
                                   end_year=NULL,
                                   exclude_years=NULL, 
                                   months=1:12,
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
  
  if( !is.numeric(months) )        {stop("months argument must be integers")}
  if( !all(months %in% c(1:12)) )  {stop("months argument must be integers between 1 and 12 (Jan-Dec)")}
  
  if( !is.numeric(rolling_days))                       {stop("rolling_days argument must be numeric")}
  if( !all(rolling_days %in% c(1:180)) )               {stop("rolling_days argument must be integers > 0 and <= 180)")}
  if( !rolling_align %in% c("right","left","center"))  {stop("rolling_align argument must be 'right', 'left', or 'center'")}
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
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
    if( length(HYDAT)>1 ) {stop("only one HYDAT station can be selected")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
  }
  
  #--------------------------------------------------------------
  # Set the flowdata for analysis
  
  # Select just Date and Value for analysis
  flowdata <- dplyr::select(flowdata,Date,Value)
  
  # add date variables to determine the min/max cal/water years
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  if (is.null(start_year)) {start_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))}
  if (is.null(end_year)) {end_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  #  Fill in the missing dates and the add the date variables again
  flowdata <- fasstr::fasstr_add_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  

  
  #--------------------------------------------------------------
  # Complete analysis
  
  # Loop through each rolling_day and compute annual min values and their dates
  Q_lowflow <- dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear))
  for (day in rolling_days) {
    flowdata_temp <- fasstr::fasstr_add_rolling_means(flowdata,days = day,align = rolling_align)
    names(flowdata_temp)[names(flowdata_temp) == paste0("Q",day,"Day")] <- "RollingValue"
    flowdata_temp <- dplyr::filter(flowdata_temp, Month %in% months)
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
  
  # Filter for start and end years and make excluded years data NA
  Q_lowflow <- dplyr::filter(Q_lowflow, Year >= start_year & Year <= end_year)
  Q_lowflow[Q_lowflow$Year %in% exclude_years,-1] <- NA
  
  # Transpose data if selected (exludes date values)
  if(transpose){
    Q_lowflow <- dplyr::select(Q_lowflow,Year,dplyr::contains("Day"),-dplyr::contains("Date"))
    Q_lowflow <- tidyr::gather(Q_lowflow,Statistic,Value,-Year)
    Q_lowflow <- tidyr::spread(Q_lowflow,Year,Value)
  }
  
  # Write the table if selected
  if(write_table){
    file_Qlowflows_table <- file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                                                       "-annual-lowflows.csv", sep=""))
    temp <- Q_lowflow
    # rounding just numeric columns
    if (transpose) {
      temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], write_digits)
    } else {
      numVars <- sapply(temp, is.numeric) 
      temp[numVars] <- lapply(temp[numVars], round, digits = write_digits) 
    }
    utils::write.csv(temp,file=file_Qlowflows_table, row.names=FALSE)
  }
  
  return(Q_lowflow)
  
  
}

