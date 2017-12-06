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

#' @title Calculate the long-term and long-term monthly summary statistics
#'
#' @description Calculates the long-term and long-term monthly mean, median, maximum, minimum, and percentiles of daily flow values 
#'    from a streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param percentiles Numeric. Vector of percentiles to calculate. Set to NA if none required. Default \code{c(10,90)}
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param custom_months Integer. Vector of months to combine to summarize (ex. \code{6:8} for Jun-Aug). Adds results to the end of table.
#'    Leave blank for no custom month summary.
#' @param custom_months_label Character. Label of custom months. For example, if choosing months 7:9  you may choose "Summer" or "Jul-Sep".
#'    Default \code{"Custom-Months"}.
#' @param transpose Logical. Switch the rows and columns of the results table. Default \code{FALSE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_table Logical. Write the table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_digits Numeric. Number of significant digits to round the results in the written table. Default \code{3}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na.rm TBD
#' 
#' @return A data frame of long-term and monthly long-term summary statistics and percentiles
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_longterm_stats(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'fasstr_longterm_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, percentiles = c(1:10))
#'
#'fasstr_longterm_stats(HYDAT = "08NM116", custom_months = c(5:9))
#'
#' }
#' @export

#--------------------------------------------------------------


fasstr_longterm_stats <- function(flowdata=NULL,
                                  HYDAT=NULL,
                                  percentiles=c(10,90),
                                  water_year=FALSE,
                                  water_year_start=10,
                                  start_year=NULL,
                                  end_year=NULL,
                                  exclude_years=NULL,
                                  custom_months=NULL,
                                  custom_months_label="Custom-Months",
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
  
  if( !is.null(custom_months) & !is.numeric(custom_months) )  {stop("custom_months argument must be integers")}
  if( !all(custom_months %in% c(1:12)) )                      {stop("custom_months argument must be integers between 1 and 12 (Jan-Dec)")}
  if( !is.na(custom_months_label) & !is.character(custom_months_label) )  {stop("custom_months_label argument must be a character string.")}
  
  
  if( !is.numeric(percentiles))                 {stop("percentiles argument must be numeric")}
  if( !all(percentiles>0 & percentiles<100))    {stop("percentiles must be >0 and <100)")}
  
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
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = water_year,water_year_start = water_year_start)
  if (is.null(start_year)) {start_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))}
  if (is.null(end_year)) {end_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  #  Fill in the missing dates and the add the date variables again
  flowdata <- fasstr::fasstr_add_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = water_year,water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
  }
  
  # Filter for the selected year
  flowdata <- dplyr::filter(flowdata,AnalysisYear>=start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata,!(AnalysisYear %in% exclude_years))
  
  #--------------------------------------------------------------
  # Complete analysis
  
  # Calculate the monthly and longterm stats
  Q_month_longterm <-   dplyr::summarize(dplyr::group_by(flowdata,MonthName),
                                         Mean = mean(Value,na.rm=TRUE),
                                         Median = median(Value,na.rm=TRUE),
                                         Maximum = max(Value,na.rm=TRUE),
                                         Minimum = min(Value,na.rm=TRUE))
  Q_all_longterm <-   dplyr::summarize(flowdata,
                                       Mean = mean(Value,na.rm=TRUE),
                                       Median = median(Value,na.rm=TRUE),
                                       Maximum = max(Value,na.rm=TRUE),
                                       Minimum = min(Value,na.rm=TRUE))
  Q_all_longterm <- dplyr::mutate(Q_all_longterm,MonthName="Long-term")
  Q_longterm <- rbind(Q_month_longterm, Q_all_longterm)
  
  # Calculate the monthly and longterm percentiles
  if (!all(is.na(percentiles))){
    for (ptile in percentiles) {
      
      Q_month_longterm_ptile <- dplyr::summarise(dplyr::group_by(flowdata,MonthName),
                                                 Percentile=quantile(Value,ptile/100, na.rm=TRUE))
      Q_all_longterm_ptile <- dplyr::summarise(flowdata,
                                               Percentile=quantile(Value,ptile/100, na.rm=TRUE))
      Q_all_longterm_ptile <- dplyr::mutate(Q_all_longterm_ptile,MonthName="Long-term")
      
      colnames(Q_month_longterm_ptile)[2] <- paste0("P",ptile)
      colnames(Q_all_longterm_ptile)[1] <- paste0("P",ptile)
      
      Q_longterm_ptiles <- rbind(Q_month_longterm_ptile, Q_all_longterm_ptile)
      Q_longterm <- merge(Q_longterm,Q_longterm_ptiles,by=c("MonthName"))
    }
  }
  
  # Rename Month column and reorder to proper levels (set in add_date_vars)
  Q_longterm <- dplyr::rename(Q_longterm,Month=MonthName)
  Q_longterm <- with(Q_longterm, Q_longterm[order(Month),])
  row.names(Q_longterm) <- c(1:nrow(Q_longterm))
  
  # If custom_months are selected, append a row on the bottom
  if( is.numeric(custom_months) & all(custom_months %in% c(1:12)) ) {
    flowdata_temp <- dplyr::filter(flowdata,Month %in% custom_months)
    Q_month_custom <-   dplyr::summarize(dplyr::group_by(flowdata_temp),
                                         Mean = mean(Value,na.rm=TRUE),
                                         Median = median(Value,na.rm=TRUE),
                                         Maximum = max(Value,na.rm=TRUE),
                                         Minimum = min(Value,na.rm=TRUE))
    Q_month_custom <- dplyr::mutate(Q_month_custom,Month=paste0(custom_months_label))
    
    if (!all(is.na(percentiles))){
      for (ptile in percentiles) {
        Q_ptile_custom <- dplyr::summarise(flowdata_temp,Percentile=quantile(Value,ptile/100, na.rm=TRUE))
        Q_ptile_custom <- dplyr::mutate(Q_ptile_custom,Month=paste0(custom_months_label))
        
        colnames(Q_ptile_custom)[1] <- paste0("P",ptile)
        Q_month_custom <- merge(Q_month_custom,Q_ptile_custom,by=c("Month"))
      }
    }
    Q_longterm <- rbind(Q_longterm, Q_month_custom)
  }
  col_names <- names(Q_longterm[-1])

  
  # Switch columns and rows
  if (transpose) {
    Q_longterm <- tidyr::gather(Q_longterm,Statistic,Value,-Month)
    Q_longterm <- tidyr::spread(Q_longterm,Month,Value)
    Q_longterm <- Q_longterm[match(col_names, Q_longterm$Statistic),]
  }
  
  #  Write out summary tables for calendar years
  if (write_table) {
    file_stat_csv <-file.path(write_dir,paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr")),"-longterm-summary-stat.csv"))
    temp <- Q_longterm
    temp[,2:ncol(temp)] <- round(temp[,2:ncol(temp)], write_digits)
    utils::write.csv(temp, file=file_stat_csv, row.names=FALSE)
  }
  
  
  return(Q_longterm)
  
  
}