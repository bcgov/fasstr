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

#' @title Calculate annual days above and below normal
#'
#' @description Calculates the number of days per year outside of the 'normal' range (typically between 25 and 75th percentiles) for
#'    each day of the year. Upper and lower-range percentiles are calcuated for each day of the year of from all years, and then each 
#'    daily flow value for each year is compared. All days above or below the normal range are included. Calculates the statistics 
#'    from all daily discharge values from all years, unless specified.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param lower_percentile=25 Numeric. Percentile indicating the lower limit of the normal range. Default \code{25}.
#' @param upper_percentile=25 Numeric. Percentile indicating the upper limit of the normal range. Default \code{75}.
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
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' 
#' @return A data frame of annual days above and below normal
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_annual_days_outside_normal(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'fasstr_annual_days_outside_normal(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------
fasstr_annual_days_outside_normal <- function(flowdata=NULL,
                                              HYDAT=NULL,
                                              lower_percentile=25,
                                              upper_percentile=75,
                                              water_year=FALSE,
                                              water_year_start=10,
                                              start_year=NULL,
                                              end_year=NULL,
                                              exclude_years=NULL,
                                              transpose=FALSE,
                                              station_name=NA,
                                              write_table=FALSE,
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
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("list of exclude_years must be numeric - ex. 1999 or c(1999,2000)")}
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.numeric(lower_percentile))                      {stop("lower_percentile must be numeric")}
  if( !all(lower_percentile>0 & lower_percentile<100))    {stop("lower_percentile must be >0 and <100")}
  if( !is.numeric(upper_percentile))                      {stop("upper_percentile must be numeric")}
  if( !all(upper_percentile>0 & upper_percentile<100))    {stop("upper_percentile must be >0 and <100")}
  if( lower_percentile >= upper_percentile )              {stop("lower_percentile must be < upper_percentile")}
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.logical(transpose))    {stop("transpose parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_table))  {stop("write_table parameter must be logical (TRUE/FALSE)")}

  if( !dir.exists(as.character(write_dir))) {
    message("directory for saved files does not exist, new directory will be created")
    if( write_table & write_dir!="." ) {dir.create(write_dir)}
  }

  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("only one HYDAT station can be selected")}
    if( !HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist")}
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
  
  # Filter the data for the start and end years
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::mutate(flowdata, Value=replace(Value, AnalysisYear %in% exclude_years, NA))
  
  # Determine years with complete data and filter for only those years
  flow_summary <- fasstr::fasstr_data_screening(flowdata=flowdata,
                                                HYDAT=NULL,
                                                water_year=water_year,
                                                start_year=start_year,
                                                end_year=end_year,
                                                water_year_start=water_year_start)
  complete_years <- flow_summary$Year[which(flow_summary$n_days==flow_summary$n_Q)]
  flowdata <- dplyr::mutate(flowdata, Value=replace(Value, !(AnalysisYear %in% complete_years), NA))
  
  
  #--------------------------------------------------------------
  # Complete analysis
  
  #Compute the normal limits for each day of the year and add each to the flowdata
  daily_normals <- dplyr::summarise(dplyr::group_by(flowdata,AnalysisDoY),
                                    LOWER=stats::quantile(Value, prob=lower_percentile/100, na.rm=TRUE),
                                    UPPER=stats::quantile(Value, prob=upper_percentile/100, na.rm=TRUE))
  flowdata_temp <- merge(flowdata, daily_normals, by="AnalysisDoY")
  
  #Compute the number of days above and below normal for each year
  Qstat <- dplyr::summarise(dplyr::group_by(flowdata_temp,AnalysisYear),
                            Days_Below_Normal=sum(Value < LOWER, na.rm=F),
                            Days_Above_Normal=sum(Value > UPPER, na.rm=F),
                            Days_Outside_Normal = Days_Below_Normal + Days_Above_Normal)
  Qstat <- dplyr::rename(Qstat,Year=AnalysisYear)
  
  
  #Remove any excluded
  Qstat[Qstat$Year %in% exclude_years,-1] <- NA
  
  # Transpose data if selected
  if(transpose){
    Qstat <- tidyr::gather(Qstat,Statistic,Value,-Year)
    Qstat <- tidyr::spread(Qstat,Year,Value)
  }
  
  # Write the table if selected
  if(write_table){
    file_Qstat_table <- file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-days-outside-normal.csv", sep=""))
    temp <- Qstat
    utils::write.csv(temp,file=file_Qstat_table, row.names=FALSE)
  }
  

  return(Qstat)
  
  
}

