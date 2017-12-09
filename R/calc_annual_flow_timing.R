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

#' @title Calculate annual flow timing
#'
#' @description Calculates annual the timing (day of year) and date of occurrence of portions of total annual flow of daily flow 
#'    values from a streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param percent_total Numeric. Percents of annual total flows to determine dates. Default \code{c(25,33.3,50,75)}.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param transpose Logical. Switch the rows and columns of the results table. Dates not transposed. Default \code{FALSE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_table Logical. Write the table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' 
#' @return A data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{DoY_'n'pct_TotalQ}{day of year for each n-percent of total volumetric discharge}
#'   \item{Date_'n'pct_TotalQ}{date (YYYY-MM-DD) for each n-percent of total volumetric discharge}
#'   Default columns:
#'   \item{DoY_25pct_TotalQ}{day of year of 25-percent of total volumetric discharge}
#'   \item{Date_25pct_TotalQ}{date (YYYY-MM-DD) of 25-percent of total volumetric discharge}
#'   \item{DoY_33.3pct_TotalQ}{day of year of 33.3-percent of total volumetric discharge}
#'   \item{Date_33.3pct_TotalQ}{date (YYYY-MM-DD) of 33.3-percent of total volumetric discharge}
#'   \item{DoY_50pct_TotalQ}{day of year of 50-percent of total volumetric discharge}
#'   \item{Date_50pct_TotalQ}{date (YYYY-MM-DD) of 50-percent of total volumetric discharge}
#'   \item{DoY_75pct_TotalQ}{day of year of 75-percent of total volumetric discharge}
#'   \item{Date_75pct_TotalQ}{date (YYYY-MM-DD) of 75-percent of total volumetric discharge}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'
#' @examples
#' \dontrun{
#' 
#'calc_annual_flow_timing(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'calc_annual_flow_timing(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, percent_total = 50)
#'
#' }
#' @export

#--------------------------------------------------------------

calc_annual_flow_timing <- function(flowdata=NULL,
                                      HYDAT=NULL,
                                      percent_total=c(25,33.3,50,75),
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
  
  if( !is.numeric(percent_total) )                 {stop("percent_total must be numeric")}
  if( !all(percent_total>0 & percent_total<100))  {stop("percent_total must be >0 and <100)")}
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.logical(transpose))    {stop("transpose parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_table))  {stop("write_table parameter must be logical (TRUE/FALSE)")}
  
  if( !dir.exists(as.character(write_dir))) {
    message("directory for saved files does not exist, new directory will be created")
    if( write_table & write_dir!="." ) {dir.create(write_dir)}
  }
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
  flowdata <- fasstr::add_cumulative_volume(flowdata,water_year = water_year,water_year_start = water_year_start)
  
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
  
  #--------------------------------------------------------------
  # Complete analysis
  
  # Loop through percents
  Qstat <-   dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear))
  for (percent in percent_total) {
    Qstat_pcnt <-   dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear),
                               TOTALQ_DAY  =  AnalysisDoY[ match(TRUE, Cumul_Volume_m3 > percent/100 * 
                                                                   ((mean(Value, na.rm=TRUE))*length(Value)*60*60*24))],
                               TOTALQ_DATE =  Date[ match(TRUE, Cumul_Volume_m3 > percent/100 *
                                                            ((mean(Value, na.rm=TRUE))*length(Value)*60*60*24))])
    names(Qstat_pcnt)[names(Qstat_pcnt) == "TOTALQ_DAY"] <- paste0("DoY_",percent,"pct_TotalQ")
    names(Qstat_pcnt)[names(Qstat_pcnt) == "TOTALQ_DATE"] <- paste0("Date_",percent,"pct_TotalQ")
    Qstat <- merge(Qstat,Qstat_pcnt,by="AnalysisYear",all=T)
    
  }
  Qstat <-   dplyr::rename(Qstat,Year=AnalysisYear)
  
  #Remove any excluded years
  Qstat[Qstat$Year %in% exclude_years,-1] <- NA
  
  
  # Transpose data if selected
  if(transpose){
    Qstat <- dplyr::select(Qstat,Year,dplyr::contains("DoY"))
    col_names <- names(Qstat[-1])
    Qstat <- tidyr::gather(Qstat,Statistic,Value,-Year)
    Qstat <- tidyr::spread(Qstat,Year,Value)
    Qstat <- Qstat[match(col_names, Qstat$Statistic),]
    row.names(Qstat) <- c(1:nrow(Qstat))
  }
  
  # Write the table if selected
  if(write_table){
    file_Qstat_table <- file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                                                   "-annual-date-of-flows.csv", sep=""))
    temp <- Qstat
    utils::write.csv(temp,file=file_Qstat_table, row.names=FALSE)
  }
  
  return(Qstat)
  
}

