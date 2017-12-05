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


#' @title Compute the number of days for each year that flows are above/below normal.
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
#' @param water_year_start Numeric. Month to start water year (1 to 12 for Jan to Dec).
#' @param start_year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start_year} is the first
#'    year of the data provided.
#' @param end_year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end_year} is the last
#'    year of the data provided.
#' @param exclude_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param transpose Logical. Switch the rows and columns of the results. Dates excluded.
#' @param normal_lower_ptile=25 Numeric. Percentile indicating the lower limit of the normal range. Default 25.
#' @param normal_upper_ptile=25 Numeric. Percentile indicating the upper limit of the normal range. Default 75.
#' @param write_table Logical.
#'    The file name will be  \code{file.path(write_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param write_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param na.rm TBD
#'
#'
#' @examples
#' \dontrun{
#' 
#' coming soon :)
#' 
#' 
#' }
#' @export

#'
#--------------------------------------------------------------


fasstr_annual_days_outside_normal <- function(flowdata=NULL,
                                      HYDAT=NULL,
                                      station_name=NA,
                                      water_year=FALSE,
                                      water_year_start=10,
                                      start_year=NULL,
                                      end_year=NULL,
                                      exclude_years=NULL,
                                      normal_lower_ptile=25,
                                      normal_upper_ptile=75,
                                      transpose=FALSE,
                                      write_table=FALSE,
                                      write_dir="."){
  
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
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {warning('flowdata cannot have negative values - check your data')}
  
  if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}
  if( length(water_year_start)>1) {stop("water_year_start must be a number between 1 and 12 (Jan-Dec)")}
  if( water_year_start <1 | water_year_start >12 ) {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  if( !(water_year_start==floor(water_year_start)))  {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.numeric(normal_lower_ptile))   {
    stop("normal_lower_ptile must be numeric")}
  if( !all(normal_lower_ptile>0 & normal_lower_ptile<100))  {
    stop("normal_lower_ptile must be >0 and <100")}
  if( !is.numeric(normal_upper_ptile))   {
    stop("normal_upper_ptile must be numeric")}
  if( !all(normal_upper_ptile>0 & normal_upper_ptile<100))  {
    stop("normal_upper_ptile must be >0 and <100")}
  if( normal_lower_ptile >= normal_upper_ptile ) {
    stop("normal_lower_ptile must be < normal_upper_ptile")}
  
  if( !is.logical(transpose))  {stop("transpose parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_table))  {stop("write_table parameter must be logical (TRUE/FALSE)")}
  
  if( !dir.exists(as.character(write_dir)))      {stop("directory for saved files does not exist")}

  
  ## check about number of years of data
  ## check for full years
  
  
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

  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  
  # FILTER FLOWDATA FOR SELECTED YEARS FOR REMAINDER OF CALCS
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
  
  
  
  #Compute the normal limits for each day of the year and add each to the flowdata
  daily_normals <- dplyr::summarise(dplyr::group_by(flowdata,AnalysisDoY),
                                    LOWER=stats::quantile(Value, prob=normal_lower_ptile/100, na.rm=TRUE),
                                    UPPER=stats::quantile(Value, prob=normal_upper_ptile/100, na.rm=TRUE))
  flowdata_temp <- merge(flowdata, daily_normals, by="AnalysisDoY")
  
  #Compute the number of days above and below normal for each year
  Qstat <- dplyr::summarise(dplyr::group_by(flowdata_temp,AnalysisYear),
                                         Days_Below_Normal=sum(Value < LOWER, na.rm=F),
                                         Days_Above_Normal=sum(Value > UPPER, na.rm=F),
                                         Days_Outside_Normal = Days_Below_Normal + Days_Above_Normal)
  Qstat <- dplyr::rename(Qstat,Year=AnalysisYear)
  
  
  #Remove any excluded
  Qstat[Qstat$Year %in% exclude_years,-1] <- NA
  
  
  if(transpose){
    Qstat <- tidyr::gather(Qstat,Statistic,Value,-Year)
    Qstat <- tidyr::spread(Qstat,Year,Value)
  }
  
  if(write_table){
    file_Qstat_table <- file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-annual-days-outside-normal.csv", sep=""))
    temp <- Qstat
    utils::write.csv(temp,file=file_Qstat_table, row.names=FALSE)
  }
  
  
  
  
  return(Qstat)
}

