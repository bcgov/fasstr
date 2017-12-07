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



#' @title Calculate all fasstr annual statistics
#'
#' @description Calculates all annual statistics of daily flow values from a streamflow dataset from all annual fasstr functions. 
#'    Calculates the statistics from all daily discharge values from all years, unless specified. Data is ideally long-term and continuous 
#'    with minimal missing/seasonal data as annual statistics are calculated. Data calculated using the folling functions:
#' \itemize{
#'  \item{fasstr_annual_stats()}
#'  \item{fasstr_annual_lowflows()}
#'  \item{fasstr_annual_total_flows()}
#'  \item{fasstr_annual_flow_timing()}
#'  \item{fasstr_monthly_stats()}
#'  \item{fasstr_annual_days_outside_normal()}
#'  }
#'    
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param basin_area Numeric. Upstream drainage basin area of the hydrometric station, in sq. km. Leave blank if \code{HYDAT} is used or 
#'    a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the basin area will be extracted from HYDAT. 
#'    Setting the basin area will replace the HYDAT basin area. 
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.
#' @param annual_percentiles Numeric. Vector of percentiles to calculate annually. Set to NA if none required. Used for fasstr_annual_stats()
#'    function. Default \code{c(10,90)}.
#' @param monthly_percentiles Numeric. Vector of percentiles to calculate monthly for each year. Set to NA if none required. Used for 
#'    fasstr_monthly_stats() function. Default \code{c(10,90)}.
#' @param lowflow_days  Numeric. The number of days to apply a rolling mean. Used for fasstr_annual_lowflows() function. Default \code{1}.
#' @param lowflow_align Character. Specifies whether the dates of the rolling mean should be specified by the first ('left'), last ('right),
#'    or middle ('center') of the rolling n-day group of observations. Used for fasstr_annual_lowflows() function. Default \code{'right'}.
#' @param totalflow_seasons Logical. Include seasonal yields and total discharges.Used for fasstr_annual_total_flows() function. 
#'    Default \code{TRUE}.
#' @param timing_percent Numeric. Percents of annual total flows to determine dates. Used for fasstr_annual_flow_timing() function. 
#'    Default \code{c(25,33.3,50,75)}.
#' @param normal_percentiles Numeric. Lower and upper percentiles, respectively indicating the limits of the normal range. 
#'    Used for fasstr_annual_days_outside_normal() function. Default \code{c(25,75)}.
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
#' @return A data frame with column "Year" and then 107 (default) variables from the fasstr annual functions.
#'    See listed functions above for default variables. Transposing data creates a column of "Statistics" and subsequent
#'    columns for each year selected.
#'
#' @examples
#' \dontrun{
#' 
#'fasstr_annual_all_stats(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'fasstr_annual_all_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------

fasstr_annual_all_stats <- function(flowdata=NULL,
                                    HYDAT=NULL,
                                    basin_area=NA, 
                                    water_year=FALSE,
                                    water_year_start=10,
                                    start_year=NULL,
                                    end_year=NULL,
                                    exclude_years=NULL,
                                    annual_percentiles=c(10,90),
                                    monthly_percentiles=c(10,20),
                                    lowflow_days=c(1,3,7,30),
                                    lowflow_align="right",
                                    totalflow_seasons=TRUE,
                                    timing_percent=c(25,33,50,75),
                                    normal_percentiles=c(25,75),
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
  
  if( !is.na(basin_area) & !is.numeric(basin_area)) {stop("basin_area argument must be numeric")}
  if( length(basin_area)>1)                         {stop("basin_area argument cannot have length > 1")}
  
  if( !is.numeric(lowflow_days))                       {stop("lowflow_days argument must be numeric")}
  if( !all(lowflow_days %in% c(1:180)) )               {stop("lowflow_days argument must be integers > 0 and <= 180)")}
  if( !lowflow_align %in% c("right","left","center"))  {stop("lowflow_align argument must be 'right', 'left', or 'center'")}
  
  if( !is.logical(totalflow_seasons))  {stop("totalflow_seasons argument must be logical (TRUE/FALSE)")}
  
  if( !is.numeric(timing_percent) )                 {stop("timing_percent must be numeric")}
  if( !all(timing_percent>0 & timing_percent<100))  {stop("timing_percent must be >0 and <100)")}
  
  if( !all(is.na(annual_percentiles)) & !is.numeric(annual_percentiles) )                 {stop("annual_percentiles argument must be numeric")}
  if( !all(is.na(annual_percentiles)) & (!all(annual_percentiles>0 & annual_percentiles<100)) )  {stop("annual_percentiles must be >0 and <100)")}
  if( !all(is.na(monthly_percentiles)) & !is.numeric(monthly_percentiles) )                 {stop("monthly_percentiles argument must be numeric")}
  if( !all(is.na(monthly_percentiles)) & (!all(monthly_percentiles>0 & monthly_percentiles<100)) )  {stop("monthly_percentiles must be >0 and <100)")}
  
  if( !is.numeric(normal_percentiles) )                {stop("normal_percentiles must be numeric")}
  if( length(normal_percentiles)!=2 )                  {stop("normal_percentiles must be two percentile values (ex. c(25,75))")}
  if( normal_percentiles[1] >= normal_percentiles[2] ) {stop("normal_percentiles[1] must be < normal_percentiles[2]")}
  if( !all(is.na(normal_percentiles)) & (!all(normal_percentiles>0 & normal_percentiles<100)) )  {stop("normal_percentiles must be >0 and <100)")}

  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.logical(transpose))    {stop("transpose parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_table))  {stop("write_table parameter must be logical (TRUE/FALSE)")}
  if( !is.numeric(write_digits))  {stop("write_digits parameter needs to be numeric")}
  write_digits <- round(write_digits[1])
  
  if( !dir.exists(as.character(write_dir))) {
    message("directory for saved files does not exist, new directory will be created")
    if( write_table & write_dir!="." ) {dir.create(write_dir)}
  }

  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("only one HYDAT station can be selected")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
    if( is.na(station_name) ) {station_name <- HYDAT}
    flowdata <- suppressMessages(tidyhydat::hy_daily_flows(station_number =  HYDAT))
    if (is.na(basin_area)) {basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = HYDAT)$DRAINAGE_AREA_GROSS)}
  }
  
  # Looks for STATION_NUMBER column to search for basin_area
  if ( is.na(basin_area) & "STATION_NUMBER" %in% names(flowdata)){
    basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = flowdata$STATION_NUMBER[1])$DRAINAGE_AREA_GROSS)
  }
  
  # Check if no basin_area if use-yield is TRUE
  if( is.na(basin_area) )  {
    warning("no basin_area provided, 'Yield' values will be NA")}
  
  #--------------------------------------------------------------
  # Set the flowdata for analysis
  
  # Select just Date and Value for analysis
  flowdata <- dplyr::select(flowdata,Date,Value)
  
  # add date variables to determine the min/max cal/water years
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  if (is.null(start_year)) {start_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))}
  if (is.null(end_year)) {end_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}

  
  #--------------------------------------------------------------
  # Complete analysis
  
  Qannual_stats <- fasstr::fasstr_annual_stats(flowdata=flowdata,
                                               HYDAT=NULL,
                                               percentiles=annual_percentiles,
                                               water_year=water_year,
                                               water_year_start=water_year_start,
                                               start_year=start_year,
                                               end_year=end_year,
                                               exclude_years=exclude_years,
                                               na.rm=na.rm)
  

  Qannual_lowflows <- fasstr::fasstr_annual_lowflows(flowdata=flowdata,
                                                     HYDAT=NULL,
                                                     rolling_days=lowflow_days,
                                                     rolling_align=lowflow_align,
                                                     water_year=water_year,
                                                     water_year_start=water_year_start,
                                                     start_year=start_year,
                                                     end_year=end_year,
                                                     exclude_years=exclude_years,
                                                     na.rm=na.rm)
  Qannual_lowflows <- dplyr::select(Qannual_lowflows,-dplyr::contains("Date"))
  
  
  Qannual_totalflows <- fasstr::fasstr_annual_total_flows(flowdata=flowdata,
                                                          HYDAT=NULL,
                                                          basin_area=basin_area,
                                                          water_year=water_year,
                                                          water_year_start=water_year_start,
                                                          start_year=start_year,
                                                          end_year=end_year,
                                                          exclude_years=exclude_years,
                                                          incl_seasons=totalflow_seasons,
                                                          na.rm=na.rm)
  
  
  Qannual_flowdates <- fasstr::fasstr_annual_flow_timing(flowdata=flowdata,
                                                         HYDAT=NULL,
                                                         percent_total=timing_percent,
                                                         water_year=water_year,
                                                         water_year_start=water_year_start,
                                                         start_year=start_year,
                                                         end_year=end_year,
                                                         exclude_years=exclude_years)
  Qannual_flowdates <- dplyr::select(Qannual_flowdates,Year,dplyr::contains("DoY"))
  
  Qannual_months <- fasstr::fasstr_monthly_stats(flowdata=flowdata,
                                                 HYDAT=NULL,
                                                 percentiles=monthly_percentiles,
                                                 water_year=water_year,
                                                 water_year_start=water_year_start,
                                                 start_year=start_year,
                                                 end_year=end_year,
                                                 exclude_years=exclude_years,
                                                 spread = TRUE,
                                                 na.rm=na.rm)
  
  Qannual_normals <- fasstr::fasstr_annual_days_outside_normal(flowdata=flowdata,
                                                               HYDAT=NULL,
                                                               normal_percentiles=normal_percentiles,
                                                               water_year=water_year,
                                                               water_year_start=water_year_start,
                                                               start_year=start_year,
                                                               end_year=end_year,
                                                               exclude_years=exclude_years)
  
  
  # Combine all and label columns
  Qstat <- merge(Qannual_stats,Qannual_lowflows,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qannual_totalflows,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qannual_flowdates,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qannual_normals,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qannual_months,by="Year",all = TRUE)
  Qstat <- tidyr::gather(Qstat,Stat,Value,2:ncol(Qstat))
  Qstat <- dplyr::mutate(Qstat,Stat=paste0(ifelse(water_year,paste("WY_"),paste("CY_")),Stat))
  col.order <- c("Year",unique(Qstat$Stat)) # to keep the same order as merged when spread below
  Qstat <- tidyr::spread(Qstat,Stat,Value)
  Qstat <- Qstat[,col.order]
  
  # Remove excluded years
  Qstat <- dplyr::filter(Qstat, Year >= start_year & Year <= end_year)
  Qstat[Qstat$Year %in% exclude_years,-1] <- NA
  
  
  # Transpose data if selected
  if(transpose){
    options(scipen = 999)
    Qstat_tpose <- tidyr::gather(Qstat,Statistic,Value,-Year)
    Qstat_tpose_temp <- dplyr::mutate(Qstat_tpose,Value=round(Value,write_digits)) # for writing to csv
    Qstat <- tidyr::spread(Qstat_tpose,Year,Value)
  }
  
  
  # Write the table if selected
  if(write_table){
    file_Qstat_table <- file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                                                   "-all-annual-statistics.csv", sep=""))
    temp <- Qstat
    temp <- round(temp, write_digits)
    if(transpose){
      temp <- tidyr::spread(Qstat_tpose_temp,Year,Value)
    }
    utils::write.csv(temp,file=file_Qstat_table, row.names=FALSE)
  }
  
  return(Qstat)
  
  
} 

