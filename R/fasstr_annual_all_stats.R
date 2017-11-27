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


#' @title Compute all annual statistics.
#'
#' @description Computes annual statistics of streamflow data. Take all functions with an annual value attached to it.
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
#' @param exclude_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param basin_area Numeric. The upstream drainage basin area (in sq. km) of the station. Used to calculate runoff yields (mm).
#'    If no value provided, yield calculations will result in NA values.
#' @param transpose Logical. Switch the rows and columns of the results.
#' @param write_table Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param table_nddigits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#' @param na.rm TBD
#'
#'
#' @examples
#' \dontrun{
#' stat.annual <- annual_stats(HYDAT="08HB048",
#'                             basin_area    = 10.1)
#' 
#' stat.annual <- annual_stats(station_name  ='Mission Creek',
#'                                flowdata     = flow,
#'                                water_year    = TRUE,
#'                                start_year    = 1967,
#'                                end_year      = 2014)
#' }
#' @export

#'
#--------------------------------------------------------------
# Compute the statistics on an (calendar and water) year basis

fasstr_annual_all_stats <- function(flowdata=NULL,
                                    HYDAT=NULL,
                                    station_name="fasstr",
                                    water_year=FALSE,
                                    water_year_start=10,
                                    start_year=NULL,
                                    end_year=NULL,
                                    exclude_years=NULL, 
                                    basin_area=NA, 
                                    lowflow_days=c(1,3,7,30),
                                    totalflow_seasons=TRUE,
                                    percentflow_days=c(25,33,50,75),
                                    transpose=FALSE,
                                    write_table=FALSE,
                                    report_dir=".",
                                    na.rm=list(na.rm.global=FALSE),
                                    table_nddigits=3){
  
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
  if( is.null(HYDAT) & any(flowdata$Value <0, na.rm=TRUE))   {stop('flowdata cannot have negative values - check your data')}
  
  if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}
  if( length(water_year_start)>1) {stop("water_year_start must be a number between 1 and 12 (Jan-Dec)")}
  if( water_year_start <1 | water_year_start >12 ) {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  if( !(water_year_start==floor(water_year_start)))  {stop("water_year_start must be an integer between 1 and 12 (Jan-Dec)")}
  
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.na(basin_area) & !is.numeric(basin_area))    {stop("basin_area parameter must be numeric")}
  if( length(basin_area)>1)        {stop("basin_area parameter cannot have length > 1")}
  
  if( !is.logical(totalflow_seasons))  {stop("totalflow_seasons must be logical (TRUE/FALSE)")}
  
  if( !is.numeric(percentflow_days))   {
    stop("percentflow_days must be numeric")}
  if( !all(percentflow_days>0 & percentflow_days<100))  {
    stop("percentflow_days must be >0 and <100")}
  
  
  if( !is.numeric(lowflow_days))   {
    stop("lowflow_days must be numeric")}
  if( !all(lowflow_days>0 & lowflow_days<=180))  {
    stop("lowflow_days must be >0 and <=180")}
  if( !all(lowflow_days==floor(lowflow_days)))  {
    stop("lowflow_days must be integers")}
  
  if( !is.logical(transpose))  {stop("transpose parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_table))  {stop("write_table parameter must be logical (TRUE/FALSE)")}
  
  if( !dir.exists(as.character(report_dir)))      {stop("directory for saved files does not exist")}
  if( !is.numeric(table_nddigits))  { stop("csv.ndddigits parameter needs to be numeric")}
  table_nddigits <- round(table_nddigits[1])  # number of decimal digits for rounding in csv files
  
  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  
  # If HYDAT station is listed, check if it exists and make it the flowdata
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    if (station_name=="fasstr") {station_name <- HYDAT}
    if (is.na(basin_area)) {basin_area <- suppressMessages(tidyhydat::hy_stations(station_number = HYDAT)$DRAINAGE_AREA_GROSS)}
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
  flowdata <- fasstr::fasstr_fill_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::fasstr_add_date_vars(flowdata,water_year = T,water_year_start = water_year_start)
  
  #  Compute the 3, 7, and 30 day rolling average values
  #flowdata <- fasstr::fasstr_add_rolling_means(flowdata,days = c(1,3,7,30))
  #flowdata <- fasstr::fasstr_add_total_volume(flowdata,water_year = water_year,water_year_start = water_year_start)
  
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
  
  
  Qannual_stats <- fasstr::fasstr_annual_stats(flowdata=flowdata,
                                               HYDAT=NULL,
                                               station_name=station_name,
                                               water_year=water_year,
                                               water_year_start=water_year_start,
                                               start_year=start_year,
                                               end_year=end_year,
                                               exclude_years=exclude_years, 
                                               percentiles=NA,
                                               transpose=FALSE,
                                               write_table=FALSE,
                                               report_dir=report_dir,
                                               na.rm=na.rm,
                                               table_nddigits=3)
  Qannual_stats <- dplyr::rename(Qannual_stats,
                                 MIN_DAILY=Minimum,
                                 MAX_DAILY=Maximum,
                                 MEAN_DAILY=Mean,
                                 MEDIAN_DAILY=Median)
  
  
  
  Qannual_lowflows <- fasstr::fasstr_annual_lowflows(flowdata=flowdata,
                                                     HYDAT=NULL,
                                                     station_name= station_name,
                                                     water_year=water_year,
                                                     water_year_start=water_year_start,
                                                     start_year=start_year,
                                                     end_year=end_year,
                                                     exclude_years=exclude_years,
                                                     rolling_days=lowflow_days,
                                                     write_table=F,
                                                     report_dir=report_dir,
                                                     na.rm=na.rm,
                                                     table_nddigits=3,
                                                     transpose = F)
  Qannual_lowflows <- dplyr::select(Qannual_lowflows,-dplyr::contains("Date"))
  
  
  
  
  Qannual_totalflows <- fasstr::fasstr_annual_total_flows(flowdata=flowdata,
                                                          HYDAT=NULL,
                                                          station_name=station_name,
                                                          water_year=water_year,
                                                          water_year_start=water_year_start,
                                                          start_year=start_year,
                                                          end_year=end_year,
                                                          exclude_years=exclude_years,
                                                          basin_area=basin_area,
                                                          seasons=totalflow_seasons,
                                                          transpose=FALSE,
                                                          write_table=FALSE,
                                                          report_dir=report_dir,
                                                          na.rm=list(na.rm.global=FALSE),
                                                          table_nddigits=3)
  
  
  Qannual_flowdates <- fasstr::fasstr_annual_flow_dates(flowdata=flowdata,
                                                     HYDAT=NULL,
                                                     station_name=station_name,
                                                     water_year=water_year,
                                                     water_year_start=water_year_start,
                                                     start_year=start_year,
                                                     end_year=end_year,
                                                     exclude_years=exclude_years,
                                                     percent_of_total=percentflow_days,
                                                     transpose=FALSE,
                                                     write_table=FALSE,
                                                     report_dir=report_dir,
                                                     na.rm=list(na.rm.global=FALSE))
  Qannual_flowdates <- dplyr::select(Qannual_flowdates,Year,dplyr::contains("DoY"))
  
  Qannual_months <- fasstr::fasstr_monthly_stats(flowdata=flowdata,
                                              HYDAT=NULL,
                                              station_name=station_name,
                                              water_year=water_year,
                                              water_year_start=water_year_start,
                                              start_year=start_year,
                                              end_year=end_year,
                                              exclude_years=exclude_years, 
                                              percentiles=c(10,20),
                                              spread=TRUE,
                                              transpose=FALSE,
                                              write_table=FALSE,
                                              report_dir=report_dir,
                                              na.rm=na.rm,
                                              table_nddigits=3)
  
  Qannual_normals <- fasstr::fasstr_annual_days_outside_normal(flowdata=flowdata,
                                                               HYDAT=NULL,
                                                               station_name=station_name,
                                                               water_year=water_year,
                                                               water_year_start=water_year_start,
                                                               start_year=start_year,
                                                               end_year=end_year,
                                                               exclude_years=exclude_years, 
                                                               normal_lower_ptile=25,
                                                               normal_upper_ptile=75,
                                                               transpose=FALSE,
                                                               write_table=FALSE,
                                                               report_dir=report_dir,
                                                               na.rm=na.rm)

  
  # Combine all and label columns
  Qstat <- merge(Qannual_stats,Qannual_lowflows,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qannual_totalflows,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qannual_flowdates,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qannual_months,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qannual_normals,by="Year",all = TRUE)
  Qstat <- tidyr::gather(Qstat,Stat,Value,2:ncol(Qstat))
  Qstat <- dplyr::mutate(Qstat,Stat=paste0(ifelse(water_year,paste("WY_"),paste("CY_")),Stat))
  col.order <- c("Year",unique(Qstat$Stat)) # to keep the same order as merged when spread below
  Qstat <- tidyr::spread(Qstat,Stat,Value)
  Qstat <- Qstat[,col.order]
  
  # Remove excluded years
  Qstat[Qstat$Year %in% exclude_years,-1] <- NA
  
  
  
  if(transpose){
    options(scipen = 999)
    Qstat_tpose <- tidyr::gather(Qstat,Statistic,Value,-Year)
    Qstat_tpose_temp <- dplyr::mutate(Qstat_tpose,Value=round(Value,table_nddigits)) # for writing to csv
    Qstat <- tidyr::spread(Qstat_tpose,Year,Value)
  }
  
  
  # See if you want to write out the summary tables?
  if(write_table){
    # Write out the summary table for comparison to excel spreadsheet for calendar year
    file_Qstat_table <- file.path(report_dir, paste(station_name,"-all_annual-statistics.csv", sep=""))
    temp <- Qstat
    temp <- round(temp, table_nddigits)
    if(transpose){
      temp <- tidyr::spread(Qstat_tpose_temp,Year,Value)
    }
    utils::write.csv(temp,file=file_Qstat_table, row.names=FALSE)
  }
  
  
  
  
  return(Qstat)
} # end of function

