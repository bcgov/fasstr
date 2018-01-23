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

#' @title Calculate annual (and seasonal) cumulative flows
#' 
#' @description Calculates annual and seasonal total flows, volumetric or runoff yield flows, from a streamflow dataset. Calculates 
#'    the statistics from all daily discharge values from all years, unless specified. For water year and seasonal data, the designated
#'    year is the year in which the year or season ends. For example, if using water years with a start month of 11, the OND season is
#'    designated by the water year which starts in November (designated by the calendar year in which it ends).
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param basin_area Numeric. The upstream drainage basin area (in sq. km) of the station. Used to calculate runoff yields (mm)
#'    or middle ('center') of the rolling n-day group of observations. Default \code{'right'}.
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required. 
#' @param incl_seasons Logical. Include seasonal yields and total discharges. Default \code{TRUE}.
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
#' @return A data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{Annual_TotalQ_m3}{annual total volumetric discharge, in cubic metres}
#'   \item{Annual_Yield_mm}{annual runoff yield, in millimetres}
#'   Default seasonal columns:
#'   \item{AMJJAS_TotalQ_m3}{Apr-Sep total volumetric discharge, in cubic metres}
#'   \item{AMJJAS_Yield_mm}{Apr-Sep runoff yield, in millimetres}
#'   \item{ONDJFM_TotalQ_m3}{Oct-Mar total volumetric discharge, in cubic metres}
#'   \item{ONDJFM__Yield_mm}{Oct-Mar runoff yield, in millimetres}   
#'   \item{AMJ_TotalQ_m3}{Apr-Jun total volumetric discharge, in cubic metres}
#'   \item{AMJ__Yield_mm}{Apr-Jun runoff yield, in millimetres}  
#'   \item{JAS_TotalQ_m3}{Jul-Sep total volumetric discharge, in cubic metres}
#'   \item{JAS__Yield_mm}{Jul-Sep runoff yield, in millimetres}  
#'   \item{JFM_TotalQ_m3}{Jan-Mar total volumetric discharge, in cubic metres}
#'   \item{JFM__Yield_mm}{Jan-Mar runoff yield, in millimetres}  
#'   \item{OND_TotalQ_m3}{Oct-Dec total volumetric discharge, in cubic metres}
#'   \item{OND__Yield_mm}{Oct-Dec runoff yield, in millimetres}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected. 
#'   
#' @examples
#' \dontrun{
#' 
#'calc_annual_cumulative_stats(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'calc_annual_cumulative_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export

#--------------------------------------------------------------
calc_annual_cumulative_stats <- function(flowdata=NULL,
                                      HYDAT=NULL,
                                      basin_area=NA,
                                      water_year=FALSE,
                                      water_year_start=10,
                                      start_year=NULL,
                                      end_year=NULL,
                                      exclude_years=NULL,
                                      incl_seasons=TRUE,
                                      transpose=FALSE,
                                      station_name=NA,
                                      write_table=FALSE,
                                      write_digits=3,
                                      write_dir=".",
                                      na.rm=list(na.rm.global=FALSE)
                                      ){
  
  
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
  
  if( !is.logical(incl_seasons))  {stop("incl_seasons argument must be logical (TRUE/FALSE)")}
  
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
  flowdata <- fasstr::add_date_variables(flowdata,water_year = T,water_year_start = water_year_start)
  if (is.null(start_year)) {start_year <- ifelse(water_year,min(flowdata$WaterYear),min(flowdata$Year))}
  if (is.null(end_year)) {end_year <- ifelse(water_year,max(flowdata$WaterYear),max(flowdata$Year))}
  if (!(start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  #  Fill in the missing dates and the add the date variables again
  flowdata <- fasstr::fill_missing_dates(flowdata, water_year = water_year, water_year_start = water_year_start)
  flowdata <- fasstr::add_date_variables(flowdata,water_year = T,water_year_start = water_year_start)
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  
  #--------------------------------------------------------------
  # CALCULATE SEASONAL STATS 
  if (incl_seasons) {
    # Setup up flowdata to have seasons and the proper years according to water year
    # Year value is designated by the year the season ends in 
    # Example: ONDJFM with calendar years 2000-2001 and water-year-start=2, means ONDJ are WY2001 (ends 
    #          in CY2001) and FM are WY2002 (ends in CY2002) so the the Year for that season is 2002.
    seasons_flowdata <- fasstr::fill_missing_dates(flowdata,water_year = T,water_year_start = 10)
    seasons_flowdata <- fasstr::add_date_variables(seasons_flowdata,water_year = T,water_year_start = 10)
    seasons_flowdata <- dplyr::mutate(seasons_flowdata,
                                      Seasons4= ifelse(Month<=3,"JFM",
                                                       ifelse(Month>=4&Month<=6,"AMJ",
                                                              ifelse(Month>=7&Month<=9,"JAS",
                                                                     ifelse(Month>=10,"OND",NA)))),
                                      Seasons2=ifelse(Month<=3|Month>=10,"ONDJFM",
                                                      ifelse(Month>=4&Month<=9,"AMJJAS",NA)),
                                      Seasons2_year=ifelse(Month>=10,
                                                           Year+1,
                                                           Year),
                                      Seasons4_year=Year)
    
    # Calculate the 2-season summaries (winter/summer)
    Qstat_2seasons <- dplyr::summarise(dplyr::group_by(seasons_flowdata,Seasons2,Seasons2_year),
                                       TotalQ_m3=mean(Value, na.rm=F)*length(Value)*60*60*24,
                                       Yield_mm=TotalQ_m3 /basin_area/1000,
                                       Max_year=max(AnalysisYear))
    Qstat_2seasons <- dplyr::ungroup(Qstat_2seasons)
    Qstat_2seasons <- dplyr::select(Qstat_2seasons,Year=Max_year,Seasons2,TotalQ_m3,Yield_mm)
    Qstat_2seasons <- tidyr::gather(Qstat_2seasons,stat,value,3:4)
    Qstat_2seasons <- dplyr::mutate(Qstat_2seasons,title=paste0(Seasons2,"_",stat))
    Qstat_2seasons <- dplyr::select(Qstat_2seasons,-Seasons2,-stat)
    Qstat_2seasons <- dplyr::filter(Qstat_2seasons, Year >= start_year & Year <= end_year)
    Qstat_2seasons <- tidyr::spread(Qstat_2seasons,title,value)
    
    # Calculate the 4-season summaries (winter/spring/summer/fall)
    Qstat_4seasons <- dplyr::summarise(dplyr::group_by(seasons_flowdata,Seasons4,Seasons4_year),
                                       TotalQ_m3=mean(Value, na.rm=F)*length(Value)*60*60*24,
                                       Yield_mm=TotalQ_m3 /basin_area/1000, 
                                       Max_year=max(AnalysisYear))
    Qstat_4seasons <- dplyr::ungroup(Qstat_4seasons)
    Qstat_4seasons <- dplyr::select(Qstat_4seasons,Year=Max_year,Seasons4,TotalQ_m3,Yield_mm)
    Qstat_4seasons <- tidyr::gather(Qstat_4seasons,stat,value,3:4)
    Qstat_4seasons <- dplyr::mutate(Qstat_4seasons,title=paste0(Seasons4,"_",stat))
    Qstat_4seasons <- dplyr::select(Qstat_4seasons,-Seasons4,-stat)
    Qstat_4seasons <- dplyr::filter(Qstat_4seasons, Year >= start_year & Year <= end_year)
    Qstat_4seasons <- tidyr::spread(Qstat_4seasons,title,value)
  }
  
  # FILTER FLOWDATA FOR SELECTED YEARS FOR REMAINDER OF CALCS
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  
  
  #--------------------------------------------------------------
  # CALCULATE ANNUAL STATS 
  Qstat_annual <-   dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear),
                                     Annual_TotalQ_m3  = (mean(Value, na.rm=na.rm$na.rm.global))*length(Value)*60*60*24,    # Yearly sum of daily avg (cms) *60*60*24 # deal with missing values
                                     Annual_Yield_mm = Annual_TotalQ_m3/basin_area/1000)
  Qstat_annual <- dplyr::rename(Qstat_annual,Year=AnalysisYear)
  Qstat <- Qstat_annual
  
  # Add in seasons if selected
  if (incl_seasons) {
    Qstat <- merge(Qstat,Qstat_2seasons, by="Year",all = T)
    Qstat <- merge(Qstat,Qstat_4seasons, by="Year",all = T)
  }
  
  # Make an excluded years NA
  Qstat[Qstat$Year %in% exclude_years,-1] <- NA
  
  
  # Transpose data if selected
  if(transpose){
    options(scipen = 999)
    Qstat_tpose <- tidyr::gather(Qstat,Statistic,Value,-Year)
    Qstat_tpose_temp <- dplyr::mutate(Qstat_tpose,Value=round(Value,write_digits))
    Qstat <- tidyr::spread(Qstat_tpose,Year,Value)
  }
  
  # Write the table if selected
  if(write_table){
    file_Qstat_table <- file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),
                                                   "-annual-total_flows.csv", sep=""))
    temp <- Qstat
    temp <- round(temp, write_digits)
    if(transpose){
      temp <- tidyr::spread(Qstat_tpose_temp,Year,Value)
    }
    utils::write.csv(temp,file=file_Qstat_table, row.names=FALSE)
  }
  

  return(Qstat)
  
}

