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

#' @title Calculate monthly summary statistics
#'
#' @description Calculates monthly mean, median, maximum, minimum, and percentiles for each month of all years of daily flow values 
#'    from a streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param percentiles Numeric. Vector of percentiles to calculate. Set to NA if none required. Default \code{c(10,20)}
#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required. 
#' @param months Integer. Vector of months to consider for analysis (ex. \code{6:8} for Jun-Aug). Leave blank if all months
#'    are required. Default \code{1:12}.    
#' @param spread Logical. All monthly statistics as column names. Default \code{FALSE}.
#' @param transpose Logical. All monthly statistics as row names. Default \code{FALSE}.
#' @param station_name Character. Name of hydrometric station or stream that will be used to create file names. Leave blank if not writing
#'    files or if \code{HYDAT} is used or a column in \code{flowdata} called 'STATION_NUMBER' contains a WSC station number, as the name
#'    will be the \code{HYDAT} value provided in the argument or column. Setting the station name will replace the HYDAT station number. 
#' @param write_table Logical. Write the table as a .csv file to specified directory. Default \code{FALSE}.
#' @param write_digits Numeric. Number of significant digits to round the results in the written table. Default \code{3}.
#' @param write_dir Character. Directory folder name of where to write tables and plots. If directory does not exist, it will be created.
#'    Default is the working directory.
#' @param na.rm TBD
#' 
#' @return A data frame of summary statistics and percentiles for each month of all years
#'
#' @examples
#' \dontrun{
#' 
#'calc_monthly_stats(flowdata = flowdata, station_name = "MissionCreek", write_table = TRUE)
#' 
#'calc_monthly_stats(HYDAT = "08NM116", water_year = TRUE, water_year_start = 8, percentiles = c(1:10))
#'
#'calc_monthly_stats(HYDAT = "08NM116", months = 7:9)
#'
#' }
#' @export

#--------------------------------------------------------------
calc_monthly_stats <- function(flowdata=NULL,
                                 HYDAT=NULL,
                                 percentiles=c(10,20),
                                 water_year=FALSE,
                                 water_year_start=10,
                                 start_year=NULL,
                                 end_year=NULL,
                                 exclude_years=NULL,
                                 months=1:12,
                                 spread=FALSE,
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
  
  if( !all(is.na(percentiles)) & !is.numeric(percentiles) )                 {stop("percentiles argument must be numeric")}
  if( !all(is.na(percentiles)) & (!all(percentiles>0 & percentiles<100)) )  {stop("percentiles must be >0 and <100)")}
  
  if( !is.na(station_name) & !is.character(station_name) )  {stop("station_name argument must be a character string.")}
  
  if( !is.logical(spread))    {stop("spread parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(transpose)) {stop("transpose parameter must be logical (TRUE/FALSE)")}
  if( spread & transpose )    {stop("both spread and transpose arguments cannot be TRUE")}
  
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
  
  
  # Filter the data for the start and end years
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata, Month %in% months)
  
  
  #--------------------------------------------------------------
  # Complete analysis
  
  # Compute basic stats for each month for each year
  Qstat_monthly <-   dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear,MonthName),
                                      Mean    = mean(Value, na.rm=na.rm$na.rm.global),    
                                      Median  = median(Value, na.rm=na.rm$na.rm.global),  
                                      Maximum	    = max (Value, na.rm=na.rm$na.rm.global), 
                                      Minimum     = min (Value, na.rm=na.rm$na.rm.global)
  )
  
  # Compute selected percentiles for each month for each year
  if (!all(is.na(percentiles))){
    for (ptile in percentiles) {
      
      Q_monthly_ptile <- dplyr::summarise(dplyr::group_by(flowdata,AnalysisYear,MonthName),
                                          Percentile=quantile(Value,ptile/100, na.rm=TRUE))
      colnames(Q_monthly_ptile)[3] <- paste0("P",ptile)
      
      Qstat_monthly <- merge(Qstat_monthly,Q_monthly_ptile,by=c("AnalysisYear","MonthName"))
    }
  }
  
  # Rename some columns
  Qstat_monthly <-   dplyr::rename(Qstat_monthly,Year=AnalysisYear,Month=MonthName)
  
  # Set the levels of the months for proper ordering
  if (water_year) {
    if (water_year_start==1) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
    } else if (water_year_start==2) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan"))
    } else if (water_year_start==3) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan", "Feb"))
    } else if (water_year_start==4) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Mar"))
    } else if (water_year_start==5) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr"))
    } else if (water_year_start==6) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May"))
    } else if (water_year_start==7) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Jul","Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun"))
    } else if (water_year_start==8) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Aug","Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul"))
    } else if (water_year_start==9) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Sep","Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug"))
    } else if (water_year_start==10) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep"))
    } else if (water_year_start==11) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct"))
    } else if (water_year_start==12) {
      Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov"))
    }
  } else {           
    Qstat_monthly$Month <- factor(Qstat_monthly$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  }
  
  # Reorder months and row.names
  Qstat_monthly <- with(Qstat_monthly, Qstat_monthly[order(Year, Month),])
  row.names(Qstat_monthly) <- c(1:nrow(Qstat_monthly))
  
  # Make any excluded years NA (doesnt remove the data)
  Qstat_monthly[Qstat_monthly$Year %in% exclude_years,3:ncol(Qstat_monthly)] <- NA
  
  # Transform data to chosen format
  # Spread data if selected
  if (spread) {
    Qstat_monthly_spread <- dplyr::summarise(dplyr::group_by(Qstat_monthly,Year))
    for (mnth in unique(Qstat_monthly$Month)) {
      Qstat_monthly_month <- dplyr::filter(Qstat_monthly,Month==mnth)
      Qstat_monthly_month <- tidyr::gather(Qstat_monthly_month,Statistic,Value,3:ncol(Qstat_monthly_month))
      Qstat_monthly_month <- dplyr::mutate(Qstat_monthly_month,StatMonth=paste0(Month,"_",Statistic))
      Qstat_monthly_month <- dplyr::select(Qstat_monthly_month,-Statistic,-Month)
      Qstat_order <- unique(Qstat_monthly_month$StatMonth)
      Qstat_monthly_month <- tidyr::spread(Qstat_monthly_month,StatMonth,Value)
      Qstat_monthly_month <-  Qstat_monthly_month[,c("Year",Qstat_order)]
      Qstat_monthly_spread <- merge(Qstat_monthly_spread,Qstat_monthly_month,by="Year",all = TRUE)
    }  
    Qstat_monthly <- Qstat_monthly_spread
  }
  # Transpose data if selected
  if (transpose) {
    Qstat_monthly_tpose_names <- c("Statistic",unique(Qstat_monthly$Year))
    Qstat_monthly_tpose <- data.frame(matrix(ncol = length(Qstat_monthly_tpose_names), nrow = 0))
    colnames(Qstat_monthly_tpose) <- Qstat_monthly_tpose_names
    
    for (mnth in unique(Qstat_monthly$Month)) {
      Qstat_monthly_month <- dplyr::filter(Qstat_monthly,Month==mnth)
      Qstat_monthly_month <- tidyr::gather(Qstat_monthly_month,Statistic,Value,3:ncol(Qstat_monthly_month))
      Qstat_monthly_month <- dplyr::mutate(Qstat_monthly_month,Statistic=paste0(Month,"_",Statistic))
      Qstat_monthly_month <- dplyr::select(Qstat_monthly_month,-Month)
      Qstat_order <- unique(Qstat_monthly_month$Statistic)
      Qstat_monthly_month <- tidyr::spread(Qstat_monthly_month,Year,Value)
      Qstat_monthly_month <- Qstat_monthly_month[match(Qstat_order, Qstat_monthly_month$Statistic),]
      Qstat_monthly_tpose <- dplyr::bind_rows(Qstat_monthly_tpose,Qstat_monthly_month)
    }  
    Qstat_monthly <- Qstat_monthly_tpose
  }
  
  
  # Write the table if selected
  if(write_table){
    file_Qmonth_table <- file.path(write_dir, paste(paste0(ifelse(!is.na(station_name),station_name,paste0("fasstr"))),"-monthly-statistics.csv", sep=""))
    temp <- Qstat_monthly
    if (spread | transpose) {
      temp <- round(temp, write_digits)
    } else {
      temp[,3:ncol(temp)] <- round(temp[,3:ncol(temp)], write_digits)
    }
    utils::write.csv(temp,file=file_Qmonth_table, row.names=FALSE)
  }
  
  
  return(Qstat_monthly)

  
}

