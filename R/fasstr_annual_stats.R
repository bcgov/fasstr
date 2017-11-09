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


#' @title Compute multiple annual statistics.
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
#' @param excluded_years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
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

fasstr_annual_stats <- function(flowdata=NULL,
                         HYDAT=NULL,
                         station_name="fasstr",
                         water_year=FALSE, #create another for own water year????
                         start_year=NULL,
                         end_year=NULL,
                         exclude_years=NULL, # list of stations
                         basin_area=NA, # if na, then all Yield values == NA
                         transpose=FALSE,
                         write_table=FALSE,        # write out statistics on calendar year
                         report_dir=".",
                         na.rm=list(na.rm.global=FALSE),
                         table_nddigits=3){              # decimal digits for csv files for statistics

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
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.na(basin_area) & !is.numeric(basin_area))    {stop("basin_area parameter must be numeric")}
  if( length(basin_area)>1)        {stop("basin_area parameter cannot have length > 1")}
  
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
    if (is.na(basin_area)) {basin_area <- tidyhydat::hy_stations(station_number = HYDAT)$DRAINAGE_AREA_GROSS}
    flowdata <- tidyhydat::hy_daily_flows(station_number =  HYDAT)
  }
  
  
  # If start/end years are not select, set them as the min/max dates
  min_year <- lubridate::year(min(flowdata$Date))-water_year
  max_year <- lubridate::year(max(flowdata$Date))
  if (!is.numeric(start_year)) {start_year <- min_year}
  if (!is.numeric(end_year)) {end_year <- max_year}
  if(! (start_year <= end_year))    {stop("start_year argument must be less than end_year argument")}
  
  
  # Fill in any missing dates with NA
  flowdata <- fasstr::fasstr_fill_missing_dates(flowdata,water_year = TRUE)

  
  # CREATE DATE VARIABLES AND ROLLING MEANS/SUMS ==============================
  
  flowdata <- fasstr::fasstr_add_date_vars(flowdata)
  flowdata <- dplyr::mutate(flowdata,
                            Seasons4= ifelse(Month<=3,"JFM",
                                             ifelse(Month>=4&Month<=6,"AMJ",
                                                    ifelse(Month>=7&Month<=9,"JAS",
                                                           ifelse(Month>=10,"OND",NA)))),
                            Seasons2=ifelse(Month<=3|Month>=10,"ONDJFM",
                                            ifelse(Month>=4&Month<=9,"AMJJAS",NA)))
  
  # Set selected year-type column for analysis
  if (water_year) {
    flowdata$AnalysisYear <- flowdata$WaterYear
    flowdata$AnalysisDoY <- flowdata$WaterDayofYear
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  
  #  Compute the 3, 7, and 30 day rolling average values
  flowdata <- fasstr::fasstr_add_rolling_means(flowdata,days = c(3,7,30))
  
  # compuate the annual cumulative total
  flowdata <- fasstr_add_total_volume(flowdata,water_year = water_year)
 # flowdata <- dplyr::mutate(dplyr::group_by(flowdata,AnalysisYear),Qcumul=cumsum(Q))
  

  
  # CALCULATE STATS ==============================
  
  ## Compute statistics on 2 seasons (must be water year) so calc'd first before filtering for selected years
  Qstat_2seasons <- dplyr::summarize(dplyr::group_by(flowdata,WaterYear,Seasons2),
                                     TOTALQ_DAILY=mean(Value, na.rm=F)*length(Value)*60*60*24,
                                     YIELDMM_DAILY=mean(Value, na.rm=F)*length(Value)*60*60*24 /basin_area/1000)
  Qstat_2seasons <- tidyr::gather(Qstat_2seasons,stat,value,3:4)
  Qstat_2seasons <- dplyr::mutate(Qstat_2seasons,title=paste0(Seasons2,"_",stat))
  Qstat_2seasons <- dplyr::select(Qstat_2seasons,-Seasons2,-stat)
  Qstat_2seasons <- tidyr::spread(Qstat_2seasons,title,value)
  Qstat_2seasons <- dplyr::rename(Qstat_2seasons,Year=WaterYear)
  Qstat_2seasons <- dplyr::filter(Qstat_2seasons, Year >= start_year & Year <= end_year)
  
  
  # FILTER DATA FOR SELECTED YEARS FOR REMAINDER OF CALCS
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= min_year & AnalysisYear <=max_year)
  
  
  ## Compute statistics on  year basis
  
  Qstat_annual <-   dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear),
                                     MIN_01Day    = min(Value, na.rm=na.rm$na.rm.global),	     
                                     MINDOY_01Day = ifelse(is.na(MIN_01Day),NA,
                                                           AnalysisDoY[which(Value==MIN_01Day)]),
                                     MIN_03Day    = min(Q3Day, na.rm=na.rm$na.rm.global),	    
                                     MINDOY_03Day = ifelse(is.na(MIN_03Day),NA,
                                                           AnalysisDoY[which(Q3Day==MIN_03Day)]),
                                     MIN_07Day    = min(Q7Day, na.rm=na.rm$na.rm.global),	     
                                     MINDOY_07Day = ifelse(is.na(MIN_07Day),NA,
                                                           AnalysisDoY[which(Q7Day==MIN_07Day)]),
                                     MIN_30Day    = min(Q30Day, na.rm=na.rm$na.rm.global),	     
                                     MINDOY_30Day = ifelse(is.na(MIN_30Day),NA,
                                                           AnalysisDoY[which(Q30Day==MIN_30Day)]),
                                     MIN_DAILY     = min (Value, na.rm=na.rm$na.rm.global),	    # CY Min Daily Q 	CY Min Daily Q
                                     MAX_DAILY	    = max (Value, na.rm=na.rm$na.rm.global),      # CY Max Daily Q
                                     MEAN_DAILY    = mean(Value, na.rm=na.rm$na.rm.global),     # CY Mean Discharge (Based on Daily avgs)
                                     MEDIAN_DAILY  = median(Value, na.rm=na.rm$na.rm.global),  # CY median Discharge (Based on Daily avgs)
                                     TOTALQ_DAILY  = MEAN_DAILY*length(Value)*60*60*24,    # Yearly sum of daily avg (cms) *60*60*24 # deal with missing values
                                     YIELDMM_DAILY = TOTALQ_DAILY/basin_area/1000 ,
                                     Date_25P_FLOW_DAILY = DayofYear[ match(TRUE, Vtotal > 0.25  *TOTALQ_DAILY)],
                                     Date_33P_FLOW_DAILY = DayofYear[ match(TRUE, Vtotal > 0.333 *TOTALQ_DAILY)],
                                     Date_50P_FLOW_DAILY = DayofYear[ match(TRUE, Vtotal > 0.50  *TOTALQ_DAILY)],
                                     Date_75P_FLOW_DAILY = DayofYear[ match(TRUE, Vtotal > 0.75  *TOTALQ_DAILY)])
  Qstat_annual <-   dplyr::rename(Qstat_annual,Year=AnalysisYear)
  
  ## Compute statistics on 4 seasons
  Qstat_4seasons <- dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear,Seasons4),
                                     TOTALQ_DAILY=mean(Value, na.rm=na.rm$na.rm.global)*length(Value)*60*60*24,
                                     YIELDMM_DAILY=mean(Value, na.rm=na.rm$na.rm.global)*length(Value)*60*60*24 /basin_area/1000)
  Qstat_4seasons <- tidyr::gather(Qstat_4seasons,stat,value,3:4)
  Qstat_4seasons <- dplyr::mutate(Qstat_4seasons,title=paste0(Seasons4,"_",stat))
  Qstat_4seasons <- dplyr::select(Qstat_4seasons,-Seasons4,-stat)
  Qstat_4seasons <- tidyr::spread(Qstat_4seasons,title,value)
  Qstat_4seasons <-   dplyr::rename(Qstat_4seasons,Year=AnalysisYear)
  
  
  
  ## Compute statistics on months
  Qstat_months <- dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear,MonthName),
                                   "_MIN_DAILY" = min   (Value, na.rm=na.rm$na.rm.global),
                                   "_MAX_DAILY" = max   (Value, na.rm=na.rm$na.rm.global),
                                   "_MEAN_DAILY"= mean  (Value, na.rm=na.rm$na.rm.global),
                                   "_MEDIAN_DAILY" = stats::median(Value, na.rm=na.rm$na.rm.global),
                                   "_P10_DAILY" = stats::quantile(Value, prob=.10, na.rm=T),
                                   "_P20_DAILY" = stats::quantile(Value, prob=.20, na.rm=T)
  )
  Qstat_months <- tidyr::gather(Qstat_months,stat,value,3:8)
  Qstat_months <- dplyr::mutate(Qstat_months,title=paste0(MonthName,stat))
  Qstat_months <- dplyr::select(Qstat_months,-MonthName,-stat)
  Qstat_months <- tidyr::spread(Qstat_months,title,value)
  Qstat_months <- dplyr::rename(Qstat_months,Year=AnalysisYear)
  
  # compute the number of days in a year outside of the 25th or 75th percentile for each day.
  daily_normals <- dplyr::summarise(dplyr::group_by(flowdata,AnalysisDoY),
                                    P25=stats::quantile(Value, prob=0.25, na.rm=TRUE),
                                    P75=stats::quantile(Value, prob=0.75, na.rm=TRUE))
  flowdata_temp <- merge(flowdata, daily_normals, by="AnalysisDoY") # merge back with the original data
  Qstat_dailynormals <- dplyr::summarise(dplyr::group_by(flowdata_temp,AnalysisYear),
                                         DAYS_BELOW_25=sum(Value < P25, na.rm=TRUE),
                                         DAYS_ABOVE_75=sum(Value > P75, na.rm=TRUE),
                                         DAYS_OUTSIDE_25_75 = DAYS_BELOW_25 + DAYS_ABOVE_75)
  Qstat_dailynormals <- dplyr::rename(Qstat_dailynormals,Year=AnalysisYear)
  
  
  # Combine all and label columns
  Qstat <- merge(Qstat_annual,Qstat_4seasons,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qstat_2seasons,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qstat_months,by="Year",all = TRUE)
  Qstat <- merge(Qstat,Qstat_dailynormals,by="Year",all = TRUE)
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
    Qstat_tpose.temp <- dplyr::mutate(Qstat_tpose,Value=round(Value,table_nddigits)) # for writing to csv
    Qstat <- tidyr::spread(Qstat_tpose,Year,Value)
  }
  
  
  # See if you want to write out the summary tables?
  if(write_table){
    # Write out the summary table for comparison to excel spreadsheet for calendar year
    file_Qstat_table <- file.path(report_dir, paste(station_name,"-annual-statistics.csv", sep=""))
    temp <- Qstat
    temp <- round(temp, table_nddigits)
    if(transpose){
      temp <- tidyr::spread(Qstat_tpose.temp,Year,Value)
    }
    utils::write.csv(temp,file=file_Qstat_table, row.names=FALSE)
  }
  

  
  
  return(Qstat)
} # end of function

