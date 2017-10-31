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
#' @description Computes annual statistics (summary statistics, trending) of streamflow data.
#' Streamflow data can be supplied through the \code{flow.data} parameter or extracted from a 
#' HYDAT database using the tidyhydat package and \code{HYDAT} parameter.
#' and (optionally) saves the results in *.csv and *.pdf files.
#'
#' @param station.name Character. Identifier name of the stream or station. Required when supplying data through \code{flow.data}.
#'    The station name will be used in plots and filenames of exported tables and plot. If using \code{HYDAT} to supply
#'    data and no \code{station.name} is provided, the HYDAT station number will be the identifier.
#' @param flow.data Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Q' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flow.data} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param water.year Logical. Set to \code{TRUE} if data should be summarized by water year (Oct-Sep) instead of the
#'    default calendar year (Jan-Dec) (\code{water.year=FALSE}). Water years are designated by the year which they end in
#'    (e.g. water year 2000 start on 1 Oct 1999 and ends on 30 Sep 2000).
#' @param start.year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start.year} is the first
#'    year of the data provided.
#' @param end.year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end.year} is the last
#'    year of the data provided.
#' @param excluded.years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param basin.area Numeric. The upstream drainage basin area (in sq. km) of the station. Used to calculate runoff yields (mm).
#'    If no value provided, yield calculations will result in NA values.
#' @param write.table Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,'-annual-cy-summary-stat.csv'))}.
#' @param write.transposed.table Logical. Should a file be created with the transposed of the annual statistics
#'    (both calendar and water year)?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,'-annual-summary-stat-trans.csv'))}.
#' @param write.summary.table Logical. Should a file be created with a flow summary over the years between the
#'    start.year and end.year (inclusive). This summary includes number of days, number of missing values,
#'    mean, median, minimum, maximum, and standard deviation of \code{flow.data$Q}.
#'    The file name will be \code{file.path(report.dir, paste(station.name,"-period-record-summary.csv", sep=""))}.
#' @param write.lowflow.table Logical. Should a file be created with the minimum value of \code{flow.data$Q} and date the
#'    minimum occured.
#'    The file name will be \code{file.path(report.dir,paste(station.name,"-lowflow-summary.csv",sep=""))}
#' @param report.dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param csv.nddigits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#' @param na.rm TBD
#'
#' @return A list with the following elements:
#'   \item{Q.flow.summary}{Data frame with flow summary.}
#'   \item{Q.stat.annual}{Data frame with summary statistics as listed at \code{\link{SummaryStatistics}}.}
#'   \item{Q.stat.annual.trans}{Data frame with transposed summary statistics as listed at \code{\link{SummaryStatistics}}.}
#'   \item{dates.missing.flow}{Data framw with dates of missing \code{flow.data$Q} between
#'          \code{start.year} and \code{end.year}}
#'   \item{file.stat.csv}{Object with file name of *.csv file with calendar year summary statistics.}
#'   \item{file.stat.trans.csv}{Object with file name of *.csv file with transposed summary statistics.}
#'   \item{file.summary.csv}{Object with file name of *.csv file with summary statistics.}
#'   \item{file.lowflow.csv}{Object with file name of *.csv file with low flow summary statistics.}
#'   \item{na.rm}{Missing value flags.}
#'   \item{Version}{Version of this function.}
#'   \item{Date}{Date function was run.}
#'
#' @examples
#' \dontrun{
#' stat.annual <- annual.stats(
#'                          station.name  ='ABCD',
#'                          basin.area  =12345,
#'                          HYDAT="08HB048",
#'                          flow.data          =flow,
#'                          water.year = TRUE,
#'                          start.year    =1960,
#'                          end.year      =2014)
#' }
#' @export

#'
#--------------------------------------------------------------
# Compute the statistics on an (calendar and water) year basis

annual.stats <- function(station.name=NULL,
                         flow.data=NULL,
                         HYDAT=NULL,
                         water.year=FALSE, #create another for own water year????
                         start.year=NULL,
                         end.year=NULL,
                         exclude.years=NULL, # list of stations
                         basin.area=NA, # if na, then all Yield values == NA
                         zyp.trending=NA,
                         zyp.alpha=0.05,
                         write.table=FALSE,        # write out statistics on calendar year
                         write.transposed.table=FALSE,  # write out statistics in transposed format (cy & wy)
                         write.summary.table=FALSE, # write out a summary of period of record
                         write.lowflow.table=FALSE,      # write out a summary of low flows
                         write.zyp.table=FALSE,
                         write.zyp.plots=NA,
                         report.dir=".",
                         na.rm=list(na.rm.global=FALSE),
                         csv.nddigits=3){              # decimal digits for csv files for statistics
                         
  #  Compute statistics on an annual (calendar and water) year basis
  #
  #  See the man-roxygen director for definition of parameters
  #
  #  Output: List with elements given above.
  #
  #############################################################
  #  Some basic error checking on the input parameters
  #
  Version <- packageVersion("fasstr")
  
  if( is.null(flow.data) & is.null(HYDAT)) {stop("flow.data or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flow.data))  {stop("Must select either flow.data or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & is.null(station.name))  {stop("station.name parameter is required with flow.data parameter.")}
  if( is.null(HYDAT) & !is.character(station.name))  {stop("station.name parameter must be a character string.")}
  if( is.null(HYDAT) & length(station.name)>1)        {stop("station.name parameter cannot have length > 1")}
  if( is.null(HYDAT) & !is.data.frame(flow.data))         {stop("flow.data parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Q") %in% names(flow.data))){
    stop("flow.data dataframe doesn't contain the variables Date and Q.")}
  if( is.null(HYDAT) & !inherits(flow.data$Date[1], "Date")){
    stop("Date column in flow.data dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flow.data$Q))          {stop("Q column in flow.data dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flow.data$Q <0, na.rm=TRUE))   {stop('flow.data cannot have negative values - check your data')}
  
  if( !is.logical(water.year))  {stop("water.year parameter must be logical (TRUE/FALSE)")}
  if( !is.null(exclude.years) & !is.numeric(exclude.years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.na(basin.area) & !is.numeric(basin.area))    {stop("basin.area parameter must be numeric")}
  if( length(basin.area)>1)        {stop("basin.area parameter cannot have length > 1")}
  
  if( !is.logical(write.table))  {stop("write.table parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write.transposed.table)){stop("write.transposed.table parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write.summary.table)){stop("write.summary.table parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write.lowflow.table)){ stop("write.lowflow.table parameter must be logical (TRUE/FALSE)")}

  if( !is.na(zyp.trending) & !zyp.trending %in% c("yuepilon","zhang"))   {
    stop('zyp.trending parameter must have either "yuepilon" or "zhang" listed')}
  if( !is.logical(write.zyp.table))  {stop("write.zyp.table parameter must be logical (TRUE/FALSE")}
  if( !is.na(write.zyp.plots) & !write.zyp.plots %in% c("png","jpeg","tiff","bmp"))  {
    stop("write.zyp.plots parameter must be logical (TRUE/FALSE)")}
  if( is.na(zyp.trending) & (write.zyp.table | !is.na(write.zyp.plots)) ) {
    stop('zyp.trending parameter method must be selected to write results')}
  if( !is.numeric(zyp.alpha))  { stop("zyp.alpha parameter needs to be numeric")}
  
  if( !dir.exists(as.character(report.dir)))      {stop("directory for saved files does not exist")}
  if( !is.numeric(csv.nddigits))  { stop("csv.ndddigits parameter needs to be numeric")}
  csv.nddigits <- round(csv.nddigits[1])  # number of decimal digits for rounding in csv files
  
  if( !is.list(na.rm))              {stop("na.rm is not a list") }
  if(! is.logical(unlist(na.rm))){   stop("na.rm is list of logical (TRUE/FALSE) values only.")}
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  
  # If HYDAT station is listed, check if it exists and make it the flow.data
  if (!is.null(HYDAT)) {
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    if (is.null(station.name)) {station.name <- HYDAT}
    flow.data <- tidyhydat::DLY_FLOWS(STATION_NUMBER = HYDAT)
    flow.data <- dplyr::select(flow.data,Date,Q=Value)
  }
  
  
  # If start/end years are not select, set them as the min/max dates
  min.year <- lubridate::year(min(flow.data$Date))-water.year
  max.year <- lubridate::year(max(flow.data$Date))
  if (!is.numeric(start.year)) {start.year <- min.year}
  if (!is.numeric(end.year)) {end.year <- max.year}
  if(! (start.year <= end.year))    {stop("start.year parameter must be less than end.year parameter")}
  
  
  #  Generate all dates between min and max dates and merge with flow
  #  data frame to generate any dates that were missing.
  #  This will automatically generate NA for the days that were not in the file
  flow.data <- merge(flow.data, 
                     data.frame(Date=seq(as.Date(paste(start.year-1,'01-01',sep='-'), "%Y-%m-%d"),
                                         as.Date(paste(end.year  ,'12-31',sep='-'), '%Y-%m-%d'), 1)),
                     all.y=TRUE)
  
  #  create the year (annual and water), seasonal, and month variables
  flow.data$Year  <- lubridate::year(flow.data$Date)
  flow.data$Month  <- lubridate::month(flow.data$Date)
  flow.data$MonthText <- month.abb[flow.data$Month]
  flow.data$WaterYear <- as.numeric(ifelse(flow.data$Month>=10,flow.data$Year+1,flow.data$Year))
  flow.data$DayofYear <- lubridate::yday(flow.data$Date)
  flow.data$WaterDoY <- ifelse(flow.data$Month<10,flow.data$DayofYear+92,
                               ifelse((as.Date(with(flow.data, paste(Year+1,01,01,sep="-")),"%Y-%m-%d")-as.Date(with(flow.data, paste(Year,01,01,sep="-")),"%Y-%m-%d"))==366,
                                      flow.data$DayofYear-274,
                                      flow.data$DayofYear-273))
  flow.data <- dplyr::mutate(flow.data,
                             Seasons4= ifelse(Month<=3,"JFM",
                                              ifelse(Month>=4&Month<=6,"AMJ",
                                                     ifelse(Month>=7&Month<=9,"JAS",
                                                            ifelse(Month>=10,"OND",NA)))),
                             Seasons2=ifelse(Month<=3|Month>=10,"ONDJFM",
                                             ifelse(Month>=4&Month<=9,"AMJJAS",NA)))
  
  # Set selected year-type column for analysis
  if (water.year) {
    flow.data$AnalysisYear <- flow.data$WaterYear
    flow.data$AnalysisDoY <- flow.data$WaterDoY
  }  else {
    flow.data$AnalysisYear <- flow.data$Year
    flow.data$AnalysisDoY <- flow.data$DayofYear
  }
  
  #  Compute the 3, 7, and 30 day rolling average values
  flow.data$Q.03DAvg <- zoo::rollapply( flow.data$Q,  3, mean, fill=NA, align="right")
  flow.data$Q.07DAvg <- zoo::rollapply( flow.data$Q,  7, mean, fill=NA, align="right")
  flow.data$Q.30DAvg <- zoo::rollapply( flow.data$Q, 30, mean, fill=NA, align="right")
  
  # compuate the annual cumulative total
  flow.data <- dplyr::mutate(dplyr::group_by(flow.data,AnalysisYear),CumQ=cumsum(Q))
  
  #  which dates have missing flows.
  dates.missing.flows <- flow.data$Date[ is.na(flow.data$Q)]
  
  #  simple summary statistics
  flow.sum <-   dplyr::summarize(dplyr::group_by(flow.data,AnalysisYear),
                                 n.days   = length(Year),
                                 n.Q      = sum (!is.na(Q)),
                                 n.miss.Q = sum ( is.na(Q)),
                                 min.Q    = min (Q,          na.rm=na.rm$na.rm.global),
                                 max.Q    = max (Q,          na.rm=na.rm$na.rm.global),
                                 mean.Q   = mean(Q,          na.rm=na.rm$na.rm.global),
                                 median.Q = stats::median(Q, na.rm=na.rm$na.rm.global),
                                 sd.Q     = stats::sd  (Q,   na.rm=na.rm$na.rm.global)
  )
  flow.sum <-   dplyr::rename(flow.sum,Year=AnalysisYear)
  
  
  ## Compute statistics on  year basis
  #################################
  
  Q.stat.annual <-   dplyr::summarize(dplyr::group_by(flow.data,AnalysisYear),
                                      MIN_01Day    = min(Q, na.rm=na.rm$na.rm.global),	      # CY Min Daily Q
                                      MINDOY_01Day = ifelse(is.na(MIN_01Day),NA,
                                                            AnalysisDoY[which.min(Q)]),# Date of CY Min Daily Q
                                      MIN_03Day    = min(Q.03DAvg, na.rm=na.rm$na.rm.global),	      # CY Min Daily Q
                                      MINDOY_03Day = ifelse(is.na(MIN_03Day),NA,
                                                            AnalysisDoY[which.min(Q.03DAvg)]),# Date of CY Min Daily Q
                                      MIN_07Day    = min(Q.07DAvg, na.rm=na.rm$na.rm.global),	      # CY Min Daily Q
                                      MINDOY_07Day = ifelse(is.na(MIN_07Day),NA,
                                                            AnalysisDoY[which.min(Q.07DAvg)]),# Date of CY Min Daily Q
                                      MIN_30Day    = min(Q.30DAvg, na.rm=na.rm$na.rm.global),	      # CY Min Daily Q
                                      MINDOY_30Day = ifelse(is.na(MIN_30Day),NA,
                                                            AnalysisDoY[which.min(Q.30DAvg)]),# Date of CY Min Daily Q
                                      MIN_DAILY     = min (Q, na.rm=na.rm$na.rm.global),	    # CY Min Daily Q 	CY Min Daily Q
                                      MAX_DAILY	    = max (Q, na.rm=na.rm$na.rm.global),      # CY Max Daily Q
                                      MEAN_DAILY    = mean(Q, na.rm=na.rm$na.rm.global),     # CY Mean Discharge (Based on Daily avgs)
                                      MEDIAN_DAILY  = median(Q, na.rm=na.rm$na.rm.global),  # CY median Discharge (Based on Daily avgs)
                                      TOTALQ_DAILY  = MEAN_DAILY*length(Q)*60*60*24,    # Yearly sum of daily avg (cms) *60*60*24 # deal with missing values
                                      YIELDMM_DAILY = TOTALQ_DAILY/basin.area/1000 ,
                                      Date_25P_FLOW_DAILY = DayofYear[ match(TRUE, CumQ > 0.25  *TOTALQ_DAILY/(60*60*24))],
                                      Date_33P_FLOW_DAILY = DayofYear[ match(TRUE, CumQ > 0.333 *TOTALQ_DAILY/(60*60*24))],
                                      Date_50P_FLOW_DAILY = DayofYear[ match(TRUE, CumQ > 0.50  *TOTALQ_DAILY/(60*60*24))],
                                      Date_75P_FLOW_DAILY = DayofYear[ match(TRUE, CumQ > 0.75  *TOTALQ_DAILY/(60*60*24))])
  Q.stat.annual <-   dplyr::rename(Q.stat.annual,Year=AnalysisYear)
  
  # four seasons
  Q.stat.seasons4 <- dplyr::summarize(dplyr::group_by(flow.data,AnalysisYear,Seasons4),
                                      TOTALQ_DAILY=mean(Q, na.rm=na.rm$na.rm.global)*length(Q)*60*60*24,
                                      YIELDMM_DAILY=mean(Q, na.rm=na.rm$na.rm.global)*length(Q)*60*60*24 /basin.area/1000)
  Q.stat.seasons4 <- tidyr::gather(Q.stat.seasons4,stat,value,3:4)
  Q.stat.seasons4 <- dplyr::mutate(Q.stat.seasons4,title=paste0(Seasons4,"_",stat))
  Q.stat.seasons4 <- dplyr::select(Q.stat.seasons4,-Seasons4,-stat)
  Q.stat.seasons4 <- tidyr::spread(Q.stat.seasons4,title,value)
  Q.stat.seasons4 <-   dplyr::rename(Q.stat.seasons4,Year=AnalysisYear)
  
  # two seasons  MUST BE WATER YEAR
  Q.stat.seasons2 <- dplyr::summarize(dplyr::group_by(flow.data,WaterYear,Seasons2),
                                      TOTALQ_DAILY=mean(Q, na.rm=na.rm$na.rm.global)*length(Q)*60*60*24,
                                      YIELDMM_DAILY=mean(Q, na.rm=na.rm$na.rm.global)*length(Q)*60*60*24 /basin.area/1000)
  Q.stat.seasons2 <- tidyr::gather(Q.stat.seasons2,stat,value,3:4)
  Q.stat.seasons2 <- dplyr::mutate(Q.stat.seasons2,title=paste0(Seasons2,"_",stat))
  Q.stat.seasons2 <- dplyr::select(Q.stat.seasons2,-Seasons2,-stat)
  Q.stat.seasons2 <- tidyr::spread(Q.stat.seasons2,title,value)
  Q.stat.seasons2 <- dplyr::rename(Q.stat.seasons2,Year=WaterYear)
  Q.stat.seasons2 <- dplyr::filter(Q.stat.seasons2, Year >= start.year & Year <= end.year)
  
  # monthly
  Q.stat.month <- dplyr::summarize(dplyr::group_by(flow.data,AnalysisYear,MonthText),
                                   "_MIN_DAILY" = min   (Q, na.rm=na.rm$na.rm.global),
                                   "_MAX_DAILY" = max   (Q, na.rm=na.rm$na.rm.global),
                                   "_MEAN_DAILY"= mean  (Q, na.rm=na.rm$na.rm.global),
                                   "_MEDIAN_DAILY" = stats::median(Q, na.rm=na.rm$na.rm.global),
                                   "_P10_DAILY" = stats::quantile(Q, prob=.10, na.rm=T),
                                   "_P20_DAILY" = stats::quantile(Q, prob=.20, na.rm=T)
  )
  Q.stat.month <- tidyr::gather(Q.stat.month,stat,value,3:8)
  Q.stat.month <- dplyr::mutate(Q.stat.month,title=paste0(MonthText,stat))
  Q.stat.month <- dplyr::select(Q.stat.month,-MonthText,-stat)
  Q.stat.month <- tidyr::spread(Q.stat.month,title,value)
  Q.stat.month <- dplyr::rename(Q.stat.month,Year=AnalysisYear)
  
  # compute the number of days in a year outside of the 25th or 75th percentile for each day.
  daily.quant <- dplyr::summarise(dplyr::group_by(flow.data,AnalysisDoY),
                                  P25=stats::quantile(Q, prob=0.25, na.rm=TRUE),
                                  P75=stats::quantile(Q, prob=0.75, na.rm=TRUE))
  flow.data.temp <- merge(flow.data, daily.quant, by="AnalysisDoY") # merge back with the original data
  Q.outside.quant <- dplyr::summarise(dplyr::group_by(flow.data.temp,AnalysisYear),
                                      DAYS_BELOW_25=sum(Q < P25, na.rm=TRUE),
                                      DAYS_ABOVE_75=sum(Q > P75, na.rm=TRUE),
                                      DAYS_OUTSIDE_25_75 = DAYS_BELOW_25 + DAYS_ABOVE_75)
  Q.outside.quant <- dplyr::rename(Q.outside.quant,Year=AnalysisYear)
  
  
  # Combine all and label columns
  Q.stat <- merge(Q.stat.annual,Q.stat.seasons4,by="Year",all = TRUE)
  Q.stat <- merge(Q.stat,Q.stat.seasons2,by="Year",all = TRUE)
  Q.stat <- merge(Q.stat,Q.stat.month,by="Year",all = TRUE)
  Q.stat <- merge(Q.stat,Q.outside.quant,by="Year",all = TRUE)
  Q.stat <- tidyr::gather(Q.stat,Stat,Value,2:ncol(Q.stat))
  Q.stat <- dplyr::mutate(Q.stat,Stat=paste0(ifelse(water.year,paste("WY_"),paste("CY_")),Stat))
  col.order <- c("Year",unique(Q.stat$Stat)) # to keep the same order as merged when spread below
  Q.stat <- tidyr::spread(Q.stat,Stat,Value)
  Q.stat <- Q.stat[,col.order]
  
  Q.stat <- dplyr::filter(Q.stat, Year >= start.year & Year <= end.year)
  Q.stat <- dplyr::filter(Q.stat, Year >= min.year & Year <=max.year)
  Q.stat[Q.stat$Year %in% exclude.years,-1] <- NA
  
  
  ## Write the files
  #################################
  
  file.summary.csv <- NA
  if(write.summary.table){
    # write out the flow summary
    file.summary.csv <- file.path(report.dir, paste(station.name,"-period-record-summary.csv", sep=""))
    temp <- flow.sum
    temp[,2:ncol(flow.sum)] <- round(temp[,2:ncol(flow.sum)], csv.nddigits)
    utils::write.csv(temp,file=file.summary.csv, row.names=FALSE)
  }
  
  # See if you want to write out the summary tables?
  file.stat.csv <- NA
  if(write.table){
    
    # Write out the summary table for comparison to excel spreadsheet for calendar year
    file.stat.csv <- file.path(report.dir, paste(station.name,"-annual-summary-stat.csv", sep=""))
    temp <- Q.stat
    temp <- round(temp, csv.nddigits)
    utils::write.csv(temp,file=file.stat.csv, row.names=FALSE)
  }
  
  
  # Write out the annual summary table in transposed format?
  file.stat.trans.csv<- NA
  options(scipen = 999)
  Q.stat.trans <- tidyr::gather(Q.stat,Statistic,Value,-Year)
  Q.stat.trans.temp <- dplyr::mutate(Q.stat.trans,Value=round(Value,csv.nddigits)) # for writing to csv
  Q.stat.trans <- tidyr::spread(Q.stat.trans,Year,Value)
  
  if(write.transposed.table){
    file.stat.trans.csv <-file.path(report.dir,paste(station.name,"-annual-summary-stat-trans.csv",sep=""))
    Q.stat.trans.temp <- tidyr::spread(Q.stat.trans.temp,Year,Value)
    utils::write.csv(Q.stat.trans.temp, file=file.stat.trans.csv, row.names=FALSE)
  }
  
  # Write out the low flow summary
  # all mins and acutal dates
  Q.lowflows <- dplyr::select(Q.stat,
                              Year,dplyr::contains("MIN_0"),dplyr::contains("MINDOY_0"),
                              dplyr::contains("MIN_3"),dplyr::contains("MINDOY_3"))
  if (water.year) {
    Q.lowflows <- dplyr::mutate(Q.lowflows,
                                WY_MINDate_01Day = as.Date(WY_MINDOY_01Day-1, origin=as.Date(paste0(Year-1,"-10-01"))),
                                WY_MINDate_03Day = as.Date(WY_MINDOY_03Day-1, origin=as.Date(paste0(Year-1,"-10-01"))),
                                WY_MINDate_07Day = as.Date(WY_MINDOY_07Day-1, origin=as.Date(paste0(Year-1,"-10-01"))),
                                WY_MINDate_30Day = as.Date(WY_MINDOY_30Day-1, origin=as.Date(paste0(Year-1,"-10-01"))) )
  } else {
    Q.lowflows <- dplyr::mutate(Q.lowflows,
                                CY_MINDate_01Day = as.Date(CY_MINDOY_01Day-1, origin=as.Date(paste0(Year,"-01-01"))),
                                CY_MINDate_03Day = as.Date(CY_MINDOY_03Day-1, origin=as.Date(paste0(Year,"-01-01"))),
                                CY_MINDate_07Day = as.Date(CY_MINDOY_07Day-1, origin=as.Date(paste0(Year,"-01-01"))),
                                CY_MINDate_30Day = as.Date(CY_MINDOY_30Day-1, origin=as.Date(paste0(Year,"-01-01"))) )
  }
  Q.lowflows <- dplyr::select(Q.lowflows,Year,
                              dplyr::contains("_01"),dplyr::contains("_03"),
                              dplyr::contains("_07"),dplyr::contains("_30"))
  Q.lowflows <- dplyr::filter(Q.lowflows, Year >= start.year & Year <= end.year)
  Q.lowflows <- dplyr::filter(Q.lowflows, Year >= min.year & Year <=max.year)
  
  file.lowflow.csv <- NA
  if(write.lowflow.table){
    file.lowflow.csv <- file.path(report.dir,paste(station.name,"-lowflow-summary.csv",sep=""))
    temp <- Q.lowflows
    temp[,c(2,5,8,11)] = round(temp[,c(2,5,8,11)],3)
    utils::write.csv(temp, file=file.lowflow.csv, row.names=FALSE)
  }
  
  
  
  Q.zyp.trends <- NA
  if (!is.na(zyp.trending)) {
    
    Q.zyp.trends <- zyp::zyp.trend.dataframe(indat = Q.stat.trans,
                                             metadata.cols = 1,
                                             method=zyp.trending)
    ## ADD SOME METRICS
    
    if (write.zyp.table) {
      file.zyp.csv <- file.path(report.dir,paste(station.name,"-zyp-",zyp.trending,"-trends-results.csv",sep=""))
      temp <- Q.zyp.trends
      utils::write.csv(temp, file=file.zyp.csv, row.names=FALSE)
    }
    
    if (!is.na(write.zyp.plots)) {
      trends.plot.data <- tidyr::gather(Q.stat.trans,Year,Value,-1)
      trends.plot.data <- dplyr::mutate(trends.plot.data,Year=as.numeric(Year))
      trends.plot.data <- dplyr::mutate(trends.plot.data,
                                        Units="Discharge (cms)",
                                        Units=replace(Units, grepl("TOTALQ",Statistic), "Total Discharge (mil. cubic metres)"),
                                        Units=replace(Units, grepl("YIELD",Statistic), "Water Yield (mm)"),
                                        Units=replace(Units, grepl("FLOW",Statistic), "Day of Year"),
                                        Units=replace(Units, grepl("DAYS",Statistic), "Number of Days"))
     
      if (write.zyp.plots=="pdf") {
      file.zyp.pdf <- file.path(report.dir,paste(station.name,"-zyp-",zyp.trending,"-trends-results.pdf",sep=""))
      pdf(file = file.zyp.pdf,8,4)
      for (metric in unique(trends.plot.data$Statistic)){
        # Filter for metric
        trends.data.metric <- dplyr::filter(trends.plot.data,Statistic==metric)
        trends.results.metric <- dplyr::filter(Q.zyp.trends,Statistic==metric)
        #int <- trends.results.metric$intercept - trends.results.metric$trend * (Start_Year)
        # Plot each metric
        trends.plot <- ggplot2::ggplot(trends.data.metric,ggplot2::aes(x=Year,y=Value))+
          ggplot2::geom_point()+
          ggplot2::geom_line(alpha = 0.3) +
          ggplot2::ggtitle(paste0(metric,"   (Sig. = ",round(trends.results.metric$sig,3),")"))+
          ggplot2::ylab(trends.data.metric$Units[1])+
          ggplot2::xlab("Year")+
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey50", fill=NA, size=.1),
                         panel.grid = ggplot2::element_line(size=.2))
        if (water.year) {trends.plot <- trends.plot + ggplot2::xlab("Water Year")}
        
        # If sig. trend, plot trend
        if (trends.results.metric$sig < zyp.alpha & !is.na(trends.results.metric$sig)) {
          trends.plot <- trends.plot +
            ggplot2::geom_abline(slope = trends.results.metric$trend, intercept = (trends.results.metric$intercept - trends.results.metric$trend * (start.year)), colour="red")
        }
        plot(trends.plot)
      }
      dev.off()
      }
   
      ## PNG/JPG
      if (write.zyp.plots %in% c("png","jpeg","tiff","bmp")) {
      plot.dir <- paste(station.name,"-zyp-",zyp.trending,"-trends-results",sep = "")
      dir.create(plot.dir)
      for (metric in unique(trends.plot.data$Statistic)){
        # Filter for metric
        
        file.zyp <- file.path(report.dir,plot.dir,paste(metric,".",write.zyp.plots,sep=""))
        
        trends.data.metric <- dplyr::filter(trends.plot.data,Statistic==metric)
        trends.results.metric <- dplyr::filter(Q.zyp.trends,Statistic==metric)
        #int <- trends.results.metric$intercept - trends.results.metric$trend * (Start_Year)
        # Plot each metric
        trends.plot <- ggplot2::ggplot(trends.data.metric,ggplot2::aes(x=Year,y=Value))+
          ggplot2::geom_point()+
          ggplot2::geom_line(alpha = 0.3) +
          ggplot2::ggtitle(paste0(metric,"   (Sig. = ",round(trends.results.metric$sig,3),")"))+
          ggplot2::ylab(trends.data.metric$Units[1])+
          ggplot2::xlab("Year")+
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey50", fill=NA, size=.1),
                         panel.grid = ggplot2::element_line(size=.2))
        if (water.year) {trends.plot <- trends.plot + ggplot2::xlab("Water Year")}
        
        # If sig. trend, plot trend
        if (trends.results.metric$sig < zyp.alpha & !is.na(trends.results.metric$sig)) {
          trends.plot <- trends.plot +
            ggplot2::geom_abline(slope = trends.results.metric$trend, intercept = (trends.results.metric$intercept - trends.results.metric$trend * (start.year)), colour="red")
        }
        ggplot2::ggsave(filename =file.zyp,trends.plot,width=8,height=4)
      }
      }
    }
  }
  
  
  ## Do some plotting
  #################################
  

  
  
  
  return(list( "station name"= station.name,
               "year type"=ifelse(!water.year,"Calendar Year (Jan-Dec)","Water Year (Oct-Sep)"),
               "year range"=paste0(start.year," - ",end.year),
               exclude.years=exclude.years,
               Q.flow.summary=flow.sum,
               Q.stat.annual=Q.stat,
               Q.stat.annual.trans=Q.stat.trans,
               Q.lowflows=Q.lowflows,
               "zyp trending method"=zyp.trending,
               Q.zyp.trends=Q.zyp.trends,
               dates.missing.flows=dates.missing.flows,
               file.stat.csv=file.stat.csv,
               file.stat.trans.csv=file.stat.trans.csv,
               file.summary.csv=file.summary.csv,
               file.lowflow.csv=file.lowflow.csv,
               na.rm = na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function

