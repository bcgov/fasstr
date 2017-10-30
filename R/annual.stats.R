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
#' @param station.name
#' @param basin.area
#' @param flow.data
#' @param water.year
#' @param start.year
#' @param end.year
#' @param write.table Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,'-annual-cy-summary-stat.csv'))}.
#' @param write.transposed.table Should a file be created with the transposed of the annual statistics
#'    (both calendar and water year)?
#'    The file name will be  \code{file.path(report.dir,paste(station.name,'-annual-summary-stat-trans.csv'))}.
#' @param write.summary.table Should a file be created with a flow summary over the years between the
#'    start.year and end.year (inclusive). This summary includes number of days, number of missing values,
#'    mean, median, minimum, maximum, and standard deviation of \code{flow.data$Q}.
#'    The file name will be \code{file.path(report.dir, paste(station.name,"-period-record-summary.csv", sep=""))}.
#' @param write.lowflow.table Should a file be created with the minimum value of \code{flow.data$Q} and date the
#'    minimum occured.
#'    The file name will be \code{file.path(report.dir,paste(station.name,"-lowflow-summary.csv",sep=""))}
#' @param plot.stat.trend Should a file be created with plots of the statistics over the years
#'         between \code{start.year} and \code{end.year}.
#'    The file name will be \code{file.path(report.dir, paste(station.name,"-annual-trend.pdf",sep=""))}
#' @param plot.cumdepart Should a file be created with plots of the yearly and cumulative departures
#'    from the grand mean between \code{start.year} and \code{end.year}.
#'    The file name will be \code{file.path(report.dir, paste(station.name,"-cumulative departure.pdf",sep=""))}
#' @param report.dir
#' @param csv.nddigits
#' @param na.rm
#' @param debug
#'
#' @return A list with the following elements:
#'   \item{Q.flow.summary}{Data frame with flow summary.}
#'   \item{Q.stat.annual}{Data frame with summary statistics as listed at \code{\link{SummaryStatistics}}.}
#'   \item{Q.stat.annual.trans}{Data frame with transposed summary statistics as listed at \code{\link{SummaryStatistics}}.}
#'   \item{dates.missing.flow}{Data framw with dates of missing \code{flow.data$Q} between
#'          \code{start.year} and \code{end.year}}
#'   \item{file.stat.csv}{Object with file name of *.csv file with calendar year summary statistics.}
#'   \item{file.stat.trans.csv}{Object with file name of *.csv file with transposed summary statistics.}
#'   \item{file.stat.trend.pdf}{Object with file name of *.pdf file with plot of statistics over time.}
#'   \item{file.cumdepart.pdf}{Object with file name of *.pdf file with plot of yearly and cumulative departures.}
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
#
#   2017-01-30 CJS First edition

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

annual.stats <- function(station.name=NULL,
                         basin.area=NA, # if na, then all Yield values == NA
                         flow.data=NULL,
                         HYDAT=NULL,
                         water.year=FALSE, #create another for own water year????
                         start.year=NULL,
                         end.year=NULL,
                         zyp.trending=NA,
                         zyp.alpha=0.05,
                         write.table=FALSE,        # write out statistics on calendar year
                         write.transposed.table=FALSE,  # write out statistics in transposed format (cy & wy)
                         write.summary.table=FALSE, # write out a summary of period of record
                         write.lowflow.table=FALSE,      # write out a summary of low flows
                         plot.stat.trend=FALSE,        # should you plot all of stat trends?
                         plot.cumdepart=FALSE,         # plot cumulative departure curves
                         write.zyp.table=FALSE,
                         write.zyp.plots=FALSE,
                         report.dir=".",
                         na.rm=list(na.rm.global=FALSE),
                         csv.nddigits=3,              # decimal digits for csv files for statistics
                         debug=FALSE){
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
  if( is.null(flow.data) & is.null(HYDAT)){stop("Flow or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flow.data))  {stop("Must select either flow.data or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & is.null(station.name))  {stop("station.name parameter is required with flow.data parameter.")}
  if( is.null(HYDAT) & !is.character(station.name))  {stop("station.name parameter must be a character string.")}
  if( is.null(HYDAT) & length(station.name)>1)        {stop("station.name parameter cannot have length > 1")}
  if( !is.na(basin.area) & !is.numeric(basin.area))    {stop("basin.area parameter must be numeric")}
  if( length(basin.area)>1)        {stop("basin.area parameter cannot have length > 1")}
  if( is.null(HYDAT) & !is.data.frame(flow.data))         {stop("flow.data parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Q") %in% names(flow.data))){
    stop("flow.data dataframe doesn't contain the variables Date and Q.")}
  if( is.null(HYDAT) & !inherits(flow.data$Date[1], "Date")){
    stop("Date column in flow.data dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flow.data$Q))          {stop("Q column in flow.data dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flow.data$Q <0, na.rm=TRUE))   {stop('flow.data cannot have negative values - check your data')}
  if( !is.logical(write.table))  {stop("write.table parameter must be logical (TRUE/FALSE")}
  if( !is.logical(write.transposed.table)){stop("write.transposed.table parameter must be logical (TRUE/FALSE")}
  if( !is.logical(write.summary.table)){stop("write.summary.table parameter must be logical (TRUE/FALSE")}
  if( !is.logical(write.lowflow.table)){ stop("write.lowflow.table parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(plot.stat.trend)) {stop("plot.stat.trend parameter must be logical (TRUE/FALSE")}
  if( !is.logical(plot.cumdepart))  {stop("plot.cumdepart parameter must be logical (TRUE/FALSE")}
  if( !is.logical(water.year))  {stop("water.year parameter must be logical (TRUE/FALSE")}

  if( !is.na(zyp.trending) & !zyp.trending %in% c("yuepilon","zhang"))   {
    stop('zyp.trending parameter must have either "yuepilon" or "zhang" listed')}
  if( !is.logical(write.zyp.table))  {stop("write.zyp.table parameter must be logical (TRUE/FALSE")}
  if( !is.logical(write.zyp.plots))  {stop("write.zyp.plots parameter must be logical (TRUE/FALSE")}
  if( is.na(zyp.trending) & (write.zyp.table | write.zyp.plots) ) {
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

  if (!is.null(HYDAT)) {
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    if (is.null(station.name)) {station.name <- HYDAT}
    flow.data <- tidyhydat::DLY_FLOWS(STATION_NUMBER = HYDAT)
    flow.data <- dplyr::select(flow.data,Date,Q=Value)
  }

  # Filter for start and end years
  if (!is.numeric(start.year)) {start.year <- lubridate::year(min(flow.data$Date))-water.year}
  if (!is.numeric(end.year)) {end.year <- lubridate::year(max(flow.data$Date))}
  if(! (start.year <= end.year))    {stop("start.year parameter must be less than end.year parameter")}

  #  Generate all dates between min and max dates and merge with flow
  #  data frame to generate any dates that were missing.
  #  This will automatically generate NA for the days that were not in the file

  min.year <- as.numeric(lubridate::year(min(flow.data$Date)))-water.year
  max.year <- lubridate::year(max(flow.data$Date))
  temp <- data.frame(Date=seq(as.Date(paste(start.year-1,'01-01',sep='-'), "%Y-%m-%d"),
                              as.Date(paste(end.year  ,'12-31',sep='-'), '%Y-%m-%d'), 1))
  flow.data <- merge(flow.data, temp, all.y=TRUE)

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


  if(debug)browser()
  #

  ## Compute statistics on  year basis
  #################################

  Q.stat.lowflow <-   dplyr::summarize(dplyr::group_by(flow.data,AnalysisYear),
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
                                                             AnalysisDoY[which.min(Q.30DAvg)]))# Date of CY Min Daily Q
  Q.stat.lowflow <-   dplyr::rename(Q.stat.lowflow,Year=AnalysisYear)

  Q.stat.annual <-   dplyr::summarize(dplyr::group_by(flow.data,AnalysisYear),
                                      MIN_DAILY     = min (Q, na.rm=na.rm$na.rm.global),	    # CY Min Daily Q 	CY Min Daily Q
                                      MAX_DAILY	    = max (Q, na.rm=na.rm$na.rm.global),      # CY Max Daily Q
                                      MEAN_DAILY    = mean(Q, na.rm=na.rm$na.rm.global),     # CY Mean Discharge (Based on Daily avgs)
                                      MEDIAN_DAILY  = median(Q, na.rm=na.rm$na.rm.global),  # CY median Discharge (Based on Daily avgs)
                                      TOTALQ_DAILY  = MEAN_DAILY*length(Q)*60*60*24,    # Yearly sum of daily avg (cms) *60*60*24 # deal with missing values
                                      CUMQ_DAILY    = TOTALQ_DAILY/(60*60*24),      # Yearly sum of daily avg (cms) # deal with missing values
                                      YIELDMM_DAILY = TOTALQ_DAILY/basin.area/1000 ,
                                      Date_25P_CUMQ_DAILY = DayofYear[ match(TRUE, CumQ > 0.25  *CUMQ_DAILY)],
                                      Date_33P_CUMQ_DAILY = DayofYear[ match(TRUE, CumQ > 0.333 *CUMQ_DAILY)],
                                      Date_50P_CUMQ_DAILY = DayofYear[ match(TRUE, CumQ > 0.50  *CUMQ_DAILY)],
                                      Date_75P_CUMQ_DAILY = DayofYear[ match(TRUE, CumQ > 0.75  *CUMQ_DAILY)])
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
  Q.stat <- merge(Q.stat.lowflow,Q.stat.annual,by="Year",all = TRUE)
  Q.stat <- merge(Q.stat,Q.stat.seasons4,by="Year",all = TRUE)
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
    if(debug)browser()
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
  Q.lowflows <- tidyr::gather(Q.stat.lowflow,Stat,Value,2:ncol(Q.stat.lowflow))
  Q.lowflows <- dplyr::mutate(Q.lowflows,Stat=paste0(ifelse(water.year,paste("WY_"),paste("CY_")),Stat))
  col.order <- c("Year",unique(Q.lowflows$Stat)) # to keep the same order as merged when spread below
  Q.lowflows <- tidyr::spread(Q.lowflows,Stat,Value)

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

    if (write.zyp.plots) {
      file.zyp.pdf <- file.path(report.dir,paste(station.name,"-zyp-",zyp.trending,"-trends-results.pdf",sep=""))

      trends.plot.data <- tidyr::gather(Q.stat.trans,Year,Value,-1)
      trends.plot.data <- dplyr::mutate(trends.plot.data,Year=as.numeric(Year))
      pdf(file = file.zyp.pdf,8,5)
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
          ggplot2::ylab("Units")+
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
  }


  ## Do some plotting
  #################################

  # make a plot of the cumulative departures
  plot_cumdepart <- function(Q.stat, variable){
    # find the grand mean over all of the years ignoring all missing values
    grand.mean <- mean( Q.stat[, variable], na.rm=TRUE)
    plotdata <- Q.stat[,c("Year",variable)]
    plotdata <- plotdata[stats::complete.cases(plotdata),]   # remove all missing values
    plotdata$diff.from.mean <- plotdata[, variable] - grand.mean
    plotdata$cum.diff.from.mean <- cumsum(plotdata$diff.from.mean)
    plot1 <- ggplot2::ggplot(data=plotdata,  ggplot2::aes(x=Year, y=cum.diff.from.mean))+
      ggplot2::ggtitle(paste(station.name," - cumulative departure curve for ",variable,sep=""))+
      ggplot2::geom_hline(yintercept=0)+
      ggplot2::geom_segment( ggplot2::aes(x=Year, y=0, xend=Year, yend=diff.from.mean), size=2)+
      ggplot2::geom_line()+
      ggplot2::ylab("Departure from the mean")
    plot1
  }

  file.cumdepart.pdf <- NA
  if(plot.cumdepart){
    # cumulative departure plots
    file.cumdepart.pdf <- file.path(report.dir, paste(station.name,"-cumulative departure.pdf",sep=""))
    var.list <- c("CY_MEAN_DAILY_SW","WY_MEAN_DAILY_SW", "CY_YIELDMM_DAILY_SW", "WY_YIELDMM_DAILY_SW",
                  "ONDJFM_YIELDMM_DAILY_SW","AMJJAS_YIELDMM_DAILY_SW"   )
    grDevices::pdf(file=file.cumdepart.pdf, h=8, w=11)
    plyr::l_ply(var.list, function(x, Q.stat){
      plot1 <- plot_cumdepart(Q.stat, x)
      graphics::plot(plot1)
    },Q.stat=Q.stat)
    grDevices::dev.off()
  }

  # Make a plot of all of the statistics over time and save to a pdf file
  file.stat.trend.pdf <- NA
  if(plot.stat.trend){
    file.stat.trend.pdf <- file.path(report.dir, paste(station.name,"-annual-trend.pdf",sep=""))
    grDevices::pdf(file.stat.trend.pdf, h=8, w=11)

    plot_trend <- function(plotdata, select){
      x <- plotdata[select,]
      myplot <- ggplot2::ggplot(data=x,  ggplot2::aes(x=Year, y=Value, group=Statistic, color=Statistic, linetype=Statistic))+
        ggplot2::ggtitle(paste(station.name, " - Trend for ", x$statgroup[1]))+
        ggplot2::geom_point()+
        ggplot2::geom_line()+
        ggplot2::xlab("Year")+
        ggplot2::ylab(x$Ylabel[1])
      if(x$transform[1] == 'log'){
        myplot <- myplot +
          ggplot2::ylab(paste("log(",x$Ylabel[1],")",sep=""))+
          ggplot2::scale_y_continuous(trans='log10')
      }
      graphics::plot(myplot)
    }

    plotdata <- reshape2::melt(Q.stat, id.var="Year", variable.name="Statistic", value.name="Value")
    plotdata$statgroup <- NA
    plotdata$transform <- ""
    plotdata$Ylabel    <- 'Value'

    # yearly minimums. Remove redundancy between CY_MIN_01DAY_SW and CY_MIN+DAILY_SW if a 01 rolling average
    # has been requested.
    set.annual.min  <- grepl("^CY_MIN_", plotdata$Statistic) & !grepl("DAILY", plotdata$Statistic)
    plotdata$statgroup [set.annual.min] <- 'CY Annual Minimums'
    plotdata$Ylabel    [set.annual.min] <- 'Flow (cms)'
    plot_trend(plotdata, set.annual.min)

    # more plots on the water year basis
    set.annual.min  <- grepl("^WY_MIN_", plotdata$Statistic) & !grepl("DAILY", plotdata$Statistic)
    plotdata$statgroup [set.annual.min] <- 'WY Annual Minimums'
    plotdata$Ylabel    [set.annual.min] <- 'Flow (cms)'
    plot_trend(plotdata, set.annual.min)

    set.annual.mindoy  <- grepl("^CY_MINDOY", plotdata$Statistic)
    plotdata$statgroup [set.annual.mindoy] <- 'Annual Day of CY for Minimums'
    plotdata$Ylabel    [set.annual.mindoy] <- 'Day into year'
    plot_trend(plotdata, set.annual.mindoy)

    # make a plot for each 3 month period. Include the daily min in period with similar range.
    annual.avg.min <- mean( plotdata$Value[ plotdata$Statistic=='CY_MIN_DAILY_SW'], na.rm=TRUE)
    set.month.min      <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[1:3],"_MIN",sep=""))
    if(range(plotdata$Value[set.month.min],na.rm=TRUE)[1] < annual.avg.min  &
       range(plotdata$Value[set.month.min],na.rm=TRUE)[2] > annual.avg.min){
      set.month.min = set.month.min | grepl("^CY_MIN_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.min] <- 'Monthly Minimums - JFM'
    plotdata$transform [set.month.min] <- "log"
    plotdata$Ylabel    [set.month.min] <- "Flow (cms)"
    plot_trend(plotdata, set.month.min)

    set.month.min      <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[4:6],"_MIN",sep=""))
    if(range(plotdata$Value[set.month.min],na.rm=TRUE)[1] < annual.avg.min  &
       range(plotdata$Value[set.month.min],na.rm=TRUE)[2] > annual.avg.min){
      set.month.min = set.month.min | grepl("^CY_MIN_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.min] <- 'Monthly Minimums - AMJ'
    plotdata$transform [set.month.min] <- "log"
    plotdata$Ylabel    [set.month.min] <- "Flow (cms)"
    plot_trend(plotdata, set.month.min)

    set.month.min      <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[7:9],"_MIN",sep=""))
    if(range(plotdata$Value[set.month.min],na.rm=TRUE)[1] < annual.avg.min  &
       range(plotdata$Value[set.month.min],na.rm=TRUE)[2] > annual.avg.min){
      set.month.min = set.month.min | grepl("^CY_MIN_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.min] <- 'Monthly Minimums - JAS'
    plotdata$transform [set.month.min] <- "log"
    plotdata$Ylabel    [set.month.min] <- "Flow (cms)"
    plot_trend(plotdata, set.month.min)

    set.month.min      <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[10:20],"_MIN",sep=""))
    if(range(plotdata$Value[set.month.min],na.rm=TRUE)[1] < annual.avg.min  &
       range(plotdata$Value[set.month.min],na.rm=TRUE)[2] > annual.avg.min){
      set.month.min = set.month.min | grepl("^CY_MIN_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.min] <- 'Monthly Minimums - OND'
    plotdata$transform [set.month.min] <- "log"
    plotdata$Ylabel    [set.month.min] <- "Flow (cms)"
    plot_trend(plotdata, set.month.min)



    # make a plot for each 3 month period. Include the daily max in period with similar range.
    annual.avg.max <- mean( plotdata$Value[ plotdata$Statistic=='CY_MAX_DAILY_SW'], na.rm=TRUE)
    set.month.max     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[1:3],"_MAX",sep=""))
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.max = set.month.max | grepl("^CY_MAX_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.max] <- 'Monthly/Annual Maximums  -  JFM'
    plotdata$transform [set.month.max] <- "log"
    plotdata$Ylabel    [set.month.max] <- "Flow (cms)"
    plot_trend(plotdata, set.month.max)

    set.month.max     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[4:6],"_MAX",sep=""))
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.max = set.month.max | grepl("^CY_MAX_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.max] <- 'Monthly/Annual Maximums  -  AMJ'
    plotdata$transform [set.month.max] <- "log"
    plotdata$Ylabel    [set.month.max] <- "Flow (cms)"
    plot_trend(plotdata, set.month.max)

    set.month.max     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[7:9],"_MAX",sep=""))
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.max = set.month.max | grepl("^CY_MAX_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.max] <- 'Monthly/Annual Maximums  -  JAS'
    plotdata$transform [set.month.max] <- "log"
    plotdata$Ylabel    [set.month.max] <- "Flow (cms)"
    plot_trend(plotdata, set.month.max)

    set.month.max     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[10:12],"_MAX",sep=""))
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.max = set.month.max | grepl("^CY_MAX_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.max] <- 'Monthly/Annual Maximums  -  OND'
    plotdata$transform [set.month.max] <- "log"
    plotdata$Ylabel    [set.month.max] <- "Flow (cms)"
    plot_trend(plotdata, set.month.max)


    # make a plot for each 3 month period. Include the daily max in period with similar range.
    annual.avg.mean <- mean( plotdata$Value[ plotdata$Statistic=='CY_MEAN_DAILY_SW'], na.rm=TRUE)
    set.month.mean     <- substr(plotdata$Statistic,1,8) %in% toupper(paste(month.abb[1:3],"_MEAN",sep=""))
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.mean = set.month.mean | grepl("^CY_MEAN_DAILY_SW", plotdata$Statistic)   }
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.mean = set.month.mean | grepl("^CY_MEDIAN_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.mean] <- 'Monthly/Annual Means/Annual Medians  -  JFM'
    plotdata$transform [set.month.mean] <- "log"
    plotdata$Ylabel    [set.month.mean] <- "Flow (cms)"
    plot_trend(plotdata, set.month.mean)

    set.month.mean     <- substr(plotdata$Statistic,1,8) %in% toupper(paste(month.abb[4:6],"_MEAN",sep=""))
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.mean = set.month.mean | grepl("^CY_MEAN_DAILY_SW", plotdata$Statistic)   }
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.mean = set.month.mean | grepl("^CY_MEDIAN_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.mean] <- 'Monthly/Annual Means/Annual Medians  -  AMJ'
    plotdata$transform [set.month.mean] <- "log"
    plotdata$Ylabel    [set.month.mean] <- "Flow (cms)"
    plot_trend(plotdata, set.month.mean)

    set.month.mean     <- substr(plotdata$Statistic,1,8) %in% toupper(paste(month.abb[7:9],"_MEAN",sep=""))
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.mean = set.month.mean | grepl("^CY_MEAN_DAILY_SW", plotdata$Statistic)   }
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.mean = set.month.mean | grepl("^CY_MEDIAN_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.mean] <- 'Monthly/Annual Means/Annual Medians  -  JAS'
    plotdata$transform [set.month.mean] <- "log"
    plotdata$Ylabel    [set.month.mean] <- "Flow (cms)"
    plot_trend(plotdata, set.month.mean)

    set.month.mean     <- substr(plotdata$Statistic,1,8) %in% toupper(paste(month.abb[10:12],"_MEAN",sep=""))
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.mean = set.month.mean | grepl("^CY_MEAN_DAILY_SW", plotdata$Statistic)   }
    if(range(plotdata$Value[set.month.max],na.rm=TRUE)[1] < annual.avg.max  &
       range(plotdata$Value[set.month.max],na.rm=TRUE)[2] > annual.avg.max){
      set.month.mean = set.month.mean | grepl("^CY_MEDIAN_DAILY_SW", plotdata$Statistic)   }
    plotdata$statgroup [set.month.mean] <- 'Monthly/Annual Means/Annual Medians  -  OND'
    plotdata$transform [set.month.mean] <- "log"
    plotdata$Ylabel    [set.month.mean] <- "Flow (cms)"
    plot_trend(plotdata, set.month.mean)


    set.month.p50     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[1:3],"_P50",sep=""))
    plotdata$statgroup [set.month.p50] <- 'Monthly P50  -  JFM'
    plotdata$transform [set.month.p50] <- "log"
    plotdata$Ylabel    [set.month.p50] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p50)

    set.month.p50     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[4:6],"_P50",sep=""))
    plotdata$statgroup [set.month.p50] <- 'Monthly P50  -  AMJ'
    plotdata$transform [set.month.p50] <- "log"
    plotdata$Ylabel    [set.month.p50] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p50)

    set.month.p50     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[7:9],"_P50",sep=""))
    plotdata$statgroup [set.month.p50] <- 'Monthly P50  -  JAS'
    plotdata$transform [set.month.p50] <- "log"
    plotdata$Ylabel    [set.month.p50] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p50)

    set.month.p50     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[10:12],"_P50",sep=""))
    plotdata$statgroup [set.month.p50] <- 'Monthly P50  - OND'
    plotdata$transform [set.month.p50] <- "log"
    plotdata$Ylabel    [set.month.p50] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p50)


    set.month.p20     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[1:3],"_P20",sep=""))
    plotdata$statgroup [set.month.p20] <- 'Monthly P20  -  JFM'
    plotdata$transform [set.month.p20] <- "log"
    plotdata$Ylabel    [set.month.p20] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p20)

    set.month.p20     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[4:6],"_P20",sep=""))
    plotdata$statgroup [set.month.p20] <- 'Monthly P20  -  AMJ'
    plotdata$transform [set.month.p20] <- "log"
    plotdata$Ylabel    [set.month.p20] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p20)

    set.month.p20     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[7:9],"_P20",sep=""))
    plotdata$statgroup [set.month.p20] <- 'Monthly P20  -  JAS'
    plotdata$transform [set.month.p20] <- "log"
    plotdata$Ylabel    [set.month.p20] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p20)

    set.month.p20     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[10:12],"_P20",sep=""))
    plotdata$statgroup [set.month.p20] <- 'Monthly P20  -  OND'
    plotdata$transform [set.month.p20] <- "log"
    plotdata$Ylabel    [set.month.p20] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p20)


    set.month.p10     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[1:3],"_P10",sep=""))
    plotdata$statgroup [set.month.p10] <- 'Monthly P10  -  JFM'
    plotdata$transform [set.month.p10] <- "log"
    plotdata$Ylabel    [set.month.p10] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p10)

    set.month.p10     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[4:6],"_P10",sep=""))
    plotdata$statgroup [set.month.p10] <- 'Monthly P10  -  AMJ'
    plotdata$transform [set.month.p10] <- "log"
    plotdata$Ylabel    [set.month.p10] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p10)

    set.month.p10     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[7:9],"_P10",sep=""))
    plotdata$statgroup [set.month.p10] <- 'Monthly P10  -  JAS'
    plotdata$transform [set.month.p10] <- "log"
    plotdata$Ylabel    [set.month.p10] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p10)

    set.month.p10     <- substr(plotdata$Statistic,1,7) %in% toupper(paste(month.abb[10:12],"_P10",sep=""))
    plotdata$statgroup [set.month.p10] <- 'Monthly P10  -  OND'
    plotdata$transform [set.month.p10] <- "log"
    plotdata$Ylabel    [set.month.p10] <- "Flow (cms)"
    plot_trend(plotdata, set.month.p10)


    set.yieldmm     <- grepl("YIELDMM", plotdata$Statistic) &
      (substr(plotdata$Statistic,1,3) %in% c("AMJ","JAS"))
    plotdata$statgroup [set.yieldmm] <- 'Water Yield (Spring/Summer)'
    plotdata$transform [set.yieldmm] <- "log"
    plotdata$Ylabel    [set.yieldmm] <- 'Yield (MM)'
    plot_trend(plotdata, set.yieldmm)

    set.yieldmm     <- grepl("YIELDMM", plotdata$Statistic) &
      (substr(plotdata$Statistic,1,3) %in% c("JFM","OND"))
    plotdata$statgroup [set.yieldmm] <- 'Water Yield (Fall/Winter)'
    plotdata$transform [set.yieldmm] <- "log"
    plotdata$Ylabel    [set.yieldmm] <- 'Yield (MM)'
    plot_trend(plotdata, set.yieldmm)

    set.yieldmm     <- grepl("YIELDMM", plotdata$Statistic) &
      (substr(plotdata$Statistic,1,2) %in% c("CY","WY"))
    plotdata$statgroup [set.yieldmm] <- 'Water Yield (Calendar and Water Year)'
    plotdata$transform [set.yieldmm] <- "log"
    plotdata$Ylabel    [set.yieldmm] <- 'Yield (MM)'
    plot_trend(plotdata, set.yieldmm)


    set.totalq     <- grepl("TOTALQ", plotdata$Statistic) &
      (substr(plotdata$Statistic,1,3) %in% c("AMJ","JAS"))
    plotdata$statgroup [set.totalq] <- 'Total Q  -  Spring/Summer'
    plotdata$transform [set.totalq] <- "log"
    plotdata$Ylabel    [set.totalq] <- 'Total Q (cms)'
    plot_trend(plotdata, set.totalq)
    set.totalq     <- grepl("TOTALQ", plotdata$Statistic) &
      (substr(plotdata$Statistic,1,3) %in% c("JFM","OND"))
    plotdata$statgroup [set.totalq] <- 'Total Q   - Fall/Winter'
    plotdata$transform [set.totalq] <- "log"
    plotdata$Ylabel    [set.totalq] <- 'Total Q (cms)'
    plot_trend(plotdata, set.totalq)
    set.totalq     <- grepl("TOTALQ", plotdata$Statistic) &
      (substr(plotdata$Statistic,1,2) %in% c("CY","WY"))
    plotdata$statgroup [set.totalq] <- 'Total Q   -  Calendar and Water Year'
    plotdata$transform [set.totalq] <- "log"
    plotdata$Ylabel    [set.totalq] <- 'Total Q (cms)'
    plot_trend(plotdata, set.totalq)

    set.annual.date     <- grepl("CY_Date", plotdata$Statistic)
    plotdata$statgroup [set.annual.date] <- 'Annual Day of CY for Total Discharge Milestones'
    plotdata$Ylabel    [set.annual.date] <- "Day into the year"
    plot_trend(plotdata, set.annual.date)

    set.annual.mindoy  <- grepl("^WY_MINDOY", plotdata$Statistic)
    plotdata$statgroup [set.annual.mindoy] <- 'Annual Day of WY for Minimums'
    plotdata$Ylabel    [set.annual.mindoy] <- 'Day into year'
    plot_trend(plotdata, set.annual.mindoy)

    set.min.max.mean <- grepl("WY_MIN_",   plotdata$Statistic) |
      grepl("WY_MAX_",   plotdata$Statistic) |
      grepl("WY_MEAN_",  plotdata$Statistic) |
      grepl("WY_MEDIAN_",plotdata$Statistic)
    plotdata$statgroup [set.min.max.mean] <- 'WY Minimum, Maximum, Mean, Median'
    plotdata$transform [set.min.max.mean] <- "log"
    plotdata$Ylabel    [set.min.max.mean] <- "Flow (cms)"
    plot_trend(plotdata, set.min.max.mean)

    set.annual.date     <- grepl("WY_Date", plotdata$Statistic)
    plotdata$statgroup [set.annual.date] <- 'Annual Day of WY for Total Discharge Milestones'
    plotdata$Ylabel    [set.annual.date] <- "Day into the year"
    plot_trend(plotdata, set.annual.date)

    set.outside         <- grepl("CY_N_", plotdata$Statistic)
    plotdata$statgroup [set.outside] <- 'Days Outside 25/75 Daily Percentiles'
    plotdata$Ylabel    [set.outside] <- 'Days'
    plot_trend(plotdata, set.outside)

    set.misc <- is.na(plotdata$statgroup)
    plotdata$statgroup[ set.misc] <- "Misc Statistics"
    plotdata$Ylabel   [ set.misc] <- 'Cumulative Flow (cms)'
    plot_trend(plotdata, set.misc)

    grDevices::dev.off()
  }
  return(list( "station name"= station.name,
               "year type"=ifelse(!water.year,"Calendar Year (Jan-Dec)","Water Year (Oct-Sep)"),
               "year range"=paste0(start.year," - ",end.year),
               Q.flow.summary=flow.sum,
               Q.stat.annual=Q.stat,
               Q.stat.annual.trans=Q.stat.trans,
               "zyp trending method"=zyp.trending,
               Q.zyp.trends=Q.zyp.trends,
               dates.missing.flows=dates.missing.flows,
               file.stat.csv=file.stat.csv,
               file.stat.trans.csv=file.stat.trans.csv,
               file.stat.trend.pdf=file.stat.trend.pdf,
               file.cumdepart.pdf=file.cumdepart.pdf,
               file.summary.csv=file.summary.csv,
               file.lowflow.csv=file.lowflow.csv,
               na.rm = na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function

