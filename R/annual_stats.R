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
#' Streamflow data can be supplied through the \code{flowdata} parameter or extracted from a 
#' HYDAT database using the tidyhydat package and \code{HYDAT} parameter.
#' and (optionally) saves the results in *.csv and *.pdf files.
#'
#' @param station_name Character. Identifier name of the stream or station. Required when supplying data through \code{flowdata}.
#'    The station name will be used in plots and filenames of exported tables and plot. If using \code{HYDAT} to supply
#'    data and no \code{station_name} is provided, the HYDAT station number will be the identifier.
#' @param flowdata Dataframe. A dataframe of daily mean streamflow data used to calculate the annual statistics. 
#'    Two columns are required: a 'Date' column with dates formatted YYYY-MM-DD and a 'Q' column with the daily 
#'    mean streamflow values in units of cubic metres per second. \code{flowdata} not required if \code{HYDAT} is used.
#' @param HYDAT Character. A HYDAT station number (e.g. "08NM116") of which to extract daily streamflow data from the HYDAT database.
#'    tidyhydat package and a downloaded SQLite HYDAT required.
#' @param water_year Logical. Set to \code{TRUE} if data should be summarized by water year (Oct-Sep) instead of the
#'    default calendar year (Jan-Dec) (\code{water_year=FALSE}). Water years are designated by the year which they end in
#'    (e.g. water year 2000 start on 1 Oct 1999 and ends on 30 Sep 2000).
#' @param start_year Numeric. The first year of streamflow data to analyze. If unset, the default \code{start_year} is the first
#'    year of the data provided.
#' @param end_year Numeric. The last year of streamflow data to analyze. If unset, the default \code{end_year} is the last
#'    year of the data provided.
#' @param excluded.years Numeric. List of years to exclude final results from. Ex. 1990 or c(1990,1995:2000).    
#' @param basin_area Numeric. The upstream drainage basin area (in sq. km) of the station. Used to calculate runoff yields (mm).
#'    If no value provided, yield calculations will result in NA values.
#' @param write_table Logical. Should a file be created with the calendar year computed percentiles?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-cy-summary-stat.csv'))}.
#' @param write_transposed_table Logical. Should a file be created with the transposed of the annual statistics
#'    (both calendar and water year)?
#'    The file name will be  \code{file.path(report_dir,paste(station_name,'-annual-summary-stat-trans.csv'))}.
#' @param write_summary_table Logical. Should a file be created with a flow summary over the years between the
#'    start_year and end_year (inclusive). This summary includes number of days, number of missing values,
#'    mean, median, minimum, maximum, and standard deviation of \code{flowdata$Q}.
#'    The file name will be \code{file.path(report_dir, paste(station_name,"-period-record-summary.csv", sep=""))}.
#' @param write_lowflow_table Logical. Should a file be created with the minimum value of \code{flowdata$Q} and date the
#'    minimum occured.
#'    The file name will be \code{file.path(report_dir,paste(station_name,"-lowflow-summary.csv",sep=""))}
#' @param report_dir Character. Folder location of where to write tables and plots. Default is the working directory.
#' @param table_nddigits Numeric. Number of significant digits to round the results in the written tables. Default is 3.
#' @param na.rm TBD
#'
#' @return A list with the following elements:
#'   \item{Qsummary}{Data frame with flow summary.}
#'   \item{Qstat_annual}{Data frame with summary statistics as listed at \code{\link{SummaryStatistics}}.}
#'   \item{Qstat_annual_tpose}{Data frame with transposed summary statistics as listed at \code{\link{SummaryStatistics}}.}
#'   \item{dates_missing_flows}{Data framw with dates of missing \code{flowdata$Q} between
#'          \code{start_year} and \code{end_year}}
#'   \item{file_Qstat_table}{Object with file name of *.csv file with calendar year summary statistics.}
#'   \item{file_Qstat_tpose_table}{Object with file name of *.csv file with transposed summary statistics.}
#'   \item{file_Qsummary_table}{Object with file name of *.csv file with summary statistics.}
#'   \item{file_Qlowflow_table}{Object with file name of *.csv file with low flow summary statistics.}
#'   \item{na.rm}{Missing value flags.}
#'   \item{Version}{Version of this function.}
#'   \item{Date}{Date function was run.}
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

annual_stats <- function(station_name=NULL,
                         flowdata=NULL,
                         HYDAT=NULL,
                         water_year=FALSE, #create another for own water year????
                         start_year=NULL,
                         end_year=NULL,
                         exclude_years=NULL, # list of stations
                         basin_area=NA, # if na, then all Yield values == NA
                         zyp_trending=NA,
                         zyp_alpha=0.05,
                         write_table=FALSE,        # write out statistics on calendar year
                         write_transposed_table=FALSE,  # write out statistics in transposed format (cy & wy)
                         write_summary_table=FALSE, # write out a summary of period of record
                         write_lowflow_table=FALSE,      # write out a summary of low flows
                         write_zyp_table=FALSE,
                         write_zyp_plots=NA,  # else = c("pdf","png","jpeg","tiff","bmp")
                         report_dir=".",
                         na.rm=list(na.rm.global=FALSE),
                         table_nddigits=3){              # decimal digits for csv files for statistics
  
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
  
  if( is.null(flowdata) & is.null(HYDAT)) {stop("flowdata or HYDAT parameters must be set")}
  if( !is.null(HYDAT) & !is.null(flowdata))  {stop("Must select either flowdata or HYDAT parameters, not both.")}
  if( is.null(HYDAT) & is.null(station_name))  {stop("station_name parameter is required with flowdata parameter.")}
  if( is.null(HYDAT) & !is.character(station_name))  {stop("station_name parameter must be a character string.")}
  if( is.null(HYDAT) & length(station_name)>1)        {stop("station_name parameter cannot have length > 1")}
  if( is.null(HYDAT) & !is.data.frame(flowdata))         {stop("flowdata parameter is not a data frame.")}
  if( is.null(HYDAT) & !all(c("Date","Q") %in% names(flowdata))){
    stop("flowdata dataframe doesn't contain the variables Date and Q.")}
  if( is.null(HYDAT) & !inherits(flowdata$Date[1], "Date")){
    stop("Date column in flowdata dataframe is not a date.")}
  if( is.null(HYDAT) & !is.numeric(flowdata$Q))          {stop("Q column in flowdata dataframe is not numeric.")}
  if( is.null(HYDAT) & any(flowdata$Q <0, na.rm=TRUE))   {stop('flowdata cannot have negative values - check your data')}
  
  if( !is.logical(water_year))  {stop("water_year parameter must be logical (TRUE/FALSE)")}
  if( !is.null(exclude_years) & !is.numeric(exclude_years)) {stop("List of years must be numeric. Ex. 1999 or c(1999,2000)")}
  
  if( !is.na(basin_area) & !is.numeric(basin_area))    {stop("basin_area parameter must be numeric")}
  if( length(basin_area)>1)        {stop("basin_area parameter cannot have length > 1")}
  
  if( !is.logical(write_table))  {stop("write_table parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_transposed_table)){stop("write_transposed_table parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_summary_table)){stop("write_summary_table parameter must be logical (TRUE/FALSE)")}
  if( !is.logical(write_lowflow_table)){ stop("write_lowflow_table parameter must be logical (TRUE/FALSE)")}
  
  if( !is.na(zyp_trending) & !zyp_trending %in% c("yuepilon","zhang"))   {
    stop('zyp_trending parameter must have either "yuepilon" or "zhang" listed')}
  if( !is.logical(write_zyp_table))  {stop("write_zyp_table parameter must be logical (TRUE/FALSE")}
  if( !is.na(write_zyp_plots) & !write_zyp_plots %in% c("pdf","png","jpeg","tiff","bmp"))  {
    stop("write_zyp_plots parameter must be logical (TRUE/FALSE)")}
  if( is.na(zyp_trending) & (write_zyp_table | !is.na(write_zyp_plots)) ) {
    stop('zyp_trending parameter method must be selected to write results')}
  if( !is.numeric(zyp_alpha))  { stop("zyp_alpha parameter needs to be numeric")}
  
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
    if (!HYDAT %in% tidyhydat::allstations$STATION_NUMBER) {stop("Station in 'HYDAT' parameter does not exist.")}
    if (is.null(station_name)) {station_name <- HYDAT}
    flowdata <- tidyhydat::DLY_FLOWS(STATION_NUMBER = HYDAT)
    flowdata <- dplyr::select(flowdata,Date,Q=Value)
  }
  
  
  # If start/end years are not select, set them as the min/max dates
  min_year <- lubridate::year(min(flowdata$Date))-water_year
  max_year <- lubridate::year(max(flowdata$Date))
  if (!is.numeric(start_year)) {start_year <- min_year}
  if (!is.numeric(end_year)) {end_year <- max_year}
  if(! (start_year <= end_year))    {stop("start_year parameter must be less than end_year parameter")}
  
  
  #  Generate all dates between min and max dates and merge with flow
  #  data frame to generate any dates that were missing.
  #  This will automatically generate NA for the days that were not in the file
  flowdata <- merge(flowdata, 
                    data.frame(Date=seq(as.Date(paste(start_year-1,'01-01',sep='-'), "%Y-%m-%d"),
                                        as.Date(paste(end_year  ,'12-31',sep='-'), '%Y-%m-%d'), 1)),
                    all.y=TRUE)
  
  # CREATE DATE VARIABLES AND ROLLING MEANS/SUMS ==============================
  
  flowdata$Year  <- lubridate::year(flowdata$Date)
  flowdata$Month  <- lubridate::month(flowdata$Date)
  flowdata$MonthText <- month.abb[flowdata$Month]
  flowdata$WaterYear <- as.numeric(ifelse(flowdata$Month>=10,flowdata$Year+1,flowdata$Year))
  flowdata$DayofYear <- lubridate::yday(flowdata$Date)
  flowdata$WaterDoY <- ifelse(flowdata$Month<10,flowdata$DayofYear+92,
                              ifelse((as.Date(with(flowdata, paste(Year+1,01,01,sep="-")),"%Y-%m-%d")-as.Date(with(flowdata, paste(Year,01,01,sep="-")),"%Y-%m-%d"))==366,
                                     flowdata$DayofYear-274,
                                     flowdata$DayofYear-273))
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
    flowdata$AnalysisDoY <- flowdata$WaterDoY
  }  else {
    flowdata$AnalysisYear <- flowdata$Year
    flowdata$AnalysisDoY <- flowdata$DayofYear
  }
  
  #  Compute the 3, 7, and 30 day rolling average values
  flowdata$Q03DAvg <- zoo::rollapply( flowdata$Q,  3, mean, fill=NA, align="right")
  flowdata$Q07DAvg <- zoo::rollapply( flowdata$Q,  7, mean, fill=NA, align="right")
  flowdata$Q30DAvg <- zoo::rollapply( flowdata$Q, 30, mean, fill=NA, align="right")
  
  # compuate the annual cumulative total
  flowdata <- dplyr::mutate(dplyr::group_by(flowdata,AnalysisYear),CumQ=cumsum(Q))
  
  
  
  # CALCULATE STATS ==============================
  
  ## Compute statistics on 2 seasons (must be water year) so calc'd first before filtering for selected years
  Qstat_2seasons <- dplyr::summarize(dplyr::group_by(flowdata,WaterYear,Seasons2),
                                     TOTALQ_DAILY=mean(Q, na.rm=na.rm$na.rm.global)*length(Q)*60*60*24,
                                     YIELDMM_DAILY=mean(Q, na.rm=na.rm$na.rm.global)*length(Q)*60*60*24 /basin_area/1000)
  Qstat_2seasons <- tidyr::gather(Qstat_2seasons,stat,value,3:4)
  Qstat_2seasons <- dplyr::mutate(Qstat_2seasons,title=paste0(Seasons2,"_",stat))
  Qstat_2seasons <- dplyr::select(Qstat_2seasons,-Seasons2,-stat)
  Qstat_2seasons <- tidyr::spread(Qstat_2seasons,title,value)
  Qstat_2seasons <- dplyr::rename(Qstat_2seasons,Year=WaterYear)
  Qstat_2seasons <- dplyr::filter(Qstat_2seasons, Year >= start_year & Year <= end_year)
  
  
  # FILTER DATA FOR SELECTED YEARS FOR REMAINDER OF CALCS
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flowdata <- dplyr::filter(flowdata, AnalysisYear >= min_year & AnalysisYear <=max_year)
  flowdata[flowdata$AnalysisYear %in% exclude_years,-1] <- NA
  
  
  
  #  which dates have missing flows.
  dates_missing_flows <- flowdata$Date[ is.na(flowdata$Q)]
  
  #  simple summary statistics
  Qsummary <-   dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear),
                                 n_days   = length(Year),
                                 n_Q      = sum (!is.na(Q)),
                                 n_missing_Q = sum ( is.na(Q)),
                                 min_Q    = min (Q,          na.rm=na.rm$na.rm.global),
                                 max_Q    = max (Q,          na.rm=na.rm$na.rm.global),
                                 mean_Q   = mean(Q,          na.rm=na.rm$na.rm.global),
                                 median_Q = stats::median(Q, na.rm=na.rm$na.rm.global),
                                 sd_Q     = stats::sd  (Q,   na.rm=na.rm$na.rm.global)
  )
  Qsummary <-   dplyr::rename(Qsummary,Year=AnalysisYear)
  
  
  ## Compute statistics on  year basis
  
  Qstat_annual <-   dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear),
                                     MIN_01Day    = min(Q, na.rm=na.rm$na.rm.global),	     
                                     MINDOY_01Day = ifelse(is.na(MIN_01Day),NA,
                                                           AnalysisDoY[which(Q==MIN_01Day)]),
                                     MIN_03Day    = min(Q03DAvg, na.rm=na.rm$na.rm.global),	    
                                     MINDOY_03Day = ifelse(is.na(MIN_03Day),NA,
                                                           AnalysisDoY[which(Q03DAvg==MIN_03Day)]),
                                     MIN_07Day    = min(Q07DAvg, na.rm=na.rm$na.rm.global),	     
                                     MINDOY_07Day = ifelse(is.na(MIN_07Day),NA,
                                                           AnalysisDoY[which(Q07DAvg==MIN_07Day)]),
                                     MIN_30Day    = min(Q30DAvg, na.rm=na.rm$na.rm.global),	     
                                     MINDOY_30Day = ifelse(is.na(MIN_30Day),NA,
                                                           AnalysisDoY[which(Q30DAvg==MIN_30Day)]),
                                     MIN_DAILY     = min (Q, na.rm=na.rm$na.rm.global),	    # CY Min Daily Q 	CY Min Daily Q
                                     MAX_DAILY	    = max (Q, na.rm=na.rm$na.rm.global),      # CY Max Daily Q
                                     MEAN_DAILY    = mean(Q, na.rm=na.rm$na.rm.global),     # CY Mean Discharge (Based on Daily avgs)
                                     MEDIAN_DAILY  = median(Q, na.rm=na.rm$na.rm.global),  # CY median Discharge (Based on Daily avgs)
                                     TOTALQ_DAILY  = MEAN_DAILY*length(Q)*60*60*24,    # Yearly sum of daily avg (cms) *60*60*24 # deal with missing values
                                     YIELDMM_DAILY = TOTALQ_DAILY/basin_area/1000 ,
                                     Date_25P_FLOW_DAILY = DayofYear[ match(TRUE, CumQ > 0.25  *TOTALQ_DAILY/(60*60*24))],
                                     Date_33P_FLOW_DAILY = DayofYear[ match(TRUE, CumQ > 0.333 *TOTALQ_DAILY/(60*60*24))],
                                     Date_50P_FLOW_DAILY = DayofYear[ match(TRUE, CumQ > 0.50  *TOTALQ_DAILY/(60*60*24))],
                                     Date_75P_FLOW_DAILY = DayofYear[ match(TRUE, CumQ > 0.75  *TOTALQ_DAILY/(60*60*24))])
  Qstat_annual <-   dplyr::rename(Qstat_annual,Year=AnalysisYear)
  
  ## Compute statistics on 4 seasons
  Qstat_4seasons <- dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear,Seasons4),
                                     TOTALQ_DAILY=mean(Q, na.rm=na.rm$na.rm.global)*length(Q)*60*60*24,
                                     YIELDMM_DAILY=mean(Q, na.rm=na.rm$na.rm.global)*length(Q)*60*60*24 /basin_area/1000)
  Qstat_4seasons <- tidyr::gather(Qstat_4seasons,stat,value,3:4)
  Qstat_4seasons <- dplyr::mutate(Qstat_4seasons,title=paste0(Seasons4,"_",stat))
  Qstat_4seasons <- dplyr::select(Qstat_4seasons,-Seasons4,-stat)
  Qstat_4seasons <- tidyr::spread(Qstat_4seasons,title,value)
  Qstat_4seasons <-   dplyr::rename(Qstat_4seasons,Year=AnalysisYear)
  
  
  
  ## Compute statistics on months
  Qstat_months <- dplyr::summarize(dplyr::group_by(flowdata,AnalysisYear,MonthText),
                                   "_MIN_DAILY" = min   (Q, na.rm=na.rm$na.rm.global),
                                   "_MAX_DAILY" = max   (Q, na.rm=na.rm$na.rm.global),
                                   "_MEAN_DAILY"= mean  (Q, na.rm=na.rm$na.rm.global),
                                   "_MEDIAN_DAILY" = stats::median(Q, na.rm=na.rm$na.rm.global),
                                   "_P10_DAILY" = stats::quantile(Q, prob=.10, na.rm=T),
                                   "_P20_DAILY" = stats::quantile(Q, prob=.20, na.rm=T)
  )
  Qstat_months <- tidyr::gather(Qstat_months,stat,value,3:8)
  Qstat_months <- dplyr::mutate(Qstat_months,title=paste0(MonthText,stat))
  Qstat_months <- dplyr::select(Qstat_months,-MonthText,-stat)
  Qstat_months <- tidyr::spread(Qstat_months,title,value)
  Qstat_months <- dplyr::rename(Qstat_months,Year=AnalysisYear)
  
  # compute the number of days in a year outside of the 25th or 75th percentile for each day.
  daily_normals <- dplyr::summarise(dplyr::group_by(flowdata,AnalysisDoY),
                                    P25=stats::quantile(Q, prob=0.25, na.rm=TRUE),
                                    P75=stats::quantile(Q, prob=0.75, na.rm=TRUE))
  flowdata_temp <- merge(flowdata, daily_normals, by="AnalysisDoY") # merge back with the original data
  Qstat_dailynormals <- dplyr::summarise(dplyr::group_by(flowdata_temp,AnalysisYear),
                                         DAYS_BELOW_25=sum(Q < P25, na.rm=TRUE),
                                         DAYS_ABOVE_75=sum(Q > P75, na.rm=TRUE),
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
  
  
  
  
  ## Write the files
  #################################
  
  file_Qsummary_table <- NA
  if(write_summary_table){
    # write out the flow summary
    file_Qsummary_table <- file.path(report_dir, paste(station_name,"-period-record-summary.csv", sep=""))
    temp <- Qsummary
    temp[,2:ncol(Qsummary)] <- round(temp[,2:ncol(Qsummary)], table_nddigits)
    utils::write.csv(temp,file=file_Qsummary_table, row.names=FALSE)
  }
  
  # See if you want to write out the summary tables?
  file_Qstat_table <- NA
  if(write_table){
    
    # Write out the summary table for comparison to excel spreadsheet for calendar year
    file_Qstat_table <- file.path(report_dir, paste(station_name,"-annual-summary-stat.csv", sep=""))
    temp <- Qstat
    temp <- round(temp, table_nddigits)
    utils::write.csv(temp,file=file_Qstat_table, row.names=FALSE)
  }
  
  
  # Write out the annual summary table in transposed format?
  file_Qstat_tpose_table<- NA
  options(scipen = 999)
  Qstat_tpose <- tidyr::gather(Qstat,Statistic,Value,-Year)
  Qstat_tpose.temp <- dplyr::mutate(Qstat_tpose,Value=round(Value,table_nddigits)) # for writing to csv
  Qstat_tpose <- tidyr::spread(Qstat_tpose,Year,Value)
  
  if(write_transposed_table){
    file_Qstat_tpose_table <-file.path(report_dir,paste(station_name,"-annual-summary-stat-trans.csv",sep=""))
    Qstat_tpose.temp <- tidyr::spread(Qstat_tpose.temp,Year,Value)
    utils::write.csv(Qstat_tpose.temp, file=file_Qstat_tpose_table, row.names=FALSE)
  }
  
  # Write out the low flow summary
  # all mins and acutal dates
  Qstat_lowflows <- dplyr::select(Qstat,
                                  Year,dplyr::contains("MIN_0"),dplyr::contains("MINDOY_0"),
                                  dplyr::contains("MIN_3"),dplyr::contains("MINDOY_3"))
  if (water_year) {
    Qstat_lowflows <- dplyr::mutate(Qstat_lowflows,
                                    WY_MINDate_01Day = as.Date(WY_MINDOY_01Day-1, origin=as.Date(paste0(Year-1,"-10-01"))),
                                    WY_MINDate_03Day = as.Date(WY_MINDOY_03Day-1, origin=as.Date(paste0(Year-1,"-10-01"))),
                                    WY_MINDate_07Day = as.Date(WY_MINDOY_07Day-1, origin=as.Date(paste0(Year-1,"-10-01"))),
                                    WY_MINDate_30Day = as.Date(WY_MINDOY_30Day-1, origin=as.Date(paste0(Year-1,"-10-01"))) )
  } else {
    Qstat_lowflows <- dplyr::mutate(Qstat_lowflows,
                                    CY_MINDate_01Day = as.Date(CY_MINDOY_01Day-1, origin=as.Date(paste0(Year,"-01-01"))),
                                    CY_MINDate_03Day = as.Date(CY_MINDOY_03Day-1, origin=as.Date(paste0(Year,"-01-01"))),
                                    CY_MINDate_07Day = as.Date(CY_MINDOY_07Day-1, origin=as.Date(paste0(Year,"-01-01"))),
                                    CY_MINDate_30Day = as.Date(CY_MINDOY_30Day-1, origin=as.Date(paste0(Year,"-01-01"))) )
  }
  Qstat_lowflows <- dplyr::select(Qstat_lowflows,Year,
                                  dplyr::contains("_01"),dplyr::contains("_03"),
                                  dplyr::contains("_07"),dplyr::contains("_30"))
  Qstat_lowflows <- dplyr::filter(Qstat_lowflows, Year >= start_year & Year <= end_year)
  Qstat_lowflows <- dplyr::filter(Qstat_lowflows, Year >= min_year & Year <=max_year)
  
  file_Qlowflow_table <- NA
  if(write_lowflow_table){
    file_Qlowflow_table <- file.path(report_dir,paste(station_name,"-lowflow-summary.csv",sep=""))
    temp <- Qstat_lowflows
    temp[,c(2,5,8,11)] = round(temp[,c(2,5,8,11)],3)
    utils::write.csv(temp, file=file_Qlowflow_table, row.names=FALSE)
  }
  
  
  
  Qtrends_zyp <- NA
  file_Qtrends_zyp_table <- NA
  file_Qtrends_zyp_plot <- NA
  if (!is.na(zyp_trending)) {
    
    Qtrends_zyp <- zyp::zyp.trend.dataframe(indat = Qstat_tpose,
                                            metadata.cols = 1,
                                            method=zyp_trending)
    ## ADD SOME METRICS
    
    if (write_zyp_table) {
      file_Qtrends_zyp_table <- file.path(report_dir,paste(station_name,"-zyp-",zyp_trending,"-trends-results.csv",sep=""))
      temp <- Qtrends_zyp
      utils::write.csv(temp, file=file_Qtrends_zyp_table, row.names=FALSE)
    }
    
    if (!is.na(write_zyp_plots)) {
      trends_plotdata <- tidyr::gather(Qstat_tpose,Year,Value,-1)
      trends_plotdata <- dplyr::mutate(trends_plotdata,Year=as.numeric(Year))
      trends_plotdata <- dplyr::mutate(trends_plotdata,
                                       Units="Discharge (cms)",
                                       Units=replace(Units, grepl("TOTALQ",Statistic), "Total Discharge (cubic metres)"),
                                       Units=replace(Units, grepl("YIELD",Statistic), "Water Yield (mm)"),
                                       Units=replace(Units, grepl("FLOW",Statistic), "Day of Year"),
                                       Units=replace(Units, grepl("DAYS",Statistic), "Number of Days"))
      
      if (write_zyp_plots=="pdf") {
        file_Qtrends_zyp_plot <- file.path(report_dir,paste(station_name,"-zyp-",zyp_trending,"-trends-results.pdf",sep=""))
        pdf(file = file_Qtrends_zyp_plot,8,4)
        for (metric in unique(trends_plotdata$Statistic)){
          # Filter for metric
          trends_trendsdata <- dplyr::filter(trends_plotdata,Statistic==metric)
          trends_resultsdata <- dplyr::filter(Qtrends_zyp,Statistic==metric)
          #int <- trends_resultsdata$intercept - trends_resultsdata$trend * (Start_Year)
          # Plot each metric
          trends_plot <- ggplot2::ggplot(trends_trendsdata,ggplot2::aes(x=Year,y=Value))+
            ggplot2::geom_point(color="skyblue4")+
            ggplot2::geom_line(alpha = 0.3,color="skyblue4") +
            ggplot2::ggtitle(paste0(metric,"   (Sig. = ",round(trends_resultsdata$sig,3),")"))+
            ggplot2::ylab(trends_trendsdata$Units[1])+
            ggplot2::xlab("Year")+
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
            ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
            ggplot2::theme_bw() +
            ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey50", fill=NA, size=.1),
                           panel.grid = ggplot2::element_blank())
          if (water_year) {trends_plot <- trends_plot + ggplot2::xlab("Water Year")}
          
          # If sig. trend, plot trend
          if (trends_resultsdata$sig < zyp_alpha & !is.na(trends_resultsdata$sig)) {
            trends_plot <- trends_plot +
              ggplot2::geom_abline(slope = trends_resultsdata$trend, intercept = (trends_resultsdata$intercept - trends_resultsdata$trend * (start_year)), colour="red")
          }
          plot(trends_plot)
        }
        dev.off()
      }
      
      ## PNG/JPG
      if (write_zyp_plots %in% c("png","jpeg","tiff","bmp")) {
        file_Qtrends_zyp_plot <- paste(report_dir,"/",station_name,"-zyp-",zyp_trending,"-trends-results",sep = "")
        dir.create(file_Qtrends_zyp_plot)
        for (metric in unique(trends_plotdata$Statistic)){
          # Filter for metric
          
          plot_dir <- file.path(file_Qtrends_zyp_plot,paste(metric,".",write_zyp_plots,sep=""))
          
          trends_trendsdata <- dplyr::filter(trends_plotdata,Statistic==metric)
          trends_resultsdata <- dplyr::filter(Qtrends_zyp,Statistic==metric)
          #int <- trends_resultsdata$intercept - trends_resultsdata$trend * (Start_Year)
          # Plot each metric
          trends_plot <- ggplot2::ggplot(trends_trendsdata,ggplot2::aes(x=Year,y=Value))+
            ggplot2::geom_point(color="skyblue4",size=3)+
            ggplot2::geom_line(alpha = 0.3,color="skyblue4") +
            #ggplot2::ggtitle(paste0(metric,"   (Sig. = ",round(trends_resultsdata$sig,3),")"))+
            ggplot2::ylab(trends_trendsdata$Units[1])+
            ggplot2::xlab("Year")+
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
            ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
            ggplot2::theme_bw() +
            ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey50", fill=NA, size=.1),
                           panel.grid = ggplot2::element_blank(),
                           axis.text = ggplot2::element_text(size=12),
                           axis.title = ggplot2::element_text(size=12))
          if (water_year) {trends_plot <- trends_plot + ggplot2::xlab("Water Year")}
          
          # If sig. trend, plot trend
          if (trends_resultsdata$sig < zyp_alpha & !is.na(trends_resultsdata$sig)) {
            trends_plot <- trends_plot +
              ggplot2::geom_abline(slope = trends_resultsdata$trend, intercept = (trends_resultsdata$intercept - trends_resultsdata$trend * (start_year)), colour="red")
          }
          ggplot2::ggsave(filename =plot_dir,trends_plot,width=8,height=4)
        }
      }
    }
  }
  
  
  ## Do some plotting
  #################################
  
  
  
  
  
  return(list( "station name"= station_name,
               "year type"=ifelse(!water_year,"Calendar Year (Jan-Dec)","Water Year (Oct-Sep)"),
               "year range"=paste0(start_year," - ",end_year),
               exclude_years=exclude_years,
               Qsummary=Qsummary,
               Qstat_annual=Qstat,
               Qstat_annual_tpose=Qstat_tpose,
               Qstat_lowflows=Qstat_lowflows,
               "zyp trending method"=zyp_trending,
               Qtrends_zyp=Qtrends_zyp,
               dates_missing_flows=dates_missing_flows,
               file_Qstat_table=file_Qstat_table,
               file_Qstat_tpose_table=file_Qstat_tpose_table,
               file_Qsummary_table=file_Qsummary_table,
               file_Qlowflow_table=file_Qlowflow_table,
               na.rm = na.rm,
               Version=Version,
               Date=Sys.time()))
} # end of function

