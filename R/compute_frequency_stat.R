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


#' @title Calculate an annual frequency analysis statistic
#'
#' @description Performs a volume frequency analysis on annual statistics from a streamflow dataset and calculates a statistic based on
#'    the provided mean n-days and return period of the statistic. For example, to determine the 7Q10 of a dataset, set the rolling_days 
#'    to \code{7} and the return_period to \code{10}. Calculates the statistic from all daily discharge values from all years and months,
#'    unless specified. Analysis methodology replicates that from \href{http://www.hec.usace.army.mil/software/hec-ssp/}{HEC-SSP}.
#'    Data calculated using compute_frequency_analysis() function.
#'
#' @param flowdata Data frame. A data frame of daily mean flow data that includes two columns: a 'Date' column with dates formatted 
#'    YYYY-MM-DD, and a numeric 'Value' column with the corresponding daily mean flow values in units of cubic metres per second. 
#'    Not required if \code{HYDAT} argument is used.
#' @param HYDAT Character. A seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to extract daily streamflow 
#'    data from a HYDAT database. \href{https://github.com/ropensci/tidyhydat}{Installation} of the \code{tidyhydat} package and a HYDAT 
#'    database are required. Not required if \code{flowdata} argument is used.
#' @param HYDAT_peaks Character. "MAX" or "MIN" instantaneous peaks pulled from HYDAT for analysis. Leave blank if not required.
#' @param rolling_days  Numeric. The number of days to apply a rolling mean. Required, unless using HYDAT_peaks.
#' @param rolling_align Character. Specifies whether the dates of the rolling mean should be specified by the first ('left'), last ('right),
#'    or middle ('center') of the rolling n-day group of observations. Default \code{'right'}.
#' @param return_period Numeric. The estimated time interval, in years, between flow events of a similar size, inverse of probability used 
#'    to estimate the frequency statistic. Required.
#' @param use_max  Logical. Analyze the annual maximums rather than the minimums. Default \code{FALSE}.
#' @param use_log  Logical. Transform flow data to log-scale before analysis. Default \code{FALSE}.
#' @param prob_plot_position Character. Plotting positions used in the frequency plots, one of "weibull","median", or "hazen". 
#'    Points are plotted against  (i-a)/(n+1-a-b) where \code{i} is the rank of the value; \code{n} is the sample size and \code{a} and 
#'    \code{b} are defined as: (a=0, b=0) for Weibull plotting positions; (a=.2; b=.3) for Median plotting postions; and (a=.5; b=.5) 
#'    for Hazen plotting positions. Default \code{"weibull"}.
#' @param fit_distr Character. Distribution to fit annual data, one of "PIII" (Pearson Log III distribution) or "weibull" 
#'    (Weibull distribution). Default \code{"PIII"}.
#' @param fit_distr_method  Character. Method used to fit the distribution, one of  "MOM" (method of moments) or "MLE" (maximum
#'     likelihood estimation). Selected as \code{"MOM"} if \code{fit_distr}=="PIII" (default) or \code{"MLE"} if \code{fit_distr}=="weibull".

#' @param water_year Logical. Use water years to group flow data instead of calendar years. Water years are designated
#'    by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Integer. Month indicating the start of the water year. Used if \code{water_year=TRUE}. Default \code{10}.
#' @param start_year Integer. First year to consider for analysis. Leave blank if all years are required.
#' @param end_year Integer. Last year to consider for analysis. Leave blank if all years are required.
#' @param exclude_years Integer. Single year or vector of years to exclude from analysis. Leave blank if all years are required.       
#' @param months Integer. Vector of months to consider for analysis (ex. \code{6:8} for Jun-Aug). Leave blank if all months
#'    are required. Default \code{1:12}.
#' @param na.rm TBD
#' 
#' @return A numeric value of the frequency analysis result, given the rolling_day and return_period
#'   
#' @examples
#' \dontrun{
#' 
#' compute_frequency_stat(HYDAT="08NM116",
#'                             start_year = 1980,
#'                             end_year = 2010)
#'                             
#' }
#' @export


compute_frequency_stat <- function(flowdata=NULL,
                                    HYDAT=NULL,
                                    HYDAT_peaks=NULL,
                                    rolling_days=NA,
                                    rolling_align="right",
                                    return_period=NA,######################################
                                    use_max=FALSE,
                                    use_log=FALSE,
                                    prob_plot_position=c("weibull","median","hazen"),
                                    fit_distr=c("PIII","weibull"),
                                    fit_distr_method=ifelse(fit_distr=="PIII","MOM","MLE"),
                                    water_year=FALSE,
                                    water_year_start=10,
                                    start_year=NULL,
                                    end_year=NULL,
                                    exclude_years=NULL,
                                    months=1:12,
                                    na.rm=list(na.rm.global=TRUE)){
  
  # replicate the frequency analysis of the HEC-SSP program
  # refer to Chapter 7 of the user manual
  
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
  
  if( length(rolling_days)!=1 )                        {stop("only one rolling_days value can be selected")}
  if( is.null(HYDAT_peaks) ) {
    if(is.na(rolling_days) )     {stop("rolling_days argument must be provided")}
    if( !is.numeric(rolling_days) )                      {stop("rolling_days argument must be numeric")}
    if( !rolling_days %in% c(1:180) )                    {stop("rolling_days argument must be integers > 0 and <= 180)")}
    if( !rolling_align %in% c("right","left","center"))  {stop("rolling_align argument must be 'right', 'left', or 'center'")}
  }
  
  if( !is.null(HYDAT_peaks)) {if(!HYDAT_peaks %in% c("MAX","MIN") ) {stop("HYDAT_peaks argument must be 'MAX', 'MIN', or leave blank")}}
  if( is.null(HYDAT) & !is.null(HYDAT_peaks) )        {stop('station in HYDAT argument must be selected with HYDAT_peaks')}
  if( !is.null(HYDAT_peaks) ) {
    if(water_year)           warning("water_year argument was ignored - HYDAT_peaks is based on calendar years")
    if(!is.na(rolling_days)) warning("rolling_days argument was ignored - HYDAT_peaks take the instantaneous min or max value from HYDAT") 
  }
  
  if( !is.list(na.rm))                {stop("na.rm is not a list") }
  if( !is.logical(unlist(na.rm))){    {stop("na.rm is list of logical (TRUE/FALSE) values only.")}
    if( !is.logical(use_log))         {stop("use_log must be logical (TRUE/FALSE)")}
    if( !is.logical(use_max))         {stop("use_max must be logical (TRUE/FALSE)")}
    if( !all(prob_plot_position %in% c("weibull","median","hazen")))  stop("prob_plot_position must be one of weibull, 
                                                                           median, or hazen")}
  if( !all(fit_distr %in% c("weibull","PIII")))       {stop("fit_distr must be one of weibull or PIII")}
  if( fit_distr[1]=='weibull' & use_log)              {stop("Cannot fit Weibull distribution on log-scale")}
  if( is.null(HYDAT) & fit_distr[1]=='weibull' & any(flowdata$Value<0, na.rm=TRUE)){stop("cannot fit weibull distribution with 
                                                                                         negative flow values")}
  if( fit_distr[1]!="PIII" & fit_distr_method[1]=="MOM"){stop('MOM only can be used with PIII distribution')}
  
  if( length(return_period)!=1 )     {stop("only one return_period value can be selected")}
  if( is.na(return_period) )        {stop("return_period argument must be provided")}
  if( !is.numeric(return_period) )   {stop("return_period argument must be numeric")}
  
  my.na.rm <- list(na.rm.global=FALSE)
  if( !all(names(na.rm) %in% names(my.na.rm))){stop("Illegal element in na.rm")}
  my.na.rm[names(na.rm)]<- na.rm
  na.rm <- my.na.rm  # set the na.rm for the rest of the function.
  
  if (!is.null(HYDAT)) {
    if( length(HYDAT)>1 ) {stop("Only one HYDAT station can be selected.")}
    if( !HYDAT %in% dplyr::pull(tidyhydat::allstations[1]) ) {stop("Station in 'HYDAT' parameter does not exist")}
  }
  
  
  
  
  # Convert return period to the probablity for fitting
  fit_quantiles <- 1/return_period
  
  
  
  # Complete analysis and extract value
  frequency_statistic <- fasstr::compute_frequency_analysis(flowdata=flowdata,
                                                             HYDAT=HYDAT,
                                                             HYDAT_peaks=HYDAT_peaks,
                                                             rolling_days=rolling_days,
                                                             rolling_align=rolling_align,
                                                             use_max=use_max,
                                                             use_log=use_log,
                                                             prob_plot_position=prob_plot_position,
                                                             prob_scale_points=c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                                                             fit_distr=fit_distr,
                                                             fit_distr_method=fit_distr_method,
                                                             fit_quantiles=fit_quantiles,
                                                             water_year=water_year,
                                                             water_year_start=water_year_start,
                                                             start_year=start_year,
                                                             end_year=end_year,
                                                             exclude_years=exclude_years,
                                                             months=months,
                                                             #station_name=NA,
                                                             #write_table_overview=FALSE,
                                                             #write_table_stats=FALSE,
                                                             #write_table_plotdata=FALSE,  # write out the plotting data
                                                             #write_table_quantiles=FALSE, # write out the fitted quantiles
                                                             #write_plot_frequency=FALSE,  # write out the frequency plot
                                                             #write_imgtype=c("pdf"),
                                                             #write_imgsize=c(4,6),
                                                             #write_digits=3,
                                                             #write_dir='.',
                                                             na.rm=list(na.rm.global=TRUE))$fitted_quantiles[1,4]
  
  return(frequency_statistic)
  
  
}
