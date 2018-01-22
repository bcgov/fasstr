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
#'    the provided mean n-days and return period of the statistic, defaults to minimum flows. For example, to determine the 7Q10 of a 
#'    dataset, set the rolling_day to \code{7} and the return_period to \code{10}. Calculates the statistic from all daily discharge 
#'    values from all years and months, unless specified.Function will calculate using all values in the provided data (no grouped 
#'    analysis). Analysis methodology replicates that from \href{http://www.hec.usace.army.mil/software/hec-ssp/}{HEC-SSP}.
#'
#' @param data Daily data to be analyzed. Options:
#' 
#'    A data frame of daily data that contains columns of dates and values.
#'    
#'    A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param dates Column in the \code{data} data frame that contains dates formatted YYYY-MM-DD. Only required if
#'    using the data frame option of \code{data} and dates column is not named 'Date'. Default \code{Date}. 
#' @param values Column in the \code{data} data frame that contains numeric flow values, in units of cubic metres per second.
#'    Only required if using the data frame option of \code{data} and values column is not named 'Value'. Default \code{Value}.
#' @param use_hydat_peaks Logical value indicating whether to use instantaneous peaks from HYDAT for analysis.
#'    Leave blank if not required.
#' @param rolling_day Numeric vector of the number of days to apply the rolling mean. Not required if using \code{use_hydat_peaks}.
#' @param rolling_align Character string identifying the direction of the rolling mean from the specified date, either by the first 
#'    ('left'), last ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
#' @param return_period Numeric vector of the estimated time interval, in years, between flow events of a similar size, 
#'    inverse of probability, used to estimate the frequency statistic. Required.
#' @param use_max  Logical valve to indicate using annual maximums rather than the minimums for analysis. Default \code{FALSE}.
#' @param use_log  Logical value to indicate log-scale tranforming of flow data before analysis. Default \code{FALSE}.
#' @param prob_plot_position Character string indicating the plotting positions used in the frequency plots, one of "weibull",
#'    "median", or "hazen". Points are plotted against  (i-a)/(n+1-a-b) where \code{i} is the rank of the value; \code{n} is the 
#'    sample size and \code{a} and \code{b} are defined as: (a=0, b=0) for Weibull plotting positions; (a=.2; b=.3) for Median 
#'    plotting postions; and (a=.5; b=.5) for Hazen plotting positions. Default \code{"weibull"}.
#' @param fit_distr Character string identifying the distribution to fit annual data, one of "PIII" (Pearson Log III distribution) 
#'    or "weibull" (Weibull distribution). Default \code{"PIII"}.
#' @param fit_distr_method  Character string identifying the method used to fit the distribution, one of  "MOM" (method of moments) 
#'    or "MLE" (maximum likelihood estimation). Selected as \code{"MOM"} if \code{fit_distr}=="PIII" (default) or \code{"MLE"} if 
#'     \code{fit_distr}=="weibull".
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#' @param exclude_years Numeric vector of years to exclude from analysis. Leave blank to include all years. 
#' @param complete_years Logical values indicating whether to include only years with complete data in analysis. Default \code{FALSE}.          
#' @param months Numeric vector of months to include in analysis (ex. \code{6:8} for Jun-Aug). Leave blank to summarize 
#'    all months (default \code{1:12}).
#' @param ignore_missing Logical value indicating whether dates with missing values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
#' 
#' @return A numeric value of the frequency analysis result, given the rolling_day and return_period
#'   
#' @examples
#' \dontrun{
#' 
#' compute_frequency_stat(data = "08NM116",
#'                        rolling_day = 7,
#'                        return_period = 10)
#'                             
#' }
#' @export


compute_frequency_stat <- function(data = NULL,
                                   dates = Date,
                                   values = Value,
                                   rolling_day = NA,
                                   rolling_align = "right",
                                   use_hydat_peaks = FALSE,
                                   return_period = NA,
                                   use_max = FALSE,
                                   use_log = FALSE,
                                   prob_plot_position = c("weibull", "median", "hazen"),
                                   fit_distr = c("PIII", "weibull"),
                                   fit_distr_method = ifelse(fit_distr == "PIII", "MOM", "MLE"),
                                   water_year = FALSE,
                                   water_year_start = 10,
                                   start_year = 0,
                                   end_year = 9999,
                                   exclude_years = NULL,
                                   complete_years = FALSE,
                                   months = 1:12,
                                   ignore_missing = FALSE){
  
  
  ## CHECKS ON DATA FOR CALC
  ##------------------------
  
  # Check if data is provided
  if(is.null(data))   stop("No data provided, must provide a data frame or HYDAT station number(s).")
  if(!is.data.frame(data) & !is.vector(data)) stop("No data provided, must provide a data frame or HYDAT station number(s).")
  
  # Check HYDAT stations
  if(is.vector(data)) {
    if(length(data) != 1)   stop("Only one HYDAT station number can be listed for this function.")
    if(!data %in% dplyr::pull(tidyhydat::allstations[1]))  stop("Station number listed in data argument does not exist in HYDAT.")
  }
  
  if(is.data.frame(data)) {
    # Get the just groups (default STATION_NUMBER), Date, and Value columns
    # This method allows the user to select the Station, Date or Value columns if the column names are different
    if(!as.character(substitute(values)) %in% names(data) & !as.character(substitute(dates)) %in% names(data)) 
      stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using 'dates' and 'values' arguments.")
    if(!as.character(substitute(dates)) %in% names(data))  
      stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
    if(!as.character(substitute(values)) %in% names(data)) 
      stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.")
    
    # Temporarily rename the Date and Value columns
    names(data)[names(data) == as.character(substitute(dates))] <- "Date"
    names(data)[names(data) == as.character(substitute(values))] <- "Value"
    
    # Check columns are in proper formats
    if(!inherits(data$Date[1], "Date"))  stop("'Date' column in data frame does not contain dates.")
    if(!is.numeric(data$Value))          stop("'Value' column in data frame does not contain numeric values.")   
  }
  
  
  ## CHECKS ON OTHER PARAMETERS
  ##---------------------------
  
  if(length(return_period) > 1)   stop("Only one return_period can be provided.")
  if(is.na(return_period) | !is.numeric(return_period)) stop("A numeric return_period value is required.")
  if(!use_hydat_peaks){
    if(length(rolling_day) > 1)   stop("Only one rolling_day can be provided.")
    if(is.na(rolling_day) | !is.numeric(rolling_day)) stop("A numeric rolling_day value is required.")
  }
  if(use_hydat_peaks){
    if(!is.na(rolling_day))  warning("rolling_day argument ignored when using use_hydat_peaks.")
    if(water_year)           warning("water_year argument ignored when using use_hydat_peaks.")
    if(length(months) < 12)  warning("months argument ignored when using use_hydat_peaks.")
  }
  
  # Convert return period to the probablity for fitting
  fit_quantiles <- 1 / return_period
  
  
  ## CALCULATE STAT
  ##---------------
  
  fasstr::compute_frequency_analysis(data = data,
                                     rolling_days = rolling_day,
                                     rolling_align = rolling_align,
                                     use_hydat_peaks = use_hydat_peaks,
                                     use_max = use_max,
                                     use_log = use_log,
                                     prob_plot_position = prob_plot_position,
                                     prob_scale_points = c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                                     fit_distr = fit_distr,
                                     fit_distr_method = fit_distr_method,
                                     fit_quantiles = fit_quantiles,
                                     water_year = water_year,
                                     water_year_start = water_year_start,
                                     start_year = start_year,
                                     end_year = end_year,
                                     exclude_years = exclude_years,
                                     complete_years = complete_years,
                                     months = months,
                                     ignore_missing = ignore_missing)$fitted_quantiles[1,4]
  
  
}
