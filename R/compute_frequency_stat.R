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
#'    dataset, set the roll_days to \code{7} and the return_period to \code{10}. Calculates the statistic from all daily discharge 
#'    values from all years and months, unless specified.Function will calculate using all values in the provided data (no grouped 
#'    analysis). Analysis methodology replicates that from \href{http://www.hec.usace.army.mil/software/hec-ssp/}{HEC-SSP}.
#'
#' @inheritParams compute_annual_frequencies
#' @param roll_days Numeric value of the number of days to apply a rolling mean. Required.
#' @param return_period Numeric vector of the estimated time interval, in years, between flow events of a similar size, 
#'    inverse of probability, used to estimate the frequency statistic. Required.
#' 
#' @return A numeric value of the frequency analysis result, given the roll_days and return_period
#'   
#' @examples
#' \dontrun{
#' 
#' compute_frequency_stat(station_number = "08NM116",
#'                        roll_days = 7,
#'                        return_period = 10)
#'                             
#' }
#' @export


compute_frequency_stat <- function(data = NULL,
                                   dates = Date,
                                   values = Value,
                                   station_number = NULL,
                                   roll_days = NA,
                                   roll_align = "right",
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
                                   months = 1:12,
                                   ignore_missing = FALSE){
  
  
  # replicate the frequency analysis of the HEC-SSP program
  # refer to Chapter 7 of the user manual
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if(is.na(roll_days)) stop("A numeric roll_days value is required.", call. = FALSE)
  rolling_days_checks(roll_days, roll_align, multiple = FALSE)
  
  if(length(return_period) > 1)   stop("Only one return_period can be provided.", call. = FALSE)
  if(is.na(return_period) | !is.numeric(return_period)) stop("A numeric return_period value is required.", call. = FALSE)
  
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  if (!is.null(station_number) & length(station_number) != 1) 
    stop("Only one station_number can be provided for this function.", call. = FALSE)
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, 
                               station_number = station_number)
  
  # Save the original columns (to check for STATION_NUMBER col at end) and ungroup if necessary
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # Check and rename columns
  flow_data <- format_dates_col(flow_data, dates = as.character(substitute(dates)))
  flow_data <- format_values_col(flow_data, values = as.character(substitute(values)))
  
  flow_data <- dplyr::select(flow_data, Date, Value)
  
  
  ## CHECKS ON OTHER PARAMETERS
  ##---------------------------
  
  # Convert return period to the probablity for fitting
  fit_quantiles <- 1 / return_period
  
  
  ## CALCULATE STAT
  ##---------------
  
  compute_annual_frequencies(data = flow_data,
                             roll_days = roll_days,
                             roll_align = roll_align,
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
                             months = months,
                             ignore_missing = ignore_missing)$fitted_quantiles[1,4]
  
  
}
