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


#' @title Perform an annual low or high-flow frequency analysis
#'
#' @description Performs a flow volume frequency analysis on annual statistics from a streamflow dataset. Defaults to low-flow frequency 
#'    analysis using annual minimums. Use \code{use_max} for annual high flow frequency analyses. Calculates the statistics from all 
#'    daily discharge values from all years, unless specified. Function will calculate using all values in the provided data (no grouped
#'    analysis). Analysis methodology replicates that from \href{http://www.hec.usace.army.mil/software/hec-ssp/}{HEC-SSP}.
#'
#' @inheritParams calc_annual_stats
#' @param use_max  Logical valve to indicate using annual maximums rather than the minimums for analysis. Default \code{FALSE}.
#' @param use_log  Logical value to indicate log-scale tranforming of flow data before analysis. Default \code{FALSE}.
#' @param prob_plot_position Character string indicating the plotting positions used in the frequency plots, one of "weibull",
#'    "median", or "hazen". Points are plotted against  (i-a)/(n+1-a-b) where \code{i} is the rank of the value; \code{n} is the 
#'    sample size and \code{a} and \code{b} are defined as: (a=0, b=0) for Weibull plotting positions; (a=.2; b=.3) for Median 
#'    plotting postions; and (a=.5; b=.5) for Hazen plotting positions. Default \code{"weibull"}.
#' @param prob_scale_points  Numeric vector of probabilities to be plotted along the X axis in the frequency plot. Inverse of 
#'    return period. Default \code{c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001)}.
#' @param fit_distr Character string identifying the distribution to fit annual data, one of "PIII" (Pearson Log III distribution) 
#'    or "weibull" (Weibull distribution). Default \code{"PIII"}.
#' @param fit_distr_method  Character string identifying the method used to fit the distribution, one of  "MOM" (method of moments) 
#'    or "MLE" (maximum likelihood estimation). Selected as \code{"MOM"} if \code{fit_distr}=="PIII" (default) or \code{"MLE"} if 
#'     \code{fit_distr}=="weibull".
#' @param fit_quantiles Numeric vector of quantiles to be estimated from the fitted distribution. 
#'    Default \code{c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01)}.
#' 
#' @return A list with the following elements:
#'   \item{Q_stat}{Data frame with computed annual summary statistics used in analysis}
#'   \item{plotdata}{Data frame with co-ordinates used in frequency plot.}
#'   \item{freqplot}{ggplot2 object with frequency plot}
#'   \item{fit}{List of fitted objects from fitdistrplus.}
#'   \item{fitted_quantiles}{Data frame with fitted quantiles.}
#'   
#'   
#' @examples
#' \dontrun{
#' 
#' compute_annual_frequencies(station_number = "08NM116",
#'                            start_year = 1980,
#'                            end_year = 2010)
#'                             
#' }
#' @export


compute_annual_frequencies <- function(data = NULL,
                                       dates = Date,
                                       values = Value,
                                       station_number = NULL,
                                       roll_days = c(1, 3, 7, 30),
                                       roll_align = "right",
                                       use_max = FALSE,
                                       use_log = FALSE,
                                       prob_plot_position = c("weibull", "median", "hazen"),
                                       prob_scale_points = c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                                       fit_distr = c("PIII", "weibull"),
                                       fit_distr_method = ifelse(fit_distr == "PIII", "MOM", "MLE"),
                                       fit_quantiles = c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01),
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
  
  rolling_days_checks(roll_days, roll_align, multiple = TRUE)
  water_year_checks(water_year, water_year_start)
  years_checks(start_year, end_year, exclude_years)
  months_checks(months)
  ignore_missing_checks(ignore_missing)
  
  
  if (!is.logical(use_log))        
    stop("use_log must be logical (TRUE/FALSE).", call. = FALSE)
  if (!is.logical(use_max))         
    stop("use_max must be logical (TRUE/FALSE).", call. = FALSE)
  if (!all(prob_plot_position %in% c("weibull","median","hazen")))  
    stop("prob_plot_position must be one of weibull, median, or hazen.", call. = FALSE)
  if (!is.numeric(prob_scale_points)) 
    stop("prob_scale_points must be numeric and between 0 and 1 (not inclusive).", call. = FALSE)
  if (!all(prob_scale_points > 0 & prob_scale_points < 1))
    stop("prob_scale_points must be numeric and between 0 and 1 (not inclusive).", call. = FALSE)
  if (!all(fit_distr %in% c("weibull", "PIII")))          
    stop("fit_distr must be one of weibull or PIII.", call. = FALSE)
  if (!is.numeric(fit_quantiles))                         
    stop("fit_quantiles must be numeric and between 0 and 1 (not inclusive).", call. = FALSE)
  if (!all(fit_quantiles > 0 & fit_quantiles < 1))        
    stop("fit_quantiles must be numeric and between 0 and 1 (not inclusive).", call. = FALSE)
  if (fit_distr[1] == 'weibull' & use_log)                
    stop("Cannot fit Weibull distribution on log-scale.", call. = FALSE)
  if (fit_distr[1] != "PIII" & fit_distr_method[1] == "MOM") 
    stop('MOM only can be used with PIII distribution.', call. = FALSE)
  
  
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
  
  if(fit_distr[1] == 'weibull' & any(flow_data$Value < 0, na.rm = TRUE))
    stop("Cannot fit weibull distribution with negative flow values.", call. = FALSE)
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill missing dates, add date variables, and add AnalysisYear
  flow_data <- analysis_prep(data = flow_data, 
                             water_year = water_year, 
                             water_year_start = water_year_start,
                             year = TRUE)
  

  
  # Loop through each roll_days and add customized names of rolling means to flow_data
  for (day in roll_days) {
    flow_data_temp <- dplyr::select(flow_data, Date, Value)
    flow_data_temp <- add_rolling_means(flow_data_temp, roll_days = day, roll_align = roll_align)
    names(flow_data_temp)[names(flow_data_temp) == paste0("Q", day, "Day")] <- paste("Q", formatC(day, width = 3, format = "d",
                                                                                                  flag = "0"), "-day-avg", sep = "")
    flow_data_temp <- dplyr::select(flow_data_temp, -Value)
    flow_data <- merge(flow_data, flow_data_temp, by = "Date", all = TRUE)
  }
  

    # Filter for the selected year
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flow_data <- dplyr::filter(flow_data, !(AnalysisYear %in% exclude_years))
  flow_data <- dplyr::filter(flow_data, Month %in% months)

  # Calculate the min or max of the rolling means for each year
  flow_data <- dplyr::select(flow_data, -Date, -Value, -Year, -Month, -MonthName, -DayofYear, -dplyr::contains("Water"))
  
  flow_data <- tidyr::gather(flow_data, Measure, value, -AnalysisYear)
  flow_data$Measure <- factor(flow_data$Measure, levels = unique(flow_data$Measure))
  
  Q_stat <- dplyr::summarise(dplyr::group_by(flow_data, AnalysisYear, Measure),
                             value = ifelse(use_max,
                                            max(value, na.rm = ignore_missing),
                                            min(value, na.rm = ignore_missing)))
  Q_stat <- dplyr::rename(Q_stat, Year = AnalysisYear)
  
  # remove any Inf values
  Q_stat$value[is.infinite(Q_stat$value)] <- NA

    # Data checks
  if (nrow(Q_stat) < 3) stop(paste0("Need at least 3 years of observations for analysis. There are only ", 
                                        nrow(Q_stat), 
                                        " years available."), call. = FALSE)
  
  
  ## COMPUTE THE ANALYSIS
  ## -------------------------------
  
  analysis <- compute_frequency_analysis(data = Q_stat,
                                         events = "Year",
                                         values = "value",
                                         measures = "Measure",
                                         use_max = use_max,
                                         use_log = use_log,
                                         prob_plot_position = prob_plot_position,
                                         prob_scale_points = prob_scale_points,
                                         fit_distr = fit_distr,
                                         fit_distr_method = fit_distr_method,
                                         fit_quantiles = fit_quantiles)
    
  return(analysis)
  
}
