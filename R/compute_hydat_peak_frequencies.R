# Copyright 2019 Province of British Columbia
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


#' @title Perform a flow frequency analysis on annual statistics
#'
#' @description Performs a volume frequency analysis on annual statistics from a streamflow dataset. Calculates the statistics from all 
#'    daily discharge values from all years, unless specified. Function will calculate using all values in the provided data (no grouped
#'    analysis). Analysis methodology replicates that from \href{http://www.hec.usace.army.mil/software/hec-ssp/}{HEC-SSP}.
#'
#' @inheritParams compute_frequency_analysis
#' @inheritParams compute_annual_frequencies
#' @param station_number A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of
#'    which to extract annual peak minimum or maximum instantaneous streamflow data from a HYDAT database. Requires \code{tidyhydat} 
#'    package and a HYDAT database.
#' 
#' @return A list with the following elements:
#'   \item{Freq_Analysis_Data}{Data frame with computed annual summary statistics used in analysis.}
#'   \item{Freq_Plot_Data}{Data frame with co-ordinates used in frequency plot.}
#'   \item{Freq_Plot}{ggplot2 object with frequency plot}
#'   \item{Freq_Fitting}{List of fitted objects from fitdistrplus.}
#'   \item{Freq_Fitted_Quantiles}{Data frame with fitted quantiles.}
#'   
#' @seealso \code{\link{compute_frequency_analysis}}
#'   
#' @examples
#' \dontrun{
#' 
#' compute_hydat_peak_frequencies(station_number = "08NM116",
#'                                start_year = 1980,
#'                                end_year = 2010)
#'                             
#' }
#' @export


compute_hydat_peak_frequencies <- function(station_number,
                                           use_max = FALSE,
                                           use_log = FALSE,
                                           prob_plot_position = c("weibull", "median", "hazen"),
                                           prob_scale_points = c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                                           fit_distr = c("PIII", "weibull"),
                                           fit_distr_method = ifelse(fit_distr == "PIII", "MOM", "MLE"),
                                           fit_quantiles = c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01),
                                           start_year,
                                           end_year,
                                           exclude_years,
                                           plot_curve = TRUE){
  
  # replicate the frequency analysis of the HEC-SSP program
  # refer to Chapter 7 of the user manual
  
 
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(station_number)) {
    station_number = NULL
  }
  if (missing(start_year)) {
    start_year = 0
  }
  if (missing(end_year)) {
    end_year = 9999
  }
  if (missing(exclude_years)) {
    exclude_years = NULL
  }
  
  years_checks(start_year, end_year, exclude_years)
  
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
  
  
  
  if (!file.exists(file.path(tidyhydat::hy_dir(),"HYDAT.sqlite3")))
    stop("A HYDAT database has not been downloaded yet using the tidyhydat::download_hydat() function. 
       Download HYDAT before using station_number argument.", call. = FALSE)
  if (is.null(station_number))       stop("A station_number must be provided.", call. = FALSE)
  if (length(station_number) != 1)   stop("Only one station_number can be provided for this function.", call. = FALSE)
  if (!all(station_number %in% dplyr::pull(suppressMessages(tidyhydat::hy_stations()[1])))) 
    stop("station_number listed does not exist in HYDAT.", call. = FALSE)
  
  
  # Get peak data
  inst_peaks <- suppressMessages(suppressWarnings(tidyhydat::hy_annual_instant_peaks(station_number)))
  if (nrow(inst_peaks) == 0) stop("No peak data available for this station_number.", call. = FALSE) 
  
  inst_peaks <- dplyr::filter(inst_peaks, Parameter == "Flow")
  if (nrow(inst_peaks) == 0) stop("No peak flow data available for this station_number.", call. = FALSE) 
  
  inst_peaks <- dplyr::filter(inst_peaks, PEAK_CODE == ifelse(use_max, "MAX", "MIN"))
  if (use_max & nrow(inst_peaks) == 0)  stop("No maximum peak flow data available for this station_number.", call. = FALSE) 
  if (!use_max & nrow(inst_peaks) == 0) stop("No minimum peak flow data available for this station_number.", call. = FALSE) 

  inst_peaks$Year <- lubridate::year(inst_peaks$Date)
  inst_peaks <- dplyr::select(inst_peaks, Year, Measure = PEAK_CODE, Value)
  inst_peaks <- dplyr::mutate(inst_peaks, Measure = paste0("Instantaneous ", ifelse(use_max,"Maximum", "Minimum")))
  
  # Filter peak data
  inst_peaks <- inst_peaks[ inst_peaks$Year >= start_year & inst_peaks$Year <= end_year,]
  inst_peaks <- dplyr::filter(inst_peaks, !(Year %in% exclude_years))
  
  # Data checks
  if (nrow(inst_peaks) < 3) stop(paste0("Need at least 3 years of observations for analysis. There are only ", 
                                        nrow(inst_peaks), 
                                        " years available."), call. = FALSE)
  
  Q_stat <- inst_peaks
  

  
  ## COMPUTE THE ANALYSIS
  ## -------------------------------
  
  analysis <- compute_frequency_analysis(data = Q_stat,
                                         events = "Year",
                                         values = "Value",
                                         measures = "Measure",
                                         use_max = use_max,
                                         use_log = use_log,
                                         prob_plot_position = prob_plot_position,
                                         prob_scale_points = prob_scale_points,
                                         fit_distr = fit_distr,
                                         fit_distr_method = fit_distr_method,
                                         fit_quantiles = fit_quantiles,
                                         plot_curve = plot_curve)
  
  return(analysis)
  
}
