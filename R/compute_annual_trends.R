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



#' @title Calculate prewhitened nonlinear trends on streamflow data
#'
#' @description Calculates prewhitened nonlinear trends on annual streamflow data. Uses the
#'    \href{https://cran.r-project.org/web/packages/zyp/index.html}{'zyp'} package to trend. Review 'zyp' to understand its methology.
#'    Use \code{trends_data} if providing your own data frame of statistics to trend. If using \code{data} or \code{station_number}, 
#'    then all annual statistics will be calculated using the calc_all_annual_stats() function which uses the following fasstr functions:
#' \itemize{
#'  \item{calc_annual_stats()}
#'  \item{calc_annual_lowflows()}
#'  \item{calc_annual_cumulative_stats()}
#'  \item{calc_annual_flow_timing()}
#'  \item{calc_monthly_stats()}
#'  \item{calc_annual_outside_normal()}
#'  }
#' 
#' @inheritParams calc_all_annual_stats
#' @param trends_data A data frame of data to trend with column names of years (or other timestep) and rows of statistics. 
#'    Leave blank if using \code{data} or \code{station_number} arguments.
#' @param zyp_method Character string identifying the prewhitened trend method to use from 'zyp', either "zhang' or "yuepilon". 
#'    Required.
#' @param incl_data Logical value indicating whether to include the trending data with the results. Default \code{TRUE}.
#' 
#' @return A data frame containing trends with the following outputs from the zyp package:
#'   \item{Statistic}{the annual statistic used for trending.}
#'   \item{lbound}{the lower bound of the trend's confidence interval}
#'   \item{trend}{the Sens' slope (trend) per unit time}
#'   \item{trendp}{the Sen's slope (trend) over the time period}
#'   \item{ubound}{the upper bound of the trend's confidence interval}
#'   \item{tau}{Kendall's tau statistic computed on the final detrended timeseries}
#'   \item{sig}{Kendall's P-value computed for the final detrended timeseries}
#'   \item{nruns}{the number of runs required to converge upon a trend}
#'   \item{autocor}{the autocorrelation of the final detrended timeseries}
#'   \item{valid_frac}{the fraction of the data which is valid (not NA) once autocorrelation is removed}
#'   \item{linear}{the least squares fit trend on the same dat}
#'   \item{intercept}{the lower bound of the trend's confidence interval}
#'   \item{lbound}{the intercept of the Sen's slope (trend)}
#'   and the following additional columns:
#'   \item{min_year}{the minimum year used in the trending}
#'   \item{max_year}{the maximum year used in the trending}
#'   \item{n_years}{the number of years with data for trending}
#'   \item{mean}{the mean of all values used for trending}
#'   \item{median}{the median of all values used for trending}
#'   \item{min}{the minimum of all values used for trending}
#'   \item{max}{the maximum of all values used for trending}
#'   Transposing data creates a column of "Trends_Statistics" and subsequent columns of each trends statistic for each annual statistic.
#'   
#' @examples
#' \dontrun{
#' 
#' compute_annual_trends(station_number = "08NM116", 
#'                       water_year = TRUE, 
#'                       water_year_start = 8,
#'                       zyp_method = "yuepilon")
#'
#' }
#' @export



compute_annual_trends <- function(data = NULL,
                                  dates = Date,
                                  values = Value,
                                  groups = STATION_NUMBER,
                                  station_number = NULL,
                                  trends_data = NULL,
                                  zyp_method = NA,
                                  basin_area = NA, 
                                  water_year = FALSE,
                                  water_year_start = 10,
                                  start_year = 0,
                                  end_year = 9999,
                                  exclude_years = NULL,
                                  annual_percentiles = c(10,90),
                                  monthly_percentiles = c(10,20),
                                  stats_days = 1,
                                  stats_align = "right",
                                  lowflow_days = c(1,3,7,30),
                                  lowflow_align = "right",
                                  timing_percent = c(25,33,50,75),
                                  normal_percentiles = c(25,75),
                                  ignore_missing = FALSE,
                                  incl_data = TRUE){       
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  
  if (is.na(zyp_method) | !zyp_method %in% c("yuepilon", "zhang") )   
    stop('zyp_trending argument must be either "yuepilon" or "zhang"', call. = FALSE)
  if (!is.logical(incl_data))  
    stop("incl_data argument must be logical (TRUE/FALSE).", call. = FALSE)
  
  if (!is.null(trends_data) & !is.null(data) & !is.null(station_number))  
    stop("Cannot use 'data', 'station_number', and 'trends_data' arguments together. Select one method of data input.", call. = FALSE)
  if (!is.null(trends_data) & !is.null(data))  
    stop("Cannot use 'data' and 'trends_data' arguments together. Select one method of data input.", call. = FALSE)
  if (!is.null(trends_data) & !is.null(station_number))  
    stop("Cannot use 'station_number' and 'trends_data' arguments together. Select one method of data input.", call. = FALSE)
  
  ## CHECKS ON TRENDS DATA
  ## ---------------------
  
  if (!is.null(trends_data))  {
    if (!is.data.frame(trends_data))  stop("trends_data argument is not a data frame.", call. = FALSE)
    
    # Save the original columns (to check for groups column later) and ungroup
    orig_cols <- names(trends_data)
    trends_data <- dplyr::ungroup(trends_data)
    
    # If no groups (default STATION_NUMBER) in data, make it so (required)
    if (!as.character(substitute(groups)) %in% orig_cols) {
      trends_data[, as.character(substitute(groups))] <- "XXXXXXX"
    }
    
    names(trends_data)[names(trends_data) == as.character(substitute(groups))] <- "STATION_NUMBER"
    
  } else {
    
    ## CHECKS ON FLOW DATA
    ## -------------------
    
    if (is.null(data) & is.null(station_number))
      stop("No data provided. Must provide daily data with 'data' or 'station_number' to analyze, or timestep data to trend using 'trends_data'.", call. = FALSE)
    
    # Check if data is provided and import it
    flow_data <- flowdata_import(data = data, 
                                 station_number = station_number)
    
    # Save the original columns (to check for STATION_NUMBER col at end) and ungroup if necessary
    orig_cols <- names(flow_data)
    flow_data <- dplyr::ungroup(flow_data)
    
    # Check and rename columns
    flow_data <- format_all_cols(data = flow_data,
                                 dates = as.character(substitute(dates)),
                                 values = as.character(substitute(values)),
                                 groups = as.character(substitute(groups)),
                                 rm_other_cols = TRUE)
    
    trends_data <- calc_all_annual_stats(data = flow_data,
                                         basin_area = basin_area, 
                                         water_year = water_year,
                                         water_year_start = water_year_start,
                                         start_year = start_year,
                                         end_year = end_year,
                                         exclude_years = exclude_years,
                                         annual_percentiles = annual_percentiles,
                                         monthly_percentiles = monthly_percentiles,
                                         stats_days = stats_days,
                                         stats_align = stats_align,
                                         lowflow_days = lowflow_days,
                                         lowflow_align = lowflow_align,
                                         timing_percent = timing_percent,
                                         normal_percentiles = normal_percentiles,
                                         transpose = TRUE,
                                         ignore_missing = ignore_missing)

    # Compute some summary stats on the input data
  colnames(trends_data)[2] <- "Statistic"
  trends_data_summary <- tidyr::gather(trends_data, Year, Value, 3:ncol(trends_data))
  trends_data_summary <- trends_data_summary[complete.cases(trends_data_summary$Value), ]
  trends_data_summary <- dplyr::summarise(dplyr::group_by(trends_data_summary, STATION_NUMBER, Statistic),
                                          min_year = as.numeric(min(Year, na.rm = TRUE)),
                                          max_year = as.numeric(max(Year, na.rm = TRUE)),
                                          n_years = sum(!is.na(Value)),
                                          mean = mean(Value, na.rm = TRUE),
                                          median = stats::median(Value, na.rm = TRUE),
                                          min = min(Value, na.rm = TRUE),
                                          max = max(Value, na.rm = TRUE))

  # Complete trends analysis
  trends_results <- zyp::zyp.trend.dataframe(indat = trends_data,
                                             metadata.cols = 2,
                                             method = zyp_method)
  
  }

  # Merge the summary stats with the results
  trends_results <- merge(trends_results, trends_data_summary, by = c("STATION_NUMBER", "Statistic"), all = TRUE)
  if(incl_data){
    trends_results <- merge(trends_results, trends_data, by = c("STATION_NUMBER", "Statistic"), all = TRUE)
  }

  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if("STATION_NUMBER" %in% orig_cols) {
    names(trends_results)[names(trends_results) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    trends_results <- dplyr::select(trends_results, -STATION_NUMBER)
  }

  dplyr::as_tibble(trends_results)
} 

