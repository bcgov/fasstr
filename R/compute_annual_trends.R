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



#' @title Calculate prewhitened nonlinear annual trends on streamflow data
#'
#' @description Calculates prewhitened nonlinear trends on annual streamflow data. Uses the
#'    \href{https://cran.r-project.org/web/packages/zyp/index.html}{'zyp'} package to trend. Review 'zyp' to understand its methology.
#'    All annual statistics calculated using the calc_all_annual_stats() function which uses the following fasstr functions:
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
#' @param zyp_method Character string identifying the prewhitened trend method to use from 'zyp', either "zhang' or "yuepilon". 
#'    Required.
#' @param include_plots Logical value indicating if annual trending plots should be included. Default \code{TRUE}.
#' @param zyp_alpha Numeric value of the significance level (ex. 0.05) of when to plot a trend line. Leave blank for no line.
#' 
#' @return A list of tibbles and optional plots from the trending analysis including:
#'   \item{Annual_Trends_Data}{a tibbble of the annual statistics used for trending}
#'   \item{Annual_Trends_Results}{a tibble of the results of the zyp trending analysis}
#'   \item{Annual_*}{each ggplot2 object for each annual trended statistic}
#' 
#' @references References from zyp package:
#' \itemize{
#'  \item{Wang, X.L. and Swail, V.R., 2001. Changes in extreme wave heights in northern hemisphere oceans and 
#'        related atmospheric circulation regimes. Journal of Climate, 14: 2204-2221.}
#'  \item{Yue, S., P. Pilon, B. Phinney and G. Cavadias, 2002. The influence of autocorrelation on the ability
#'        to detect trend in hydrological series. Hydrological Processes, 16: 1807-1829.}
#'  \item{Zhang, X., Vincent, L.A., Hogg, W.D. and Niitsoo, A., 2000. Temperature and Precipitation Trends in
#'        Canada during the 20th Century. Atmosphere-Ocean 38(3): 395-429.}
#'  \item{Sen, P.K., 1968. Estimates of the Regression Coefficient Based on Kendall's Tau. Journal of the 
#'        American Statistical Association Vol. 63, No. 324: 1379-1389.}
#'        }
#'      
#' @seealso \code{\link[zyp]{zyp-package}}, 
#'          \code{\link[zyp]{zyp.trend.dataframe}}, 
#'          \code{\link{calc_all_annual_stats}}
#'   
#' @examples
#' \dontrun{
#' 
#' # Compute trends statistics using data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' compute_annual_trends(data = flow_data,
#'                       zyp_method = "yuepilon")
#' 
#' # Compute trends statistics using station_number with defaults
#' compute_annual_trends(station_number = "08NM116",
#'                       zyp_method = "yuepilon")
#'                       
#' # Compute trends statistics and plot a trend line if the significance is less than 0.05
#' compute_annual_trends(station_number = "08NM116",
#'                       zyp_method = "yuepilon",
#'                       zyp_alpha = 0.05)
#'                       
#' # Compute trends statistics and do not plot the results
#' compute_annual_trends(station_number = "08NM116",
#'                       zyp_method = "yuepilon",
#'                       include_plots = FALSE)
#' }
#' @export



compute_annual_trends <- function(data,
                                  dates = Date,
                                  values = Value,
                                  groups = STATION_NUMBER,
                                  station_number,
                                  zyp_method,
                                  basin_area, 
                                  water_year_start = 1,
                                  start_year,
                                  end_year,
                                  exclude_years,
                                  annual_percentiles = c(10,90),
                                  monthly_percentiles = c(10,20),
                                  stats_days = 1,
                                  stats_align = "right",
                                  lowflow_days = c(1,3,7,30),
                                  lowflow_align = "right",
                                  timing_percent = c(25,33,50,75),
                                  normal_percentiles = c(25,75),
                                  ignore_missing = FALSE,
                                  include_plots = TRUE,
                                  zyp_alpha){       
  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if (missing(data)) {
    data = NULL
  }
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
  if (missing(basin_area)) {
    basin_area = NA
  }
  if (missing(zyp_method)) {
    zyp_method = NA
  }
  if (missing(zyp_alpha)) {
    zyp_alpha = NA
  }
  
  zyp_method_checks(zyp_method)
  zyp_alpha_checks(zyp_alpha)
  if (!is.logical(include_plots))         
    stop("include_plots must be logical (TRUE/FALSE).", call. = FALSE)
  
  
  ## CHECKS ON FLOW DATA
  ## -------------------
  
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
  trends_data_summary <- trends_data_summary[stats::complete.cases(trends_data_summary$Value), ]
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
  
  
  # Merge the summary stats with the results
  trends_results <- merge(trends_results, trends_data_summary, by = c("STATION_NUMBER", "Statistic"), all = TRUE)
  
  # Order the list of stats in order of all_stats
  trends_results <- dplyr::arrange(trends_results, STATION_NUMBER, Statistic)
  
  
  # Create plots if required
  if (include_plots) {
    
    # Create the list to place all plots
    plots_list <- list()
    
    for (stn in unique(trends_results$STATION_NUMBER)) {
      
      trends_results_stn <- dplyr::filter(trends_results, STATION_NUMBER == stn)
      trends_results_stn <- dplyr::select(trends_results_stn, -STATION_NUMBER)
      
      trends_data_stn <- dplyr::filter(trends_data, STATION_NUMBER == stn)
      trends_data_stn <- dplyr::select(trends_data_stn, -STATION_NUMBER)
      
      ## PLOT TRENDS
      ## -----------
      
      # Set data for plotting
      trends_data_stn <- tidyr::gather(trends_data_stn, Year, Value, -1)
      trends_data_stn <- dplyr::filter(trends_data_stn, Year >= min(trends_results_stn$min_year, na.rm = TRUE))
      
      trends_data_stn <- dplyr::mutate(trends_data_stn, Year = as.numeric(Year))
      
      trends_data_stn <- dplyr::mutate(trends_data_stn,
                                       Units= "Discharge (cms)",
                                       Units = replace(Units, grepl("Yield_mm", Statistic), "Yield (mm)"),
                                       Units = replace(Units, grepl("Volume_m3", Statistic), "Volume (m3)"),
                                       Units = replace(Units, grepl("DoY", Statistic), "Day of Year"),
                                       Units = replace(Units, grepl("Days", Statistic), "Number of Days"))
      
      
      # Loop through each statistic and plot the annual data, add trendline if < zyp_alpha
      for (stat in unique(trends_results_stn$Statistic)){
        # Filter for metric
        trends_data_stat <- dplyr::filter(trends_data_stn, Statistic == stat)
        trends_results_stat <- dplyr::filter(trends_results_stn, Statistic == stat)
        int <- trends_results_stat$intercept - trends_results_stat$trend * trends_results_stat$min_year
        # Plot each metric
        trends_plot <- ggplot2::ggplot(trends_data_stat, ggplot2::aes(x = Year, y = Value)) +
          ggplot2::geom_point(shape = 1, size = 2, colour = "darkblue", stroke = 2, na.rm = TRUE) +
          # ggplot2::geom_line(alpha = 0.3, na.rm = TRUE) +
          ggplot2::ggtitle(paste0(stat," (sig. = ", round(trends_results_stat$sig, 3), ")")) +
          #{if(length(unique(trends_results$STATION_NUMBER)) > 1) ggplot2::ggtitle(paste0(stn, ": ", stat,"   (Sig. = ", round(trends_results_stat$sig, 3), ")"))} +
          ggplot2::xlab("Year") +
          ggplot2::ylab(trends_data_stat$Units) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                         panel.grid = ggplot2::element_line(size = .2),
                         axis.title = ggplot2::element_text(size = 12),
                         axis.text = ggplot2::element_text(size = 10))
        # If sig. trend, plot trend
        if(!is.na(zyp_alpha) & trends_results_stat$sig < zyp_alpha & !is.na(trends_results_stat$sig)) {
          trends_plot <- trends_plot +
            ggplot2::geom_abline(slope = trends_results_stat$trend, intercept = int, colour = "red", linetype = "longdash")
        }
        
        if (length(unique(trends_results$STATION_NUMBER)) == 1) {
          plots_list[[ stat ]] <- trends_plot
        } else {
          plots_list[[ paste0(stn, "_", stat) ]] <- trends_plot
        }
      }
    }
  }
  
  
  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if("STATION_NUMBER" %in% orig_cols) {
    names(trends_results)[names(trends_results) == "STATION_NUMBER"] <- as.character(substitute(groups))
    names(trends_data)[names(trends_data) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    trends_results <- dplyr::select(trends_results, -STATION_NUMBER)
    trends_data <- dplyr::select(trends_data, -STATION_NUMBER)
  }
  
  # Create list of objects
  trends_list <- list("Annual_Trends_Data" = dplyr::as_tibble(trends_data),
                      "Annual_Trends_Results" = dplyr::as_tibble(trends_results))
  
  # Add plots to list
  if (include_plots) {
    trends_list <- append(trends_list, plots_list)
  }
  
  return(trends_list)
  
} 

