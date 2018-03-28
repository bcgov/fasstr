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


#' @title Plot prewhitened nonlinear annual trends on streamflow data
#'
#' @description Plots the annual data and significance trend from results from a compute_annual_trends() analysis data frame.
#' 
#' @param trends_results A data frame of results from the compute_annual_trends() function, with original data included
#'    (compute_annual_trends(incl_data = TRUE)). Uses the only the "Statistics" and zyp results columns, along with the 
#'    annual data to plot.
#' @param zyp_alpha Numeric value of the significance level (ex. 0.05) of when to plot a trend line. Leave blank for no line.
#' 
#' @return A list of ggplot2 objects with plots of trended annual statistics, as the name of each object, with a trend line
#'    plotted if less than the zyp_alpha provided.
#'
#' @examples
#' \dontrun{
#' 
#' trends <- compute_annual_trends(station_number = "08NM116",
#'                                 zyp_method = "yuepilon",
#'                                 start_year = 1980, 
#'                                 end_year = 2010)
#'  
#' plot_annual_trends(trends_results = trends,
#'                    zyp_alpha = 0.05)
#'
#' }
#' @export



plot_annual_trends <- function(trends_results = NULL,
                               zyp_alpha = NA){          
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if(is.null(trends_results))        
    stop("Must provide a data frame of compute_annual_trends() results with incl_data = TRUE.", call. = FALSE)
  if(!is.data.frame(trends_results)) 
    stop("Must provide a data frame of compute_annual_trends() results with incl_data = TRUE.", call. = FALSE)
  
  if(!all(c("lbound", "trend", "trendp", "tau", "sig", "linear", "n_years") %in% names(trends_results)))
    stop("Must provide a data frame of compute_annual_trends() results with incl_data = TRUE.", call. = FALSE)
  
  if(!is.na(zyp_alpha) & !is.numeric(zyp_alpha) )              
    stop("zyp_alpha must be numeric.", call. = FALSE)
  if(!is.na(zyp_alpha) & !all(zyp_alpha > 0 & zyp_alpha < 1))  
    stop("timing_percent must be > 0 and < 1)", call. = FALSE)
  
  
  ## EXTRACT DATA AND RESULTS
  ## ------------------------
  
  if ("STATION_NUMBER" %in% colnames(trends_results)) {
    trends_results <- dplyr::select(trends_results, -STATION_NUMBER)
  }
  
  trends_data <- trends_results[,c(1, 20:ncol(trends_results))]
  trends_results <- trends_results[,c(1:19)]
  
  
  ## PLOT TRENDS
  ## -----------
  
  # Create the list to place all plots
  trends_plots <- list()
  
  
  # Set data for plotting
  trends_data <- tidyr::gather(trends_data, Year, Value, -1)
  trends_data <- dplyr::mutate(trends_data, Year = as.numeric(Year))
  
  trends_data <- dplyr::mutate(trends_data,
                               Units= "Discharge (cms)",
                               Units = replace(Units, grepl("Yield_mm", Statistic), "Runoff Yield (mm)"),
                               Units = replace(Units, grepl("TotalQ_m3", Statistic), "Total Discharge (cubic metres)"),
                               Units = replace(Units, grepl("DoY", Statistic), "Day of Year"),
                               Units = replace(Units, grepl("Days", Statistic), "Number of Days"))
  
  
  # Loop through each statistic and plot the annual data, add trendline if < zyp_alpha
  for (stat in unique(trends_results$Statistic)){
    # Filter for metric
    trends_data_stat <- dplyr::filter(trends_data, Statistic == stat)
    trends_results_stat <- dplyr::filter(trends_results, Statistic == stat)
    int <- trends_results_stat$intercept - trends_results_stat$trend * trends_results_stat$min_year
    # Plot each metric
    trends_plot <- ggplot2::ggplot(trends_data_stat, ggplot2::aes(x = Year, y = Value)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(alpha = 0.3) +
      ggplot2::ggtitle(paste0(stat,"   (Sig. = ", round(trends_results_stat$sig, 3), ")")) +
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
        ggplot2::geom_abline(slope = trends_results_stat$trend, intercept = int, colour = "red")
    }
    
    trends_plots[[paste0(stat, "-trends")]] <- trends_plot
    
    
    
  }
  
  suppressWarnings(print(
    trends_plots
  ))
  
}

