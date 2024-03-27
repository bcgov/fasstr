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

#' @title Plot monthly summary statistics (as ribbons)
#'
#' @description Plots means, medians, maximums, minimums, and percentiles as ribbons for each month of all years of flow values 
#'    from a daily streamflow data set. Calculates statistics from all values, unless specified. Data calculated using the 
#'    \code{calc_monthly_stats()} function. Produces a list containing a plot for each statistic. Returns a list of plots.
#' 
#' @inheritParams calc_monthly_stats
#' @inheritParams plot_monthly_stats
#' @inheritParams plot_annual_stats
#' @inheritParams plot_annual_stats2
#' 
#' @return A list of ggplot2 objects for each monthly statistic for each station provided that contain:
#'   \item{Monthly Mean Flows}{mean of all daily flows for a given month and year}
#'   \item{Monthly Median Flows}{median of all daily flows for a given month and year}
#'   \item{Monthly Maximum Flows}{maximum of all daily flows for a given month and year}
#'   \item{Monthly Minimum Flows}{minimum of all daily flows for a given month and year}
#'   \item{Monthly P'n' Flows}{(optional) each n-th percentile selected for a given month and year}
#'   
#' @seealso \code{\link{calc_monthly_stats}}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot monthly statistics using a data frame and data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' results <- plot_monthly_stats2(data = flow_data,
#'                                start_year = 1980)
#' 
#' # Plot monthly statistics for water years starting in October
#' results <- plot_monthly_stats2(station_number = "08NM116",
#'                               start_year = 1980,
#'                               end_year = 2010,
#'                               water_year_start = 10)
#'                    
#' }
#' @export



plot_monthly_stats2 <- function(data,
                                dates = Date,
                                values = Value,
                                groups = STATION_NUMBER,
                                station_number,
                                roll_days = 1,
                                roll_align = "right",
                                water_year_start = 1,
                                start_year,
                                end_year,
                                exclude_years,
                                months = 1:12,
                                complete_years = FALSE,
                                ignore_missing = FALSE,
                                allowed_missing = ifelse(ignore_missing,100,0),
                                plot_extremes = TRUE,
                                plot_inner_percentiles = TRUE,
                                plot_outer_percentiles = TRUE,
                                inner_percentiles = c(25,75),
                                outer_percentiles = c(5,95),
                                log_discharge = TRUE,
                                log_ticks = ifelse(log_discharge, TRUE, FALSE),
                                scales_discharge = "fixed",
                                include_title = FALSE){
  
  
  ## ARGUMENT CHECKS 
  ## others will be check in calc_ function
  ## ---------------
  
  if (missing(data)) {
    data <- NULL
  }
  if (missing(station_number)) {
    station_number <- NULL
  }
  if (missing(start_year)) {
    start_year <- 0
  }
  if (missing(end_year)) {
    end_year <- 9999
  }
  if (missing(exclude_years)) {
    exclude_years <- NULL
  }
  
  logical_arg_check(log_discharge)
  log_ticks_checks(log_ticks, log_discharge)
  logical_arg_check(include_title)
  ptile_ribbons_checks(inner_percentiles, outer_percentiles)
  scales_checks(scales_discharge)
  if (scales_discharge == "free") scales_discharge <- "free_y"
  logical_arg_check(plot_extremes)
  logical_arg_check(plot_inner_percentiles)
  logical_arg_check(plot_outer_percentiles)
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)),
                               rm_other_cols = TRUE)
  
  
  
  ## CALC STATS
  ## ----------
  
  monthly_data <- calc_monthly_stats(data = flow_data,
                                     percentiles = c(inner_percentiles, outer_percentiles),
                                     roll_days = roll_days,
                                     roll_align = roll_align,
                                     water_year_start = water_year_start,
                                     start_year = start_year,
                                     end_year = end_year,
                                     exclude_years = exclude_years, 
                                     months = months,
                                     complete_years = complete_years,
                                     ignore_missing = ignore_missing,
                                     allowed_missing = allowed_missing)
  
  if (complete_years) {
    # Remove all leading NA years
    monthly_data <- dplyr::filter(dplyr::group_by(monthly_data, STATION_NUMBER),
                                  Year >= Year[min(which(!is.na(.data[[names(monthly_data)[4]]])))])
  }
  
  # monthly_data
  ## PLOT STATS
  ## ----------
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (cubic metres)", #expression(Volume~(m^3))
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Yield (mm)", 
                                "Discharge (cms)")) #expression(Discharge~(m^3/s))
  
  fill_manual_list <- c()
  if (plot_extremes) {
    fill_manual_list <- c(fill_manual_list, "lightblue2")
    names(fill_manual_list) <- c(names(fill_manual_list), "Minimum-Maximum")
  }
  
  if (is.numeric(outer_percentiles)) {
    fill_manual_list <- c(fill_manual_list, "lightblue3")
    outer_name <- paste0(min(outer_percentiles),"-",max(outer_percentiles), " Percentiles")
    names(fill_manual_list) <- c(names(fill_manual_list)[1:(length(fill_manual_list)-1)], outer_name)
  }
  
  if (is.numeric(inner_percentiles)) {
    fill_manual_list <- c(fill_manual_list, "lightblue4")
    inner_name <- paste0(min(inner_percentiles),"-",max(inner_percentiles), " Percentiles")
    names(fill_manual_list) <- c(names(fill_manual_list)[1:(length(fill_manual_list)-1)], inner_name)
  }
  
  colour_manual_list <- c("Mean" = "paleturquoise", "Median" = "dodgerblue4")
  colour_manual_labels <- c("Mean", "Median")
  
  # Create the daily stats plots
  monthly_plots <- dplyr::group_by(monthly_data, STATION_NUMBER)
  monthly_plots <- tidyr::nest(monthly_plots)
  monthly_plots <- dplyr::mutate(
    monthly_plots,
    plot = purrr::map2(
      data, STATION_NUMBER,
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year)) +
        {if(plot_extremes) ggplot2::geom_ribbon(ggplot2::aes(ymin = Minimum, ymax = Maximum, fill = "Minimum-Maximum"), na.rm = FALSE)} +
        {if(is.numeric(outer_percentiles) & plot_outer_percentiles)
          ggplot2::geom_ribbon(ggplot2::aes_string(ymin = paste0("P",min(outer_percentiles)),
                                                   ymax = paste0("P",max(outer_percentiles)),
                                                   fill = paste0("'",outer_name,"'")), na.rm = FALSE)} +
        {if(is.numeric(inner_percentiles) & plot_inner_percentiles)
          ggplot2::geom_ribbon(ggplot2::aes_string(ymin = paste0("P",min(inner_percentiles)),
                                                   ymax = paste0("P",max(inner_percentiles)),
                                                   fill = paste0("'",inner_name,"'")), na.rm = FALSE)} +
        ggplot2::geom_line(ggplot2::aes(y = Median, colour = "Median"), size = 0.5, na.rm = TRUE) +
        ggplot2::geom_line(ggplot2::aes(y = Mean, colour = "Mean"), size = 0.5, na.rm = TRUE) +
        ggplot2::facet_wrap(~Month, scales = scales_discharge, strip.position = "top") +
        ggplot2::scale_x_continuous(expand = c(0,0))+
        {if(!log_discharge) ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.02)),
                                                        breaks = scales::pretty_breaks(n = 8),
                                                        labels = scales::label_number(scale_cut = append(scales::cut_short_scale(),1,1)))} +
        {if(log_discharge) ggplot2::scale_y_log10(expand = ggplot2::expansion(mult = c(0.02, 0.02)),
                                                  breaks = scales::log_breaks(n = 8, base = 10),
                                                  labels = scales::label_number(scale_cut = append(scales::cut_short_scale(),1,1)))} +  
        {if(log_discharge) ggplot2::annotation_logticks(base= 10, "left", colour = "grey25", size = 0.3,
                                                        short = ggplot2::unit(.07, "cm"), mid = ggplot2::unit(.15, "cm"),
                                                        long = ggplot2::unit(.2, "cm")) }+
        ggplot2::xlab(ifelse(water_year_start ==1, "Year", "Water Year"))+
        ggplot2::ylab("Discharge (cms)")+
        ggplot2::theme_bw()+
        ggplot2::labs(color = 'Monthly Statistics') +
        {if (include_title & .y != "XXXXXXX") ggplot2::labs(color = paste0(.y,'\n \nMonthly Statistics')) } +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 10, colour = "grey25"),
                       axis.title = ggplot2::element_text(size = 12, colour = "grey25"),
                       axis.ticks = ggplot2::element_line(size = .1, colour = "grey25"),
                       axis.ticks.length = ggplot2::unit(0.05, "cm"),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,0,0,0)),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(size = .1),
                       legend.text = ggplot2::element_text(size = 9, colour = "grey25"),
                       legend.box = "vertical",
                       legend.justification = "right",
                       legend.key.size = ggplot2::unit(0.4, "cm"),
                       legend.spacing = ggplot2::unit(-0.4, "cm"),
                       legend.background = ggplot2::element_blank()) +
        ggplot2::guides(colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2, title = NULL)) +
        ggplot2::scale_fill_manual(values = fill_manual_list) +
        ggplot2::scale_color_manual(values = colour_manual_list, labels = colour_manual_labels)
    ))
  
  
  # Create a list of named plots extracted from the tibble
  plots <- monthly_plots$plot
  if (length(unique(monthly_plots$STATION_NUMBER)) == 1) {
    names(plots) <- "Monthly_Statistics"
  } else {
    names(plots) <- paste0(monthly_plots$STATION_NUMBER, "_Monthly_Statistics")
  }
  plots
}

