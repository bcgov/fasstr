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


#' @title Plot annual lowflows
#'
#' @description Plot annual n-day minimum values, and the day of year of occurrence of daily flow values from a 
#'    streamflow dataset. Plots statistics from all daily discharge values from all years, unless specified. Data
#'    calculated from calc_annual_lowflows() function.
#'
#' @inheritParams calc_annual_lowflows
#' @inheritParams plot_annual_stats
#'    
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Annual_Minimums}{ggplot2 object of annual minimums of selected n-day rolling means}
#'   \item{Annual_Minimums_Days}{ggplot2 object of the day of years of annual minimums of selected n-day rolling means}
#'   
#' @seealso \code{\link{calc_annual_lowflows}}
#' 
#' @examples
#' \dontrun{
#' 
#' # Plot statistics with default rolling days and alignment
#' plot_annual_lowflows(station_number = "08NM116") 
#' 
#' # Plot statistics with custom rolling days and alignment
#' plot_annual_lowflows(station_number = "08NM116",
#'                      roll_days = c(3,7),
#'                      roll_align = "center")
#' }
#' @export



plot_annual_lowflows <- function(data,
                                 dates = Date,
                                 values = Value,
                                 groups = STATION_NUMBER,
                                 station_number,
                                 roll_days = c(1, 3, 7, 30),
                                 roll_align = "right",
                                 water_year_start = 1,
                                 start_year,
                                 end_year,
                                 exclude_years,
                                 months = 1:12,
                                 ignore_missing = FALSE,
                                 log_discharge = FALSE,
                                 include_title = FALSE){
  
  
  
  ## ARGUMENT CHECKS 
  ## others will be check in calc_ function
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
  
  log_discharge_checks(log_discharge) 
  include_title_checks(include_title)
  
  
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
  
  lowflow_stats <- calc_annual_lowflows(data = flow_data,
                                        roll_days = roll_days,
                                        roll_align = roll_align,
                                        water_year_start = water_year_start,
                                        start_year = start_year,
                                        end_year = end_year,
                                        exclude_years = exclude_years, 
                                        months = months,
                                        ignore_missing = ignore_missing)
  
 
  
  

  ## SET ORDER
  
  
  # Gather data and plot the minimums day
  lowflow_doy <- dplyr::select(lowflow_stats, STATION_NUMBER, Year, dplyr::contains("DoY"))
  stat_levels <- names(lowflow_doy[-(1:2)])
  stat_levels <- substr(stat_levels, 5, nchar(as.character(stat_levels)))
  stat_levels <- paste0(gsub("_Day_DoY", "", stat_levels), " Day Mininum")
  
  lowflow_doy <- tidyr::gather(lowflow_doy, Statistic, Value, -STATION_NUMBER, -Year)
  lowflow_doy <- dplyr::mutate(lowflow_doy, Statistic = substr(Statistic, 5, nchar(as.character(Statistic))))
  lowflow_doy <- dplyr::mutate(lowflow_doy, Statistic = paste0(gsub("_Day_DoY", "", Statistic), " Day Mininum"))
  lowflow_doy <- dplyr::mutate(lowflow_doy, Statistic = as.factor(Statistic))
  levels(lowflow_doy$Statistic) <- stat_levels



  # Gather data and plot the minimums values
  lowflow_values <- dplyr::select(lowflow_stats, STATION_NUMBER, Year, dplyr::contains("Day"), -dplyr::contains("DoY"), -dplyr::contains("Date"))

  lowflow_values <- tidyr::gather(lowflow_values, Statistic, Value, -STATION_NUMBER, -Year)
  lowflow_values <- dplyr::mutate(lowflow_values, Statistic = substr(Statistic, 5, nchar(Statistic)))
  lowflow_values <- dplyr::mutate(lowflow_values, Statistic = paste0(gsub("_Day", "", Statistic), " Day Mininum"))
  lowflow_values <- dplyr::mutate(lowflow_values, Statistic = as.factor(Statistic))
  levels(lowflow_values$Statistic) <- stat_levels


  ## PLOT STATS
  ## ----------
  
  # Create axis label based on input columns
  y_axis_title <- ifelse(as.character(substitute(values)) == "Volume_m3", "Volume (m3)",
                         ifelse(as.character(substitute(values)) == "Yield_mm", "Runoff Yield (mm)", 
                                "Discharge (cms)"))
  
  # Create plots for each STATION_NUMBER in a tibble (see: http://www.brodrigues.co/blog/2017-03-29-make-ggplot2-purrr/)
  doy_plots <- dplyr::group_by(lowflow_doy, STATION_NUMBER)
  doy_plots <- tidyr::nest(doy_plots)
  doy_plots <- dplyr::mutate(doy_plots,
                              plot = purrr::map2(data, STATION_NUMBER, 
        ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, y = Value, color = Statistic)) +
          ggplot2::geom_line(alpha = 0.5, na.rm = TRUE)+
          ggplot2::geom_point(na.rm = TRUE)+
          ggplot2::facet_wrap(~Statistic, ncol = 1, strip.position = "top")+
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
          {if(length(unique(lowflow_doy$Year)) < 8) ggplot2::scale_x_continuous(breaks = unique(lowflow_doy$Year))}+
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
          ggplot2::ylab("Day of Year")+
          ggplot2::xlab("Year")+
         # ggplot2::scale_color_brewer(palette = "Set1") +
          ggplot2::theme_bw() +
          ggplot2::guides(colour = FALSE)+
          {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(paste(.y)) } +
          ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                         panel.grid = ggplot2::element_line(size = .2),
                         axis.title = ggplot2::element_text(size = 12),
                         axis.text = ggplot2::element_text(size = 10),
                         plot.title = ggplot2::element_text(hjust = 1, size = 9, colour = "grey25"),
                         strip.background = ggplot2::element_blank(),
                         strip.text = ggplot2::element_text(hjust = 0, face = "bold", size = 10))
                              ))
  
  flow_plots <- dplyr::group_by(lowflow_values, STATION_NUMBER)
  flow_plots <- tidyr::nest(flow_plots)
  flow_plots <- dplyr::mutate(flow_plots,
                             plot = purrr::map2(data, STATION_NUMBER, 
           ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, y = Value, color = Statistic)) +
             ggplot2::geom_line(alpha = 0.5, na.rm = TRUE)+
             ggplot2::geom_point(na.rm = TRUE)+
             ggplot2::facet_wrap(~Statistic, ncol = 1, strip.position = "top")+
             ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
             {if(length(unique(lowflow_values$Year)) < 8) ggplot2::scale_x_continuous(breaks = unique(lowflow_values$Year))}+
             ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
             ggplot2::ylab(y_axis_title)+
             ggplot2::xlab("Year")+
             ggplot2::scale_color_brewer(palette = "Set1") +
             ggplot2::theme_bw() +
             ggplot2::guides(colour = FALSE)+
             {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(paste(.y)) } +
             ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                            panel.grid = ggplot2::element_line(size = .2),
                            axis.title = ggplot2::element_text(size = 12),
                            axis.text = ggplot2::element_text(size = 10),
                            plot.title = ggplot2::element_text(hjust = 1, size = 9, colour = "grey25"),
                            strip.background = ggplot2::element_blank(),
                            strip.text = ggplot2::element_text(hjust = 0, face = "bold", size = 10))
                             ))
           
  
  # Create a list of named plots extracted from the tibble
  plots_1 <- flow_plots$plot
  plots_2 <- doy_plots$plot
  
  if (nrow(flow_plots) == 1) {
    names(plots_1) <- "Annual_Low_Flows"
    names(plots_2) <- "Annual_Low_Flows_Dates"
  } else {
    names(plots_1) <- paste0(flow_plots$STATION_NUMBER, "_Annual_Low_Flows")
    names(plots_2) <- paste0(doy_plots$STATION_NUMBER, "_Annual_Low_Flows_Dates")
  }
  
  # Add the plots to the plot list
  plots <- c(plots_1, plots_2)
  
  
  
  plots
  
  
}

