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


#' @title Plot annual lowflows
#'
#' @description Plot annual n-day minimum values, and the day of year of occurrence of daily flow values from a 
#'    streamflow dataset. Plots statistics from all daily discharge values from all years, unless specified. Data
#'    calculated from calc_annual_lowflows() function.
#'
#' @inheritParams calc_annual_flow_timing
#' @inheritParams plot_annual_stats
#'    
#' @return A list of the following ggplot2 objects:
#'   \item{Annual_Minimums}{ggplot2 object of annual minimums of selected n-day rolling means}
#'   \item{Annual_Minimums_Days}{ggplot2 object of the day of years of annual minimums of selected n-day rolling means}
#'   
#' @examples
#' \dontrun{
#' 
#' plot_annual_lowflows(data = "08NM116", 
#'                      water_year = TRUE, 
#'                      water_year_start = 8, 
#'                      roll_days = c(3,7))
#'
#' }
#' @export



plot_annual_lowflows <- function(data = NULL,
                                 dates = Date,
                                 values = Value,
                                 station_number = NULL,
                                 roll_days = c(1, 3, 7, 30),
                                 roll_align = "right",
                                 water_year = FALSE,
                                 water_year_start = 10,
                                 start_year = 0,
                                 end_year = 9999,
                                 exclude_years = NULL,
                                 months = 1:12,
                                 ignore_missing = FALSE,
                                 log_discharge = FALSE){
  
  
  
  ## ARGUMENT CHECKS 
  ## others will be check in calc_ function
  ## ---------------
  
  log_discharge_checks(log_discharge) 
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, station_number = station_number)
  
  # Check and rename columns
  flow_data <- format_plot_cols(data = flow_data, 
                                dates = as.character(substitute(dates)),
                                values = as.character(substitute(values)))
  
  
  ## CALC STATS
  ## ----------
  
  lowflow_stats <- calc_annual_lowflows(data = flow_data,
                                        roll_days = roll_days,
                                        roll_align = roll_align,
                                        water_year = water_year,
                                        water_year_start = water_year_start,
                                        start_year = start_year,
                                        end_year = end_year,
                                        exclude_years = exclude_years, 
                                        months = months,
                                        ignore_missing = ignore_missing)
  
  # Remove STATION_NUMBER columns if HYDAT was used and set up data
  if("STATION_NUMBER" %in% colnames(lowflow_stats)) {
    lowflow_stats <- dplyr::ungroup(lowflow_stats)
    lowflow_stats <- dplyr::select(lowflow_stats, -STATION_NUMBER)
  }

  ## SET ORDER
  
  
  # Gather data and plot the minimums day
  lowflow_doy <- dplyr::select(lowflow_stats, Year, dplyr::contains("DoY"))
  stat_levels <- names(lowflow_doy[-1])
  stat_levels <- substr(stat_levels, 5, nchar(as.character(stat_levels)))
  stat_levels <- paste0(gsub("_Day_DoY", "", stat_levels), " Day Mininum")
  
  lowflow_doy <- tidyr::gather(lowflow_doy, Statistic, Value, -1)
  lowflow_doy <- dplyr::mutate(lowflow_doy, Statistic = substr(Statistic, 5, nchar(as.character(Statistic))))
  lowflow_doy <- dplyr::mutate(lowflow_doy, Statistic = paste0(gsub("_Day_DoY", "", Statistic), " Day Mininum"))
  lowflow_doy <- dplyr::mutate(lowflow_doy, Statistic = as.factor(Statistic))
  levels(lowflow_doy$Statistic) <- stat_levels

  
  

  # Gather data and plot the minimums values
  lowflow_values <- dplyr::select(lowflow_stats, Year, dplyr::contains("Day"), -dplyr::contains("DoY"), -dplyr::contains("Date"))

  lowflow_values <- tidyr::gather(lowflow_values, Statistic, Value, -1)
  lowflow_values <- dplyr::mutate(lowflow_values, Statistic = substr(Statistic, 5, nchar(Statistic)))
  lowflow_values <- dplyr::mutate(lowflow_values, Statistic = paste0(gsub("_Day", "", Statistic), " Day Mininum"))
  lowflow_values <- dplyr::mutate(lowflow_values, Statistic = as.factor(Statistic))
  levels(lowflow_values$Statistic) <- stat_levels
  

  ## PLOT STATS
  ## ----------


  doy_plot <- ggplot2::ggplot(data = lowflow_doy, ggplot2::aes(x = Year, y = Value))+
    ggplot2::geom_line(ggplot2::aes(colour = Statistic), alpha = 0.5)+
    ggplot2::geom_point(ggplot2::aes(colour = Statistic))+
    ggplot2::facet_wrap(~Statistic, ncol = 1, strip.position = "right")+
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
    ggplot2::ylab("Day of Year")+
    ggplot2::xlab("Year")+
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme_bw() +
    ggplot2::guides(colour = FALSE)+
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                   panel.grid = ggplot2::element_line(size = .2),
                   axis.title = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 10))


  min_plot <- ggplot2::ggplot(data = lowflow_values, ggplot2::aes(x = Year, y = Value))+
    ggplot2::geom_line(ggplot2::aes(colour = Statistic))+
    ggplot2::geom_point(ggplot2::aes(colour = Statistic))+
    ggplot2::facet_wrap(~Statistic, ncol = 1, strip.position = "right")+
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
    ggplot2::ylab("Discharge (cms)")+
    ggplot2::xlab("Year")+
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme_bw() +
    ggplot2::guides(colour = FALSE)+
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                   panel.grid = ggplot2::element_line(size = .2),
                   axis.title = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 10))



  low_flow_plots <- list()
  low_flow_plots[["Annual_Minimums"]] <- min_plot
  low_flow_plots[["Annual_Minimum_Days"]] <- doy_plot


  return(low_flow_plots)
  
}

