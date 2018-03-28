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

#' @title Plot annual and seasonal total flows
#' 
#' @description Plots annual and seasonal total flows, volumetric or runoff yield flows, from a streamflow dataset. Calculates 
#'    the statistics from all daily discharge values from all years, unless specified. Data calculated from
#'    plot_annual_cumulative_stats() function. For water year and seasonal data, the designated
#'    year is the year in which the year or season ends. For example, if using water years with a start month of 11, the OND season is
#'    designated by the water year which starts in November (designated by the calendar year in which it ends).
#'
#' @inheritParams calc_annual_cumulative_stats
#' @inheritParams plot_annual_stats
#'    
#' @return A list of the following ggplot2 objects:
#'   \item{TotalQ_Annual}{ggplot2 object of annual total volumetric discharge, in cubic metres}
#'   \item{TotalQ_Two_Seasons}{ggplot2 object of Oct-Mar and Apr-Sep total volumetric discharges, in cubic metres}
#'   \item{TotalQ_Four_Seasons}{ggplot2 object of Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec total volumetric discharges, in cubic metres}
#'   If \code{use_yield} argument is used the list will contain the following objects:
#'   \item{Yield_Annual}{ggplot2 object of annual runoff yield, in millimetres}
#'   \item{Yield_Two_Seasons}{ggplot2 object of Oct-Mar and Apr-Sep runoff yields, in millimetres}
#'   \item{Yield_Four_Seasons}{ggplot2 object of Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec runoff yields, in millimetres}
#'   
#'   
#' @examples
#' \dontrun{
#' 
#' 
#' plot_annual_cumulative_stats(station_number = "08NM116", 
#'                              water_year = TRUE, 
#'                              water_year_start = 8)
#'
#' }
#' @export



plot_annual_cumulative_stats <- function(data = NULL,
                                         dates = Date,
                                         values = Value,
                                         groups = STATION_NUMBER,
                                         station_number = NULL,
                                         use_yield = FALSE, 
                                         basin_area = NA,
                                         water_year = FALSE,
                                         water_year_start = 10,
                                         start_year = 0,
                                         end_year = 9999,
                                         exclude_years = NULL, 
                                         incl_seasons = FALSE,
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
                                values = as.character(substitute(values)),
                                groups = as.character(substitute(groups)),
                                use_groups = TRUE)
  

  ## CALC STATS
  ## ----------
  
  cumulative_stats <- calc_annual_cumulative_stats(data = flow_data,
                                                   use_yield = use_yield, 
                                                   basin_area = basin_area,
                                                   water_year = water_year,
                                                   water_year_start = water_year_start,
                                                   start_year = start_year,
                                                   end_year = end_year,
                                                   exclude_years = exclude_years, 
                                                   incl_seasons = incl_seasons)

  # Remove STATION_NUMBER columns if HYDAT was used and set up data
  if("STATION_NUMBER" %in% colnames(cumulative_stats)) {
    cumulative_stats <- dplyr::ungroup(cumulative_stats)
    cumulative_stats <- dplyr::select(cumulative_stats, -STATION_NUMBER)
  }
  
  # Extract each annual/seasonal datasets
  annual_data <- cumulative_stats[,1:2]
  annual_data <- tidyr::gather(annual_data, Statistic, Value, -Year)
  annual_data <- dplyr::mutate(annual_data, Statistic = substr(Statistic, 1, 6))
  
  if(incl_seasons) {
    seasons2_data <- cumulative_stats[,c(1,3:4)]
    seasons2_data <- tidyr::gather(seasons2_data, Statistic, Value, -Year)
    seasons2_data <- dplyr::mutate(seasons2_data, Statistic = substr(Statistic, 1, 7))
    seasons2_data$Statistic <- factor(seasons2_data$Statistic, levels = unique(seasons2_data$Statistic))
    
    seasons4_data <- cumulative_stats[,c(1,5:8)]
    seasons4_data <- tidyr::gather(seasons4_data, Statistic, Value, -Year)
    seasons4_data <- dplyr::mutate(seasons4_data, Statistic = substr(Statistic, 1, 7))
    seasons4_data$Statistic <- factor(seasons4_data$Statistic, levels = unique(seasons4_data$Statistic))
  }
  
  ## PLOT STATS
  ## ----------
  
  if(!incl_seasons){
    cumulative_plot <- ggplot2::ggplot(data = annual_data, ggplot2::aes(x = Year, y = Value)) +
      ggplot2::geom_line(ggplot2::aes(colour = Statistic), alpha = 0.5) +
      ggplot2::geom_point(ggplot2::aes(colour = Statistic))+
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      ggplot2::ylab("Total Discharge (cubic metres)") +
      {if (use_yield) ggplot2::ylab("Runoff Yield (mm)")} +
      ggplot2::xlab("Year")+
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::theme_bw() +
      ggplot2::guides(colour = FALSE)+
      ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                     panel.grid = ggplot2::element_line(size = .2),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.text = ggplot2::element_text(size = 10))
  }
  
  
  if(incl_seasons){
    
    # Create a list to place the plots
    cumulative_plot <- list()
    
    # Join the season data to loop through
    data_list <- list(Annual = annual_data, "Two_Seasons" = seasons2_data, "Four_Seasons" = seasons4_data)
    
    title_num <- 1 #used to extract the dataframe name
    for (x in data_list) {
      title <- names(data_list[title_num])
      
      plot <- ggplot2::ggplot(data = x, ggplot2::aes(x = Year, y = Value)) +
        ggplot2::geom_line(ggplot2::aes(colour = Statistic), alpha = 0.5) +
        ggplot2::geom_point(ggplot2::aes(colour = Statistic), alpha = 0.5) +
        {if(length(unique(x$Statistic)) > 1) ggplot2::facet_wrap(~Statistic, ncol = 1, strip.position = "right")} +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
        ggplot2::ylab("Total Discharge (cubic metres)") +
        {if (use_yield) ggplot2::ylab("Runoff Yield (mm)")} +
        ggplot2::xlab("Year")+
        ggplot2::scale_color_brewer(palette = "Set1") +
        ggplot2::theme_bw() +
        ggplot2::guides(colour = FALSE)+
        #{if(length(unique(x$Statistic)) > 1) ggplot2::theme(strip.background = ggplot2::element_rect(colour = "black", fill = "white"))} +
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                       panel.grid = ggplot2::element_line(size = .2),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.text = ggplot2::element_text(size = 10))
      cumulative_plot[[paste0(title, ifelse(use_yield, "_Yield", "_Total_Volume"))]] <- plot

      title_num <- title_num + 1
    }
  }
    

  
  
  suppressWarnings(print(
    cumulative_plot
    ))
  
}
