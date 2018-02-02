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
#' @description Plots annual and seasonal total flows, volumetric and runoff yield flows, from a streamflow dataset. Calculates 
#'    the statistics from all daily discharge values from all years, unless specified. Data calculated from
#'    plot_annual_cumulative_stats() function. For water year and seasonal data, the designated
#'    year is the year in which the year or season ends. For example, if using water years with a start month of 11, the OND season is
#'    designated by the water year which starts in November (designated by the calendar year in which it ends).
#'
#' @param data Daily data to be analyzed. Options:
#' 
#'    A data frame of daily data that contains columns of dates and values.
#'    
#'    A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param dates Column in the \code{data} data frame that contains dates formatted YYYY-MM-DD. Only required if
#'    using the data frame option of \code{data} and dates column is not named 'Date'. Default \code{Date}. 
#' @param values Column in the \code{data} data frame that contains numeric flow values, in units of cubic metres per second.
#'    Only required if using the data frame option of \code{data} and values column is not named 'Value'. Default \code{Value}. 
#' @param use_yield Logical value indicating whether to use yield runoff, in mm, instead of volumetric. Default \code{FALSE}.
#' @param basin_area Upstream drainage basin area to apply to daily observations. Options:
#'    
#'    Leave blank if \code{groups} is STATION_NUMBER with HYDAT station numbers to extract basin areas from HYDAT.
#'    
#'    Single numeric value to apply to all observations.
#'    
#'    List each basin area for each grouping factor (can override HYDAT value) as such \code{c("08NM116" = 795, "08NM242" = 10)}.
#'    Factors not listed will result in NA basin areas.
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#' @param exclude_years Numeric vector of years to exclude from analysis. Leave blank to include all years.             
#' @param incl_seasons Logical value indication whether to include seasonal yields and total discharges. Default \code{TRUE}.
#' @param log_discharge Logical value to indicate plotting the discharge axis (Y-axis) on a logarithmic scale. Default \code{TRUE}.
#'    
#' @return A list of the following ggplot2 objects (yield plots unavailable if no basin_area):
#'   \item{TotalQ_Annual}{ggplot2 object of annual total volumetric discharge, in cubic metres}
#'   \item{TotalQ_Two_Seasons}{ggplot2 object of Oct-Mar and Apr-Sep total volumetric discharges, in cubic metres}
#'   \item{TotalQ_Four_Seasons}{ggplot2 object of Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec total volumetric discharges, in cubic metres}
#'   \item{Yield_Annual}{ggplot2 object of annual runoff yield, in millimetres}
#'   \item{Yield_Two_Seasons}{ggplot2 object of Oct-Mar and Apr-Sep runoff yields, in millimetres}
#'   \item{Yield_Four_Seasons}{ggplot2 object of Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec runoff yields, in millimetres}
#'   
#'   
#' @examples
#' \dontrun{
#' 
#' 
#'plot_annual_cumulative_stats(data = "08NM116", water_year = TRUE, water_year_start = 8)
#'
#' }
#' @export



plot_annual_cumulative_stats <- function(data = NULL,
                                         dates = Date,
                                         values = Value,
                                         use_yield = FALSE, 
                                         basin_area = NA,
                                         water_year = FALSE,
                                         water_year_start = 10,
                                         start_year = 0,
                                         end_year = 9999,
                                         exclude_years = NULL, 
                                         incl_seasons = FALSE,
                                         log_discharge = FALSE){
  
  
  
  ## CHECKS ON DATA FOR CALC
  ##------------------------
  
  # Check if data is provided
  if(is.null(data))   stop("No data provided, must provide a data frame or HYDAT station number(s).")
  if(!is.data.frame(data) & !is.vector(data)) stop("No data provided, must provide a data frame or HYDAT station number(s).")
  
  # Check HYDAT stations
  if(is.vector(data)) {
    if(length(data) != 1)   stop("Only one HYDAT station number can be listed for this function.")
    if(!data %in% dplyr::pull(tidyhydat::allstations[1]))  stop("Station number listed in data argument does not exist in HYDAT.")
  }
  
  if(is.data.frame(data)) {
    # Get the just groups (default STATION_NUMBER), Date, and Value columns
    # This method allows the user to select the Station, Date or Value columns if the column names are different
    if(!as.character(substitute(values)) %in% names(data) & !as.character(substitute(dates)) %in% names(data)) 
      stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using 'dates' and 'values' arguments.")
    if(!as.character(substitute(dates)) %in% names(data))  
      stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
    if(!as.character(substitute(values)) %in% names(data)) 
      stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.")
    
    # Temporarily rename the Date and Value columns
    data <- data[,c(as.character(substitute(dates)),
                    as.character(substitute(values)))]
    colnames(data) <- c("Date","Value")
    data <- dplyr::ungroup(data)
    
    
    # Check columns are in proper formats
    if(!inherits(data$Date[1], "Date"))  stop("'Date' column in data frame does not contain dates.")
    if(!is.numeric(data$Value))          stop("'Value' column in data frame does not contain numeric values.")   
    
    # Remove these to fix warnings?
    rm(c(dates,values))
  }
  
  if(!is.logical(log_discharge))  stop("log_discharge argument must be logical (TRUE/FALSE).")
  
  
  ## CALC STATS
  ## ----------
  
  cumulative_stats <- calc_annual_cumulative_stats(data = data,
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
    

  
  
  return(cumulative_plot)
  
}
