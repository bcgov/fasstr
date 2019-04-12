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


#' @title Plot annual flow timing
#'
#' @description Plots annual the timing (day of year) of occurrence of portions of total annual flow of daily flow 
#'    values from a streamflow dataset. Calculates the statistics from all daily discharge values from all years, unless specified.
#'    Data calculated using calc_annual_flow_timing() function.
#'
#' @inheritParams calc_annual_flow_timing
#' @inheritParams plot_annual_stats
#'
#' @return A list of ggplot2 objects with the following for each station provided:
#'   \item{Annual_Flow_Timing}{a plot that contains each n-percent of total volumetric discharge}
#'   Default plots on each object:   
#'   \item{DoY_25pct_TotalQ}{day of year of 25-percent of total volumetric discharge}
#'   \item{DoY_33.3pct_TotalQ}{day of year of 33.3-percent of total volumetric discharge}
#'   \item{DoY_50pct_TotalQ}{day of year of 50-percent of total volumetric discharge}
#'   \item{DoY_75pct_TotalQ}{day of year of 75-percent of total volumetric discharge}
#'   
#' @references 
#' \itemize{
#'  \item{Barnett, T.P., Pierce, D.W., Hidalgo, H.G., Bonfils, C., Santer, B.D., Das, T., Bala, G., Wood, A.W.,
#'        Nozawa, T., Mirin, A.A., Cayan, D.R., Dettinger, M.D., 2008. Human-Induced Clanges in the Hydrology of 
#'        the Western United States. Science 319, 1080-1083.}
#'        }
#'        
#' @seealso \code{\link{calc_annual_flow_timing}}
#'   
#' @examples
#' \dontrun{
#' 
#' # Plot statistics with default percent totals
#' plot_annual_flow_timing(station_number = "08NM116") 
#' 
#' # Plot statistics with custom percent totals
#' plot_annual_flow_timing(station_number = "08NM116",
#'                         percent_total = 50)
#' }
#' @export


plot_annual_flow_timing <- function(data,
                                    dates = Date,
                                    values = Value,
                                    groups = STATION_NUMBER,
                                    station_number,
                                    percent_total = c(25,33.3,50,75),
                                    water_year_start = 1,
                                    start_year,
                                    end_year,
                                    exclude_years,
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
  
  timing_stats <- calc_annual_flow_timing(data = flow_data,
                                          dates = Date,
                                          values = Value,
                                          percent_total = percent_total,
                                          water_year_start = water_year_start,
                                          start_year = start_year,
                                          end_year = end_year,
                                          exclude_years = exclude_years)
  
  timing_stats <- dplyr::select(timing_stats, STATION_NUMBER, Year, dplyr::contains("TotalQ"), -dplyr::contains("Date"))
  timing_stats <- tidyr::gather(timing_stats, Statistic, Value, -STATION_NUMBER, -Year)
  timing_stats <- dplyr::mutate(timing_stats, Statistic = substr(Statistic, 5, nchar(Statistic)))
  timing_stats <- dplyr::mutate(timing_stats, Statistic = paste0(gsub("pct_TotalQ", "", Statistic), " Percent"))
  
  
  
  ## PLOT STATS
  ## ----------
  
  # Create plots for each STATION_NUMBER in a tibble (see: http://www.brodrigues.co/blog/2017-03-29-make-ggplot2-purrr/)
  timing_plots <- dplyr::group_by(timing_stats, STATION_NUMBER)
  timing_plots <- tidyr::nest(timing_plots)
  timing_plots <- dplyr::mutate(timing_plots,
                              plot = purrr::map2(data, STATION_NUMBER, 
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, y = Value, color = Statistic)) +
        ggplot2::geom_line(alpha = 0.5, na.rm = TRUE) +
        ggplot2::geom_point(na.rm = TRUE) +
        {if(length(percent_total) > 1) ggplot2::facet_wrap(~Statistic, scales = "free_y", ncol = 1, strip.position = "top")} +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
        {if(length(unique(timing_stats$Year)) < 8) ggplot2::scale_x_continuous(breaks = unique(timing_stats$Year))}+
        ggplot2::ylab("Day of Year") +
        ggplot2::xlab("Year") +
        #ggplot2::scale_color_brewer(palette = "Set1") +
        ggplot2::theme_bw() +
        ggplot2::guides(colour = FALSE) +
        {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(paste(.y)) } +
        ggplot2::theme(legend.position = "right", 
                       legend.spacing = ggplot2::unit(0, "cm"),
                       legend.justification = "top",
                       legend.text = ggplot2::element_text(size = 9),
                       panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                       panel.grid = ggplot2::element_line(size = .2),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.text = ggplot2::element_text(size = 10),
                       plot.title = ggplot2::element_text(hjust = 1, size = 9, colour = "grey25"),
                       strip.background = ggplot2::element_blank(),
                       strip.text = ggplot2::element_text(hjust = 0, face = "bold", size = 10))
                              ))
  
  # Create a list of named plots extracted from the tibble
  plots <- timing_plots$plot
  if (nrow(timing_plots) == 1) {
    names(plots) <- "Annual_Flow_Timing"
  } else {
    names(plots) <- paste0(timing_plots$STATION_NUMBER, "_Annual_Flow_Timing")
  }
  
  plots
      
  
}

