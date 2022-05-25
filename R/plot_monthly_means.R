# Copyright 2022 Province of British Columbia
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


#' @title Plot monthly means and percent LTMADs
#'
#' @description Plot monthly means and add long-term mean annual discharge percentages. Calculates statistics
#'    from all values, unless specified. Mean data calculated using \code{calc_longterm_daily_stats()} function. 
#'    Returns a list of plots.
#'
#' @inheritParams calc_monthly_stats
#' @inheritParams calc_longterm_mean
#' @inheritParams plot_monthly_stats
#' @param plot_months Numeric vector of months to include on the plot after calculating statistics. 
#'     For example, \code{3} for March or \code{6:8} for Jun-Aug. Differs from 'months' argument where that
#'     argument filters for specific months, this one just chooses which months to plot. Default \code{1:12}.
#' @param percent_MAD Numeric vector of percentages of long-term mean annual discharge to add to the plot (ex. \code{20} for 20 percent 
#'    MAD or \code{c(5,10,20)} for multiple percentages). Set to \code{NA} for none. Default \code{c(10,20,100)}.
#'    
#'    
#' @return A list of ggplot2 objects for with the following plots for each station provided:
#'   \item{Annual_Means}{a plot that contains annual means with the long-term mean as the x-axis intercept}
#'   
#' @seealso \code{\link{calc_longterm_daily_stats}}
#' @seealso \code{\link{calc_longterm_mean}}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot monthly means
#' plot_monthly_means(station_number = "08NM116",
#'                    complete_years = TRUE)
#'
#' # Plot mean flows with custom LTMADs
#' plot_monthly_means(station_number = "08NM116",
#'                    complete_years = TRUE,
#'                    percent_MAD = c(5,10,20,100))
#'                    
#' # Plot mean flows and plot just summer months
#' plot_monthly_means(station_number = "08NM116",
#'                    complete_years = TRUE, 
#'                    plot_months = 6:9)
#'                   
#' }
#' @export


plot_monthly_means <- function(data,
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
                               plot_months = 1:12,
                               complete_years = FALSE,
                               ignore_missing = FALSE,
                               include_title = FALSE,
                               percent_MAD = c(10,20,100)){ 
  
  ## ARGUMENT CHECKS
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
  
  logical_arg_check(include_title)
  numeric_checks(percent_MAD)
  
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
  
  monthly_stats <- calc_longterm_daily_stats(data = flow_data,
                                             roll_days = roll_days,
                                             roll_align = roll_align,
                                             water_year_start = water_year_start,
                                             start_year = start_year,
                                             end_year = end_year,
                                             exclude_years = exclude_years, 
                                             months = months,
                                             complete_years = complete_years,
                                             ignore_missing = ignore_missing,
                                             include_longterm = FALSE)
  
  
  monthly_stats <- dplyr::select(monthly_stats, STATION_NUMBER, Month, Mean)
  
  
  if (!all(is.na(percent_MAD))) {
    lt_mad <- suppressWarnings(calc_longterm_mean(data = flow_data,
                                                  roll_days = roll_days,
                                                  roll_align = roll_align,
                                                  water_year_start = water_year_start,
                                                  start_year = start_year,
                                                  end_year = end_year,
                                                  exclude_years = exclude_years, 
                                                  months = months,
                                                  complete_years = complete_years,
                                                  percent_MAD = percent_MAD)
    )
    
    if (100 %in% percent_MAD) {
      lt_mad <- lt_mad[!names(lt_mad) %in% '100%MAD']
    }
    if (!100 %in% percent_MAD) {
      lt_mad <- lt_mad[!names(lt_mad) %in% c('100%MAD','LTMAD')]
    }
    names(lt_mad) <- gsub("%MAD","% LTMAD",names(lt_mad))
    
    
    lt_mad <- tidyr::pivot_longer(lt_mad, -1, names_to = "LTMAD_Percent", values_to = "Value")
    
    monthly_stats <- dplyr::left_join(monthly_stats, lt_mad, by = "STATION_NUMBER")
    monthly_stats <- dplyr::filter(monthly_stats, Month %in% month.abb[plot_months])
    
    if (100 %in% percent_MAD) {
      monthly_stats <- dplyr::mutate(
        monthly_stats,
        LTMAD_Percent = factor(LTMAD_Percent, levels = c("LTMAD", unique(monthly_stats$LTMAD_Percent)[unique(monthly_stats$LTMAD_Percent) != "LTMAD"])))
    } else {
      monthly_stats <- dplyr::mutate(monthly_stats,
                                     LTMAD_Percent = factor(LTMAD_Percent, levels = unique(monthly_stats$LTMAD_Percent)))
    }
  }
  # return(monthly_stats)
  
  monthly_stats <- dplyr::mutate(group_by(monthly_stats, STATION_NUMBER, Month), 
                                 Mean = ifelse(duplicated(Mean), NA, Mean))
  
  ## PLOT STATS
  ## ----------
  
  tidy_plots <- dplyr::group_by(monthly_stats, STATION_NUMBER)
  tidy_plots <- tidyr::nest(tidy_plots)
  tidy_plots <- dplyr::mutate(
    tidy_plots,
    plot = purrr::map2(
      data, STATION_NUMBER,
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Month)) +
        ggplot2::geom_bar(mapping = ggplot2::aes(y = Mean, fill = "Monthly Mean"),
                          stat = "identity",  na.rm = TRUE, colour = "black", width = 0.9) +#dplyr::distinct(dplyr::select(.,1:2)), 
        {if(!all(is.na(percent_MAD))) ggplot2::geom_hline(data = .,
                                                          mapping = ggplot2::aes(yintercept = Value, colour = LTMAD_Percent),
                                                          size = 0.7, linetype = 2, na.rm = TRUE) }+
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::ylab("Discharge (cms)") +
        {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(paste(.y)) } +
        ggplot2::xlab("Month")+
        ggplot2::scale_fill_manual(values = c("Monthly Mean" = "#21918c"), name = "Statistics")+
        ggplot2::scale_color_viridis_d(option = "B", name = NULL, end = 0.9)+
        ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
                        colour = ggplot2::guide_legend(order = 2))+
        ggplot2::theme_bw() +
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                       panel.grid = ggplot2::element_line(size = .2),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.text = ggplot2::element_text(size = 10),
                       plot.title = ggplot2::element_text(hjust = 1, size = 9, colour = "grey25"),
                       panel.grid.minor.y = ggplot2::element_blank(),
                       legend.key.size = ggplot2::unit(0.4, "cm"),
                       legend.spacing = ggplot2::unit(-0.4, "cm"),
                       legend.background = ggplot2::element_blank())
    ))
  
  # Create a list of named plots extracted from the tibble
  plots <- tidy_plots$plot
  if (nrow(tidy_plots) == 1) {
    names(plots) <- "Monthly_Means"
  } else {
    names(plots) <- paste0(tidy_plots$STATION_NUMBER, "_Monthly_Means")
  }
  
  plots
  
}

