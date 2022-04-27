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


#' @title Plot annual means compared to the long-term mean
#'
#' @description Plot annual means using the long-term annual mean as the point of reference for annual means. Calculates statistics
#'    from all values, unless specified. Data calculated using \code{calc_annual_stats()} function. Returns a list of plots.
#'
#' @inheritParams calc_annual_stats
#' @param include_title Logical value to indicate adding the group/station number to the plot, if provided. Default \code{FALSE}.
#' @param percentiles Numeric vector of percentiles of annual means to plot, up to two values. Set to \code{NA} if none required. 
#'     Default \code{c(10,90)}.
#'
#' @return A list of ggplot2 objects for with the following plots for each station provided:
#'   \item{Annual_Means}{a plot that contains annual means with the long-term mean as the x-axis intercept}
#'   
#' @seealso \code{\link{calc_annual_stats}}
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Plot annual means
#' plot_annual_means(station_number = "08NM116")
#'
#' # Plot mean flows from July-September
#' plot_annual_means(station_number = "08NM116", 
#'                   months = 7:9)
#'                   
#' }
#' @export


plot_annual_means <- function(data,
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
                              include_title = FALSE,
                              percentiles = c(10,90)){ 
  
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
  percentiles <- sort(percentiles[1:2])
  numeric_range_checks(percentiles)
  
  
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
  
  annual_stats <- calc_annual_stats(data = flow_data,
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
  
  # Remove all leading NA years
  annual_stats <- dplyr::filter(dplyr::group_by(annual_stats, STATION_NUMBER),
                                Year >= Year[min(which(!is.na(.data[[names(annual_stats)[3]]])))])
  
  annual_stats <- dplyr::select(annual_stats, STATION_NUMBER, Year, Mean)
  
  lt_mad <- dplyr::group_by(annual_stats, STATION_NUMBER)
  lt_mad <- dplyr::summarise(lt_mad, 
                             LTMAD = mean(Mean, na.rm = TRUE),
                             Ptile1 = quantile(Mean, probs = percentiles[1]/100, na.rm=TRUE),
                             Ptile2 = quantile(Mean, probs = percentiles[2]/100, na.rm=TRUE))
  
  annual_stats <- dplyr::left_join(annual_stats, lt_mad, by = "STATION_NUMBER")
  annual_stats <- dplyr::mutate(annual_stats, 
                                MAD_diff = Mean - LTMAD)
  annual_stats <- annual_stats[stats::complete.cases(annual_stats$Mean), ]
  
  
  ## PLOT STATS
  ## ----------
  
  # p1_lab <- paste0(roll_days_high,"-day Maximum") #"#440154FF" #
  # low_lab <- paste0(roll_days_low,"-day Minimum") #"#440154FF" #
  # if (plot_lowflow & plot_highflow) {
  #   cols <- c(low_col,high_col)
  #   names(cols) <- c(low_lab, high_lab)
  # } else if (!plot_lowflow & plot_highflow) {
  #   cols <- c(high_col)
  #   names(cols) <- c(high_lab)
  # } else if (plot_lowflow & !plot_highflow) {
  #   cols <- c(low_col)
  #   names(cols) <- c(low_lab)
  # }
  
  if (all(is.na(percentiles))) {
    ptile_cols <- c("Long-term MAD" = 1)
  } else {
    ptile_lab <- ifelse(any(is.na(percentiles)), paste0("MAD P",percentiles[!is.na(percentiles)]),
                        paste0("MAD P", paste0(percentiles, collapse = " and ")))
    ptile_cols <- c(1,2)
    names(ptile_cols) <- c("Long-term MAD",ptile_lab)
  }
  
  # Create plots for each STATION_NUMBER in a tibble (see: http://www.brodrigues.co/blog/2017-03-29-make-ggplot2-purrr/)
  tidy_plots <- dplyr::group_by(annual_stats, STATION_NUMBER)
  tidy_plots <- tidyr::nest(tidy_plots)
  tidy_plots <- dplyr::mutate(
    tidy_plots,
    plot = purrr::map2(
      data, STATION_NUMBER,
      ~ggplot2::ggplot(data = ., ggplot2::aes(x = Year, y = MAD_diff)) +
        ggplot2::geom_bar(stat = "identity", mapping = ggplot2::aes(fill = "MAD Difference"), na.rm = TRUE, colour = "black", width = 1) +
        ggplot2::geom_hline(size = 0.5, alpha = 0.7, na.rm = TRUE,
                            mapping = ggplot2::aes(yintercept = unique(Ptile1) - unique(LTMAD), linetype = ptile_lab)) +
        ggplot2::geom_hline(size = 0.5, alpha = 0.7, na.rm = TRUE,
                            mapping = ggplot2::aes(yintercept = unique(Ptile2) - unique(LTMAD), linetype = ptile_lab)) +
        ggplot2::geom_hline(size = 0.5, mapping = ggplot2::aes(yintercept = 0, linetype = "Long-term MAD")) +                         
        ggplot2::scale_y_continuous(labels = function(x) round(x + unique(.$LTMAD),3),
                                    breaks = scales::pretty_breaks(n = 10)) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
        {if(length(unique(annual_stats$Year)) < 8) ggplot2::scale_x_continuous(breaks = unique(annual_stats$Year))}+
        ggplot2::ylab("Mean Annual Discharge (cms)") + #expression(Mean~Annual~Discharge~(m^3/s))
        {if (include_title & .y != "XXXXXXX") ggplot2::ggtitle(paste(.y)) } +
        ggplot2::xlab(ifelse(water_year_start ==1, "Year", "Water Year"))+
        ggplot2::scale_fill_manual(values = c("MAD Difference" = "#21918c"))+
        ggplot2::scale_linetype_manual(values = ptile_cols)+
        ggplot2::guides(fill = ggplot2::guide_legend(order = 1), 
                        colour = ggplot2::guide_legend(order = 2))+
        ggplot2::theme_bw() +
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                       panel.grid = ggplot2::element_line(size = .2),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.text = ggplot2::element_text(size = 10),
                       plot.title = ggplot2::element_text(hjust = 1, size = 9, colour = "grey25"),
                       legend.title = ggplot2::element_blank(),
                       legend.spacing = ggplot2::unit(-.02, "cm"),)
    ))
  
  # Create a list of named plots extracted from the tibble
  plots <- tidy_plots$plot
  if (nrow(tidy_plots) == 1) {
    names(plots) <- "Annual_Means"
  } else {
    names(plots) <- paste0(tidy_plots$STATION_NUMBER, "_Annual_Means")
  }
  
  plots
  
}

