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

#' @title Calculate annual summary and missing data statistics for screening data
#'
#' @description Calculates means, medians, maximums, minimums, standard deviations of annual flows and data availability and missing 
#'    data statistics, and symbol counts (if column exists) for each year and month of each year. Calculates the statistics from all 
#'    daily discharge values from all years, unless specified. Returns a tibble with statistics.
#'
#' @inheritParams calc_annual_stats
#' @inheritParams plot_flow_data_symbols
#' @param include_symbols Logical. Include columns of counts of symbol categories from the symbols column.
#'
#' @return A tibble data frame with the following columns:
#'   \item{Year}{calendar or water year selected}
#'   \item{n_days}{number of days per year}
#'   \item{n_Q}{number of days per year with flow data}
#'   \item{n_missing_Q}{number of days per year with no flow data}
#'   \item{No_Symbol}{number of days with no symbol category, if include_symbol=TRUE}
#'   \item{x_Symbol}{number of days with a specific symbol category (x) from symbols column, if include_symbol=TRUE}
#'   \item{Maximum}{annual maximum of all daily flows for a given year}
#'   \item{Mean}{annual mean of all daily flows for a given year}
#'   \item{Median}{annual median of all daily flows for a given year}
#'   \item{StandardDeviation}{annual 1 standard deviation of all daily flows for a given year}
#'   and the following monthly missing columns (order will depend on water_year_month):
#'   \item{Jan_missing_Q}{number of Jan days per year with no flow data}
#'   \item{Feb_missing_Q}{number of Feb days per year with no flow data}
#'   \item{Mar_missing_Q}{number of Mar days per year with no flow data}
#'   \item{Apr_missing_Q}{number of Apr days per year with no flow data}
#'   \item{May_missing_Q}{number of May days per year with no flow data}
#'   \item{Jun_missing_Q}{number of Jun days per year with no flow data}
#'   \item{Jul_missing_Q}{number of Jul days per year with no flow data}
#'   \item{Aug_missing_Q}{number of Aug days per year with no flow data}
#'   \item{Sep_missing_Q}{number of Sep days per year with no flow data}
#'   \item{Oct_missing_Q}{number of Oct days per year with no flow data}
#'   \item{Nov_missing_Q}{number of Nov days per year with no flow data}
#'   \item{Dec_missing_Q}{number of Dec days per year with no flow data}
#'   Transposing data creates a column of "Statistics" and subsequent columns for each year selected.
#'   
#' @examples
#' # Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
#' if (file.exists(tidyhydat::hy_downloaded_db())) {
#' 
#' # Calculate screening statistics using data frame and data argument with defaults
#' flow_data <- tidyhydat::hy_daily_flows(station_number = "08NM116")
#' screen_flow_data(data = flow_data)
#' 
#' # Calculate screening statistics using station_number argument with defaults
#' screen_flow_data(station_number = "08NM116")
#'                   
#' # Calculate screening statistics for water years starting in October
#' screen_flow_data(station_number = "08NM116",
#'                  water_year_start = 9)
#'                   
#' # Calculate screening statistics for 7-day flows for July-September months only
#' screen_flow_data(station_number = "08NM116",
#'                  roll_days = 7,
#'                  months = 7:9)
#'                  
#' }
#' @export



screen_flow_data <- function(data,
                             dates = Date,
                             values = Value,
                             groups = STATION_NUMBER,
                             symbols = "Symbol",
                             station_number,
                             roll_days = 1,
                             roll_align = "right",
                             water_year_start = 1,
                             start_year,
                             end_year,
                             months = 1:12,
                             transpose = FALSE,
                             include_symbols = TRUE){             
  
  
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
  
  rolling_days_checks(roll_days, roll_align)
  water_year_checks(water_year_start)
  years_checks(start_year, end_year, exclude_years = NULL)
  months_checks(months = months)
  transpose_checks(transpose)
  
  if (length(include_symbols) > 1)        stop("Only one include_symbols logical value can be listed.", call. = FALSE)
  if (!is.logical(include_symbols))       stop("include_symbols argument must be logical (TRUE/FALSE).", call. = FALSE)
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, 
                               station_number = station_number)
  
  symb_in_data <- as.character(substitute(symbols)) %in% names(flow_data)
  
  # Save the original columns (to check for STATION_NUMBER col at end) and ungroup if necessary
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # Check and rename columns
  flow_data <- format_all_cols(data = flow_data,
                               dates = as.character(substitute(dates)),
                               values = as.character(substitute(values)),
                               groups = as.character(substitute(groups)),
                               symbols = as.character(substitute(symbols)),
                               rm_other_cols = TRUE,
                               keep_symbols = include_symbols & symb_in_data)
  # flow_data
  # if (include_symbols & symb_in_data) {
  # symbols <- flow_data$Symbol
  #   }
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill missing dates, add date variables, and add WaterYear
  flow_data <- analysis_prep(data = flow_data,
                             water_year_start = water_year_start)
  
  # Add rolling means to end of dataframe
  flow_data <- add_rolling_means(data = flow_data, roll_days = roll_days, roll_align = roll_align)
  colnames(flow_data)[ncol(flow_data)] <- "RollingValue"
  
  # Filter for the selected year (remove excluded years after)
  flow_data <- dplyr::filter(flow_data, WaterYear >= start_year & WaterYear <= end_year)
  flow_data <- dplyr::filter(flow_data, Month %in% months)
  
  
  # CALCULATE STATISTICS
  ## --------------------
  
  # Calculate basic stats
  Q_summary <- dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear),
                                n_days = length(WaterYear),
                                n_Q = sum(!is.na(RollingValue)),
                                n_missing_Q = sum(is.na(RollingValue)),
                                Minimum = ifelse(n_Q == 0, NA, min(RollingValue, na.rm = TRUE)),
                                Maximum = ifelse(n_Q == 0, NA, max(RollingValue,na.rm = TRUE)),
                                Mean = ifelse(n_Q == 0, NA, mean(RollingValue,na.rm = TRUE)),
                                Median = stats::median(RollingValue, na.rm = TRUE),
                                StandardDeviation = stats::sd(RollingValue, na.rm = TRUE))
  
  #Remove Nans and Infs
  Q_summary$Mean[is.nan(Q_summary$Mean)] <- NA
  Q_summary$Maximum[is.infinite(Q_summary$Maximum)] <- NA
  Q_summary$Minimum[is.infinite(Q_summary$Minimum)] <- NA
  
  
  ## Add Symbols
  if (include_symbols & symb_in_data) {
    symb_summary <- dplyr::mutate(flow_data, Symbol = ifelse(is.na(Value), "Missing", Symbol))
    symb_summary <- dplyr::group_by(symb_summary, STATION_NUMBER, WaterYear, Symbol)
    symb_summary <- dplyr::count(symb_summary)
    symb_summary <- dplyr::mutate(symb_summary, Symbol = dplyr::case_when(is.na(Symbol) | Symbol == "" ~ "No_Symbol",
                                                                          TRUE ~ paste0(Symbol,"_Symbol")))
    symb_summary <- dplyr::ungroup(symb_summary)
    symb_summary <- tidyr::pivot_wider(symb_summary, names_from = Symbol, values_from = n, values_fill = 0)
    symb_summary <- dplyr::ungroup(symb_summary)
    Q_summary <- dplyr::left_join(Q_summary, symb_summary, by = c("STATION_NUMBER", "WaterYear"))
    symb_cols <- names(symb_summary)[-(1:2)]
    Q_summary <- dplyr::select(Q_summary, STATION_NUMBER, WaterYear, n_days, n_Q, n_missing_Q, symb_cols, 
                               dplyr::everything())
    if ("Missing_Symbol" %in% names(Q_summary)) Q_summary <- dplyr::select(Q_summary, -Missing_Symbol)
  }
  
  
  # Calculate for each month for each year
  Q_summary_month <-   dplyr::summarize(dplyr::group_by(flow_data, STATION_NUMBER, WaterYear, MonthName),
                                        n_missing_Q = sum(is.na(RollingValue)))
  Q_summary_month <- dplyr::rename(Q_summary_month, Month = MonthName)
  Q_summary_month <- dplyr::mutate(Q_summary_month, Month = paste0(Month, "_missing_Q"))
  
  if (water_year_start == 1) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q",
                                                                      "Apr_missing_Q", "May_missing_Q", "Jun_missing_Q",
                                                                      "Jul_missing_Q", "Aug_missing_Q", "Sep_missing_Q",
                                                                      "Oct_missing_Q", "Nov_missing_Q", "Dec_missing_Q"))
  } else if (water_year_start == 2) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q",
                                                                      "May_missing_Q", "Jun_missing_Q", "Jul_missing_Q",
                                                                      "Aug_missing_Q", "Sep_missing_Q", "Oct_missing_Q",
                                                                      "Nov_missing_Q", "Dec_missing_Q", "Jan_missing_Q"))
  } else if (water_year_start == 3) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                      "Jun_missing_Q", "Jul_missing_Q", "Aug_missing_Q",
                                                                      "Sep_missing_Q", "Oct_missing_Q", "Nov_missing_Q",
                                                                      "Dec_missing_Q", "Jan_missing_Q", "Feb_missing_Q"))
  } else if (water_year_start == 4) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Apr_missing_Q", "May_missing_Q", "Jun_missing_Q",
                                                                      "Jul_missing_Q", "Aug_missing_Q", "Sep_missing_Q",
                                                                      "Oct_missing_Q", "Nov_missing_Q", "Dec_missing_Q",
                                                                      "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q"))
  } else if (water_year_start == 5) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("May_missing_Q", "Jun_missing_Q", "Jul_missing_Q",
                                                                      "Aug_missing_Q", "Sep_missing_Q", "Oct_missing_Q",
                                                                      "Nov_missing_Q", "Dec_missing_Q", "Jan_missing_Q",
                                                                      "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q"))
  } else if (water_year_start == 6) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Jun_missing_Q", "Jul_missing_Q", "Aug_missing_Q",
                                                                      "Sep_missing_Q", "Oct_missing_Q", "Nov_missing_Q",
                                                                      "Dec_missing_Q", "Jan_missing_Q", "Feb_missing_Q",
                                                                      "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q"))
  } else if (water_year_start == 7) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Jul_missing_Q", "Aug_missing_Q", "Sep_missing_Q",
                                                                      "Oct_missing_Q", "Nov_missing_Q", "Dec_missing_Q",
                                                                      "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q",
                                                                      "Apr_missing_Q", "May_missing_Q", "Jun_missing_Q"))
  } else if (water_year_start == 8) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Aug_missing_Q", "Sep_missing_Q", "Oct_missing_Q",
                                                                      "Nov_missing_Q", "Dec_missing_Q", "Jan_missing_Q",
                                                                      "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q",
                                                                      "May_missing_Q", "Jun_missing_Q", "Jul_missing_Q"))
  } else if (water_year_start == 9) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Sep_missing_Q", "Oct_missing_Q", "Nov_missing_Q",
                                                                      "Dec_missing_Q", "Jan_missing_Q", "Feb_missing_Q",
                                                                      "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                      "Jun_missing_Q", "Jul_missing_Q", "Aug_missing_Q"))
  } else if (water_year_start == 10) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Oct_missing_Q", "Nov_missing_Q", "Dec_missing_Q",
                                                                      "Jan_missing_Q", "Feb_missing_Q", "Mar_missing_Q",
                                                                      "Apr_missing_Q", "May_missing_Q", "Jun_missing_Q",
                                                                      "Jul_missing_Q", "Aug_missing_Q", "Sep_missing_Q"))
  } else if (water_year_start == 11) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Nov_missing_Q", "Dec_missing_Q", "Jan_missing_Q",
                                                                      "Feb_missing_Q", "Mar_missing_Q", "Apr_missing_Q",
                                                                      "May_missing_Q", "Jun_missing_Q", "Jul_missing_Q",
                                                                      "Aug_missing_Q", "Sep_missing_Q", "Oct_missing_Q"))
  } else if (water_year_start == 12) {
    Q_summary_month$Month <- factor(Q_summary_month$Month, levels = c("Dec_missing_Q", "Jan_missing_Q", "Feb_missing_Q",
                                                                      "Mar_missing_Q", "Apr_missing_Q", "May_missing_Q",
                                                                      "Jun_missing_Q", "Jul_missing_Q", "Aug_missing_Q",
                                                                      "Sep_missing_Q", "Oct_missing_Q", "Nov_missing_Q"))
  }
  
  
  Q_summary_month <- tidyr::spread(Q_summary_month, Month, n_missing_Q)
  
  Q_summary <- merge(Q_summary, Q_summary_month, by = c("STATION_NUMBER", "WaterYear"), all = TRUE)
  Q_summary <- dplyr::rename(Q_summary, Year = WaterYear)
  row_order <- names(Q_summary[, -1])
  
  
  
  
  # If transpose if selected, switch columns and rows
  if (transpose) {
    # Get list of columns to order the Statistic column after transposing
    stat_levels <- names(Q_summary[-(1:2)])
    
    # Transpose the columns for rows
    Q_summary <- tidyr::gather(Q_summary, Statistic, Value, -STATION_NUMBER, -Year)
    Q_summary <- tidyr::spread(Q_summary, Year, Value)
    
    # Order the columns
    Q_summary$Statistic <- factor(Q_summary$Statistic, levels = stat_levels)
    Q_summary <- with(Q_summary, Q_summary[order(STATION_NUMBER, Statistic),])
  }
  
  # Recheck if station_number/grouping was in original flow_data and rename or remove as necessary
  if(as.character(substitute(groups)) %in% orig_cols) {
    names(Q_summary)[names(Q_summary) == "STATION_NUMBER"] <- as.character(substitute(groups))
  } else {
    Q_summary <- dplyr::select(Q_summary, -STATION_NUMBER)
  }
  
  
  dplyr::as_tibble(Q_summary)
  
}

