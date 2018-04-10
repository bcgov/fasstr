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


#' @title Perform an annual low or high-flow frequency analysis
#'
#' @description Performs a flow frequency analysis on annual statistics from a streamflow dataset. Defaults to low-flow frequency 
#'    analysis using annual minimums. Use \code{use_max} for annual high flow frequency analyses. Calculates the statistics from all 
#'    daily discharge values from all years, unless specified. Function will calculate using all values in the provided data (no grouped
#'    analysis). Analysis methodology replicates that from \href{http://www.hec.usace.army.mil/software/hec-ssp/}{HEC-SSP}.
#'
#' @inheritParams calc_annual_stats
#' @param use_max  Logical valve to indicate using annual maximums rather than the minimums for analysis. Default \code{FALSE}.
#' @param use_log  Logical value to indicate log-scale tranforming of flow data before analysis. Default \code{FALSE}.
#' @param prob_plot_position Character string indicating the plotting positions used in the frequency plots, one of "weibull",
#'    "median", or "hazen". Points are plotted against  (i-a)/(n+1-a-b) where \code{i} is the rank of the value; \code{n} is the 
#'    sample size and \code{a} and \code{b} are defined as: (a=0, b=0) for Weibull plotting positions; (a=.2; b=.3) for Median 
#'    plotting postions; and (a=.5; b=.5) for Hazen plotting positions. Default \code{"weibull"}.
#' @param prob_scale_points  Numeric vector of probabilities to be plotted along the X axis in the frequency plot. Inverse of 
#'    return period. Default \code{c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001)}.
#' @param fit_distr Character string identifying the distribution to fit annual data, one of "PIII" (Pearson Log III distribution) 
#'    or "weibull" (Weibull distribution). Default \code{"PIII"}.
#' @param fit_distr_method  Character string identifying the method used to fit the distribution, one of  "MOM" (method of moments) 
#'    or "MLE" (maximum likelihood estimation). Selected as \code{"MOM"} if \code{fit_distr}=="PIII" (default) or \code{"MLE"} if 
#'     \code{fit_distr}=="weibull".
#' @param fit_quantiles Numeric vector of quantiles to be estimated from the fitted distribution. 
#'    Default \code{c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01)}.
#' 
#' @return A list with the following elements:
#'   \item{Q_stat}{Data frame with Computed annual summary statistics used in analysis}
#'   \item{plotdata}{Data frame with Co-ordinates used in frequency plot.}
#'   \item{freqplot}{ggplot2 object with frequency plot}
#'   \item{fit}{List of fitted objects from fitdistrplus.}
#'   \item{fitted_quantiles}{Data frame with fitted quantiles.}
#'   \item{overview}{Data frame overview of parameters used in analysis.}
#'   
#'   
#' @examples
#' \dontrun{
#' 
#' compute_annual_frequencies(station_number = "08NM116",
#'                            start_year = 1980,
#'                            end_year = 2010)
#'                             
#' }
#' @export


compute_annual_frequencies <- function(data = NULL,
                                       dates = Date,
                                       values = Value,
                                       station_number = NULL,
                                       roll_days = c(1, 3, 7, 30),
                                       roll_align = "right",
                                       use_max = FALSE,
                                       use_log = FALSE,
                                       prob_plot_position = c("weibull", "median", "hazen"),
                                       prob_scale_points = c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                                       fit_distr = c("PIII", "weibull"),
                                       fit_distr_method = ifelse(fit_distr == "PIII", "MOM", "MLE"),
                                       fit_quantiles = c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01),
                                       water_year = FALSE,
                                       water_year_start = 10,
                                       start_year = 0,
                                       end_year = 9999,
                                       exclude_years = NULL,
                                       months = 1:12,
                                       ignore_missing = FALSE){
  
  # replicate the frequency analysis of the HEC-SSP program
  # refer to Chapter 7 of the user manual
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  rolling_days_checks(roll_days, roll_align, multiple = TRUE)
  water_year_checks(water_year, water_year_start)
  years_checks(start_year, end_year, exclude_years)
  months_checks(months)
  ignore_missing_checks(ignore_missing)
  
  
  if (!is.logical(use_log))        
    stop("use_log must be logical (TRUE/FALSE).", call. = FALSE)
  if (!is.logical(use_max))         
    stop("use_max must be logical (TRUE/FALSE).", call. = FALSE)
  if (!all(prob_plot_position %in% c("weibull","median","hazen")))  
    stop("prob_plot_position must be one of weibull, median, or hazen.", call. = FALSE)
  if (!is.numeric(prob_scale_points)) 
    stop("prob_scale_points must be numeric and between 0 and 1 (not inclusive).", call. = FALSE)
  if (!all(prob_scale_points > 0 & prob_scale_points < 1))
    stop("prob_scale_points must be numeric and between 0 and 1 (not inclusive).", call. = FALSE)
  if (!all(fit_distr %in% c("weibull", "PIII")))          
    stop("fit_distr must be one of weibull or PIII.", call. = FALSE)
  if (!is.numeric(fit_quantiles))                         
    stop("fit_quantiles must be numeric and between 0 and 1 (not inclusive).", call. = FALSE)
  if (!all(fit_quantiles > 0 & fit_quantiles < 1))        
    stop("fit_quantiles must be numeric and between 0 and 1 (not inclusive).", call. = FALSE)
  if (fit_distr[1] == 'weibull' & use_log)                
    stop("Cannot fit Weibull distribution on log-scale.", call. = FALSE)
  if (fit_distr[1] != "PIII" & fit_distr_method[1] == "MOM") 
    stop('MOM only can be used with PIII distribution.', call. = FALSE)
  
  
  ## FLOW DATA CHECKS AND FORMATTING
  ## -------------------------------
  
  if (!is.null(station_number) & length(station_number) != 1) 
    stop("Only one station_number can be provided for this function.", call. = FALSE)
  
  # Check if data is provided and import it
  flow_data <- flowdata_import(data = data, 
                               station_number = station_number)
  
  # Save the original columns (to check for STATION_NUMBER col at end) and ungroup if necessary
  orig_cols <- names(flow_data)
  flow_data <- dplyr::ungroup(flow_data)
  
  # Check and rename columns
  flow_data <- format_dates_col(flow_data, dates = as.character(substitute(dates)))
  flow_data <- format_values_col(flow_data, values = as.character(substitute(values)))
  
  flow_data <- dplyr::select(flow_data, Date, Value)
  
  if(fit_distr[1] == 'weibull' & any(flow_data$Value < 0, na.rm = TRUE))
    stop("Cannot fit weibull distribution with negative flow values.", call. = FALSE)
  
  ## PREPARE FLOW DATA
  ## -----------------
  
  # Fill missing dates, add date variables, and add AnalysisYear
  flow_data <- analysis_prep(data = flow_data, 
                             water_year = water_year, 
                             water_year_start = water_year_start,
                             year = TRUE)
  

  
  # Loop through each roll_days and add customized names of rolling means to flow_data
  for (day in roll_days) {
    flow_data_temp <- dplyr::select(flow_data, Date, Value)
    flow_data_temp <- add_rolling_means(flow_data_temp, roll_days = day, roll_align = roll_align)
    names(flow_data_temp)[names(flow_data_temp) == paste0("Q", day, "Day")] <- paste("Q", formatC(day, width = 3, format = "d",
                                                                                                  flag = "0"), "-day-Avg", sep = "")
    flow_data_temp <- dplyr::select(flow_data_temp, -Value)
    flow_data <- merge(flow_data, flow_data_temp, by = "Date", all = TRUE)
  }
  

    # Filter for the selected year
  flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
  flow_data <- dplyr::filter(flow_data, !(AnalysisYear %in% exclude_years))
  flow_data <- dplyr::filter(flow_data, Month %in% months)

  # Calculate the min or max of the rolling means for each year
  flow_data <- dplyr::select(flow_data, -Date, -Value, -Year, -Month, -MonthName, -DayofYear, -WaterYear, -WaterDayofYear)
  
  flow_data <- tidyr::gather(flow_data, Measure, value, -AnalysisYear)
  Q_stat <- dplyr::summarise(dplyr::group_by(flow_data, AnalysisYear, Measure),
                             value = ifelse(use_max,
                                            max(value, na.rm = ignore_missing),
                                            min(value, na.rm = ignore_missing)))
  Q_stat <- dplyr::rename(Q_stat, Year = AnalysisYear)

  # Data checks
  if (nrow(Q_stat) < 3) stop(paste0("Need at least 3 years of observations for analysis. There are only ", 
                                        nrow(Q_stat), 
                                        " years available."), call. = FALSE)
  
  
  
  ## Define functions for analysis
  ##------------------------------
  
  # This code chunk removes error for "no visible binding for '<<-' assignment to 'dPIII'" etc
  dPIII <- NULL
  pPIII <- NULL
  qPIII <- NULL
  mPIII <- NULL
  
  
  # Define the log=Pearson III function needed for fitting at the GLOBAL environment level
  dPIII <<- function(x, shape, location, scale) PearsonDS::dpearsonIII(x, shape, location, scale, log = FALSE)
  pPIII <<- function(q, shape, location, scale) PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
  qPIII <<- function(p, shape, location, scale) PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
  
  mPIII <<- function(order, shape, location, scale){
    # compute the empirical first 3 moments of the PIII distribution
    if(order == 1) return(location + shape * scale)
    if(order == 2) return(scale * scale * shape)
    if(order == 3) return(2 / sqrt(shape) * sign(scale))
  }
  
  #--------------------------------------------------------------
  # Plot the data on the distrubtion

  # Compute the summary table for output
  Q_stat_output <- tidyr::spread(Q_stat, Measure, value)

  # See if a (natural) log-transform is to be used in the frequency analysis?
  # This flag also controls how the data is shown in the frequency plot
  if(use_log)Q_stat$value <- log(Q_stat$value)

  # make the plot. Remove any missing or infinite or NaN values
  Q_stat <- Q_stat[is.finite(Q_stat$value),]  # remove missing/ Inf/ NaN values

  # get the plotting positions
  # From the HEC-SSP package, the  plotting positions are (m-a)/(n+1-a-b)
  a <- 0; b <- 0
  if(prob_plot_position[1] == 'weibull'){a <- 0.0; b <- 0.0}
  if(prob_plot_position[1] == 'median' ){a <- 0.3; b <- 0.3}
  if(prob_plot_position[1] == 'hazen'  ){a <- 0.5; b <- 0.5}
  plotdata <- plyr::ddply(Q_stat, "Measure", function(x, a, b, use_max){
    # sort the data
    x <- x[ order(x$value),]
    x$prob <- ((1:length(x$value)) - a)/((length(x$value) + 1 - a - b))
    if(use_max)x$prob <- 1 - x$prob   # they like to use p(exceedance) if using a minimum
    x$dist.prob <- stats::qnorm(1 - x$prob)
    x
  }, a = a, b = b, use_max = use_max)

  # change the measure labels in the plot
  plotdata2 <- plotdata

  # Setting dates and values to actual values. Some sort of environment() error in plotting due to function environment with dates and values
  # Error in as.list.environment(x, all.names = TRUE) :
  #   object 'Value' not found
  dates = "Date"
  values = "Value"

  freqplot <- ggplot2::ggplot(data = plotdata2, ggplot2::aes(x = prob, y = value, group = Measure, color = Measure),
                              environment = environment())+
    #ggplot2::ggtitle(paste(station_name, " Volume Frequency Analysis"))+
    ggplot2::geom_point()+
    ggplot2::xlab("Probability")+
    ggplot2::scale_x_continuous(trans = scales::probability_trans("norm", lower.tail = FALSE),
                                breaks = prob_scale_points,
                                sec.axis = ggplot2::sec_axis(trans = ~1/.,
                                                           name = 'Return Period',
                                                           breaks = c(1.01,1.1,2,5,10,20,100,1000),
                                                           labels = function(x){ifelse(x < 2, x, round(x,0))}))+
    ggplot2::theme(axis.title.x.top = ggplot2::element_text(size = 10),
                   legend.title = ggplot2::element_blank(), legend.key.size = ggplot2::unit(.1,"in"))

  if(!use_max){ freqplot <- freqplot + ggplot2::theme(legend.justification = c(1, 1), legend.position = c(1, 1))}
  if(use_max){ freqplot <- freqplot + ggplot2::theme(legend.justification = c(1,0), legend.position = c(1, 0))}
  if(!use_log){ freqplot <- freqplot + ggplot2::scale_y_log10(breaks = scales::pretty_breaks(n = 10))}
  if(use_log){ freqplot <- freqplot + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10))}
  if(use_log &  use_max ){freqplot <- freqplot + ggplot2::ylab("ln(Annual Maximum Flow (cms))")}  # adjust the Y axis label
  if(use_log & !use_max){freqplot <- freqplot + ggplot2::ylab("ln(Annual Minimum Flow (cms))")}
  if(!use_log &  use_max ){freqplot <- freqplot + ggplot2::ylab("Annual Maximum Flow (cms)")}
  if(!use_log & !use_max){freqplot <- freqplot + ggplot2::ylab("Annual Minimum Flow (cms)")}

  #--------------------------------------------------------------
  # fit the distribution to each measure

  # log-Pearson III implies that the log(x) has a 3-parameter gamma distribution
  ePIII <- function(x, order){
    # compute (centered) empirical centered moments of the data
    if(order == 1) return(mean(x))
    if(order == 2) return(stats::var(x))
    if(order == 3) return(e1071::skewness(x, type = 2))
  }

  fit <- plyr::dlply(Q_stat, "Measure", function(x, distr, fit_method){
    start=NULL
    # PIII is fit to log-of values unless use_log has been set, in which case data has previous been logged
    if(distr == 'PIII' & !use_log){x$value <- log10(x$value)}
    # get starting values
    if(distr == 'PIII'){
      # Note that the above forgot to mulitply the scale by the sign of skewness .
      # Refer to Page 24 of the Bulletin 17c
      m <- mean(x$value)
      v <- stats::var(x$value)
      s <- stats::sd(x$value)
      g <- e1071::skewness(x$value, type = 2)

      # This can be corrected, but HEC Bulletin 17b does not do these corrections
      # Correct the sample skew for bias using the recommendation of
      # Bobee, B. and R. Robitaille (1977). "The use of the Pearson Type 3 and Log Pearson Type 3 distributions revisited."
      # Water Resources Reseach 13(2): 427-443, as used by Kite
      #n <- length(x$value)
      #g <- g*(sqrt(n*(n-1))/(n-2))*(1+8.5/n)
      # We will use method of moment estimates as starting values for the MLE search

      my_shape <- (2 / g) ^ 2
      my_scale <- sqrt(v) / sqrt(my_shape) * sign(g)
      my_location <- m - my_scale * my_shape

      start <- list(shape = my_shape, location = my_location, scale = my_scale)
    }

    if(fit_method == "MLE") {fit <- fitdistrplus::fitdist(x$value, distr, start = start, control = list(maxit = 1000)) }# , trace=1, REPORT=1))
    if(fit_method == "MOM") {fit <- fitdistrplus::fitdist(x$value, distr, start = start,
                                                          method = "mme", order = 1:3, memp = ePIII, control = list(maxit = 1000))
    } # fixed at MOM estimates
    fit
  }, distr = fit_distr[1], fit_method = fit_distr_method[1])


  #--------------------------------------------------------------
  # extracted the fitted quantiles from the fitted distribution

  fitted_quantiles <- plyr::ldply(names(fit), function (measure, prob, fit, use_max, use_log){
    # get the quantiles for each model
    x <- fit[[measure]]
    # if fitting minimums then you want EXCEEDANCE probabilities
    if(use_max) prob <- 1 - prob
    quant <- stats::quantile(x, prob = prob)
    quant <- unlist(quant$quantiles)
    if(x$distname == 'PIII' & !use_log)quant <- 10 ^ quant # PIII was fit to the log-values
    if(use_max) prob <- 1 - prob  # reset for adding to data frame
    if(use_log) quant <- exp(quant) # transforma back to original scale
    res <- data.frame(Measure = measure, distr = x$distname, prob = prob, quantile = quant , stringsAsFactors = FALSE)
    rownames(res) <- NULL
    res
  }, prob = fit_quantiles, fit = fit, use_max = use_max, use_log = use_log)

  # get the transposed version
  fitted_quantiles$Return <- 1 / fitted_quantiles$prob
  fitted_quantiles_output <- tidyr::spread(fitted_quantiles, Measure, quantile)
  fitted_quantiles_output <- dplyr::rename(fitted_quantiles_output,
                                           Distribution = distr,
                                           Probability = prob,
                                           "Return Period" = Return)


  # analysis.summary <- list(#"station name" = station_name,
  #   "year type" = ifelse(!water_year, "Calendar Year (Jan-Dec)", "Water Year (Oct-Sep)"),
  #   "year range" = paste0(start_year, " - ", end_year),
  #   "excluded years" = paste(exclude_years, collapse = ', '),
  #   "min or max" = ifelse(use_max, "maximums", "minimums"),
  #   roll_days = paste(roll_days, collapse = ', '),
  #   fit_distr = fit_distr[1],  # distributions fit to the data
  #   fit_distr_method = fit_distr_method[1],
  #   prob_plot_position = prob_plot_position[1]
  # )
  # analysis.options.df <- data.frame("Option" = names(analysis.options),
  #                                   "Selection" = unlist(analysis.options, use.names = FALSE))


  list(Q_stat = Q_stat,
       plotdata = dplyr::as_tibble(plotdata),  # has the plotting positions for each point in frequency analysis
       freqplot = freqplot,
       fit = fit,               # list of fits of freq.distr to each measure
       fitted_quantiles = fitted_quantiles_output#,             # fitted quantiles and their transposition
       #overview = analysis.options.df
  )

}
