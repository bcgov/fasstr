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


#' @title Perform a custom annual frequency analysis
#'
#' @description Performs a flow volume frequency analysis from a streamflow dataset. Defaults to ranking by minimums; 
#'    use \code{use_max} for to rank by maximum flows. Calculates the statistics from events
#'    and flow values provided. Columns of events (years), their values (mins or maxs), and identifiers (lowflows, highflows, etc),
#'    Function will calculate using all values in the provided data (no grouped analysis). Analysis methodology replicates that 
#'    from \href{http://www.hec.usace.army.mil/software/hec-ssp/}{HEC-SSP}.
#'
#' @param data  A data frame of flow data that contains columns of events, flow values, and measures (data type).
#' @param events Column in \code{data} that contains event identifiers, typically year values. Default \code{"Year"}.
#' @param values  Column in \code{data} that contains numeric flow values, in units of cubic metres per second. Default \code{"Value"}.
#' @param measures  Column in \code{data} that contains measure identifiers (example data: '7-day low' or 'Annual Max'). Can have multiple
#'    measures (ex. '7-day low' and '30-day low') in column if multiple statistics are desired. Default \code{"Measure"}.
#' @param use_max  Logical value to indicate using annual maximums rather than the minimums for analysis. Default \code{FALSE}.
#' @param use_log  Logical value to indicate log-scale transforming of flow data before analysis. Default \code{FALSE}.
#' @param prob_plot_position Character string indicating the plotting positions used in the frequency plots, one of "weibull",
#'    "median", or "hazen". Points are plotted against  (i-a)/(n+1-a-b) where \code{i} is the rank of the value; \code{n} is the 
#'    sample size and \code{a} and \code{b} are defined as: (a=0, b=0) for Weibull plotting positions; (a=.2; b=.3) for Median 
#'    plotting positions; and (a=.5; b=.5) for Hazen plotting positions. Default \code{"weibull"}.
#' @param prob_scale_points  Numeric vector of probabilities to be plotted along the X axis in the frequency plot. Inverse of 
#'    return period. Default \code{c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001)}.
#' @param fit_distr Character string identifying the distribution to fit annual data, one of "PIII" (Pearson Log III distribution) 
#'    or "weibull" (Weibull distribution). Default \code{"PIII"}.
#' @param fit_distr_method  Character string identifying the method used to fit the distribution, one of  "MOM" (method of moments) 
#'    or "MLE" (maximum likelihood estimation). Selected as \code{"MOM"} if \code{fit_distr}=="PIII" (default) or \code{"MLE"} if 
#'     \code{fit_distr}=="weibull".
#' @param fit_quantiles Numeric vector of quantiles to be estimated from the fitted distribution. 
#'    Default \code{c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01)}.
#' @param plot_curve Logical value to indicate plotting the computed curve on the probability plot. Default \code{TRUE}.
#' @param remove_zeros Logical value to indicate removing any zero flow values from the dataset.
#' 
#' @return A list with the following elements:
#'   \item{Freq_Analysis_Data}{Data frame with computed annual summary statistics used in analysis.}
#'   \item{Freq_Plot_Data}{Data frame with co-ordinates used in frequency plot.}
#'   \item{Freq_Plot}{ggplot2 object with frequency plot}
#'   \item{Freq_Fitting}{List of fitted objects from fitdistrplus.}
#'   \item{Freq_Fitted_Quantiles}{Data frame with fitted quantiles.}
#'   
#'   
#' @examples
#' \dontrun{
#' 
#'  # Working example:
#' 
#' # Calculate some values to use for a frequency analysis 
#' # (requires years, values for those years, and the name of the measure/metric)
#' low_flows <- calc_annual_lowflows(station_number = "08NM116", 
#'                                   start_year = 1980, 
#'                                   end_year = 2000,
#'                                   roll_days = 7)
#' low_flows <- dplyr::select(low_flows, Year, Value = Min_7_Day)
#' low_flows <- dplyr::mutate(low_flows, Measure = "7-Day")
#' 
#' # Compute the frequency analysis using the default parameters
#' results <- compute_frequency_analysis(data = low_flows,
#'                                       events = Year,
#'                                       values = Value,
#'                                       measure = Measure)
#'                             
#' }
#' @export


compute_frequency_analysis <- function(data,
                                       events = Year,
                                       values = Value,
                                       measures = Measure,
                                       use_max = FALSE,
                                       use_log = FALSE,
                                       prob_plot_position = c("weibull", "median", "hazen"),
                                       prob_scale_points = c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001),
                                       fit_distr = c("PIII", "weibull"),
                                       fit_distr_method = ifelse(fit_distr == "PIII", "MOM", "MLE"),
                                       fit_quantiles = c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01),
                                       plot_curve = TRUE,
                                       remove_zeros = FALSE){
  
  # replicate the frequency analysis of the HEC-SSP program
  # refer to Chapter 7 of the user manual
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
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
  if (!is.logical(plot_curve))
    stop("plot_curve must be logical (TRUE or FALSE).")
  
  
  # Check if data is provided
  if (missing(data))    
    stop("A data frame of annual data must be provided using the 'data' argument.", call. = FALSE)
  
  if (!is.data.frame(data))  
    stop("data argument is not a data frame.", call. = FALSE)
  data <- as.data.frame(data)
  
  # Check the values and events columns
  if (!as.character(substitute(events)) %in% names(data)) 
    stop("Events column not found in data frame. Rename events column to 'Years' or identify the column using 'events' argument.", call. = FALSE)

  if (!as.character(substitute(values)) %in% names(data)) 
    stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.", call. = FALSE)
  names(data)[names(data) == as.character(substitute(values))] <- "Value"
  if (!is.numeric(data$Value))  
    stop("Values in values column must be numeric.", call. = FALSE)
  
  if (!as.character(substitute(measures)) %in% names(data)) 
    stop("Measures not found in data frame. Rename measure column to 'Measure' or identify the column using 'measures' argument.", call. = FALSE)
  names(data)[names(data) == as.character(substitute(measures))] <- "Measure"
  
  if (remove_zeros) {
    data <- dplyr::filter(data, Value > 0)
  }
  
  # Set the Q_stat dataframe
  Q_stat <-  data
  
  if(fit_distr[1] == 'weibull' & any(Q_stat$Value < 0, na.rm = TRUE))
    stop("Cannot fit weibull distribution with negative flow values.", call. = FALSE)
  
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
  Q_stat_output <- tidyr::spread(Q_stat, Measure, Value)
  
  # See if a (natural) log-transform is to be used in the frequency analysis?
  # This flag also controls how the data is shown in the frequency plot
  if(use_log)Q_stat$Value <- log(Q_stat$Value)
  
  # make the plot. Remove any missing or infinite or NaN Values
  Q_stat <- Q_stat[is.finite(Q_stat$Value),]  # remove missing/ Inf/ NaN Values
  
  # get the plotting positions
  # From the HEC-SSP package, the  plotting positions are (m-a)/(n+1-a-b)
  a <- 0; b <- 0
  if(prob_plot_position[1] == 'weibull'){a <- 0.0; b <- 0.0}
  if(prob_plot_position[1] == 'median' ){a <- 0.3; b <- 0.3}
  if(prob_plot_position[1] == 'hazen'  ){a <- 0.5; b <- 0.5}
  plotdata <- plyr::ddply(Q_stat, "Measure", function(x, a, b, use_max){
    # sort the data
    x <- x[ order(x$Value),]
    x$prob <- ((seq_len(length(x$Value))) - a)/((length(x$Value) + 1 - a - b))
    if(use_max)x$prob <- 1 - x$prob   # they like to use p(exceedance) if using a minimum
    #x$dist.prob <- stats::qnorm(1 - x$prob) temporarilty remove
    x$return <- 1/x$prob
    x
  }, a = a, b = b, use_max = use_max)
  
  # change the measure labels in the plot
  plotdata2 <- plotdata
  
  # Setting dates and Values to actual Values. Some sort of environment() error in plotting due to function environment with dates and Values
  # Error in as.list.environment(x, all.names = TRUE) :
  #   object 'Value' not found
  events <- "Year"
  values <- "Value"
  measures <- "Measure"
  
  
  plotdata2$Measure <- factor(plotdata2$Measure, levels = unique(plotdata2$Measure))
  
  freqplot <- ggplot2::ggplot(data = plotdata2, ggplot2::aes(x = prob, y = Value, group = Measure, color = Measure),
                              environment = environment())+
    #ggplot2::ggtitle(paste(station_name, " Volume Frequency Analysis"))+
    ggplot2::geom_point()+
    ggplot2::xlab("Probability")+
    ggplot2::scale_x_continuous(trans = scales::probability_trans("norm", lower.tail = FALSE),
                                breaks = prob_scale_points,
                                sec.axis = ggplot2::dup_axis(name = 'Return Period',
                                                             labels = function(x){ifelse(1/x < 2, round(1/x,2), round(1/x,0))}
                                )
    )+
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme_bw() +
    ggplot2::labs(color = paste0('Events')) +    
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
                   panel.grid = ggplot2::element_line(size = .2),
                   axis.title = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 10),
                   axis.title.x.top = ggplot2::element_text(size = 12),
                   #legend.position = "right", 
                   #legend.spacing = ggplot2::unit(0, "cm"),
                   #legend.justification = "top",
                   legend.text = ggplot2::element_text(size = 10),
                   legend.title = ggplot2::element_text(size = 10))
  
  
  legend.title.align <- 1
  if(!use_max){ freqplot <- freqplot + ggplot2::theme(legend.justification = c(1, 1), legend.position = c(.98, .98))}
  if(use_max){ freqplot <- freqplot + ggplot2::theme(legend.justification = c(1,0), legend.position = c(.98, 0.02))}
  if(!use_log){ freqplot <- freqplot + ggplot2::scale_y_log10(breaks = scales::pretty_breaks(n = 10))}
  if(use_log){ freqplot <- freqplot + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10))}
  if(use_log &  use_max ){freqplot <- freqplot + ggplot2::ylab(expression(lnDischarge~(m^3/s)))}  # adjust the Y axis label
  if(use_log & !use_max){freqplot <- freqplot + ggplot2::ylab(expression(lnDischarge~(m^3/s)))}
  if(!use_log &  use_max ){freqplot <- freqplot + ggplot2::ylab(expression(Discharge~(m^3/s)))}
  if(!use_log & !use_max){freqplot <- freqplot + ggplot2::ylab(expression(Discharge~(m^3/s)))}
  
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
    start <- NULL
    # PIII is fit to log-of Values unless use_log has been set, in which case data has previous been logged
    if(distr == 'PIII' & !use_log){x$Value <- log10(x$Value)}
    # get starting Values
    if(distr == 'PIII'){
      # Note that the above forgot to mulitply the scale by the sign of skewness .
      # Refer to Page 24 of the Bulletin 17c
      m <- mean(x$Value)
      v <- stats::var(x$Value)
      s <- stats::sd(x$Value)
      g <- e1071::skewness(x$Value, type = 2)
      
      # This can be corrected, but HEC Bulletin 17b does not do these corrections
      # Correct the sample skew for bias using the recommendation of
      # Bobee, B. and R. Robitaille (1977). "The use of the Pearson Type 3 and Log Pearson Type 3 distributions revisited."
      # Water Resources Reseach 13(2): 427-443, as used by Kite
      #n <- length(x$Value)
      #g <- g*(sqrt(n*(n-1))/(n-2))*(1+8.5/n)
      # We will use method of moment estimates as starting Values for the MLE search
      
      my_shape <- (2 / g) ^ 2
      my_scale <- sqrt(v) / sqrt(my_shape) * sign(g)
      my_location <- m - my_scale * my_shape
      
      start <- list(shape = my_shape, location = my_location, scale = my_scale)
    }
    
    if(fit_method == "MLE") {fit <- fitdistrplus::fitdist(x$Value, distr, start = start, control = list(maxit = 1000)) }# , trace=1, REPORT=1))
    if(fit_method == "MOM") {fit <- fitdistrplus::fitdist(x$Value, distr, start = start,
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
    if(x$distname == 'PIII' & !use_log)quant <- 10 ^ quant # PIII was fit to the log-Values
    if(use_max) prob <- 1 - prob  # reset for adding to data frame
    if(use_log) quant <- exp(quant) # transforma back to original scale
    res <- data.frame(Measure = measure, distr = x$distname, prob = prob, quantile = quant , stringsAsFactors = FALSE)
    rownames(res) <- NULL
    res
  }, prob = fit_quantiles, fit = fit, use_max = use_max, use_log = use_log)
  
  # get the transposed version
  fitted_quantiles$Return <- 1 / fitted_quantiles$prob
  
  fitted_quantiles$Measure <- factor(fitted_quantiles$Measure, levels = unique(fitted_quantiles$Measure))
  
  fitted_quantiles_output <- tidyr::spread(fitted_quantiles, Measure, quantile)
  fitted_quantiles_output <- dplyr::rename(fitted_quantiles_output,
                                           Distribution = distr,
                                           Probability = prob,
                                           "Return Period" = Return)
  
  ## Add fitted curves to the freqplot
  fit_quantiles_plot <-  seq(to = 0.99, from = 0.01, by = .005)
  fitted_quantiles_plot <- plyr::ldply(names(fit), function (measure, prob, fit, use_max, use_log){
    # get the quantiles for each model
    x <- fit[[measure]]
    # if fitting minimums then you want EXCEEDANCE probabilities
    if(use_max) prob <- 1 - prob
    quant <- stats::quantile(x, prob = prob)
    quant <- unlist(quant$quantiles)
    if(x$distname == 'PIII' & !use_log)quant <- 10 ^ quant # PIII was fit to the log-Values
    if(use_max) prob <- 1 - prob  # reset for adding to data frame
    #if(use_log) quant <- exp(quant) # transforma back to original scale     #commented out sep10 by JG
    res <- data.frame(Measure = measure, distr = x$distname, prob = prob, quantile = quant , stringsAsFactors = FALSE)
    rownames(res) <- NULL
    res
  }, prob = fit_quantiles_plot, fit = fit, use_max = use_max, use_log = use_log)
  
  
  if (plot_curve) {
    freqplot <- freqplot +
      ggplot2::geom_line(data = fitted_quantiles_plot, ggplot2::aes(x = prob, y = quantile, group = Measure, color = Measure)) +
      ggplot2::labs(color = paste0('Events and\nComputed Curve'))  
  }
  
  
  
  # Other modifications for outputs
  
  row.names(Q_stat) <- seq_len(nrow(Q_stat))
  
  
  plotdata <- dplyr::rename(plotdata, 
                            Probability = prob,
                            "Return Period" = return)
  
  
  freq_results <- list(Freq_Analysis_Data = dplyr::as_tibble(Q_stat),
                       Freq_Plot_Data = dplyr::as_tibble(plotdata),  # has the plotting positions for each point in frequency analysis
                       Freq_Plot = freqplot,
                       Freq_Fitting = fit,               # list of fits of freq.distr to each measure
                       Freq_Fitted_Quantiles = dplyr::as_tibble(fitted_quantiles_output)#,             # fitted quantiles and their transposition
  )
  
  
  
  freq_results
}
