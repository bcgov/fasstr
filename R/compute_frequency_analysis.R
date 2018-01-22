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


#' @title Perform a volume frequency analysis on annual statistics
#'
#' @description Performs a volume frequency analysis on annual statistics from a streamflow dataset. Calculates the statistics from all 
#'    daily discharge values from all years, unless specified. Function will calculate using all values in the provided data (no grouped
#'    analysis). Analysis methodology replicates that from \href{http://www.hec.usace.army.mil/software/hec-ssp/}{HEC-SSP}.
#'
#' @param data Daily data to be analyzed. Options:
#' 
#'    A data frame of daily data that contains columns of dates and values.
#'    
#'    A character string vector of a single seven digit Water Survey of Canada station number (e.g. \code{"08NM116"}) of which to 
#'    extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.   
#' @param dates Column in the \code{data} data frame that contains dates formatted YYYY-MM-DD. Only required if
#'    using the data frame option of \code{data} and dates column is not named 'Date'. Default \code{Date}. 
#' @param values Column in the \code{data} data frame that contains numeric flow values, in units of cubic metres per second.
#'    Only required if using the data frame option of \code{data} and values column is not named 'Value'. Default \code{Value}.
#' @param use_hydat_peaks Logical value indicating whether to use instantaneous peaks from HYDAT for analysis.
#'    Leave blank if not required.
#' @param rolling_days Numeric vector of the number of days to apply the rolling mean. Default \code{c(3,7,30)}.
#' @param rolling_align Character string identifying the direction of the rolling mean from the specified date, either by the first 
#'    ('left'), last ('right), or middle ('center') day of the rolling n-day group of observations. Default \code{'right'}.
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
#' @param water_year Logical value indicating whether to use water years to group data instead of calendar years. Water years 
#'    are designated by the year in which they end. Default \code{FALSE}.
#' @param water_year_start Numeric value indicating the month of the start of the water year. Used if \code{water_year = TRUE}. 
#'    Default \code{10}.
#' @param start_year Numeric value of the first year to consider for analysis. Leave blank to use the first year of the source data.
#' @param end_year Numeric value of the last year to consider for analysis. Leave blank to use the last year of the source data.
#' @param exclude_years Numeric vector of years to exclude from analysis. Leave blank to include all years. 
#' @param complete_years Logical values indicating whether to include only years with complete data in analysis. Default \code{FALSE}.          
#' @param months Numeric vector of months to include in analysis (ex. \code{6:8} for Jun-Aug). Leave blank to summarize 
#'    all months (default \code{1:12}).
#' @param ignore_missing Logical value indicating whether dates with missing values should be included in the calculation. If
#'    \code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only statistics from time periods 
#'    with no missing dates will be returned. Default \code{TRUE}.
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
#' compute_frequency_analysis(data="08NM116",
#'                            start_year = 1980,
#'                            end_year = 2010)
#'                             
#' }
#' @export


compute_frequency_analysis <- function(data = NULL,
                                       dates = Date,
                                       values = Value,
                                       rolling_days = c(1, 3, 7, 30),
                                       rolling_align = "right",
                                       use_hydat_peaks = FALSE,
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
                                       complete_years = FALSE,
                                       months = 1:12,
                                       ignore_missing = FALSE){
  
  # replicate the frequency analysis of the HEC-SSP program
  # refer to Chapter 7 of the user manual
  
  ## Check non-data source arguments
  
  ## CHECKS ON OTHER ARGUMENTS
  ## -------------------------
  
  if(!is.logical(water_year))         stop("water_year argument must be logical (TRUE/FALSE).")
  if(!is.numeric(water_year_start))   stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(length(water_year_start)>1)      stop("water_year_start argument must be a number between 1 and 12 (Jan-Dec).")
  if(!water_year_start %in% c(1:12))  stop("water_year_start argument must be an integer between 1 and 12 (Jan-Dec).")
  
  if(length(start_year)>1)        stop("Only one start_year value can be listed")
  if(!start_year %in% c(0:9999))  stop("start_year must be an integer.")
  if(length(end_year)>1)          stop("Only one end_year value can be listed")
  if(!end_year %in% c(0:9999))    stop("end_year must be an integer.")
  if(start_year > end_year)       stop("start_year must be less than or equal to end_year.")
  
  if(!is.null(exclude_years) & !is.numeric(exclude_years)) stop("List of exclude_years must be numeric - ex. 1999 or c(1999,2000).")
  if(!all(exclude_years %in% c(0:9999)))                   stop("Years listed in exclude_years must be integers.")
  
  if(!is.logical(complete_years))         stop("complete_years argument must be logical (TRUE/FALSE).")
  
  if(!is.null(months) & !is.numeric(months)) stop("months argument must be numbers between 1 and 12 (Jan-Dec).")
  if(!all(months %in% c(1:12)))              stop("months argument must be numbers between 1 and 12 (Jan-Dec).")
  
  if(!is.logical(use_log))         stop("use_log must be logical (TRUE/FALSE).")
  if(!is.logical(use_max))         stop("use_max must be logical (TRUE/FALSE).")
  if(!all(prob_plot_position %in% c("weibull","median","hazen")))  stop("prob_plot_position must be one of weibull, median, or hazen.")
  if(!is.numeric(prob_scale_points)) stop("prob_scale_points must be numeric and between 0 and 1 (not inclusive).")
  if(!all(prob_scale_points > 0 & prob_scale_points < 1))stop("prob_scale_points must be numeric and between 0 and 1 (not inclusive).")
  if(!all(fit_distr %in% c("weibull", "PIII")))          stop("fit_distr must be one of weibull or PIII.")
  if(!is.numeric(fit_quantiles))                         stop("fit_quantiles must be numeric and between 0 and 1 (not inclusive).")
  if(!all(fit_quantiles > 0 & fit_quantiles < 1))        stop("fit_quantiles must be numeric and between 0 and 1 (not inclusive).")
  if(fit_distr[1] == 'weibull' & use_log)                stop("Cannot fit Weibull distribution on log-scale.")
  if(fit_distr[1] != "PIII" & fit_distr_method[1] == "MOM") stop('MOM only can be used with PIII distribution.')
  
  
  ## CHECKS and SETUP DATA
  ## -------------------------
  
  ### HYDAT PEAK DATA
  if(!is.logical(use_hydat_peaks)) stop("use_hydat_peaks must be logical (TRUE/FALSE).")
  if(use_hydat_peaks){
    # Checks for use_hydat_peaks
    if(is.null(data) | is.data.frame(data))   stop("A HYDAT station number must be listed in data argument when using use_hydat_peaks.")
    if(is.vector(data) & length(data) != 1)   stop("Only one HYDAT station number can be listed for this function.")
    if(!data %in% dplyr::pull(tidyhydat::allstations[1])) stop("Station number listed in data argument does not exist in HYDAT.")
    if(water_year)           warning("water_year argument ignored when using use_hydat_peaks.")
    if(length(months) < 12)  warning("months argument ignored when using use_hydat_peaks.")
    
    # Get peak data
    inst_peaks <- suppressMessages(tidyhydat::hy_annual_instant_peaks(data))
    inst_peaks <- dplyr::filter(inst_peaks, Parameter == "Flow")
    inst_peaks <- dplyr::filter(inst_peaks, PEAK_CODE == ifelse(use_max, "MAX", "MIN"))
    inst_peaks$Year <- lubridate::year(inst_peaks$Date)
    inst_peaks <- dplyr::select(inst_peaks, Year, Measure = PEAK_CODE, value = Value)
    inst_peaks <- dplyr::mutate(inst_peaks, Measure = paste0("INST_", Measure))
    
    # Filter peak data
    inst_peaks <- inst_peaks[ inst_peaks$Year >= start_year & inst_peaks$Year <= end_year,]
    inst_peaks <- dplyr::filter(inst_peaks, !(Year %in% exclude_years))
    
    Q_stat <- inst_peaks
  }
  
  ### HYDAT or dataframe data
  if(!use_hydat_peaks) {
    if(!is.numeric(rolling_days))                        stop("rolling_days argument must be numeric.")
    if(!all(rolling_days %in% c(1:180)))                 stop("rolling_days argument must be integers > 0 and <= 180).")
    if(!rolling_align %in% c("right", "left", "center")) stop("rolling_align argument must be 'right', 'left', or 'center'.")
    
    ## CHECKS ON FLOW DATA
    ## -------------------
    
    # Check if data is provided
    if(is.null(data))   stop("No data provided, must provide a data frame or HYDAT station number(s).")
    if(is.vector(data)) {
      if(length(data) != 1)   stop("Only one HYDAT station number can be listed for this function.")
      if(!data %in% dplyr::pull(tidyhydat::allstations[1]))  stop("Station number listed in data argument does not exist in HYDAT.")
      flow_data <- suppressMessages(tidyhydat::hy_daily_flows(station_number = data))
    } else {
      flow_data <- data
    }
    if(!is.data.frame(flow_data)) stop("Incorrect selection for data argument, must provide a data frame or HYDAT station number.")
    flow_data <- as.data.frame(flow_data) # Getting random 'Unknown or uninitialised column:' warnings if using tibble
    
    flow_data <- dplyr::ungroup(flow_data)
    
    # Get the just Date and Value columns
    # This method allows the user to select the Station, Date or Value columns if the column names are different
    if(!as.character(substitute(values)) %in% names(flow_data) & !as.character(substitute(dates)) %in% names(flow_data)) 
      stop("Dates and values not found in data frame. Rename dates and values columns to 'Date' and 'Value' or identify the columns using 'dates' and 'values' arguments.")
    if(!as.character(substitute(dates)) %in% names(flow_data))  
      stop("Dates not found in data frame. Rename dates column to 'Date' or identify the column using 'dates' argument.")
    if(!as.character(substitute(values)) %in% names(flow_data)) 
      stop("Values not found in data frame. Rename values column to 'Value' or identify the column using 'values' argument.")
    
    # Gather required columns
    flow_data <- flow_data[,c(as.character(substitute(dates)),
                              as.character(substitute(values)))]
    colnames(flow_data) <- c("Date","Value")
    
    # Check columns are in proper formats
    if(!inherits(flow_data$Date[1], "Date"))  stop("'Date' column in provided data frame does not contain dates.")
    if(!is.numeric(flow_data$Value))          stop("'Value' column in provided data frame does not contain numeric values.")
    
    if(fit_distr[1] == 'weibull' & any(flow_data$Value < 0, na.rm = TRUE))
      stop("Cannot fit weibull distribution with negative flow values.")
    
    # Setup data
    flow_data <- fill_missing_dates(data = flow_data, water_year = water_year, water_year_start = water_year_start)
    flow_data <- add_date_variables(data = flow_data, water_year = water_year, water_year_start = water_year_start)
    
    # Set selected year-type column for analysis
    if (water_year) {
      flow_data$AnalysisYear <- flow_data$WaterYear
    }  else {
      flow_data$AnalysisYear <- flow_data$Year
    }
    
    # Loop through each rolling_days and add customized names of rolling means to flow_data
    for (day in rolling_days) {
      flow_data_temp <- dplyr::select(flow_data, Date, Value)
      flow_data_temp <- add_rolling_means(flow_data_temp, days = day, align = rolling_align)
      names(flow_data_temp)[names(flow_data_temp) == paste0("Q", day, "Day")] <- paste("Q", formatC(day, width = 3, format = "d", 
                                                                                                    flag = "0"), "-day-Avg", sep = "")
      flow_data_temp <- dplyr::select(flow_data_temp, -Value)
      flow_data <- merge(flow_data, flow_data_temp, by = "Date", all = TRUE)
    }
    
    # Filter for the selected year
    flow_data <- dplyr::filter(flow_data, AnalysisYear >= start_year & AnalysisYear <= end_year)
    flow_data <- dplyr::filter(flow_data, !(AnalysisYear %in% exclude_years))
    flow_data <- dplyr::filter(flow_data, Month %in% months)
    
    # Remove incomplete years if selected
    if(complete_years){
      comp_years <- dplyr::summarise(dplyr::group_by(flow_data, STATION_NUMBER, AnalysisYear),
                                     complete_yr = ifelse(sum(!is.na(RollingValue)) == length(AnalysisYear), TRUE, FALSE))
      flow_data <- merge(flow_data, comp_years, by = "AnalysisYear")
      flow_data <- dplyr::filter(flow_data, complete_yr == "TRUE")
      flow_data <- dplyr::select(flow_data, -complete_yr)
    }
    
    # Calculate the min or max of the rolling means for each year
    flow_data <- flow_data[,-(1:6)]
    flow_data <- tidyr::gather(flow_data, Measure, value, -1)
    Q_stat <- dplyr::summarise(dplyr::group_by(flow_data, AnalysisYear, Measure),
                               value = ifelse(use_max, 
                                              max(value, na.rm = ignore_missing), 
                                              min(value, na.rm = ignore_missing)))
    Q_stat <- dplyr::rename(Q_stat, Year = AnalysisYear)
  }


  ## Define functions for analysis
  ##------------------------------

  # Define the log=Pearson III function needed for fitting at the GLOBAL environment level
  dPIII <<-function(x, shape, location, scale) PearsonDS::dpearsonIII(x, shape, location, scale, log = FALSE)
  pPIII <<-function(q, shape, location, scale) PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
  qPIII <<-function(p, shape, location, scale) PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)

  mPIII <<-function(order, shape, location, scale){
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
  if(prob_plot_position[1] == 'weibull'){a <- 0; b <- 0}
  if(prob_plot_position[1] == 'median' ){a <-.3; b <-.3}
  if(prob_plot_position[1] == 'hazen'  ){a <-.5; b <-.5}
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
  if(!use_hydat_peaks) {
    plotdata2$Measure <- paste(formatC(as.numeric(substr(plotdata2$Measure, 2, 4)), width = 3), "-day Avg", sep = "")
  }
  
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
    ggplot2::scale_x_continuous(trans=scales::probability_trans("norm", lower.tail=FALSE),
                                breaks=prob_scale_points,
                                sec.axis=ggplot2::sec_axis(trans=~1/.,
                                                           name='Return Period',
                                                           breaks=c(1.01,1.1,2,5,10,20,100,1000),
                                                           labels=function(x){ifelse(x<2,x,round(x,0))}))+
    ggplot2::theme(axis.title.x.top = ggplot2::element_text(size=10),
                   legend.title=ggplot2::element_blank(), legend.key.size=ggplot2::unit(.1,"in"))

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
  #   rolling_days = paste(rolling_days, collapse = ', '),
  #   fit_distr = fit_distr[1],  # distributions fit to the data
  #   fit_distr_method = fit_distr_method[1],
  #   prob_plot_position = prob_plot_position[1]
  # )
  # analysis.options.df <- data.frame("Option" = names(analysis.options),
  #                                   "Selection" = unlist(analysis.options, use.names = FALSE))


  list(Q_stat = Q_stat,
       plotdata = plotdata,  # has the plotting positions for each point in frequency analysis
       freqplot = freqplot,
       fit = fit,               # list of fits of freq.distr to each measure
       fitted_quantiles = fitted_quantiles_output#,             # fitted quantiles and their transposition
       #overview = analysis.options.df
  )

}
