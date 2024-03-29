% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_hydat_peak_frequencies.R
\name{compute_hydat_peak_frequencies}
\alias{compute_hydat_peak_frequencies}
\title{Perform a frequency analysis on annual peak statistics from HYDAT}
\usage{
compute_hydat_peak_frequencies(
  station_number,
  use_max = FALSE,
  use_log = FALSE,
  prob_plot_position = c("weibull", "median", "hazen"),
  prob_scale_points = c(0.9999, 0.999, 0.99, 0.9, 0.5, 0.2, 0.1, 0.02, 0.01, 0.001,
    1e-04),
  fit_distr = c("PIII", "weibull"),
  fit_distr_method = ifelse(fit_distr == "PIII", "MOM", "MLE"),
  fit_quantiles = c(0.975, 0.99, 0.98, 0.95, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.01),
  start_year,
  end_year,
  exclude_years,
  plot_curve = TRUE
)
}
\arguments{
\item{station_number}{A character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of
which to extract annual peak minimum or maximum instantaneous streamflow data from a HYDAT database. Requires \code{tidyhydat} 
package and a HYDAT database.}

\item{use_max}{Logical value to indicate using maximums rather than the minimums for analysis. Default \code{FALSE}.}

\item{use_log}{Logical value to indicate log-scale transforming of flow data before analysis. Default \code{FALSE}.}

\item{prob_plot_position}{Character string indicating the plotting positions used in the frequency plots, one of \code{'weibull'},
\code{'median'}, or \code{'hazen'}. Points are plotted against  (i-a)/(n+1-a-b) where \code{i} is the rank of the value; \code{n} is the 
sample size and \code{a} and \code{b} are defined as: (a=0, b=0) for Weibull plotting positions; (a=.2; b=.3) for Median 
plotting positions; and (a=.5; b=.5) for Hazen plotting positions. Default \code{'weibull'}.}

\item{prob_scale_points}{Numeric vector of probabilities to be plotted along the X axis in the frequency plot. Inverse of 
return period. Default \code{c(.9999, .999, .99, .9, .5, .2, .1, .02, .01, .001, .0001)}.}

\item{fit_distr}{Character string identifying the distribution to fit annual data, one of \code{'PIII'} (Log Pearson Type III)
or \code{'weibull'} (Weibull) distributions. Default \code{'PIII'}.}

\item{fit_distr_method}{Character string identifying the method used to fit the distribution, one of \code{'MOM'} (method of
moments) or \code{'MLE'} (maximum likelihood estimation). Selected as \code{'MOM'} if \code{fit_distr ='PIII'} (default) or 
\code{'MLE'} if \code{fit_distr = 'weibull'}.}

\item{fit_quantiles}{Numeric vector of quantiles to be estimated from the fitted distribution. 
Default \code{c(.975, .99, .98, .95, .90, .80, .50, .20, .10, .05, .01)}.}

\item{start_year}{Numeric value of the first year to consider for analysis. Leave blank or set well before start date (i.e.
\code{1800}) to use from the first year of the source data.}

\item{end_year}{Numeric value of the last year to consider for analysis. Leave blank or set well after end date (i.e.
\code{2100}) to use up to the last year of the source data.}

\item{exclude_years}{Numeric vector of years to exclude from analysis. Leave blank or set to \code{NULL} to include all years.}

\item{plot_curve}{Logical value to indicate plotting the computed curve on the probability plot. Default \code{TRUE}.}
}
\value{
A list with the following elements:
  \item{Freq_Analysis_Data}{Data frame with computed annual summary statistics used in analysis.}
  \item{Freq_Plot_Data}{Data frame with co-ordinates used in frequency plot.}
  \item{Freq_Plot}{ggplot2 object with frequency plot}
  \item{Freq_Fitting}{List of fitted objects from fitdistrplus.}
  \item{Freq_Fitted_Quantiles}{Data frame with fitted quantiles.}
}
\description{
Performs a volume frequency analysis on annual peak statistics (instantaneous minimums or maximums) extracted from
   HYDAT. Calculates statistics from all years, unless specified. The \code{data} argument is not available. Analysis 
   methodology replicates that from \href{https://www.hec.usace.army.mil/software/hec-ssp/}{HEC-SSP}.  Returns a list of tibbles 
   and plots.
}
\examples{
\dontrun{

# Working examples (see arguments for further analysis options):

# Compute an annual peak frequency analysis using default arguments (instantaneous lows)
results <- compute_hydat_peak_frequencies(station_number = "08NM116",
                                          start_year = 1980,
                                          end_year = 2010)
                               
# Compute an annual peak frequency analysis using default arguments (instantaneous highs)
results <- compute_hydat_peak_frequencies(station_number = "08NM116",
                                          start_year = 1980,
                                          end_year = 2010,
                                          use_max = TRUE)
                                                            
}
}
\seealso{
\code{\link{compute_frequency_analysis}}
}
