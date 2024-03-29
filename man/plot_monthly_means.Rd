% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_monthly_means.R
\name{plot_monthly_means}
\alias{plot_monthly_means}
\title{Plot monthly means and percent LTMADs}
\usage{
plot_monthly_means(
  data,
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
  percent_MAD = c(10, 20, 100)
)
}
\arguments{
\item{data}{Data frame of daily data that contains columns of dates, flow values, and (optional) groups (e.g. station numbers).
Leave blank or set to \code{NULL} if using \code{station_number} argument.}

\item{dates}{Name of column in \code{data} that contains dates formatted YYYY-MM-DD. Only required if dates column name is not 
'Date' (default). Leave blank or set to \code{NULL} if using \code{station_number} argument.}

\item{values}{Name of column in \code{data} that contains numeric flow values, in units of cubic metres per second.
Only required if values column name is not 'Value' (default). Leave blank if using \code{station_number} argument.}

\item{groups}{Name of column in \code{data} that contains unique identifiers for different data sets, if applicable. Only required 
if groups column name is not 'STATION_NUMBER'. Function will automatically group by a column named 'STATION_NUMBER' if 
present. Remove the 'STATION_NUMBER' column beforehand to remove this grouping. Leave blank if using \code{station_number} 
argument.}

\item{station_number}{Character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of
which to extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.
Leave blank if using \code{data} argument.}

\item{roll_days}{Numeric value of the number of days to apply a rolling mean. Default \code{1}.}

\item{roll_align}{Character string identifying the direction of the rolling mean from the specified date, either by the first 
(\code{'left'}), last (\code{'right'}), or middle (\code{'center'}) day of the rolling n-day group of observations. 
Default \code{'right'}.}

\item{water_year_start}{Numeric value indicating the month (\code{1} through \code{12}) of the start of water year for
analysis. Default \code{1}.}

\item{start_year}{Numeric value of the first year to consider for analysis. Leave blank or set well before start date (i.e.
\code{1800}) to use from the first year of the source data.}

\item{end_year}{Numeric value of the last year to consider for analysis. Leave blank or set well after end date (i.e.
\code{2100}) to use up to the last year of the source data.}

\item{exclude_years}{Numeric vector of years to exclude from analysis. Leave blank or set to \code{NULL} to include all years.}

\item{months}{Numeric vector of months to include in analysis. For example, \code{3} for March, \code{6:8} for Jun-Aug or 
\code{c(10:12,1)} for first four months (Oct-Jan) when \code{water_year_start = 10} (Oct). Default summarizes all 
months (\code{1:12}).}

\item{plot_months}{Numeric vector of months to include on the plot after calculating statistics. 
For example, \code{3} for March or \code{6:8} for Jun-Aug. Differs from 'months' argument where that
argument filters for specific months, this one just chooses which months to plot. Default \code{1:12}.}

\item{complete_years}{Logical values indicating whether to include only years with complete data in analysis. Default \code{FALSE}.}

\item{ignore_missing}{Logical value indicating whether dates with missing values should be included in the calculation. If
\code{TRUE} then a statistic will be calculated regardless of missing dates. If \code{FALSE} then only those statistics from
time periods with no missing dates will be returned. Default \code{FALSE}.}

\item{include_title}{Logical value to indicate adding the group/station number to the plot, if provided. Default \code{FALSE}.}

\item{percent_MAD}{Numeric vector of percentages of long-term mean annual discharge to add to the plot (ex. \code{20} for 20 percent 
MAD or \code{c(5,10,20)} for multiple percentages). Set to \code{NA} for none. Default \code{c(10,20,100)}.}
}
\value{
A list of ggplot2 objects for with the following plots for each station provided:
  \item{Annual_Means}{a plot that contains annual means with the long-term mean as the x-axis intercept}
}
\description{
Plot monthly means and add long-term mean annual discharge percentages. Calculates statistics
   from all values, unless specified. Mean data calculated using \code{calc_longterm_daily_stats()} function. 
   Returns a list of plots.
}
\examples{
# Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
if (file.exists(tidyhydat::hy_downloaded_db())) {

# Plot monthly means
plot_monthly_means(station_number = "08NM116",
                   complete_years = TRUE)

# Plot mean flows with custom LTMADs
plot_monthly_means(station_number = "08NM116",
                   complete_years = TRUE,
                   percent_MAD = c(5,10,20,100))
                   
# Plot mean flows and plot just summer months
plot_monthly_means(station_number = "08NM116",
                   complete_years = TRUE, 
                   plot_months = 6:9)
                  
}
}
\seealso{
\code{\link{calc_longterm_daily_stats}}

\code{\link{calc_longterm_mean}}
}
