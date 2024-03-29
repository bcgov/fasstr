% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_daily_yield.R
\name{add_daily_yield}
\alias{add_daily_yield}
\title{Add a daily volumetric water yield column to daily flows}
\usage{
add_daily_yield(
  data,
  values = Value,
  groups = STATION_NUMBER,
  station_number,
  basin_area
)
}
\arguments{
\item{data}{Data frame of daily data that contains columns of dates, flow values, and (optional) groups (e.g. station numbers).
Leave blank or set to \code{NULL} if using \code{station_number} argument.}

\item{values}{Name of column in \code{data} that contains numeric flow values, in units of cubic metres per second.
Only required if values column name is not 'Value' (default). Leave blank if using \code{station_number} argument.}

\item{groups}{Name of column in \code{data} that contains unique identifiers for different data sets, if applicable. Only required 
if groups column name is not 'STATION_NUMBER'. Function will automatically group by a column named 'STATION_NUMBER' if 
present. Remove the 'STATION_NUMBER' column beforehand to remove this grouping. Leave blank if using \code{station_number} 
argument.}

\item{station_number}{Character string vector of seven digit Water Survey of Canada station numbers (e.g. \code{"08NM116"}) of
which to extract daily streamflow data from a HYDAT database. Requires \code{tidyhydat} package and a HYDAT database.
Leave blank if using \code{data} argument.}

\item{basin_area}{Upstream drainage basin area, in square kilometres, to apply to observations. Three options:

(1) Leave blank if \code{groups} is STATION_NUMBER with HYDAT station numbers to extract basin areas from HYDAT.

(2) A single numeric value to apply to all observations.

(3) List each basin area for each group/station in groups (can override HYDAT value if listed) as such \code{c("08NM116" = 795, 
"08NM242" = 10)}. If group is not listed the HYDAT area will be applied if it exists, otherwise it will be \code{NA}.}
}
\value{
A tibble data frame of the source data with an additional column:
  \item{Yield_mm}{daily water yield, in units of millimetres}
}
\description{
Add a column of daily water yields to a daily streamflow data set, in units of millimetres. Converts the discharge to a depth
  of water based on the upstream drainage basin area.
}
\examples{
# Run if HYDAT database has been downloaded (using tidyhydat::download_hydat())
if (file.exists(tidyhydat::hy_downloaded_db())) {

# Add a column of yields based on HYDAT basin area
add_daily_yield(station_number = "08NM116")
                     
# Add a column of yields based on a custom basin area
add_daily_yield(station_number = "08NM116",
                basin_area = 800)
                
}
}
