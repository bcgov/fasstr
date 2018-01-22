
<!-- README.md is generated from README.Rmd. Please edit that file -->
<a rel="Exploration" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a>

fasstr
======

The Flow Analysis Summary Statistics Tool for R (`fasstr`) is a set of [R](http://www.r-project.org) functions to tidy, summarize, analyze, trend, and visualize streamflow data. This package summarizes continuous daily mean streamflow data into various daily, monthly, annual, and long-term statistics, completes annual trends and frequency analyses, and provides output tables and plots.

Features
--------

This package provides a set of quick solution functions for streamflow data tidying/preparation (add\_ and fill\_ functions), screening (screen\_ functions), statistical analyses (calc\_ and compute\_ functions), and visualization (plot\_ functions), amongst others.

Useful in-function arguments include the utilization of the `tidyhydat` package to pull streamflow data from a Water Survey of Canada [HYDAT](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html) database for analyses; and arguments for filtering of years and months in analyses, option to choose water years for analyses instead of calendar years (and choice of start month), and customizing how missing dates are handled.

Installation
------------

To install the `fasstr` package, you need to install the `devtools` package then the `fasstr` package

``` r
install.packages("devtools")
devtools::install_github("bcgov/fasstr", ref = "devel")
```

Then to call the `fasstr` functions you can either load the package using the `library()` function or access a specific function using a double-colon (e.g. `fasstr::calc_daily_stats()`). Several other packages will be installed in addition including [tidyhydat](https://cran.r-project.org/web/packages/tidyhydat/index.html) for data gathering, [zyp](https://cran.r-project.org/web/packages/zyp/index.html) for trending, [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) for creating plots, and [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html) and [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html) for various data wrangling and summarizing functions, amongst others. Many of the other packages are required for the frequency analysis functions.

``` r
library(fasstr)
```

To utilize the `tidyhydat` features, you will need to download a HYDAT database using the `tidyhydat::download_hydat()` function.

Usage
-----

### Data Input

All functions in `fasstr` require a data frame of daily mean streamflow from one or more hydrometric stations. Long-term and continuous datasets are preferred for most analyses, but seasonal and partial data can be used. Other daily time series data, like temperature, precipitation or water levels, may also be used, but with certain caution as some calculations/conversions are based on units of streamflow (cubic metres per second). Data is provided to each function using the `data` argument with two options: a data frame of daily data or a vector of HYDAT station numbers (ex. `08NM116` or `c(08NM116,08NM242)`).

Using the data frame option, a data frame of daily data containing columns of dates (YYYY-MM-DD in date format), values (mean daily discharge in cubic metres per second in numeric format), and, optionally, grouping identifiers (character string of station names or numbers) is called. By default the functions will look for columns identified as 'Date', 'Value', and 'STATION\_NUMBER' (grouping results by STATION\_NUMBER), respectively, but columns of different names can be identified using the `dates`, `values`, `grouping` column arguments (PROVIDE EXAMPLE), respectively. The following is an example of an appropriate flow\_data dataframe:

``` r
str(flow_data)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    16102 obs. of  2 variables:
#>  $ Date : Date, format: "1972-12-01" "1972-12-02" ...
#>  $ Value: num  3 0.94 0.385 0.241 0.207 ...
```

Alternatively, you can directly extract a HYDAT flow data frame directly from a HYDAT database by listing HYDAT station numbers in the `data` argument (ex. `data = "08NM116"` or `data = c("08NM116","08NM242")`). A data frame of daily streamflow data for all stations listed will be extracted using `tidyhydat`.

This package allows for multiple stations (or other groupings such as time-periods of a flow record (ex. pre/post an event)) to be analyzed in many of the functions provided station identifiers are provided using the `grouping` column argument (defaults to STATION\_NUMBER). If grouping column doesn't exist or is improperly named, then all values listed in the `values` column will be summarized. Note that the plotting functions do not use the `grouping` arguments (as just one plot is typically produced), so just one station/group should be provided.

### Function Types

#### Data Tidying/Preparation (add\_ and fill\_)

They add columns of variables and converted flow to data frame.

#### Analysis (screen\_, calc\_, and compute\_)

Screen data for missing dates, Calculate - long-term, annual, monthly, daily summary statistics Compute - trending and frequency analyses (plots and tables produced)

#### Visualization (plot\_)

plot the data (some set, some customizable)

### Function Options

#### Daily Rolling Means

Can choose rolling means for many functions.

#### Year and Month Filtering

To customize your analyses for specific time periods, you can designate the start and end years of your analysis using the `start_year` and `end_year` arguments and remove any unwanted years (for partial datasets for example) by listing them in the `excluded_years` argument (e.g. `excluded_years=c(1990,1992:1994)`). Leaving these arguments blank will result in the summary/analysis of all years of the provided dataset.

To group analyses by water, or hydrologic, years instead of calendar years, if desired, you can use `water_year=TRUE` within most functions (default is `water_year=FALSE`). A water year can be defined as a 12-month period that comprises a complete hydrologic cycle (wet seasons can typically cross calendar year), typically starting with the month with minimum flows (the start of a new water recharge cycle). As water years commonly start in October, the default water year is October for `fasstr`. If another start month is desired, you can choose is using the `water_year_start` argument (numeric month) to designate the water year time period. The water year label is designated by the year it ends in (e.g. water year 2000 goes from Oct 1, 1999 to Sep 30, 2000). Start, end and excluded years will be based on the specified water year.

#### Drainage Basin Area

Yield runoff statistics calculated in the some of the functions require an upstream drainage basin area (in sq. km) using the `basin_area` argument, where required. If no basin areas are supplied, all yield results will be `NA`. To apply a basin area (10 sqkm for example) to all daily observations, set the argument as `basin_area = 10`. If there are mulitple stations or groups to apply mulitple basin areas (using the `grouping` argument), set them individually using this option: `basin_area = c("08NM116"=795, "08NM242"=22)`. If a STATION\_NUMBER column exists with HYDAT station numbers, the function will automatically use the basin areas provided in HYDAT, if available, so `basin_area` is not required.

#### Handling Missing Dates

Coming soon. ignore\_missing argument. different functions have different defaults....

Examples
--------

### Summary statistics example: long-term statistics

To determine the summary statistics of an entire dataset and by month (mean, median, maximum, minimum, and some percentiles) you can use the `calc_longterm_stats()` function. If the 'Mission Creek near East Kelowna' hydrometric station is of interest you can list the station number in the `HYDAT` argument to obtain the data (if `tidyhydat` and HYDAT are installed).

``` r
calc_longterm_stats(data = "08NM116", start_year = 1981, end_year = 2010,
                    custom_months = 7:9, custom_months_label = "Summer")
#> # A tibble: 14 x 8
#>    STATION_NUMBER     Month      Mean Median Maximum Minimum    P10    P90
#>  *          <chr>    <fctr>     <dbl>  <dbl>   <dbl>   <dbl>  <dbl>  <dbl>
#>  1        08NM116       Jan  1.217472  1.000    9.50   0.160 0.5400  1.850
#>  2        08NM116       Feb  1.156100  0.970    4.41   0.140 0.4742  1.994
#>  3        08NM116       Mar  1.847916  1.405    9.86   0.380 0.7048  3.800
#>  4        08NM116       Apr  8.318406  6.255   37.90   0.505 1.6290 17.500
#>  5        08NM116       May 23.576258 20.750   74.40   3.830 9.3330 41.220
#>  6        08NM116       Jun 21.513999 19.500   84.50   0.450 6.0990 38.900
#>  7        08NM116       Jul  6.481003  3.900   54.50   0.332 1.0200 15.000
#>  8        08NM116       Aug  2.125394  1.570   13.30   0.427 0.7749  4.292
#>  9        08NM116       Sep  2.189149  1.580   14.60   0.364 0.7347  4.352
#> 10        08NM116       Oct  2.099909  1.595   15.20   0.267 0.7935  3.980
#> 11        08NM116       Nov  2.041519  1.730   11.70   0.260 0.5600  3.901
#> 12        08NM116       Dec  1.297002  1.050    7.30   0.342 0.5000  2.333
#> 13        08NM116 Long-term  6.167362  1.890   84.50   0.140 0.6800 19.300
#> 14        08NM116    Summer  3.613834  1.980   54.50   0.332 0.7989  7.641
```

### Plotting example 1: daily summary statistics

To visualize the daily streamflow patterns on an annual basis, the `plot_daily_stats()` function will plot out various summary statistics for each day of the year. Data can also be filtered for certain years of interest (a 1981-2010 normals period for this example) using the `start_year` and `end_year` arguments. Multiple plots are produced with this function, so this example plots just the summary statistics (`[1]`).

``` r
plot_daily_stats(data = "08NM116",
                 start_year = 1981,
                 end_year = 2010,
                 log_discharge = TRUE,
                 include_year = 1991)
```

![](tools/readme/README-plot1-1.png)

### Plotting example 2: flow duration curves

Flow duration curves can be produced using the `plot_flow_duration()` function.

``` r
plot_flow_duration(data = "08NM116",
                   start_year = 1981,
                   end_year = 2010)
```

![](tools/readme/README-plot2-1.png)

### Analysis example: low-flow frequency analysis

This package also provides a function, `compute_frequency_analysis()`, to complete frequency analyses (using the same methods as [HEC-SSP](http://www.hec.usace.army.mil/software/hec-ssp/)). The default fitting distribution is 'log-Pearson Type III', but the 'weibull' distribution can also be used. Other default plotting and fitting methods are described in the function documentation. For this example, the 7-day low-flow (low-flow is default) quantiles are calculated for the Mission Creek hydrometric station using the 'log-Pearson Type III' distribution. With this, several low-flow indicators can be determined (i.e. 7Q5, 7Q10).

``` r
compute_frequency_analysis(data = "08NM116",
                           start_year = 1981,
                           end_year = 2010,
                           rolling_days = 7)[5]
#> $fitted_quantiles
#>    Distribution Probability Return Period Q007-day-Avg
#> 1          PIII       0.010    100.000000    0.1929445
#> 2          PIII       0.050     20.000000    0.2770067
#> 3          PIII       0.100     10.000000    0.3318582
#> 4          PIII       0.200      5.000000    0.4084737
#> 5          PIII       0.500      2.000000    0.5881156
#> 6          PIII       0.800      1.250000    0.8122160
#> 7          PIII       0.900      1.111111    0.9463443
#> 8          PIII       0.950      1.052632    1.0651498
#> 9          PIII       0.975      1.025641    1.1735280
#> 10         PIII       0.980      1.020408    1.2066583
#> 11         PIII       0.990      1.010101    1.3050198
```

Project Status
--------------

This package is under development. This package is maintained by the Water Protection and Sustainability Branch of the [British Columbia Ministry of Environment and Climate Change Strategy](https://www2.gov.bc.ca/gov/content/environment/air-land-water/water).

Getting Help or Reporting an Issue
----------------------------------

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/fasstr/issues/).

How to Contribute
-----------------

If you would like to contribute to the package, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

License
-------

    Copyright 2017 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
