
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fasstr <img src="man/figures/fasstr.PNG" align="right" />

<!-- badges: start -->

<a id="devex-badge" rel="Delivery" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/delivery.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a>
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Travis build
status](https://travis-ci.org/bcgov/fasstr.svg?branch=master)](https://travis-ci.org/bcgov/fasstr)

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/fasstr)](https://cran.r-project.org/package=fasstr)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/fasstr?color=brightgreen)](https://CRAN.R-project.org/package=fasstr)
[![cran
checks](https://cranchecks.info/badges/worst/fasstr)](https://CRAN.R-project.org/web/checks/check_results_fasstr.html)
<!-- badges: end -->

## 

The Flow Analysis Summary Statistics Tool for R (‘fasstr’) is a set of
[R](http://www.r-project.org) functions to clean, summarize, analyze,
trend, and visualize streamflow data. This package summarizes continuous
daily mean streamflow data into various daily, monthly, annual, and
long-term statistics, completes annual trends and frequency analyses, in
both table and plot formats.

This package provides functions with solutions for streamflow data:

  - cleaning (to prepare data for analyses; `add_*` and `fill_*`
    functions),
  - screening (to look for outliers and missing data; `screen_*`
    functions),
  - analyzing (basic summary statistics, frequency analyses, trending
    ;`calc_*` and `compute_*` functions), and
  - visualizing (to plot statistics; `plot_*` functions), amongst
    others.

Useful features of functions include:

  - the integration of the ‘tidyhydat’ package to pull streamflow data
    from a Water Survey of Canada
    [HYDAT](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html)
    database for analyses;
  - arguments for filtering of years and months in analyses and plotting
    (internally tidys your data);
  - choosing the start month of your water year;
  - selecting for rolling day averages (e.g. 7-day rolling average);
  - plotting options; and,
  - choosing how missing dates are handled, amongst others.

## Installation

You can install ‘fasstr’ using the following code. It may take a few
moments as there are several dependency packages will also be installed,
including [‘tidyhydat’](https://CRAN.R-project.org/package=tidyhydat)
for downloading Water Survey of Canada hydrometric data,
[‘zyp’](https://CRAN.R-project.org/package=zyp) for trending,
[‘ggplot2’](https://CRAN.R-project.org/package=ggplot2) for creating
plots, and [‘dplyr’](https://CRAN.R-project.org/package=dplyr) and
[‘tidyr’](https://CRAN.R-project.org/package=tidyr) for various data
wrangling and summarizing functions, amongst others.

``` r
install.packages("fasstr")
```

To install the development version of the ‘fasstr’ package, you need to
install the remotes package then the ‘fasstr’ package.

``` r
if(!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("bcgov/fasstr")
```

To utilize the ‘tidyhydat’ features (using the station\_number
argument), you will need to download a HYDAT database using the
`tidyhydat::download_hydat()` function.

## Vignettes

  - [Get Started with
    fasstr](https://bcgov.github.io/fasstr/articles/fasstr.html)
  - [fasstr Users
    Guide](https://bcgov.github.io/fasstr/articles/fasstr_users_guide.html)
  - [Computing an Annual Trends
    Analysis](https://bcgov.github.io/fasstr/articles/fasstr_trending_analysis.html)
  - [Computing a Volume frequency
    Analysis](https://bcgov.github.io/fasstr/articles/fasstr_frequency_analysis.html)
  - [Computing a Full fasstr
    Analysis](https://bcgov.github.io/fasstr/articles/fasstr_full_analysis.html)
  - [Under the
    Hood](https://bcgov.github.io/fasstr/articles/fasstr_under_the_hood.html)

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/fasstr/issues/).

## How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

    Copyright 2019 Province of British Columbia
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 
    
       http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

## 

This package is maintained by the [Water Protection and Sustainability
Branch of the British Columbia Ministry of Environment and Climate
Change
Strategy](https://www2.gov.bc.ca/gov/content/environment/air-land-water/water).