fasstr 0.5.0
=========================

Updated: 21 Apr 2022

### New functions:

  * plot_flow_data_symbols() - plots daily data with coloured points designating symbol categories from symbols column
  * plot_annual_symbols() - plots symbols by day of year or annual counts or percent of days with symbols from symbols column  
  * plot_annual_stats2() - plotting annual stats in ribbons (similar to plot_daily_stats)
  * plot_monthly_stats2() - plotting faceted monthly stats in ribbons (similar to plot_daily_stats)
  * calc_annual_highflows() and plot_annual_highflow() - calcs/plots annual high flow data, similar to _annual_lowflow() functions
  * plot_annual_peaks() - plots the values and dates from annual minimums and maximums for each year, from calc_annual_peaks()
    * plot_annual_peaks_year() - plots the values and dates from annual minimums and maximums fora given year
  * calc_annual_normal_days() and plot_annual_normal_days() - counts number of normal and above/below normal days - replaced deprecated _annual_outside_normal() functions
  * plot_annual_normal_days_year - plot days above normal, below normal and normal for a specific year
  * plot_annual_flow_timing_year - plot annual timing of flows for a specific year

### Function updates:

  * plot_missing_data() - new default plot type in tile format. Can change to original bar plot using plot_type = "bar".
  * plot_data_screening() - now changes the geom_point style based on if data is complete or has missing data (plot_availability argument) and now allows choice of annual summary statistics (include_stats argument)
  * screen_flow_data() - now add columns of counts of symbol categories provided by symbol column (HYDAT Symbol column). Can remove columns by using include_symbols = FALSE.
    * plot_annual_cumulative_stats() - new default plot type in bar format. Can change to original line plot using plot_type = "line".
    * calc_annual_outside_normal() and plot_annual_outside_normal() - deprecated, replaced with calc_annual_normal_days() and plot_annual_normal_days()
  
### Other updates:
  
  * Can now choose 0 and 100 percentiles (was previously between 1 and 99)
  * Added 'complete_years' argument to several functions for cross-package consistency. Doesn't change functionality, but sets/overrides ignore_missing to FALSE and allowed_missing to 0.
  * Removed HYDAT.sqlite file check in compute_HYDAT_peak_frequencies() - leaves check to tidyhydat
  * "Year" axes now labeled "Water Year" if water_year_start != 1
  * Some appropriate "Day of Year" axes now labeled "Day of Water Year" if water_year_start != 1
  * Changed y-axis labels on plots using scales::cut_short_scale() to remove exponents on large numbers
  * Annual plots with leading empty years (gaps, missing dates, etc) will now shift plot to first year of data
  
### Bug fixes:
  
  * complete_years function now applied to all functions in compute_ and write_full_analysis functions

### Internal code:
  
  * cleaning up argument check functions (logical, numeric etc)
  * new origin_date internal function
  * new filter_complete_yrs arg option (keep_all) to keep all years of data, just setting NA if incomplete
  
  
  

fasstr 0.4.1
=========================

Updated: 10 Dec 2021

#### Updates:

  * Fixed major bug: Seasons identifiers were mislabeled when water_year_start > 1 for add_seasons() and various functions use seasonal cumulative totals
  * Updated documentation - 'months' and other param descriptions
  * plot_flow_duration() - removed annotation ticks when not in log-scale
  * added 'log_ticks' argument to customize adding annotation_ticks or not when log scale
  * removed 'log_discharge' from plot_annual_lowflows()

fasstr 0.4.0
=========================

Updated: 3 Dec 2021

#### Updates:

  * added flexible 'allowed_missing' argument to applicable functions, that allows users to choose the percentage of missing
    dates, rather than binary ignore_missing. ignore_missing argument still functions as it should (to maintain historical and
    future use), and is only superseded when allowed_missing argument is utilized. This argument is currently applied to functions 
    that make most logical sense (annual and monthly functions) and are appropriate for calculations, i.e. basin summary statistics
    (means, percentiles, minimums, maximums). Versions of this, allowed_missing_annual and allowed_missing_monthly, exists for some
    functions that have both annual and monthly statistics and may have different allowances; only applied to summary statistics.
    The new argument is applied to the following functions:
    * allowed_missing argument: calc_annual_lowflows(), calc_annual_peaks(), calc_annual_stats(), calc_monthly_stats(), 
      compute_annual_frequencies(), compute_frequency_quantile(), plot_annual_stats(), plot_annual_lowflows(), 
      plot_annual_means(), plot_monthly_stats() 
    * allowed_missing_annual and allowed_missing_monthly: calc_all_annual_stats(), compute_annual_trends(),
      compute_full_analysis(), write_full_analysis()
    
  * added 'months' argument to following functions (covers all screen_, calc_, plot_, and compute_ functions now):
    * plot_flow_data() - will plot only data from selected months
    * add_cumulative_yield() and add_cumulative_volume() - will accumulate flows just in selected months
    * plot_longterm_daily_stats() - no ribbons where data gaps
    * plot_longterm_monthly_stats() - no ribbons where data gaps
    * calc_ and plot_daily_cumulative_stats() - will accumulate flows just in selected months
    * calc_ and plot_monthly_cumulative_stats() - will accumulate flows just in selected months
    * calc_ and plot_annual_cumulative_stats() - will only use months select, won't calc seasonal totals if not all months
    * calc_ and plot_annual_flow_timing() - will use timing from just selected months
    * calc_all_annual_stats() - will only use months select, won't calc seasonal totals if not all months
    * compute_annual_trends() - will only use months select, won't calc seasonal totals if not all months
    * compute_ and write_full_analysis() - will only use months select, won't calc seasonal totals if not all months
    
  * Updated 'Users Guide' vignette and Cheat Sheet with new argument updates
  * New vignette for GitHub Page: Using USGS dataRetrieval R package with fasstr
  * fixed bug: multiple custom groups/STATION_NUMBER's appropriately match to data when using a named vector
  * fixed bug: plot_daily_cumulative_stats() now plots accurate custom years
  * fixed bug: removed unnecessary legend items from plot_flow_duration()
  * fixed bug: corrected 'months' argument in calc_ and plot_annual_outside_normal() to properly filter
  * fixed bug: removed unnecessary NA warning in plot_longterm_daily and plot_longterm_monthly_stats
  * renamed GitHub branches from 'master' to 'main' and updated web links
  
fasstr 0.3.3
=========================

Updated: 20 Oct 2021

#### Updates:

  * For trending, changed default/recommended method to 'zhang' over 'yuelipon' (https://github.com/pacificclimate/zyp/issues/6)
  * Removed check for a hydat.sqlite3 file; tidyhydatstop error catches error
  * Removed lubridate package dependency
  * Fixed ggplot2::guides deprecated arguments warning
  * Simplified some internal coding for efficiency
  * Removed some function examples to reduce timing; examples of usage in vignettes
  * Removed TravisCI, added Github Action R-CMD-check using usethis::use_github_action_check_standard()


fasstr 0.3.2
=========================

Updated: 06 Nov 2020

#### Updates:

  * Added calc_annual_peaks() function to calculate both annual n-day min. and max. flows with dates of occurrence.
  * Added error in volume frequency analysis when providing zero or negative flow values to the logPIII dist.
  * Updated volume frequency analysis documentation.
  * Updated all vignettes for package updates, corrections, and issues.
  * Added new 'Get Started' vignette
  * Updated Cheatsheet
  * changed yield terminology to water yield from runoff yield
  * changed plot axis titles with expression()'s (i.e. m3/s --> cms)
  

fasstr 0.3.1
=========================

Updated: 08 Jan 2020

#### Updates:

  * added/modified files in prep for CRAN submission
  * fixed vignette coding errors
  * renamed function calc_longterm_mad to calc_longterm_mean
  * updated vignettes and README files
  * removed calc_longterm_stats and plot_longterm_stats
  * updated Examples in all documentation (replaced \dontrun{} with a function checking for a HYDAT, left for writing functions and larger analyses)
  * fixed multiple typos
  * updated DESCRIPTION file


fasstr 0.3.0
=========================

Updated: 14 May 2019

#### Updates:

  * Removed logical "water_year" argument and now "water_year_start" controls the start of year alone (default now 1 for January)
  * Renamed calc_longterm_stats and plot_longterm stats to calc_longterm_daily_stats and plot_longterm_daily_stats
  * Renamed 'include_year' argument to 'add_year' for some functions
  * complete_full_analysis just creates R objects; a new write_full_analysis saves the results in a single Excel document, with some plots in a plots folder; renamed 'sections' to 'analyses'
  * add_date_variables() outputs changed slightly to match new water_year_start argument
  * Replaced NULL and NA default values for function arguments to nothing, cleaning up documentation.
  * Replaced the package writexls with openxls for more functionality
  * Plotting: changed facet wrap labels, changed trending symbols
  * WSC station numbers with lower-case letters now accepted (08nm116 and 08NM116 both work)
  * basin_area argument as list now will default to HYDAT area if not listed, otherwise NA
  * Return Period axis has returned to the frequency analysis plot
  * Updated all documentation examples
  * Internal coding (MonthName levels coding cleaned up) 

#### New:

  * calc_longterm_monthly_stats and plot_longterm_monthly_stats that summarizes annual monthly mean flows data
  * calc_longterm_daily_stats and plot_longterm_daily_stats that summarizes daily mean flows data (calc_longterm_stats equivalent)
  * write_full_analysis writes the full analysis and doesn't create any objects in R
  * testthat functions for testing package (internal usage)

  
#### Bugs Fixed:

  * No warning now in annual functions if NA values produced for years listed in "exclude_years" argument
  
fasstr 0.2.8
=========================

Updated: 11 December 2018

#### Updates:

  * README updates
  * Renamed calc_lt_mad() and calc_lt_percentile() to calc_longterm_mad() and calc_longterm_percentile(), respectively.
  * Changed the plot_missing_dates points and lines to bar plots.
  * changed add_seasons() function from adding two 4 and 2 seasons columns to adding own custom seasons of desired lengths. All subsequent function and documentation updated were also completed.
  * Renamed some annual and seasonal total volume axes, column names, and plots names.
  * Updated the licence year in .R files.
  * Temporarily removed 'Return Period' axis from frequency plots due to ggplot2 3.0.1 issues.
  
#### Bugs Fixed:

  * compute_full_analysis() now plots all daily statistics plots with years
  
#### New:

  * New internal function add_water_months() to streamline some processes




fasstr 0.2.7
=========================

Updated: 27 September 2018

#### New:

  * compute_frequency_analysis() function to calculate a frequency analysis with custom data (update other frequency analysis functions to use this function internally)
  * Vignettes completed:
      * Users Guide
      * Full Analysis Guide
      * Trends Analysis Guide
      * Frequency Analysis Guide
      * Under the Hood

#### Updates:

  * frequency analysis updates:
      * option to plot the computed curve or not
      * changed the names of the outputted objects
      * changed the measure names (rolling-day names of the annual_freq analysis (ex from Q007-day-avg to 7-Day))
      * All parameter documentation info in the compute_frequency_analysis function
      * updated full_analysis to include new changes
  * Completed Users Guide vignette
  * Completed Full Analysis vignette
  * Completed Frequency Analysis Vignette
  * Updated filetype argument in the write_ functions
  * Updated some plot object names
  
#### Bug Fixes

  * Added trendline to trends plots in compute_full_analysis
  
  
fasstr 0.2.6
=========================

Updated: 28 June 2018

#### New:

  * plot_annual_means() function to plot annual mean and long-term means
    
#### Updates:

  * Axes breaks and ticks on most plots
  * Renamed compute_frequency_stat() to compute_frequency_quantile()
  * calc_ and plot_annual_stats, compute_frequency description updates
  * Renamed Timeseries folder to Screening in compute_full_analysis() function
  * README Updates
  * Vignettes (renamed full_analysis)
  

fasstr 0.2.5.1
=========================

Updated: 19 June 2018

* Fixed write_objects_list to save in folder it creates
* Fixed calc_daily_stats to remove partial data years

fasstr 0.2.5
=========================

Updated: 18 June 2018

#### New:

  * New write_objects_list() function to write all plots and tables from a list of objects (help with frquency analysis writing)
  * Added vignettes:
    * 'fasstr' Users Guide (to be completed)
    * Trending Analysis Guide (to be completed)
    * Volume Frequency Analysis Guide (to be completed)
    * Under the Hood (to be completed)
    
#### Major Function Updates:

  * Renamed write_full_analysis to compute_full_analysis which creates a list object with all results with option to write everything. Fixed various bugs and issues.
  * Joined compute_annual_trends and plot_annual_trends.  It now consists of a list with a tibble of annual data, a tibble of trending results, and all trending plots
  * Added 'months' argument to calc_long_term() and all screening and missing data functions; also an include_longterm argument for longterm to choose whether to include or not
  
#### Other Updates:

  * Fixed bug where 'groups' column was not kept in resulting tibble, and add_basin_area(), add_rolling_means()
  * Removed Nan and Inf values from calc_monthly_stats when no data existed for a month
  * Plots of longterm_stats, daily_stats, and flow_duration plot nothing (instead of error) if all data is NA
  * Updated documentation for some functions
  * Removed colour brewer Set1 on some annual plots due to a lack of colours in set


fasstr 0.2.4
=========================

Updated: 8 May 2018

  * Fixed bug where groups function did not work if not "STATION_NUMBER"
  * Added warning if not all dates are dates in column
  * Added references in annual_flow_timing() and trending functions
  * Added 'See Also' documentation for many related functions
  * Removed error from daily and monthly cumulative stats/plots with no basin area (now produces NA)
  * Updated write_full_analysis so no section 7 is completed with insufficient data
  

fasstr 0.2.3
=========================

Updated: 17 April 2018

  * Updated write_full_analysis() documentation

fasstr 0.2.2
=========================

Updated: 17 April 2018

  * Added write_full_analysis() function to write almost almost all plots and tables in a folder
  * Added some interal checks functions
  

fasstr 0.2.1
=========================

Updated: 13 April 2018

  * Reformatted examples script in testing folder as a temporary help document until a vignette is built
  * Moved the previous examples script to a new testing script in the same folder



fasstr 0.2.0
=========================

Updated: 9 April 2018

### NEW FEATURES
  * Summarize and plot by multiple groups (ex. stations) 
  * Selecting columns of dates, values, and groupings (ex. station numbers) when using 'data' argument
  * Exported data frames are tibbles
  * Exported plots ('gg' class) are within lists (multiple plots are produced with many plot_ functions)
  * Updated plot formatting
  * More functions utilizing rolling average days
  * More filtering arguments for some functions (months, complete_years)
  * 'na' argument is now 'ignore_missing' with default of FALSE for all functions
  * Additional write_, calc_, add_, compute_, and plot_ functions
  * Renaming of some functions
  * Exports pipe operator, %>%, to facilitate tidy analyses
  
### REMOVED FEATURES
  * Choice for writing within functions - moved to their own write_ data and plot functions

### INTERNAL IMPROVEMENTS
  * Reformatted internal structure, including adding internal functions for efficiency and argument checks
  * Used @Inheritparams to simplify documentation
 


fasstr 0.1.0
=========================

Updated: 9 Dec 2017

* Initial package beta version
* Use HYDAT argument to extract HYDAT data using 'tidyhydat' package
* Included add_, calc_, compute_, and plot_functions
* Included README with basic examples
