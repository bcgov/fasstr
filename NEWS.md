fasstr 0.2.5
=========================

##### Updated: 18 May 2018

  * Added vignettes:
    * 'fasstr' Users Guide
    * Trending Analysis Guide
    * Volume Frequency Analysis Guide
  * Updated documentation for some functions


fasstr 0.2.4
=========================

##### Updated: 8 May 2018

  * Fixed bug where groups function did not work if not "STATION_NUMBER"
  * Added warning if not all dates are dates in column
  * Added references in annual_flow_timing() and trending functions
  * Added 'See Also' documentation for many related functions
  * Removed error from daily and monthly cumulative stats/plots with no basin area (now produces NA)
  * Updated write_full_analysis so no section 7 is completed with insufficient data
  

fasstr 0.2.3
=========================

##### Updated: 17 April 2018

  * Updated write_full_analysis() documentation

fasstr 0.2.2
=========================

##### Updated: 17 April 2018

  * Added write_full_analysis() function to write almost almost all plots and tables in a folder
  * Added some interal checks functions
  

fasstr 0.2.1
=========================

##### Updated: 13 April 2018

  * Reformatted examples script in testing folder as a temporary help document until a vignette is built
  * Moved the previous examples script to a new testing script in the same folder



fasstr 0.2.0
=========================

##### Updated: 9 April 2018

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

##### Updated: 9 Dec 2017

* Initial package beta version
* Use HYDAT argument to extract HYDAT data using 'tidyhydat' package
* Included add_, calc_, compute_, and plot_functions
* Included README with basic examples