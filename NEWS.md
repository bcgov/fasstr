fasstr 0.2.0
=========================

##### Updated on: 9 April 2018

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

##### Updated on: 9 Dec 2017

* Initial package beta version
* Use HYDAT argument to extract HYDAT data using 'tidyhydat' package
* Included add_, calc_, compute_, and plot_functions
* Included README with basic examples