fasstr 0.3.1
=========================

## Resubmission

This is a resubmission (initial submission 2019-12-12). In this version I have:

* Shortened the title to less than 65 characters
* Added a link the Water Survey of Canada database (removed short-form name) in the Description
* Added references to methods in the description field in the DESCRIPTION file
* Replaced "Apache License (==2.0)" with "Apache License 2.0" in the DESCRIPTION file, and removed the "| file LICENSE" text
* Replaced \dontrun{} for most function documentation examples with an if-statement that checks for a local HYDAT (required for examples to run/test)
* Kept \dontrun{} for function documentation examples with longer run times (tests > 5sec) or functions that write files to disk


## Test environments

* win-builder (via `devtools::check_win_devel()` and `devtools::check_win_release()`)
* local Windows 10, R 3.6.1 (via R CMD check --as-cran)
* ubuntu, R 3.6.1 (travis-ci) (release)
* ubuntu, R 3.6.1 (travis-ci) (devel)
* Fedora Linux, R-devel, clang, gfortran - r-hub
* Ubuntu Linux 16.04 LTS, R-release, GCC - r-hub
* Debian Linux, R-devel, GCC - r-hub
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit - r-hub
* Windows Server 2008 R2 SP1, R-release, 32/64 bit - r-hub
* macOS 10.11 El Capitan, R-release (experimental) - r-hub


## R CMD check results
There were no ERRORs or WARNINGs or NOTES. 
