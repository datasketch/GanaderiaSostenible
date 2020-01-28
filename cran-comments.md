## Test environments
* local Ubuntu 18. R version 3.6.2 (2019-12-12)
* Windows with devtools::check_win_*
* rhub

## R CMD check results
There were no ERRORs, WARNINGs or NOTES on ubuntu

On windows devel. It seems that it cannot find the shinypanels package, which is new but I have been able to install it with install.packages("shinypanels")



New submission
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: 'shinypanels'

See section 'The DESCRIPTION file' in the 'Writing R Extensions'
manual.
* DONE
Status: 1 ERROR, 1 NOTE




## Reverse dependencies

This is a new release, so there are no reverse dependencies.

