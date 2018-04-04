# Run this script before sourcing any files from this repository
# Checks for required packages and installs missing packages 

# packages loaded by utilityfunctions.r, stats.r, and ggplot_config.r
reqd.packages <- c('tools','readxl','reshape2','plyr','dplyr','stringr','magrittr','tidyr','jsonlite','outliers','plotly')

# get list of packages that are not installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if any missing packages, install them
if(length(new.packages)){install.packages(new.packages)}
