## codyr
#### useful toolkit in R
To source all functions, do the following:
1. If using this repository for the first time, run `requirements.r`.  
This will ensure that all required R packages are installed in the current environment.
2. Source the toolkit with the following command in your R console:  
`source('path/to/repo/codyr/utilityfunctions.r', chdir = T)`  
**Note:** `chdir` flag is important, as `utilityfunctions.r` sources all other files using relative paths

##### `utilityfunctions.r`
* Contains general functions for reading, shaping, and simple data manipulations
* Sources other scripts from repository

##### `stats.r`
* Contains functions for statistical analysis including:
 * Normalization operations
 * Population tests for distribution and outliers

##### `ggplot_config.r`
* Contains color palettes and plot options for `ggplot2`  
To make "pretty" figures, simply add `plot.opts` to any `ggplot` object:  
`ggplot(data = mtcars, aes(x = wt, y = mpg)) +  
geom_point() +  
plot.opts`
