#========================= CODY FUNCTIONS =========================#
# The following functions are for general use in loading, reshaping, 
#   visualizing, and exporting datasets.

# set up your workspace
require('reshape2')
require('plyr')
require('dplyr')
require('stringr')
require('magrittr')
require('jsonlite')
require('tools')
require('readxl')
require('ggplot2')
require('gplots')
require('plotly')
require('outliers')
require('here')

# read in .csv or .xlsx file
read.default <- function(file, ...){
  # file = Path and filename plus extension. May be .csv or .xlsx
  # sheet = for Excel files, name or index of sheet to read
  
  # timer
  ptm <- proc.time()
  
  if(file_ext(file) == 'csv'){
    df <- read.csv(file, check.names = T, stringsAsFactors = F, ...)
  }else{if(file_ext(file) %in% c('xls', 'xlsx')){
    df <- read_excel(file, ...)
  }else{
    df <- NA
  }}
  print(proc.time() - ptm) # see how long this took
  return(df)
}

# read in all files of common type from folder, concatenate by row
read.all <- function(filetype, dir = '.', ...){
  # filetype = name of extension to read ('csv', 'xls', or 'xlsx')
  # dir = directory to read files from 
  
  # timer
  ptm <- proc.time()
  
  vars <- list() # initiate empty list for later concatenation of dfs
  for(f in list.files(path = dir, pattern = filetype, full.names = T, recursive = T)){
    if(str_detect(f, regex('\\/\\~\\$')) && str_detect(f, filetype)){
      # ignore files that are open by Windows
    }else{
      name <- make.names(f) # get syntactically valid name of file
      print(paste0('Reading ',name))
      df <- read.default(f, ...) # read csv or Excel file into dataframe
      df$file <- name # create 'file' column that has metadata pointing to file name
      assign(name, df) # rename the df as the file ID
      vars <- append(vars, name) # add name of new df to list of variables
    }
  }
  # concatenate rows of all dfs
  combined <- eval(parse(text = paste0('do.call(rbind.fill, args = list(',paste(vars,collapse = ','),'))')))
  print(proc.time() - ptm) # see how long this took
  return(combined)
}

# read .csv file from line that matches string
read.from <- function(file, start.str, end.str = NA, offset = 0, ...){
  # file = Path and filename plus extension. May be .csv file.
  # start.str = Regular Expression contained by line to start reading from
  # end.str = Regular Expression contained by line to end reading
  # offset = number of lines from start.str to beginning of data
  #   e.g., if your file looks like below, you could have start.str = 'Standards,',
  #     and offset = 1 so your header row contains actual column names.
  #   Standards,,,,
  #   Name,Replicate,Well,Cts,Expected nM
  #   STD01,1,A01,18.384,20
  
  # timer
  ptm <- proc.time()
  
  skip.no <- grep(pattern = start.str, x = readLines(file)) - 1 + offset
  if(!is.na(end.str)){
    end.no <- grep(pattern = end.str, x = readLines(file))
    print(paste0('Reading ', tail(strsplit(file,'/')[[1]],n=1), ' from line ', skip.no, ' to line ', end.no))
    df <- read.default(file, skip = skip.no, nrows = end.no - skip.no + offset, ...)
  }else{
    print(paste0('Reading ', tail(strsplit(file,'/')[[1]],n=1), ' from line ', skip.no))
    df <- read.default(file, skip = skip.no, ...)
  }
  
  print(proc.time() - ptm) # see how long this took
  return(df)
}

# save ggplot as .png image
to.png <- function(plt, destination = 'plt.png', w = 8, h = 8, r = 700){
  # plt = ggplot object
  # destination = filepath, name, and extension of output
  # w, h = width, height in inches
  # r = resolution
  ggsave(filename = destination, plot = plt, width = w, height = h, units = 'in', dpi = r)
}

# convert POSIXct date-time object to total time in minutes, ignoring date
to.minutes <- function(x){
  # takes a POSIXct or POSIXt object and converts it to time in minutes
  hr.min.sec <- as.numeric(strsplit(strsplit(as.character(x), '\\ ')[[1]][2], ':')[[1]])
  minutes <- hr.min.sec[1]*60 + hr.min.sec[2] + hr.min.sec[3]/60
  return(minutes)
}

# convert time in minutes to hr:min:sec as string
from.minutes <- function(x){
  # takes a numeric value of minutes (float) and converts to hr:min:sec
  frac <- x %% 1
  sec <- round(frac * 60, 0)
  min <- (x - frac) %% 60
  hr <- (x - min - frac) / 60
  hr.min.sec <- paste0(hr,':',min,':',sec)
  return(hr.min.sec)
}

# do some quick mafs
quickmafs <- function(a, b, c){
	# a, b, c = three numeric values to do quick mafs on
	if(class(a) != 'numeric' | class(b) != 'numeric' | class(c) != 'numeric'){
		print("SKRAA BOP POP!")
	}else{
		maf1 <- a + b
		maf2 <- maf1 - c
		print(paste0(a," plus ",b," is ",maf1,". Minus ",c," is ",maf2,", QUICKMAFS!"))
	}
}

# exact string match from single, delimited string
checkstring <- function(checkstr,fullstr,delimiter = '\\|'){
  # function to check for exact string matches within a string delimited by a character of choice
  ls <- strsplit(fullstr, split = delimiter)
  return(checkstr %in% ls[[1]])
}

# split vector by delimeter, return element as vector
vectorsplit <- function(v, delim = '\\_', keep = 1){
  # v = vector of strings to split
  # delim = delimiter
  # keep = which element to return as list
  return(sapply(strsplit(as.character(v),delim), `[`, keep))
}

# enumerate replicate values in a dataframe column
enumerate <- function(df, col){
  # df = dataframe to operate on (passed as a string)
  # col = column containing values to enumerate (passed as a string) 
  
  x = eval(parse(text = df)) # convert string into evaluable expression for df
  v = as.character(eval(parse(text = paste0(df,'$',col)))) # convert strings into evaluable expression for df$col
  
  if(length(unique(v)) == length(v)){
    print(paste0('All values in ', df, '$', col, ' are unique!'))
  }
  if(length(unique(v)) == 1){
    print(paste0('All values in ', df, '$', col, ' are the same!'))
  }
  
  reps <- list() # initialize empty list of replicate values
  for(row in 1:length(v)){
    reps <- append(reps, (x %>% dplyr::ungroup() %>% dplyr::slice(1:row) %>% dplyr::count(eval(parse(text = col))) %>% dplyr::rename(var = `eval(parse(text = col))`) %>% filter(var == v[row]))$n)
  }
  return(unlist(reps))
}

# normalize each value in df to the median for its row. return molten dataframe.
row.norm <- function(matrix, id.vars=c('Sample'), strtegy = ''){
  # matrix = dataframe to normalize values in
  # id.vars = vector of column names to ignore when normalizing
  # strtegy = transformation to perform on normalized fraction. 'log2', 'log', 'log10', etc. Default is none.
  
  # calculate the median for each row for all columns
  matrix$row.median <- apply(matrix[-which(names(matrix) %in% id.vars)], # ignore any ID variables and normalize over desired values only
                             FUN = function(x){median(as.numeric(x), na.rm = T)},
                             MARGIN = 1)
  melt <- melt(matrix, id.vars = c(unlist(id.vars), 'row.median')) # get values into one column
  melt$norm <- eval(parse(text = paste0('unlist(',strtegy,'(melt$value/melt$row.median))'))) # calculate normalized value for each column based on row median
  return(melt)
}

# normalize each value in df to the global median. return molten dataframe.
global.norm <- function(matrix, id.vars=c('Sample'), strtegy = ''){
  # matrix = dataframe to normalize values in
  # id.vars = vector of column names to ignore when normalizing
  # strtegy = transformation to perform on normalized fraction. 'log2', 'log', 'log10', etc. Default is none.
  
  melt <- melt(matrix, id.vars = id.vars) # get values into one column
  global.median <- median(melt$value, na.rm = T) # calculate the median for all rows and all columns
  melt$norm <- eval(parse(text = paste0('unlist(',strtegy,'(melt$value/global.median))'))) # calculate normalized value for each column based on global median
  return(melt)
}

# test for normality of a dataset
norm.test <- function(v, qq = TRUE){
  # uses Shaprio-Wilk test to determine if a vector of data is normally distributed
  # log2 and log10-transforms data to see if those are normally distributed as well
  # v = vector of numerical data to test for normal distribution
  # qq = output a quantile-quantile plot against the normal distribution?
  
  results <- data.frame(transformation = c('Original','Log2-Transformed','Log10-Transformed'),shapiro.results = c('Undet.','Undet.','Undet.'),
                        shapiro.pval = c(shapiro.test(v)$p.value, 
                                         try_default(shapiro.test(log2(v))$p.value,default = NA,quiet = T), 
                                         try_default(shapiro.test(log10(v))$p.value,default = NA,quiet = T))
                        )
  # determine if these distributions and transformed distributions are normal
  results$shapiro.results <- unlist(lapply(results$shapiro.pval, FUN = function(x){
    if(is.na(x)){return('Not Real')}
    if(x < 0.05){return('Not Normal')} 
    if(x >= 0.05){return('Normal')}
    }))
  # output q-q plot if necessary
  if(isTRUE(qq)){
    qqnorm(v)
    qqline(v)
  }
  # print results to console
  for(x in 1:nrow(results)){
    print(paste0(results$transformation[x],' distribution is ',results$shapiro.results[x],', with a p-value of ', signif(results$shapiro.pval[x], 5)))
  }
  return(results)
}

# test for and remove outliers in a dataset
outlier.test <- function(v){
  # uses Grubbs test to iteratively identify outliers in a vector of data
  # returns a mask (logical vector) of values to keep
  # v = vector of numerical data to test for outliers
  options(digits = 20) # prevent rounding during as.numeric
  
  vhold <- v # hold on to a copy of full v for masking on
  ref <- 1 # initialize reference for checking vector length. start at 1 to get into loop
  norm <- 1 # initialize normalizer for output vector
  norm.mask <- rep(T, length(v)) # initialize logical vector of TRUEs for output
  
  while(length(v) != ref){
    # do the test
    grubb <- grubbs.test(v)
    
    if(grubb$p.value >= 0.05){
      # if there are no outliers, print message to console and return vector as-is
      
      ref <- length(v) # reference becomes full vector length
      print('There are no outliers in the dataset')
      
    }else{
      # if an outlier is detected, identify it, remove from vector, and loop
      
      ref <- length(v) # reference becomes full vector length before you remove an outlier
      v.mask <- round(v,10) != round(as.numeric(strsplit(grubb$alternative, '\\ ')[[1]][3]),10) # determine position where outlier is in current vector
      vhold.mask <- round(vhold,10) != round(as.numeric(strsplit(grubb$alternative, '\\ ')[[1]][3]),10) # determine position where outlier is in original vector
      norm.mask <- norm.mask + vhold.mask 
      print(paste0(grubb$alternative, ' at position ', which(vhold.mask == FALSE), ' in the vector')) # prints outlier value to console
      
      v <- v[v.mask] # remove outlier from vector
      # check to make sure you didn't mess the vector up
      if(length(v) < ref - 1){
        print('Warning: Removed more than one element with the same outlier value')
      }
      
      norm <- norm + 1 # iterate normalizer for output vector
    }
  }
  
  norm.mask <- norm.mask/norm # normalize aggregate logical vector to number of samples removed. TRUE + TRUE = 2 / 2 (for 1 loop) = 1 = TRUE 
  norm.mask[norm.mask != 1] <- 0
  norm.mask <- as.logical(norm.mask)
  return(norm.mask) # return the logical vector
  
}

# linear model for regressions
lm_eqn <- function(df,x,y){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

# plot multiple complete plot objects on one image
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL){
  # Multiple plot function
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if(is.null(layout)){
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if(numPlots==1){
    print(plots[[1]])
  }else{
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for(i in 1:numPlots){
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }}}

# color scale
asgn_colors <- list("primblue"="#4298cc",
                    "primorange"="#ee7624",
                    "primgray"="#435563",
                    "green"="#32d339",
                    "yellow"="#cedc00",
                    "red"="#e1261c",
                    "purple"="#74538F",
                    "teal"="#20cbd4",
                    "blue"="#3f97b5",
                    "lightgray"="#c8c9c7",
                    "darkgray"="#54585a")

# preferred plotting options
# call these by adding them (+) to a ggplot object
plot.opts <- list(
  theme_bw(),
  theme(text = element_text(colour = asgn_colors$darkgray),
        legend.text=element_text(size=9),
        axis.line = element_line(colour = asgn_colors$darkgray),
        axis.title=element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        plot.title=element_text(size=18))
)
