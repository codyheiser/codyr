#========================= CODY FUNCTIONS =========================#
# The following functions are for general use in loading, reshaping, 
#   visualizing, and exporting datasets.

# set up your workspace
require('reshape2')
require('plyr')
require('dplyr')
require('stringr')
require('jsonlite')
require('tools')
require('readxl')
require('ggplot2')
require('gplots')
require('plotly')

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
    name <- make.names(tail(strsplit(file_path_sans_ext(f), split = '\\/')[[1]], n = 1)) # get syntactically valid name of file alone
    print(paste0('Reading ',name))
    df <- read.default(f, ...) # read csv or Excel file into dataframe
    df$file <- name # create 'file' column that has metadata pointing to file name
    assign(name, df) # rename the df as the file ID
    vars <- append(vars, name) # add name of new df to list of variables
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

# test for normality of a dataset
norm.test <- function(v, qq = TRUE){
  # uses Shaprio-Wilk test to determine if a vector of data is normally distributed
  # log2 and log10-transforms data to see if those are normally distributed as well
  # v = vector of numerical data to test for normal distribution
  # qq = output a quantile-quantile plot against the normal distribution?
  
  results <- data.frame(transformation = c('Original','Log2-Transformed','Log10-Transformed'),shapiro.results = c('Undet.','Undet.','Undet.'),
                        shapiro.pval = c(shapiro.test(v)$p.value, shapiro.test(log2(v))$p.value, shapiro.test(log10(v))$p.value))
  # determine if these distributions and transformed distributions are normal
  results$shapiro.results <- unlist(lapply(results$shapiro.pval, FUN = function(x){
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
  
}

# test for and remove outliers in a dataset
outlier.test <- function(v){
  # uses Grubbs test to iteratively identify outliers in a vector of data
  # v = vector of numerical data to test for outliers
  
  ref <- 1 # initialize reference for checking vector length. start at 1 to get into loop
  
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
      print(grubb$alternative) # prints outlier value to console
      v <- v[v != as.numeric(strsplit(grubb$alternative, '\\ ')[[1]][3])] # remove outlier from vector
      
      # check to make sure you didn't mess the vector up
      if(length(v) < ref - 1){
        print('Warning: Removed more than one element with the same outlier value')
      }
    }
  }
  
  return(v) # return the vector
  
}


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
