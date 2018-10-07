#========================= CODY FUNCTIONS =========================#
# The following functions are for general use in loading, reshaping,
#   manipulating, and exporting datasets.

# Set up your workspace:
# general packages
require('reshape2')
require('plyr')
require('dplyr')
require('stringr')
require('magrittr')
require('tidyr')
require('jsonlite')
require('tools')
require('readxl')
# other source files
source('stats.r')
source('ggplot_config.r')
# working directory tracking using 'here'
require('here')

# read in .csv or .xlsx file
read.default <- function(file, ...){
  # file = Path and filename plus extension. May be .csv or .xlsx
  # sheet = for Excel files, name or index of sheet to read

  # timer
  ptm <- proc.time()

  if(file_ext(file) == 'csv'){
    df <- read.csv(file, check.names = T, stringsAsFactors = F, ...)
  }else{if(file_ext(file) == 'txt'){
    df <- read.table(file, header = T, sep = '\t', ...)
  }else{if(file_ext(file) %in% c('xls', 'xlsx')){
    df <- read_excel(file, ...)
  }else{
    df <- NA
  }}}
  print(proc.time() - ptm) # see how long this took
  return(df)
}

# read in all files of common type from folder, concatenate by row
read.all <- function(filetype, dir = '.', ...){
  # filetype = name of extension to read ('csv', 'xls', or 'xlsx'). you can also use globs (e.g. 'myfile*.csv')
  # dir = directory to read files from

  # timer
  ptm <- proc.time()

  vars <- list() # initiate empty list for later concatenation of dfs
  for(f in list.files(path = dir, pattern = glob2rx(filetype), recursive = T)){
    if(str_detect(f, regex('\\/\\~\\$'))){
      # ignore files that are open by Windows
    }else{
      name <- make.names(f) # get syntactically valid name of file
      print(paste0('Reading ',name))
      df <- read.default(file_path_as_absolute(paste0(dir,f)), ...) # read csv or Excel file into dataframe
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

# split a column of strings from a data.frame to two columns by a delimiter
columnsplit <- function(df, clmn, newnames = c('name1', 'name2'), drop.orig = F, ...){
  # df = data.frame to operate on
  # clmn = name of column in df to split
  # newnames = vector containing new column names, in order
  # drop.orig = remove original clmn from data.frame?
  # ... = options to pass to vectorsplit function; specifically "delimiter = '\\_'"
  eval(parse(text = paste0('df$',newnames[1],'<-vectorsplit(df$',clmn,',...,keep=1)'))) # put string from before delim into column with name1
  eval(parse(text = paste0('df$',newnames[2],'<-vectorsplit(df$',clmn,',...,keep=2)'))) # put string from after delim into column with name2
  if(drop.orig){
    eval(parse(text = paste0('df<-subset(df, select = -',clmn,')'))) # drop original column from df if drop.orig flag set to TRUE
  }
  return(df)
}

# conditionally mutate rows of dataframe as part of dplyr::mutate function
mutate_cond <- function(.data, condition, ..., envir = parent.frame()){
  # This function performs a __mutate__ operation on a subset of rows of a dataframe without the need for __filter__ or __group_by__
  # and returns the output in place. Because of this, you cannot create new columns within this function as you can using normal
  # __dplyr::mutate__. Instead, ensure output columns are already initialized in the dataframe.
  condition <- eval(substitute(condition), .data, envir)
  .data[condition,] %>%
    mutate(...) -> .data[condition,]
  return(.data)
}
