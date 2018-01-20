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
read.default <- function(file, sheet = 1){
  # file = Path and filename plus extension. May be .csv or .xlsx
  # sheet = for Excel files, name or index of sheet to read
  
  # timer
  ptm <- proc.time()
  
  if(file_ext(file) == 'csv'){
    # read file with StringsAsFactors == F to get raw vectors
    print(ptm - proc.time()) # see how long this took
    return(read.csv(file, check.names = T, stringsAsFactors = F))
  }else{if(file_ext(file) == 'xlsx'){
    print(ptm - proc.time()) # see how long this took
    return(read_excel(file, sheet))
  }else{
    print(ptm - proc.time()) # see how long this took
    return(NA)
  }}
}

# read in all files of common type from folder, concatenate by row
read.all <- function(filetype, dir = '.', recurse = T){
  # filetype = name of extension to read ('csv', 'xls', or 'xlsx')
  # dir = directory to read files from 
  # recurse = read from all subdirectories?
  
  # timer
  ptm <- proc.time()
  
  vars <- list() # initiate empty list for later concatenation of dfs
  for(f in list.files(path = dir, pattern = filetype, full.names = T, recursive = recurse)){
    df <- read.default(f) # read csv file or first sheet of Excel file into dataframe
    name <- tail(strsplit(file_path_sans_ext(f), split = '\\/')[[1]], n = 1) # get name of file alone
    df$file <- name # create 'file' column that has metadata pointing to file name
    assign(name, df) # rename the df as the file ID
    vars <- append(vars, name) # add name of new df to list of variables
  }
  
  concatframe <- do.call(rbind.fill, args = eval(parse(text=paste0("list(",paste(unlist(vars),collapse=","),")"))))
  # see how long this took:
  print(proc.time() - ptm)
  return(concatframe)
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
