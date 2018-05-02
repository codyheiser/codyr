#========================= GGPLOT2 CONFIG =========================#
# The following functions are for preferences and common settings  
#   for use with ggplot2 visualization package

# set up your workspace
require('outliers')

# linear model for regressions
lm_eqn <- function(df,x,y){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

# normalize each value in df to the median for its row. return molten dataframe.
row.norm <- function(matrix, id.vars=c('Sample'), norm.to = 'median', trans = ''){
  # matrix = dataframe to normalize values in
  # id.vars = vector of column names to ignore when normalizing
  # norm.to = summary statistic to normalize to. ('median', 'mean', 'sum', etc.)
  # trans = transformation to perform on normalized fraction. 'log2', 'log', 'log10', etc. Default is none.
  
  # calculate the normalizer for each row for all columns
  matrix$normalizer <- apply(matrix[-which(names(matrix) %in% id.vars)], # ignore any ID variables and normalize over desired values only
                             FUN = function(x){eval(parse(text = paste0(norm.to,'(as.numeric(x), na.rm = T)')))},
                             MARGIN = 1)
  melt <- melt(matrix, id.vars = c(unlist(id.vars), 'normalizer')) # get values into one column
  melt$norm <- eval(parse(text = paste0('unlist(',trans,'(melt$value/melt$normalizer))'))) # calculate normalized value for each column based on normalizer value
  return(melt)
}

# normalize each value in df to a global normalizer. return molten dataframe.
global.norm <- function(matrix, id.vars=c('Sample'), norm.to = 'median', trans = ''){
  # matrix = dataframe to normalize values in
  # id.vars = vector of column names to ignore when normalizing
  # norm.to = summary statistic to normalize to. ('median', 'mean', 'sum', etc.)
  # trans = transformation to perform on normalized fraction. ('log2', 'log', 'log10', etc.) Default is none.
  
  melt <- melt(matrix, id.vars = id.vars) # get values into one column
  normalizer <- eval(parse(text = paste0(norm.to,'(melt$value, na.rm = T)'))) # calculate normalized value for all rows and all columns
  melt$norm <- eval(parse(text = paste0('unlist(',trans,'(melt$value/normalizer))'))) # calculate normalized value for each column based on normalizer value
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
