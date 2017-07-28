## NWK 7/25/2017
#Debugging
library(testit)


metric_filename <- "data/counts-raw.txt.gz"
raw_counts <- read.delim(metric_filename,stringsAsFactors = T)


mean_metric_per_var <- function(metric, variable) {
  unique_variable <- unique(variable)
  result <- numeric(length = length(unique_variable))
  if(anyNA(metric)){
    warning("Look out! metric has NA values!")
  }
  #print(result)
  names(result) <- as.character(unique_variable)
  for (v in unique_variable) {
    result[as.character(v)] <- mean(metric[variable == v],na.rm=T)
  }
  return(result)
}

debug(mean_metric_per_var)
undebug(mean_metric_per_var)
x <-mean_metric_per_var(metric = raw_counts$backtweetsCount,
                    variable=raw_counts$year)


x <-mean_metric_per_var(metric = raw_counts$backtweetsCount[raw_counts$journal %in% c("pbio","pone")],
                        variable=raw_counts$journal[raw_counts$journal %in% c("pbio","pone")])
x
pbio_pone <-raw_counts$journal[raw_counts$journal %in% c("pbio","pone")]

debug(mean_metric_per_var)
mean_metric_per_var(metric = raw_counts$facebookLikeCount,
                        variable=raw_counts$year)


calc_sum_stat <- function(df, cols) {
  stopifnot(all(dim(df) > 0))
  stopifnot(is.character(cols))
  
  df_sub <- df[, cols,drop=F]
  stopifnot(is.data.frame(df_sub))
  sum_stat <- apply(df_sub, 1, mean)
  stopifnot(!anyNA(sum_stat))
  return(sum_stat)
}


fb_df <- raw_counts[,grepl("facebook",colnames(raw_counts))]
fb_df <- select(raw_counts,contains("facebook"))
dim(fb_df)
fb_sum_rows <- apply(fb_df,1,sum)
fb_sum_cols <- apply(fb_df,2,sum)

fb_mean_rows <- calc_sum_stat(raw_counts,1:4)

#options(error=recover)

mendeley_mean_rows <- calc_sum_stat(raw_counts,
                              "mendeleyReadersCount")
all.equal(mendeley_mean_rows,
          raw_counts$mendeleyReadersCount)
x <-1:5
str(raw_counts[,x,drop=F])

# Defensive Programming 

## stopifnot

x <- 1
if (!is.character(x)){
  stop("x must be a character vector")
}

stopifnot(is.character(x), length(x) == 1)

mean_metric_per_var <- function(metric, variable) {
  stopifnot(length(metric) == length(variable), 
            is.numeric(metric))
  unique_variable <- unique(variable)
  result <- numeric(length = length(unique_variable))
  if(anyNA(metric)){
    warning("Look out! metric has NA values!")
  }
  names(result) <- as.character(unique_variable)
  for (v in unique_variable) {
    result[as.character(v)] <- mean(metric[variable == v],na.rm=T)
    stopifnot(!is.na(result[as.character(v)]))
  }
  return(result)
}

mean_metric_per_var(raw_counts$backtweetsCount, raw_counts$year[1:20])


## testit

assert("one equals one", 1 == 1)
assert("5 + 7 = 13", 5 + 7 == 13)

assert("These are true statements", 0 == 0, 0 == 2, 3 == 3)

has_warning(1 + 1)

has_warning(1:2 + 1:3)


assert("Throws error", has_error(1 + "a"))

# Testing : empty data frame
assert("Testing Empty Data Frame" , has_error(calc_sum_stat(data.frame(), c("wosCountThru2011", "f1000factor"))))

# Testing : non-character vector
assert("Non-Character Input for columns throws error" , has_error(calc_sum_stat(raw_counts, c(1, 2))))


# Testing NA-input
assert("NA Input does not result in NA-output", !anyNA(calc_sum_stat(raw_counts, c("wosCountThru2011", "facebookLikeCount"))))


my_mean <- function(x){
  stopifnot(length(x) > 0)
  result <- sum(x) / length(x)
  return(result)
}

## Unit Tests for my_mean

### Test : Actually calculates the mean

x <- c(0,1,2)
assert("Correctly Estimates the Mean", 
       my_mean(x) == 1)

assert("Empty vector throws an error", 
       has_error(my_mean(c())))

assert("Character vector throws an error", 
       has_error(my_mean(c("a","b", "c"))))

