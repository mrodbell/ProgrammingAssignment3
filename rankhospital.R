##
##  Function: rankhospital
##
##  Purpose: rank hospitals
##
rankhospital <- function(state, outcome, num = "best") {
  
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  ## Read outcome data
  data_set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid (check to see if any records use state)
  if (sum(data_set$State==state)==0) {
    stop("invalid state")
  }
  ## capitalize outcome
  outcome <- sapply(outcome, .simpleCap)
  ## replace spaces with periods
  dot_outcome <- gsub(" ", ".", outcome)
  
  ## check for outcome in column name
  matches <- 0
  for (i in 1:ncol(data_set)) {
    if (grepl(dot_outcome, colnames( data_set[i]))) { matches <- matches + 1}
  }
  if (matches == 0) {
    stop("invalid outcome")
  }
  
  ## build column name
  death_rate_column_name <- paste0("Hospital.30.Day.Death..Mortality..Rates.from.", dot_outcome )
  ## grab subset based on state
  state_data <- subset( data_set, State==state )
  
  ## clean up and use only legit data
  state_data[[death_rate_column_name]] <- as.numeric(state_data[[death_rate_column_name]])
  
  good <- !is.na(state_data[[death_rate_column_name]])
  ## remove invalid data
  state_data <- subset(state_data, good)
  
  ## get the ordering
  ordst <- order(state_data[[death_rate_column_name]],state_data$Hospital.Name)
  sorted_data <- state_data[ordst,]  
  
  if (num == "best" ) {
    return( sorted_data[1,]$Hospital.Name )
  } else if (num == "worst") {
    ## get index of last item
    index <- nrow( sorted_data )
    return( sorted_data[index,]$Hospital.Name )
  } else {
    index <- nrow( sorted_data )
    if (num > index) {
      return(NA)
    } else {
      return( sorted_data[num,]$Hospital.Name )
    }
  }
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
