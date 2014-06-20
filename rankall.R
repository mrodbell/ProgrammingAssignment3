rankall <- function(outcome, num = "best") {
  
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  ## Read outcome data
  data_set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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

  ## clean up and use only legit data
  data_set[[death_rate_column_name]] <- as.numeric(data_set[[death_rate_column_name]])
  
  good <- !is.na(data_set[[death_rate_column_name]])
  ## remove invalid data
  data_set <- subset(data_set, good)
  
  ## build list of states to be checked
  statelist <- unique(data_set$State)
  
  ## initialize data frame for results
  df <- data.frame(0,2)
  names(df) <- c("Hospital.Name","State")
  df_ix <- 1
  
  for (state in statelist) {
    ## pull the state info
    ## grab subset based on state
    state_data <- subset( data_set, State==state )
    
    ## get the ordering
    ordst <- order(state_data[[death_rate_column_name]],state_data$Hospital.Name)
    sorted_data <- state_data[ordst,]   
    
    if (num == "best" ) {
      df[df_ix]$Hospital.Name <- sorted_data[1,]$Hospital.Name 
    } else if (num == "worst") {
      ## get index of last item
      index <- nrow( sorted_data )
      df[df_ix]$Hospital.Name <- sorted_data[index,]$Hospital.Name 
    } else {
      index <- nrow( sorted_data )
      if (num > index) {
        df[df_ix]$Hospital.Name <- NA
      } else {
        df[df_ix]$Hospital.Name <- sorted_data[index,]$Hospital.Name 
      }
    }
    df[df_ix]$State <- state
    df_ix <- df_ix + 1
  }
  return(df)
}
