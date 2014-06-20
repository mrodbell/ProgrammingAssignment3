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
  
  ## build list of states to be checked (and put in alphabetical order)
  statelist <- unique(data_set$State)
  ordstlist <- order(statelist)
  statelist <- statelist[ordstlist]
  
  return_data <- data.frame()
 
  for (state in statelist) {
    ## pull the state info
    ## grab subset based on state
    state_data <- subset( data_set, State==state )
    
    ## sort the data within the state
    ordst <- order(state_data[[death_rate_column_name]],state_data$Hospital.Name)
    sorted_data <- state_data[ordst,]   
    
    if (num == "best" ) {
      hospital <- sorted_data[1,]$Hospital.Name 
    } else if (num == "worst") {
      ## get index of last item
      index <- nrow( sorted_data )
      hospital <- sorted_data[index,]$Hospital.Name 
    } else {
      index <- nrow( sorted_data )
      if (num > index) {
        hospital <- NA
      } else {
        hospital <- sorted_data[index,]$Hospital.Name 
      }
    }
    row <- c(hospital, state)
    return_data <- rbind(return_data, row)
  }
  return(return_data)
}
