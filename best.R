best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  x <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  validOutComeName <- c("heart attack", "heart failure", "pneumonia")
  
 #extracted the data i need from the whole bunch of data
  
   y   <- as.data.frame(cbind(x[, 2],   # hospital Name
                              x[, 7],   # State code
                              x[, 11],  # For heart attack
                              x[, 17],  # For heart failure
                              x[, 23]), # For pneumonia
                        stringsAsFactors = FALSE)
  colnames(y) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  # check if outcome is valid
  
  if(!outcome %in% validOutComeName ){
    stop("invalid outcome")
  }
  else if(!state %in% y[, "state"]){
    stop("invalid state")
  }
  else {
    z <- y[y$state == state, ]    # filter rows for called states
    h <- as.numeric(z[, eval(outcome)])
    min_val <- min(h, na.rm = TRUE)
    result  <- z[, "hospital"][which(h == min_val)]
    output  <- result[order(result)]
  }
  output
}
