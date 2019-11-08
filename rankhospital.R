rankhospital <- function(state, outcome, rank = "best"){
  
  ## Read csv file
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
  else if (is.numeric(rank)) {
    z <- y[y$state == state, ]    # filter rows for called states

    z[, eval(outcome)] <- as.numeric(z[, eval(outcome)])
    z <- z[order(z[, eval(outcome)], z[, "hospital"]), ]
    output <- z[, "hospital"][rank]
  } 
  # rank is not numeric
  else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      z <- y[y$state == state, ]   
      z[, eval(outcome)] <- as.numeric(z[, eval(outcome)])
      z <- z[order(z[, eval(outcome)], z[, "hospital"], decreasing = TRUE), ]
      output <- z[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  output
}