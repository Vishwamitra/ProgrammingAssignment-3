rankall <- function(outcome, num = "best"){
 
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
  
  # convert outcomes to nemeric
  y[, eval(outcome)] <- as.numeric(y[, eval(outcome)])
  
  sorted  <- list()
  
  ## Check that state and outcome are valid
  
  if (!outcome %in% validOutComeName){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    
    by_state <- with(y, split(y, state))
    
    for (i in seq_along(by_state)){
      by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                           by_state[[i]][, "hospital"]), ]
      sorted[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
    }
    result <- do.call(rbind, sorted)
    
    output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      by_state <- with(y, split(y, state))
      
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"]), ]
        sorted[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, sorted)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else if (num == "worst") {
      by_state <- with(y, split(y, state))
      
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"], 
                                             decreasing = TRUE), ]
        sorted[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, sorted)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else {
      stop('invalid num')
    }
  }
  output
}