best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  StateChecker <- unique(data[, "State"])
  if(!(state %in% StateChecker)){stop("Invalid State!")}
  if(outcome == "heart attack"||outcome == "Heart Attack"){outcome = 11}
  if(outcome == "heart failure"||outcome == "Heart Failure"){outcome = 17}
  if(outcome == "pneumonia"||outcome == "Pneumonia"){outcome = 23}
  if(!is.numeric(outcome)){stop("Invalid Outcome!")}
  x <- subset(data, State == state)
  x <- x[order(as.numeric(x[, outcome])), ]
  return (x[1, "Hospital.Name"])
}