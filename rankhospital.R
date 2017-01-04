rankhospital <- function(state, outcome, rank = "best"){
  options(warn=-1)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  StateChecker <- unique(data[, "State"])
  if(!(state %in% StateChecker)){stop("Invalid State!")}
  if(rank == "best"||rank == "Best"){rank = 1}
  if(outcome == "heart attack"||outcome == "Heart Attack"){outcome = 11}
  if(outcome == "heart failure"||outcome == "Heart Failure"){outcome = 17}
  if(outcome == "pneumonia"||outcome == "Pneumonia"){outcome = 23}
  if(!is.numeric(outcome)){stop("Invalid Outcome!")}
  x <- subset(data, State == state)
  sentVal = FALSE
  x <- x[!is.na(as.numeric(x[, outcome])), ]
  if(rank == "worst"||rank == "Worst"){
    sentVal = TRUE
    rank = 1
  }
  if (rank > nrow(x)){
    return(NA)
  }
  x <- x[order(x[, "Hospital.Name"]), ]
  x <- x[order(as.numeric(x[, outcome]), decreasing = sentVal), ]
  return (x[rank, "Hospital.Name"])
}