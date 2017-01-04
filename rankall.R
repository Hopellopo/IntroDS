rankall <- function(outcome, rank = "best") {
  options(warn=-1)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(rank == "best"||rank == "Best"){rank = 1}
  if(outcome == "heart attack"||outcome == "Heart Attack"){outcome = 11}
  if(outcome == "heart failure"||outcome == "Heart Failure"){outcome = 17}
  if(outcome == "pneumonia"||outcome == "Pneumonia"){outcome = 23}
  if(!is.numeric(outcome)){stop("Invalid Outcome!")}
  States <- unique(data[, "State"])
  States <- States[sort.list(States)]
  output <- c()
  rankIsWorst = FALSE
  if(rank == "worst"||rank == "Worst"||rank == -1){rankIsWorst = TRUE}
  for(state in States){
    x <- subset(data, State == state)
    x <- x[!is.na(as.numeric(x[, outcome])), ]
    x <- x[order(as.numeric(x[, outcome])), ]
    if(rankIsWorst){rank = nrow(x)}
    if(rank > nrow(x)){
      output <- c(output, NA)
    }
    else{
      output <- c(output, x[rank, "Hospital.Name"])
    }  
  }
  return(data.frame(States, output))
}