PercentageOfAboveAverageHospitalsPerState <- function(outcome) {
  options(warn=-1)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(outcome == "heart attack"||outcome == "Heart Attack"){outcome = 11}
  if(outcome == "heart failure"||outcome == "Heart Failure"){outcome = 17}
  if(outcome == "pneumonia"||outcome == "Pneumonia"){outcome = 23}
  States <- unique(data[, "State"])
  data <- data[!is.na(as.numeric(data[, outcome])), ]
  data[, outcome] <- as.numeric(data[, outcome])
  TotalValidHospitals = as.numeric(nrow(data))
  NatAverage = sum(data[, outcome])
  NatAverage = NatAverage/TotalValidHospitals
  if(outcome==11){AboveAverage <- subset(data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack < NatAverage)}
  if(outcome==17){AboveAverage <- subset(data, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure < NatAverage)}
  if(outcome==23){AboveAverage <- subset(data, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia < NatAverage)}
  States <- States[sort.list(States)]
  output <- c()
  for(state in States){
    HospitalsInState <- subset(data, State == state)
    AboveAvgInState = subset(AboveAverage, State == state)
    output <- c(output, as.numeric((as.numeric(nrow(AboveAvgInState))/as.numeric(nrow(HospitalsInState)))*100))
  }
  return(data.frame(States, output))
}