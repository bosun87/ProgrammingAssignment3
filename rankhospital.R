library(dplyr)

rankhospital <- function(state,outcome,num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that state and outcome are valid
  
  disease <- c("heart attack","heart failure","pneumonia")
  position <- c(11,17,23)
  
  if(!is.element(state,data$State)) {
    
    stop("invalid state")
  }
  
  if(!is.element(outcome,disease)) {
    
    stop("invalid outcome")
  }
  
  index <- which(outcome==disease)[[1]]
  col <- position[index]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data[,col] <- as.numeric(data[,col])
  good <- !is.na(data[,col])
  data <- data[good,]
  data <- data %>%
    filter(State == state)
  
  result <- data.frame(name = data[,2],rate = data[,col])
  
  result <- arrange(result,rate,name)
  
  rows <- nrow(result)
  
  if(num == "best") {
    
    num <- 1
  }
  
  else if(num == "worst") {
    
    num <- rows
  }
  
  if(num > rows) {
    
    return(NA)
  }
  
  result[,1][num]
  
  
  
}