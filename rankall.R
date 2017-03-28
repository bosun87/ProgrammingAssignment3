library(dplyr)

rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that outcome are valid
  
  disease <- c("heart attack","heart failure","pneumonia")
  position <- c(11,17,23)
  
  if(!is.element(outcome,disease)) {
    
    stop("invalid outcome")
  }
  
  index <- which(outcome==disease)[[1]]
  col <- position[index]
  
  ## For each state, find the hospital of the given rank
  s <- split(data,data$State)
  
  res_name <- vector(mode = "character",length = 0)
  res_state <- vector(mode = "character", length = 0)
  
  for( i in 1:length(s)) {
    
     rank <- num
    
     data <- s[[i]]
     res_state <- c(res_state,data$State[1])
     data[,col] <- as.numeric(data[,col])
     good <- !is.na(data[,col])
     data <- data[good,]
     result <- data.frame(name = data[,2],rate = data[,col],stringsAsFactors = FALSE)
     
     result <- arrange(result,rate,name)
     
     rows <- nrow(result)
     
     if(rank == "best") {
       
       rank <- 1
     }
     
     else if(rank == "worst") {
       
       rank <- rows
     }
     
     if(rank > rows) {
       
       res_name <- c(res_name,NA)
     }
     
     else {
       
       res_name <- c(res_name,result[,1][rank])
     }
    
  }
  
  matrix <- data.frame(name = res_name,state = res_state,stringsAsFactors = FALSE)
  
}