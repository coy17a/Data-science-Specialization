library(readr)

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

best <- function(state,outcome) {
outcomes <- c("heart attack","heart failure","pneumonia")
data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data <- data[,c(2,7,11,17,23)]
names(data) <- c("name","state",outcomes)
df<- data[(data[,2]== state),]
if (nrow(df) > 0){ 
  if (outcome %in% outcomes){
   df[,outcome] <- as.numeric(df[,outcome])
   dfo <- df[,outcome]
   df <- df[!(dfo == "Not Available" | is.na(dfo)),]
   b <- df[df[,outcome]== min(df[,outcome]),1]
   return (b)
  }
  else{ print("Error: invalid outcome")}  
  }
 else {
  print("Erro: invalid state")
}
}

best("AK", "pneumonia")

