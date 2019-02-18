
rankall <- function(outcome,num = "best") {
  outcomes <- c("heart attack","heart failure","pneumonia")
  data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[,c(2,7,11,17,23)]
  names(data) <- c("name","state",outcomes)
  if (outcome %in% outcomes){
    data[,outcome] <- as.numeric(data[,outcome])
    dfo <- data[,outcome]
    data <- data[!(dfo == "Not Available" | is.na(dfo)),]
    data <- data[order(data[,outcome],data[,1]),]
    df <- split(data,data["state"])
    b <- data.frame(state=character(), hospital = character())
    for (s in df){
      r <- data.frame(state=character(), hospital = character())
      if (num == "worst"){
        x <- s[["state"]][nrow(s)]
        y <- s[["name"]][nrow(s)]
        b <- rbind(b,data.frame(state=x , hospital= y))
      } else if ( num == "best") {
        x<- s[["state"]][1]
        y<- s[["name"]][1]
        b <- rbind(b,data.frame(state=x , hospital= y))
        
      } else if (num < nrow(s)){
        x <- s[["state"]][num]
       y <- s[["name"]][num]
        b <- rbind(b,data.frame(state=x , hospital= y))
      } else {
        x<- s[["state"]][1]
        y <- NA
        b <- rbind(b,data.frame(state=x , hospital= y))
      }    
    }
    return(b)
  }
  else{ print("Invalid outcome")}  
  
}

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)