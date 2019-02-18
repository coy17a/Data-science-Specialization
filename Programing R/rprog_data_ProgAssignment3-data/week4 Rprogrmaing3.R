
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
      b <- c()
      for (s in df){
        r <- c()
        if (num == "worst"){
        r <- c(s[["state"]][nrow(s)],s[["name"]][nrow(s)])
        b <- rbind(b,r)
        } else if ( num == "best") {
          r <- c(s[["state"]][1],s[["name"]][1])
          b <- rbind(b,r)
        
        } else if (num < nrow(s)){
          r <- c(s[["state"]][num],s[["name"]][num])
          b <- rbind(b,r)
        } else {
          r <- c(s[["state"]][1],NA)
          b <- rbind(b,r)
        }    
      }
      return(b)
    }
    else{ print("Invalid outcome")}  

}

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
