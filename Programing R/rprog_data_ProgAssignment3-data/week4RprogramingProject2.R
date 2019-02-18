
rankhospital <- function(state,outcome,num = "best") {
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
      b <- df[order(df[,outcome],df[,1]),]
      if (num == "worst"){
        return(b[nrow(b),1])
      } else if ( num == "best") {
        return(b[1,1]) 
      } else if (num <nrow(b)){
        return (b[num,1])
      } else {
        return(NA)
      }
    }
    else{ print("Error: invalid outcome")}  
  }
  else {
    print("Erro: invalid state")
  }
}
rankhospital("NY", "heart attack", 7)
