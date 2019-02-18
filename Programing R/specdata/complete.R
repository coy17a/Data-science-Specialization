library(readr)
d <- c("specdata")
id <- 54

complete <- function(directory, id =1:332) {
  data <- c()
  for (i in id) {
    number <- as.character(i)
    if(i < 10 ){
      filenum <- paste("/00",number,sep = "")
    } else if (i <100) {
      filenum <-  paste("/0",number,sep="")
    } else {
      filenum <- paste("/",number,sep="")
    }
    fileP <- paste("./",directory,filenum,".csv",sep="")
    dataF <- read_csv(fileP)
    dataF <- dataF[complete.cases(dataF),]
    data <- rbind(data, c(i,nrow(dataF)))
  }
  return(data)
}

z <- complete(d,id)




#dataF <- data[complete.cases(data),]
#data <- c(data,c(id,nrow(datF)))