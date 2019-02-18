library(readr)
d <- c("specdata")
p <- c("sulfate")
treshold <-150


corr <- function(directory, cases = 0,id=1:332) {
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
    if(nrow(dataF) > cases){
      rel <- cor(dataF$sulfate,dataF$nitrate)
      data <- c(data,rel)
    }
    
  }
  return(data)
}

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
