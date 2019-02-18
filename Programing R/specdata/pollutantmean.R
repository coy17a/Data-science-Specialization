library(readr)
d <- c("specdata")
p <- c("nitrate")
id <- 34

pollutantmean <- function(directory, pollutant, id =1:332) {
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
  pdata <- dataF[,pollutant]
  data <- rbind(data,pdata)
  pmean <- unlist(lapply(data, mean, na.rm = TRUE))
}
return(pmean)
}

z <- pollutantmean(d,p)
