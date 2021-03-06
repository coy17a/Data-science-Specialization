library(data.table)
DT<- fread("getdata_data_ss06pid.csv")
system.time(mean(DT[DT$SEX==1,]$pwgtp15))
system.time(mean(DT$pwgtp15,by=DT$SEX))
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(rowMeans(DT)[DT$SEX==1])
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(DT[,mean(pwgtp15),by=SEX])