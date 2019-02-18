library(readxl)
ngas <- read_excel("getdata_data_DATA.gov_NGAP.xlsx", range=cell_rows(18:23))
ngas <- ngas[,7:15]
dat <- ngas
sum(dat$Zip*dat$Ext,na.rm=T)
