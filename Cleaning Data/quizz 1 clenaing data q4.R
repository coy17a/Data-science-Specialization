
library(xml2)
docurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
test <- read_xml(docurl)
zip <- xml_find_all(test,".//zipcode")
target <- "21231"
count <- 0
for (code in zip) {
  if(xml_text(code) == target){
    count <- count + 1
  }
}
