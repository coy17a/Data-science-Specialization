
library(xml2)
docurl <- "http://biostat.jhsph.edu/~jleek/contact.html"
test <- read_html(docurl)
xml_text(test)

con <- docurl
html_code <- readLines(con)
nchar(html_code[100])
