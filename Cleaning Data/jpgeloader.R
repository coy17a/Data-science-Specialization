library (jpeg)

t <- readJPEG("getdata_jeff.jpg",native = TRUE)

quantile(t, probs = seq(0.8,0.5))
