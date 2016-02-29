library(rvest)

setwd("C:/Users/n043341/Documents/Documents/Projects/AP_Info Packs/Dx Streetlights/Other DSLMP Strategy Work")
data <- read.csv("LSTD Pics Requested.csv", stringsAsFactors = FALSE)

scrape_first_image <- function(url,pid) {
  UrlPage <- read_html (url)
  ImgNode <- UrlPage %>% html_nodes("img")
  for(j in 1:length(ImgNode)){
  link <- html_attr(ImgNode[j], "src")
  link <- gsub(" ","%20",link)
  #method must inexplicably be "libcurl" to work
  download.file(link, paste(sep = "",pid,"_",j,".jpg"), method="libcurl", mode = 'wb',quiet = TRUE)
  }
}

for(i in 1:nrow(data)){
  try(scrape_first_image(data[i,c("URL")],data[i,c("Plant.Number")]),silent=TRUE)
  print(i)
}
