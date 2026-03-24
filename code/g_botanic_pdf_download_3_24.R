#try webscraping wtih g_botanic patent numbers

#example url : https://image-ppubs.uspto.gov/dirsearch-public/print/downloadPdf/PP32577

library(rvest)
library(stringr)
library(stringi)
library(strex)
library(tidyverse)

gbot_native <- read.csv("data/output/gbot_native_3_24_26.csv")

#remove a null result
gbot_native <- gbot_native[-1269,]
  
j <- gbot_native$patent_id[1]
#scrape pdfs
for(j in gbot_native$patent_id[1874:1922]){
  url<-paste0("https://image-ppubs.uspto.gov/dirsearch-public/print/downloadPdf/",j) #create URL
  dest_file <- paste0("data/output/gbot_pdfs/", j, ".pdf") #create destination
  download.file(url, destfile = dest_file, mode = "wb")
}


