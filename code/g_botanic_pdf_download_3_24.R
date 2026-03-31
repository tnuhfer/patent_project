#try webscraping wtih g_botanic patent numbers

#example url : https://image-ppubs.uspto.gov/dirsearch-public/print/downloadPdf/PP32577

library(rvest)
library(stringr)
library(stringi)
library(strex)
library(tidyverse)
library(httr)

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

#try webscraping html from google patents
# https://patents.google.com/patent/USPP33329P2/en?oq=USPP22449

for(j in gbot_native$patent_id[1:5]){
  url<-paste0("https://patents.google.com/patent/US",j, "en?oq=US", j) #create URL
  page <- read_html(url) #read URL
  # Extract the desired information using CSS selectors
  species <- page %>% html_nodes(xpath="//*[@id='p-0002']") %>% html_text(trim=TRUE)
  if(length(genus) == 0) species <- NA
}

#function to get species
scrape = function(x){
  species <- x %>% read_html() %>% html_nodes(xpath="//*[@id='p-0002']") %>% html_text(trim=TRUE)
}

#make url list - most have "P2", some "P3" 
url_list <- c()
for(j in gbot_native$patent_id){
  url1 <- paste0("https://patents.google.com/patent/US",j, "P2/en?oq=US", j)
  url2 <- paste0("https://patents.google.com/patent/US",j, "P3/en?oq=US", j)
#try url and see if that works
  #if not, sub P3
if (http_error(url1) == TRUE) {
  url_list <- append(url_list, url2)
} else url_list <- append(url_list, url1)
}

#now loop by skipping errors
scrape_results <- lapply(url_list, possibly(scrape, NA))

#scrape description
scrape_des = function(x){
  description <- x %>% read_html() %>% html_nodes(xpath="/html/body/search-app/article/section[6]/div/div") %>% html_text(trim=FALSE)
}

scrape_des_results <- lapply(url_list, possibly(scrape_des, NA))

#webscraping not providing more than the PDFs do, skip it 
