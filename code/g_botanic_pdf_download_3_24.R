#try webscraping wtih g_botanic patent numbers

#example url : https://image-ppubs.uspto.gov/dirsearch-public/print/downloadPdf/PP32577

library(rvest)
library(stringr)
library(stringi)
library(strex)
library(tidyverse)
library(httr)
library(surveydata)

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
#scrape = function(x){
#  species <- x %>% read_html() %>% html_nodes(xpath="//*[@id='p-0002']") %>% html_text(trim=TRUE)
#}

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

#now loop  species by skipping errors
#scrape_results <- lapply(url_list, possibly(scrape, NA))

#scrape description
scrape_des = function(x){
  description <- x %>% read_html() %>% html_nodes(xpath="/html/body/search-app/article/section[6]/div/div") %>% html_text(trim=FALSE)
}

scrape_des_results <- lapply(url_list[1:20], possibly(scrape_des, NA))

#webscraping more readable than pdfs

#try parsing text, big encoding errors


#fix encoding
scrape_des_results <- gsub("â\u0080\u0098", "'", unlist(scrape_des_results)) 
scrape_des_results <- gsub("â\u0080\u0099", "'", scrape_des_results) 
scrape_des_results <- gsub("Ã\u0097", " x ", scrape_des_results) 
scrape_des_results <- gsub("â\u0080\u009c", "'", scrape_des_results) 
scrape_des_results <- gsub("â\u0080\u009d", "'", scrape_des_results)
scrape_des_results <- gsub("â\u0080\u0082", " ", scrape_des_results)
scrape_des_results <- gsub("â\u0080\u0094", "--", scrape_des_results)

scape_des_results <- fix_common_encoding_problems(scrape_des_results)
test <- scrape_des_results %>% unlist %>% as.data.frame() 

#try getting the 25 characters after the first appearance of "inflorescence"
test_results <- c()
for(g in 1:20){
  position <- regexpr("Petals", scrape_des_results[g], ignore.case = TRUE)[1]
  if (position == -1){ 
    phrase <- "no data"}else{
      phrase <- substr(scrape_des_results[g], (position + 6), (position + 31))
    }
test_results[g] <- phrase
}
test_results <- as.data.frame(test_results)
test_results$test_results[grepl("per flower", test_results$test_results) == FALSE] <- NA

