#pdf scraping
library(tidyverse)
library(pdftools)

test <- pdf_text("test/USPP22449.pdf")

print(test)
 

scrape_des_results[16] %>% str_split("\n") %>% unlist() 
