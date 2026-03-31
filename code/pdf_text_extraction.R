#pdf scraping
library(tidyverse)
library(pdftools)

test <- pdf_text("test/USPP22449.pdf")

print(test)
  #separate_wider_delim(1, delim = "\n", names_sep = "_", too_few = "align_start") %>% View()
test[1]
test[2]
