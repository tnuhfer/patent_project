#downloaded all plant patents A01h* in US from Lens as JSON file

#get native plants from USDA
library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(strex)
library(TNRS)

usda <- read.csv("C:/Users/lnuhfer/OneDrive - University of Massachusetts/Plant Data/USDA/USDA_plants_codes.txt")
#fix encoding
x <- "\xd7"
Encoding(x) <- "UTF-8"
usda$Scientific.Name <- iconv(usda$Scientific.Name, "UTF-8", "UTF-8",sub= 'x ')

#resolve USDA
#first reduce to native only
usda <- usda[(grepl("L48\\(N", usda$Native.Status)),] 
usda_names <- data.frame(c(1:23642), usda$Scientific.Name)
names(usda_names) <- c("id", "name")
usda_resolved <- TNRS(usda_names)

#filter to only 90% match or greater and isolate
usda_native_resolved <- usda_resolved %>% filter(Overall_score > 0.9) %>% select(Accepted_name)

write.csv(usda_native_resolved, "C:/Users/lnuhfer/OneDrive - University of Massachusetts/genome_hort/usda_native_resolved.csv")

#now try to bring in the JSON object
library(jsonlite)


lines <- readLines("C:/Users/lnuhfer/OneDrive - University of Massachusetts/genome_hort/plant-patents-json-lens.jsonl")
lines <- lapply(lines, fromJSON)
lines <- lapply(lines, unlist)
json <- bind_rows(lines)
#simplify to lens id, doc number, date published, titke, classifications, abstract, desctiption
relevant <- c(1, 3, 5, 24, 34, 35, 60, 64)
json_parsed <- json %>% select(all_of(relevant))
write.csv(json_parsed, "C:/Users/lnuhfer/OneDrive - University of Massachusetts/genome_hort/json_lens_parsed.csv")

