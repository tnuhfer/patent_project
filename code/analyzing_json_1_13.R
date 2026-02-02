library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(strex)
library(TNRS)
usda_native_resolved <- read.csv("C:/Users/lnuhfer/OneDrive - University of Massachusetts/genome_hort/usda_native_resolved.csv")
json_lens_parsed <- read.csv("C:/Users/lnuhfer/OneDrive - University of Massachusetts/genome_hort/json_lens_parsed.csv", comment.char="#")



test <- json_lens_parsed[]
#reduce text, botanical name commonly in first few hundred characters
test$reduced <- strtrim(test$description.text, 500)
#remove NAs
test <- test %>% filter(!is.na(reduced))
#export each reduced description as its' own text file for gnfinder
for(i in 1:length(test$reduced)){
  a <- test$reduced[i]
  writeLines(a, paste0("C:/Users/lnuhfer/names/patent_",i,".txt"))
}


#filter out hybrids
test <- test %>% filter(!grepl("hybrid", reduced))


#split text
test <- test %>% separate(reduced, c("ignore", "name"), sep = "designation:", remove = FALSE, extra = "merge", fill = "right")
test <- test %>% separate(reduced, c("ignore2", "name2"), sep = "botanically known as ", remove = FALSE, extra = "merge", fill = "right")
test <- test %>% separate(reduced, c("ignore3", "name3"), sep = "classification:", remove = FALSE, extra = "merge", fill = "right")

#combine - prioritize in order listed above
test$name_text <- test$name
test$name_text[is.na(test$name_text)] <- test$name2[is.na(test$name_text)]
test$name_text[is.na(test$name_text)] <- test$name3[is.na(test$name_text)]

test <- test %>% select(-c("ignore", "ignore2", "ignore3", "name", "name2", "name3"))

#try "Latin name:" and "Latin name of the genus and species:", "Latin name of the genus and species of the plant claimed",  "Latin name of the genus and species of the claimed plant
test <- test %>% separate(reduced, c("ignore", "name"), sep = "Latin name:", remove = FALSE, extra = "merge", fill = "right")
test <- test %>% separate(reduced, c("ignore2", "name2"), sep = "Latin name of the genus and species:", remove = FALSE, extra = "merge", fill = "right")
test <- test %>% separate(reduced, c("ignore3", "name3"), sep = "Latin name of the genus and species of the plant claimed", remove = FALSE, extra = "merge", fill = "right")
test <- test %>% separate(reduced, c("ignore4", "name4"), sep = "Latin name of the genus and species of the claimed plant", remove = FALSE, extra = "merge", fill = "right")
test <- test %>% separate(reduced, c("ignore5", "name5"), sep = "Genus and species:", remove = FALSE, extra = "merge", fill = "right")

#combine - prioritize in order listed above
test$name_text[is.na(test$name_text)] <- test$name[is.na(test$name_text)]
test$name_text[is.na(test$name_text)] <- test$name2[is.na(test$name_text)]
test$name_text[is.na(test$name_text)] <- test$name3[is.na(test$name_text)]
test$name_text[is.na(test$name_text)] <- test$name4[is.na(test$name_text)]
test$name_text[is.na(test$name_text)] <- test$name5[is.na(test$name_text)]
#clean up
test <- test %>% select(-c("ignore", "ignore2", "ignore3", "ignore4", "ignore5", "name", "name2", "name3", "name4", "name5"))

#filter out hybrid symbols
test <- test %>% filter(!grepl("×", name_text)) %>% filter(!grepl(" x ", name_text)) %>% filter(!grepl(" X ", name_text))

test$name_text <- str_trim(test$name_text)

test$name_text <- sub(":", "", test$name_text)

#split first two words off
test <- test %>% separate(name_text, into = c("genus", "species", "extra"), sep = " ", remove = FALSE, extra = "merge", fill = "right")
test$extra <- NULL
#clean species column
test$species <- gsub('[[:punct:] ]+','',test$species)

test <- test %>% unite(binomial, genus, species, sep = " ", na.rm = TRUE)
test <- test %>% filter(binomial != "")
test$binomial <- str_trim(test$binomial)
#resolve test names
test_tnrs <- data.frame(1:length(unique(test$binomial)), unique(test$binomial))
names(test_tnrs) <- c("id", "name")

test_resolved <- TNRS(test_tnrs, accuracy = 0.9)
test_resolved <- test_resolved %>% filter(!(Accepted_name_rank %in% c("", "family", "genus")))

test <- test %>% left_join(select(test_resolved, c(Name_submitted, Accepted_name)), by = join_by("binomial" == "Name_submitted")) 

test <- test %>% filter(Accepted_name %in% usda_native_resolved$Accepted_name)

#1190 patents of 198 species - but patent descriptions prior to 1970 didn't download. I think I can get PDFs, but not necessarily OCR records?
#Should see if lance can get gnfinder to work. See if we can do a bulk PDF download from the API
#If not, it's only doing to be 1970+

#take these out to look at in excel
write.csv(test, "C:/Users/lnuhfer/OneDrive - University of Massachusetts/genome_hort/test_data.csv")

