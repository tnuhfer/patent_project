#download PDFs for gbotanic

library(tidyverse)
library(rvest)
library(stringr)
library(stringi)
library(strex)
library(TNRS)

#bring in gbotanic
gbot <- read.delim("data/input/g_botanic.tsv")

#bring in resolved usda plants
usda <- read.csv("data/input/usda_native_resolved.csv")
usda <- usda[-1]

#resolve gbotanic

#resolve taxonomy of gbot
#create a 2 column dataframe with a column of row ID numbers and the unique submitted names from ERA
gbot_tnrs <- data.frame(c(1:length(unique(gbot$latin_name))), unique(gbot$latin_name))
#rename columns
names(gbot_tnrs) <- c("ID", "submitted")
#Run TNRS - this step can be slow. Create a new object called "gbot_resolved" for the results
gbot_resolved <- TNRS(gbot_tnrs, accuracy = 0.9)
#If accepted name was blank, or genus level or greater, replace with NA
gbot_resolved$Accepted_name[gbot_resolved$Accepted_name == "" | gbot_resolved$Accepted_name_rank == "genus"] <- NA
#now, we are going to select only a few relevant columns from the TNRS output and join it to the gbot based on the submitted name
#right join will have the same number of rows as the gbot dataframe
gbot <- gbot_resolved %>% 
  select(Name_submitted, Accepted_name) %>%
  right_join(gbot, by = join_by(Name_submitted == latin_name)) #join_by says that these two columns should be used to match the datasets bc they are the same data

#We will want to filter out non-native species now
gbot_native <- gbot %>% filter(Accepted_name %in% usda$Accepted_name)

write.csv(gbot_native, "data/output/gbot_native_3_24_26.csv")
