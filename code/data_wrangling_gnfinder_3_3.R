#wrangling gnfinder output
library(tidyverse)
patent <- read.csv("C:/Users/lnuhfer/OneDrive - University of Massachusetts/patent_project/data/output/patent_test_text.csv", header=FALSE)
#replace blanks with NA
patent[patent == ""] <- NA
#give column names
patent$V0 <- 1:78603

table(patent$V1)

indexes <- patent %>% select(V1, V0) %>% filter(V1 == "Index")
indexes$NewRank <- 1:35224

patent <- patent %>% left_join(indexes, join_by(V0 == V0))

for (i in 1:78603){
if (is.na(patent$NewRank[i])) {
  patent$NewRank[i] <- patent$NewRank[i-1]
}
}

#bring in alphabetical index
alphabetical_index <- read.csv("C:/Users/lnuhfer/OneDrive - University of Massachusetts/patent_project/data/output/alphabetical_index.csv")

patent %>% filter(V1.x != "Index") %>% View()

patent <- patent %>% left_join(alphabetical_index, join_by(NewRank == X))
