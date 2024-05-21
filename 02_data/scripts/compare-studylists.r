library(readxl)
library(dplyr)
library(stringdist)
library(tidyr)
library(writexl)


# Read the Excel file
studylist_evidencemap <- read_excel("02_data/cleandata/evidencemap_studylist_unique.xlsx")

merged_df_extracted<- read_excel("02_data/cleandata/merged_df.xlsx")




studylist_extracted <-tolower(unique(merged_df_extracted$key))

# Remove "p", "hp", and "chr" from the studylist_extracted vector
studylist_extracted <- gsub("p|hp|chr", "", studylist_extracted)


studylist_evidencemap$keys_column <- tolower(studylist_evidencemap$keys_column)

studylist_evidencemap$studykey <- gsub("p|hp|chr", "",studylist_evidencemap$keys_column)

View(studylist_evidencemap)


studylist_evidencemap <- studylist_evidencemap %>%
  mutate(
    extracted = ifelse(studykey %in% studylist_extracted, "yes", "no"),
    closest_match = studylist_extracted[stringdist::amatch(keys_column, studylist_extracted, maxDist = Inf)]
  )

View(studylist_evidencemap)

table(studylist_evidencemap$extracted)

length(studylist_extracted)

studylist_evidencemap$extracted 

write_xlsx(studylist_evidencemap, "studylist_evidencemap_comparision.xlsx")

sum(unique(studylist_extracted) %in% studylist_evidencemap$studykey)

not_presented <- setdiff(unique(studylist_extracted), studylist_evidencemap$studykey)

print(not_presented)

library(dplyr)


new_names <- c("van os _2002_" = "vanos_2002", "stefanis _2004_" = "stefanis_2004", "degenhart and hall_2001_" = "degenhartandhall_2001", "ferdindand_2005_"="ferdinand_2005", "bergé(2016)_2016_" = "bergé_2016", "shimmelmann_2012_" = "schimmelmann_2012_", "j.m. stone_2014_"   =" stone_2014_", "r. emsley_2020_"= "emsley_2020_", "van der meer_2015_" ="vandermeer_2015_",  "kristine rømer thomsen_2018_"=  "romerthomsen_2018_", "bourque_20013_" ="bourque_2013_", "isaac et al_2005_" ="isaac_2005_", "vandjik_2012_", "gonzalez-into_2009_" ="gonzalez-pinto_2009_")



# Rename the values
studylist_extracted <- recode(studylist_extracted, !!!new_names)