library(readxl)
library(dplyr)

# Read the Excel file
studylist_evidencemap <- read_excel("02_data/cleandata/evidencemap_studylist_unique.xlsx")

merged_df_extracted<- read_excel("02_data/cleandata/merged_df.xlsx")

names(merged_df_extracted)



# Convert keys_column to lowercase
studylist_evidencemap$keys_column <- tolower(studylist_evidencemap$keys_column)

# Convert studylist_extracted to lowercase
studylist_extracted <- tolower(studylist_extracted)

# Install and load the stringdist package if not already done
if (!require(stringdist)) {
  install.packages("stringdist")
  library(stringdist)
}

# Convert keys_column to lowercase
studylist_evidencemap$keys_column <- tolower(studylist_evidencemap$keys_column)

# Convert studylist_extracted to lowercase
studylist_extracted <- tolower(studylist_extracted)

# Add the tidyverse package
library(tidyverse)

# Add the closest match from studylist_extracted to each keys_column value
studylist_evidencemap <- studylist_evidencemap %>%
  mutate(
    extracted = ifelse(keys_column %in% studylist_extracted, "yes", "no"),
    closest_match = replace_na(studylist_extracted[stringdist::amatch(keys_column, studylist_extracted)], "No match found")
  )

view(studylist_evidencemap)

studylist_extracted 