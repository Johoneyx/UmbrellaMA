library(readxl)
library(dplyr)

# Read the Excel file
studylist_evidencemap <- read_excel("02_data/cleandata/evidencemap_studylist_unique.xlsx")

studylist_extracted <- read_excel("02_data/cleandata/merged_df.xlsx")

# Rename the column to 'key' for merging
names(studylist_evidencemap)[names(studylist_evidencemap) == 'keys_column'] <- 'key'

# Merge the dataframes
merged_data <- merge(merged_df, evidencemap_studylist_unique, by = "key", all = TRUE)

# Create the 'extracted' column
merged_data$extracted <- ifelse(!is.na(merged_data$key), "x", NA)