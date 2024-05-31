library(readxl)
library(dplyr)
library(stringdist)
library(tidyr)
library(writexl)


# Read the Excel file
studylist_evidencemap_unique <- read_excel("02_data/cleandata/df_studylist_cohort_unique.xlsx")
merged_df_extracted<- read_excel("02_data/cleandata/merged_df_clean.xlsx")

merged_df_code_unique <- unique(merged_df_clean$studycode)


studylist_evidencemap_comp <- studylist_evidencemap_unique %>%
  mutate(
    extracted = ifelse(studycode %in% merged_df_code_unique, "yes", "no"),
    closest_match = merged_df_code_unique[stringdist::amatch(studycode, merged_df_code_unique, maxDist = Inf)]
  )

write_xlsx(studylist_evidencemap_comp, "02_data/cleandata/studylist_evidencemap_comparision.xlsx")

