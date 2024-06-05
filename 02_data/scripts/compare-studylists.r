library(readxl)
library(dplyr)
library(stringdist)
library(tidyr)
library(writexl)


# Read the Excel file
studylist_evidencemap_unique <- read_excel("02_data/cleandata/df_studylist_cohort_unique.xlsx")

merged_df_clean<- read_excel("02_data/cleandata/merged_df_clean.xlsx")

merged_df_code_unique <- unique(merged_df_clean$studycode)


studylist_evidencemap_comp <- studylist_evidencemap_unique %>%
  mutate(
    extracted = ifelse(studycode %in% merged_df_code_unique, "yes", "no"),
    closest_match = merged_df_code_unique[stringdist::amatch(studycode, merged_df_code_unique, maxDist = Inf)]
  )

#murrie 2020 sind studien die übergang von cannabis-induzierter psychose zu schiyophrenia prevalenzen berechnet
  studylist_evidencemap_comp <- studylist_evidencemap_comp %>%
  filter(reviews != "Murrie (2020)")

write_xlsx(studylist_evidencemap_comp, "02_data/cleandata/studylist_evidencemap_comparision.xlsx")

studies_to_check <- studylist_evidencemap_comp %>%
filter(extracted == "no")

write_xlsx(studies_to_check, "02_data/cleandata/studies_to_check.xlsx")