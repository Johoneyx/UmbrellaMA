library(readxl)
library(dplyr)
library(stringdist)
library(tidyr)
library(writexl)



merged_dt_rob <- read_excel("02_data/cleandata/merged_dt_rob_clean.xlsx")
merged_df_clean<- read_excel("02_data/cleandata/merged_df_clean.xlsx")


merged_df_code_unique <- merged_df_clean %>%
  distinct(studycode, .keep_all = TRUE)

merged_df_code_unique <- merged_df_code_unique  %>% 
  mutate(
    rob_done = ifelse(studycode %in% merged_dt_rob$studycode, "yes", "no"),
    closest_match = ifelse(rob_done == "no", merged_dt_rob$studycode[stringdist::amatch(studycode, merged_dt_rob$studycode, maxDist = Inf)], NA)
  )

write_xlsx(merged_df_code_unique, "02_data/cleandata/dataextraction_rob_comparision.xlsx")


