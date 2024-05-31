library(openxlsx)

# Read the Excel file
df_studylist <- as.data.frame(read.xlsx("02_data/cleandata/evidencemap_studylist.xlsx"))

levels(as.factor(df_studylist$studydesign))

df_studylist_cohort <- df_studylist %>%
  filter(studydesign %in% c("longitudinal", "cohort", "prospective cohort", "cross-sectional and longitudinal")) %>%
filter(Topic != "Genetic Moderators") 

df_studylist_cohort_unique <- df_studylist_cohort %>%
  distinct(studycode .keep_all = TRUE)


View(df_studylist_cohort_unique)

# Write evidencemap_studylist to an Excel file
write.xlsx(df_studylist_cohort, "02_data/cleandata/df_studylist_cohort.xlsx")

# Write evidencemap_studylist to an Excel file
write.xlsx(df_studylist_cohort_unique, "02_data/cleandata/df_studylist_cohort_unique.xlsx")


View(df_studylist_cohort)