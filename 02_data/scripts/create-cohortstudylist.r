library(openxlsx)

# Read the Excel file
df_studylist <- as.data.frame(read.xlsx("02_data/cleandata/evidencemap_studylist_unique.xlsx"))

levels(as.factor(df_studylist$studydesign))

df_studylist_cohort <- df_studylist %>%
  filter(studydesign %in% c("longitudinal", "cohort", "prospective cohort", "cross-sectional and longitudinal","case-only"))


# Write evidencemap_studylist to an Excel file
write.xlsx(df_studylist_cohort, "df_studylist_cohort.xlsx")