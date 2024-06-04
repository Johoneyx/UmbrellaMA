library(openxlsx)

# Read the Excel file
evidencemap_tidy <- as.data.frame(read.xlsx("02_data/cleandata/evidencemap_tidy.xlsx"))


df_studylist_cohort <- evidencemap_tidy %>%
  filter(studydesign %in% c("longitudinal", "cohort", "prospective cohort", "cross-sectional and longitudinal")) #%>%
#filter(Topic != "Genetic Moderators") 

View(evidencemap_tidy)

# Write evidencemap_studylist to an Excel file
write.xlsx(df_studylist_cohort, "02_data/cleandata/df_studylist_cohort.xlsx")


df_studylist_cohort_unique <- df_studylist_cohort %>%
  distinct(PublicationID, .keep_all = TRUE)

# Write evidencemap_studylist to an Excel file
write.xlsx(df_studylist_cohort_unique, "02_data/cleandata/df_studylist_cohort_unique.xlsx")

View(df_studylist_cohort_unique)
View(df_studylist_cohort)


