library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)


# Read the Excel file
evidencemap_tidy <- as.data.frame(read.xlsx("02_data/cleandata/evidencemap_tidy.xlsx"))


df_studylist_cohort <- evidencemap_tidy %>%
  filter(potstudies == 1 & excluded !=1)
#filter(Topic != "Genetic Moderators") 


# Write evidencemap_studylist to an Excel file
write.xlsx(df_studylist_cohort, "02_data/cleandata/df_studylist_cohort.xlsx")


df_studylist_cohort_unique <- df_studylist_cohort %>%
  distinct(PublicationID, .keep_all = TRUE)

# Write evidencemap_studylist to an Excel file
write.xlsx(df_studylist_cohort_unique, "02_data/cleandata/df_studylist_cohort_unique.xlsx")

View(df_studylist_cohort_unique)
View(df_studylist_cohort)


