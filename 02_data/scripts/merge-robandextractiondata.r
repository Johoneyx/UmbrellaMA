library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)

evidencemap_studylist <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/evidencemap_studylist_unique.xlsx")

merged_df_clean <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/merged_df_clean.xlsx")

merged_dt_rob_clean <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/merged_dt_rob_clean.xlsx")

# Sort evidencemap_studylist by First_author
evidencemap_studylist_view<- evidencemap_studylist %>%
 select(firstauthor, year, studycode) %>%
  distinct(studycode, .keep_all = TRUE) %>%
  arrange(firstauthor)
  
  View(evidencemap_studylist_view)
 
 

merged_dt_rob_clean_view <-
merged_dt_rob_clean %>%


View(merged_dt_rob_clean_view)

# Select columns and ensure unique StudyKey
merged_df_clean_view <- merged_df_clean %>%
  select(firstauthor, pubyear, studycode) %>%
  distinct(studycode, .keep_all = TRUE) %>%
  arrange(firstauthor)

# View the dataframe
View(merged_df_clean_view)

