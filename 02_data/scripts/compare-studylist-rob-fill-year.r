library(readxl)
library(dplyr)
library(stringdist)
library(tidyr)
library(writexl)


merged_dt_rob<- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/merged_dt_rob_clean.xlsx")

studylist_cohort <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/df_studylist_cohort_unique.xlsx")




studylist_cohort <- studylist_cohort %>%
  mutate(review_sl = str_trim(tolower(reviews))) %>%
  mutate(firstauthor_sl = tolower(firstauthor))


merged_dt_rob <- merged_dt_rob %>%
  mutate(review_rob = str_trim(tolower(Review))) %>%
  mutate(firstauthor_rob = tolower(First_author))

# Perform a left join to merge the data frames based on matching columns
merged_data <- merged_dt_rob %>%
  left_join(studylist_cohort, by = c("review_rob" = "review_sl", "firstauthor_rob" = "firstauthor_sl"))

# Update the year column in merged_dt_rob with the year values from studylist_cohort
merged_dt_rob <- merged_data %>%
  mutate(year_merged = coalesce(Year, year))

# View the updated merged_dt_rob
View(merged_dt_rob)