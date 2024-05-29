library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)

merged_dt_rob <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/mergeddata/merged_dt_rob.xlsx")

merged_dt_rob <- merged_dt_rob  %>% 
  mutate(firstauthor = ifelse(str_detect(Primary, "et al"), 
                               str_extract(Primary, ".*(?= et al)"), 
                               str_extract(Primary, ".*(?=\\()")))


# Rename "Schibart" to "Schubart"
merged_dt_rob <- merged_dt_rob %>%
  mutate(First_author = str_replace_all(firstauthor, "Schibart", "Schubart"))


# Create a new variable "Year"
merged_dt_rob <- merged_dt_rob  %>% 
  mutate(Year = str_extract(Primary, "\\b\\d{4}\\b"))



merged_dt_rob$studycode <- paste((gsub("\\s", "",merged_dt_rob$firstauthor)), merged_dt_rob$Year,  sep = "_")


View(merged_dt_rob) 

write_xlsx(merged_dt_rob, "02_data/cleandata/merged_dt_rob_clean.xlsx")

