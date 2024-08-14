library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)

evidencemaplist <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/studylist_evidencemapandextraction.xlsx")


df_rob <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/df_rob_withcompletestudycode.xlsx")



studylist <- evidencemaplist %>% 
filter(is.na(Exclusion1)& is.na(Exclusion_coded))

View(studylist)

View(df_rob)

studylistandrob <- studylist %>%
left_join(df_rob, by ="studycode")

View(studylistandrob)

rob_left <- studylistandrob %>%
filter(is.na(TotalStars))

View(rob_left)

write_xlsx(rob_left, "C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/to_extract/rob_left_todo.xlsx")



