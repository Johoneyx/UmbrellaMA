# Create df_studylist
library(ggplot)
library(qreport)
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)

emap_studylist_cohort <- as.data.frame(read_xlsx("02_data/cleandata/df_studylist_cohort.xlsx"))


table(emap_studylist_cohort$studydesign)

typecount <- aggregate(population ~  )

barplot(table(emap_studylist_cohort$studydesign))


# Create a bar plot of the population variable
ggplot(emap_studylist_cohort, aes(x = population)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Population", y = "Count", title = "Count of each level of the population variable")

 

dataOverview(emap_studylist_cohort)


