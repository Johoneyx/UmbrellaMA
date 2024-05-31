library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)
library(Hmisc)
library(qreport)
library(consort)

evidencemap_studylist<- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/evidencemap_studylist.xlsx"))



# Create a new variable "id"
evidencemap_studylist <- evidencemap_studylist %>%
  mutate(id = row_number())

evidencemap_studylist <- evidencemap_studylist %>%
  mutate(id = row_number(),
         potentially_prospective_longitudinal = ifelse(studydesign %in% c("prospective cohort", "longitudinal", "cross-sectional and longitudinal", "case-only", "cohort"), "yes", "no"))

# Create a new variable "unique"
evidencemap_studylist <- evidencemap_studylist %>%
  mutate(unique = ifelse(duplicated(studycode), 0, 1))



evidencemap_studylist[, pop := seqFreq("HP" = broad_topic == "HP",
"CHR" = broad_topic == "CHR",
"P" = broad_topic == "P", noneNA=TRUE)]

evidencemap_studylist[, potstudies := seqFreq("prospective cohort" = (studydesign == "prospective cohort"),                                   "longitudinal" = (studydesign == "longitudinal" | studydesign == "cross-sectional and longitudinal"),
                                              "cohort" = (studydesign == "cohort"), noneNA=TRUE)]

consort_plot(
evidencemap_studylist,
orders = c(id = "All primary studies of all systematic reviews",
pop = "divided by population",
unique = "duplicate studies removed",
potentially_prospective_longitudinal ="potentially prospective longitudinal",
potstudies ="potential prospective cohort studies"),
side_box = c("pop", "potstudies"),
labels = c("1" = "Umbrella Review Groening et al", "2" ="Studies identified")
)

