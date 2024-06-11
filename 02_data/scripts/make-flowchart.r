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

evidencemap_tidy<- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/evidencemap_tidy.xlsx"))


evidencemap_tidy <- evidencemap_tidy %>%
mutate(id = row_number())




evidencemap_tidy[,.q(unique, potstudies, fin, revpop) :=
.(ifelse(unique == 1, 1, NA),
ifelse(potstudies == 1 & unique == 1, 1, NA),
ifelse(potstudies == 1 & is.na(Exclusion_coded) & unique == 1, 1, NA),
ifelse(potstudies == 1 & is.na(Exclusion_coded) & unique_study_pop == 1, broad_topic, NA)
)]

evidencemap_tidy[, exc := seqFreq(
  "not prospective longitudinal" = Exclusion_coded == "not prospective longitudinal", 
  "outcome not relevant" = Exclusion_coded == "no relevant outcome", 
  "duplicated publication of same cohortstudy" = Exclusion_coded =="double-publication", 
  noneNA =TRUE
)]


eo  <- attr(evidencemap_tidy[, exc], 'obs.per.numcond')
mult <- paste0('1, 2, ≥3 exclusions: n=',
                eo[2], ', ',
                eo[3], ', ',
                eo[-(1:3)]  )



consort_plot(
evidencemap_tidy,
orders = c(id = "All primary studies of all systematic reviews",
unique = "duplicate studies removed",
revpop = "studies per topic",
potstudies ="potentially prospective longitudinal",
exc = "Excluded",
fin = "final studies",
revpop = "studies per topic"
),
side_box = "exc", 
allocation = "revpop",
labels = c("1" = "Umbrella Review Groening et al", "2" ="Studies identified"))

