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

View(evidencemap_studylist)


# Create a new variable "unique"
evidencemap_studylist <- evidencemap_studylist %>%
  mutate(unique = ifelse(duplicated(studycode), 0, 1))

# Create a new variable "id"
evidencemap_studylist <- evidencemap_studylist %>%
  mutate(id = row_number())


evidencemap_studylist[, unique = unique == 1]

# Create a new variable "prospective_cohort"
evidencemap_studylist <- evidencemap_studylist %>%
  mutate(prospective_cohort = case_when(
    studydesign == "prospective cohort" ~ "yes",
    studydesign %in% c("longitudinal", "cohort") ~ "maybe",
    TRUE ~ "no"
  ))

evidencemap_studylist[, pop := seqFreq("HP" = broad_topic == "HP",
"CHR" = broad_topic == "CHR",
"P" = broad_topic == "P", noneNA=TRUE)]

evidencemap_studylist[, potstudies := seqFreq("prospective cohort" = studydesign == "prospective cohort", 
"longitudinal" = studydesign == "longitudinal"| studydesign == "cross-sectional and longitudinal"
"cohort" = studydesign == "cohort", noneNA=TRUE)]




h <- function(n, label) paste0(label, ' (n=', n, ')')

htab <- function(x, label=NULL, split=! length(label), br='\n') {
  tab <- table(x)
  w <- if(length(label)) paste0(h(sum(tab), label), ':', br)
  f <- if(split) h(tab, names(tab)) 
  else
    paste(paste0('   ', h(tab, names(tab))), collapse=br)
  if(split) return(f)
  paste(w, f, sep=if(length(label))'' else br)
}  
count <- function(x, by=rep(1, length(x)))
  tapply(x, by, sum, na.rm=TRUE)

 w <- evidencemap_studylist[, {
 g <-
add_box(txt=h(nrow(evidencemap_studylist), "All studies in all systematic reviews"))                  |>
   add_split(htab(pop))                              plot(g)
}]

