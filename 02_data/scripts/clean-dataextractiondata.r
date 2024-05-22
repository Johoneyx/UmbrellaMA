library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)

merged_df <-as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/mergeddata/merged_df.xlsx"))

#create firstauthor variable
merged_df$firstauthor <- ifelse(is.na(str_extract(merged_df$author, ".*(?= \\()")), merged_df$authoronly, str_extract(merged_df$author, ".*(?= \\()"))

#to lowercase
merged_df$firstauthor <-tolower(merged_df$firstauthor)


# Definining new names
new_names <- c("l.clausen"= "clausen","jennifer l. seddon"= "seddon", "ferdindand"="ferdinand", "bergé(2016)" = "berge", "shimmelmann" = "schimmelmann", "j.m. stone"   =" stone", "r. emsley"= "emsley",   "kristine rømer thomsen" =  "romer thomsen", "isaac et al" ="isaac", "vandijk" ="van djik", "gonzalez-into" ="gonzalez-pinto", "auther  et al." = "auther", "miller et al" = "miller", "rössler" = "roessler")

# Rename authors
merged_df$firstauthor <- recode(merged_df$firstauthor, !!!new_names)


#create publicationyear variable
merged_df$pubyear <- ifelse(is.na(merged_df$year), 
                                    str_extract(merged_df$author, "\\((\\d+)\\)"), 
                                    merged_df$year)


# Remove all brackets
merged_df$pubyear <- gsub("\\(|\\)", "", merged_df$pubyear)


#create psycont (psychosiscontinuumpopulation) variable
merged_df$psycontpop<- str_extract(merged_df$dt_name, "^[^_]*")


merged_df$studycode <- paste((gsub("\\s", "", merged_df$firstauthor)), merged_df$pubyear, merged_df$psycontpop, sep = "_")



studycode_unique <- unique(merged_df$studycode)
studycode_unique


































































































