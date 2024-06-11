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
merged_df$firstauthor <- ifelse(is.na(str_extract(merged_df$author, ".*(?=\\()")), merged_df$authoronly, str_extract(merged_df$author, ".*(?=\\()"))

#to lowercase
merged_df$firstauthor <-tolower(merged_df$firstauthor)


# Definining new names
new_names <- c("l.clausen"= "clausen","jennifer l. seddon"= "seddon", "ferdindand"="ferdinand", "bergé(2016)" = "bergé", "shimmelmann " = "schimmelmann", "j.m. stone"   =" stone", "r. emsley"= "emsley",   "kristine rømer thomsen" =  "romer thomsen", "isaac et al " ="isaac", "vandijk" ="van dijk", "arsenault "= "arseneault", "auther  et al." = "auther", "miller et al " = "miller", "roessler" = "rössler", "setien-suero "= "setién-suero", "gonzalez-pinto"= "gonzález-pinto",  "martinez-arevalo"= "martínez arévalo", "buchy (2015" = "buchy","auther et al. "= "auther", "degenhart and hall" = "degenhardt and hall", "riecher-roessler" = "riecher-rössler", "arias horjadas" = "arias horcajadas", "degenhart" = "degenhardt and hall")

print(sort(merged_df_clean$firstauthor))

# Rename authors
merged_df$firstauthor <- recode(merged_df$firstauthor, !!!new_names)


# Select rows where author equals "Foti(2010)" and change year to 2010
merged_df$year[merged_df$author == "Foti(2010)"] <- 2010


#create publicationyear variable
merged_df$pubyear <- ifelse(is.na(merged_df$year), str_extract(merged_df$author, "\\((\\d+)\\)"), 
merged_df$year)


# Remove all brackets
merged_df$pubyear <- gsub("\\(|\\)", "", merged_df$pubyear)


#create psycont (psychosiscontinuumpopulation) variable
merged_df$psycontpop<- str_extract(merged_df$dt_name, "^[^_]*")


merged_df$study_year_psycont<- paste((gsub("\\s", "", merged_df$firstauthor)), merged_df$pubyear, merged_df$psycontpop, sep = "_")


merged_df$studycode <- paste((gsub("\\s", "", merged_df$firstauthor)), merged_df$pubyear,  sep = "_")

#after checking studylist data correcting some wrong studycodes
merged_df <- merged_df %>%
mutate(studycode, str_replace_all(studycode, "hadden_2026", "hadden_2018"))

# Create dataextraction variable
merged_df$dataextraction <- ifelse(is.na(merged_df$"extracted by"), merged_df$dt_name, merged_df$"extracted by")


# Recode values in the dataextraction column
merged_df$dataextraction <- recode(merged_df$dataextraction, 
                                   "HP_S_KD" = "Kaito & David", 
                                   "HP_D_KR" = "Kaito and Riccardo", 
                                   "CHR_S_KD" = "Kaito and David", 
                                   "P_J" = "Johanna", 
                                   "HP_J" = "Johanna", 
                                   "P_KC" = "Kaito and Carolina")

merged_df_clean <- merged_df %>% filter(!is.na(firstauthor))



write_xlsx(merged_df_clean, "02_data/cleandata/merged_df_clean.xlsx")























































