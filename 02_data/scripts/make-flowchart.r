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

cohort_data_extracted <- as.data.table(read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/cohort_df_clean.xlsx"))


View(evidencemap_tidy)
View(cohort_data_extracted)


#**********************Datapreparation************************************
    +     
studylist <- evidencemap_tidy[,c("Topic","studydesign","reviews","outcome","studycode","Exclusion_coded")]#only select Topic, studycode, unique, potstudies, exclusion_coded
extracted <- cohort_data_extracted[,c("studycode","population")]  #only select studycode and population, ``
extracted <- unique(cohort_data_extracted[, .(studycode, population)], by = "studycode")  # Keep only the first occurrence of each studycode

# Merge studylist and extracted by studycode, keeping all cases from studylist
merged_data <- merge(studylist, extracted, by = "studycode", all.x = TRUE)

# Fill in the population variable with "not extracted" if it is NA
merged_data[is.na(population), population := "not extracted"]

# View the resulting data table
View(merged_data)


merged_data <- merged_data %>%
  mutate(id= row_number()) %>%
  mutate(Exclusion1 = case_when(
    reviews == "Murrie (2020)" ~ "outcome not relevant",
    studydesign %notin% c("cohort", "prospective cohort", "longitudinal") ~ "not potentially prospective cohort",
    TRUE ~ NA_character_
  
  ))

# Update Exclusion1 to "duplicate" if it is NA and the same studycode has already occurred
merged_data <- merged_data %>%
  group_by(studycode) %>%
  mutate(Exclusion1 = ifelse(is.na(Exclusion1) & row_number() > 1, "duplicate", Exclusion1)) %>%
  ungroup()

# View the resulting data table
View(merged_data)




#********************Consortplot***********************************************

consort_plot(
merged_data,
orders = c(id = "All primary studies of all systematic reviews",
Exclusion1 = "Exclusion",
id = "Extraction started",
Exclusion_coded = "Exclusion",
population = "divided by population",
id = "Final studies"
)
,
side_box = c("Exclusion1","Exclusion_coded"), 
allocation = "population",
labels = c("1" = "Umbrella Review Groening et al", "2"="Dataextraction from Primary Studies"))





#all studies all reviews
#Exclusion1
#outcome not relevant = transitionrates to schizophrenia from cannabis-induced psychosis (NOT Murrie(2020))
#not potentially prospective longitudinal (NOT prospective, cohort, longitudinal)

#potentially relevant studies

#Exclusion2
#duplicates removed (unique studycode)
#Exclusion in screening process 
# outcome not relevant (everything except Murrie(2020))
# duplicated publication of same cohortstudy
# not potentially longitudinal cohort study 

#Split (HP,CHR,P)
#already extracted, need to be extracted


studiesleft <-merged_data%>%filter(is.na(Exclusion1) & is.na(Exclusion_coded) & population =="not extracted")

write_xlsx(studiesleft, "02_data/to_extract/studiesleft.xlsx")

write_xlsx(merged_data,"C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/studylist_evidencemapandextraction.xlsx" )




