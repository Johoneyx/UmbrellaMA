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
  mutate(First_author = str_replace_all(firstauthor, "Schibart", "Schubart"))%>%
  mutate(First_author = str_replace_all(firstauthor,"romer thomsen","rømerthomsen"))

# Create a new variable "Year"
merged_dt_rob <- merged_dt_rob  %>% 
  mutate(Year = str_extract(Primary, "\\b\\d{4}\\b"))



merged_dt_rob$studycode <- paste((gsub("\\s", "",merged_dt_rob$firstauthor)), merged_dt_rob$Year,  sep = "_")


View(merged_dt_rob) 

write_xlsx(merged_dt_rob, "02_data/cleandata/merged_dt_rob_clean.xlsx")



evidencemap_studylist <- read_xlsx("02_data/cleandata/evidencemap_studylist.xlsx")



evidencemap_studylist <- evidencemap_studylist %>%mutate(reviews = str_replace(reviews, "(\\w+)(\\()", "\\1 \\2"))


# Perform the join and create the Year_filledin variable
merged_dt_rob <- merged_dt_rob %>%
  left_join(evidencemap_studylist, by = c("First_author" = "authorww", "Review" = "reviews")) %>%
  mutate(Year_filledin = ifelse(is.na(Year), year, Year)) 

View(merged_dt_rob %>% filter(is.na(Year_filledin)))

View(merged_dt_rob)

#manually checked the following years
#Koning_2008
#miettunen_2008
#NA
#rössler_2012
#Korver_2010
#vanOs_2002
#tien_1990
#wiles_2006
#mcgrath_2010
#riecher-rössler_2008
#Bloemen_2010
#De Vylder_2014
#Korver_2010
#Philips_2002
#Gill_2015
#vanTricht_2013
#Solovij_2011
#Schoeler_2016a
#Schoeler_2016b
#AddingtonandAddington_2007

# studycodes that I created searched referencelist or reviewpapers for the years
study_codes <- c(
  "Koning_2008", "Miettunen_2008", NA, "Rössler_2012", "Korver_2010", 
  "VanOs_2002", "Tien_1990", "Wiles_2006", "Mcgrath_2010", 
  "Riecher-rössler_2008", "Bloemen_2010", "DeVylder_2014", "Korver_2010", 
  "Philips_2002", "Gill_2015", "VanTricht_2013", "VanDijk_2012","Solovij_2011","AddingtonandAddington_2007", 
  "Schoeler_2016a", "Schoeler_2016b"
)

na_rows <- which(is.na(merged_dt_rob$Year_filledin))

#here i check if NA matches the amount of studycodes in the vector
if (length(na_rows) == length(study_codes)) {
  merged_dt_rob$studycode.y[na_rows] <- study_codes
} else {
  stop("The number of NA rows does not match the length of study_codes.")
}

# View the updated dataframe
View(merged_dt_rob %>% filter(is.na(Year_filledin)))

# View the entire dataframe
View(merged_dt_rob)

merged_dt_rob <- merged_dt_rob %>%
  mutate(studycode.y = ifelse(is.na(studycode.y), paste0(First_author, "_", Year_filledin), studycode.y))

# View the updated dataframe
View(merged_dt_rob %>% filter(is.na(studycode.y)))

# View the entire dataframe
View(merged_dt_rob)



# Replace "*" with "1" and "**" with "2" in merged_dt_rob
merged_dt_rob <- merged_dt_rob %>%
  mutate(across(everything(), ~ str_replace_all(., "\\*\\*", "2"))) %>%
  mutate(across(everything(), ~ str_replace_all(., "\\*", "1")))

# View the updated dataframe
View(merged_dt_rob)
