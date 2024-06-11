library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(metafor)
library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)

#read in different data files of all the populations
df_HP_map <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet=1) %>%
  as.data.frame() %>%
  filter(rowSums(is.na(.)) != ncol(.))

df_CHR_map <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =2) %>%
  as.data.frame() %>%
  filter(rowSums(is.na(.)) != ncol(.))

df_P_map <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =3) %>%
  as.data.frame() %>%
  filter(rowSums(is.na(.)) != ncol(.))

df_Mod_map <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =4) %>%
  as.data.frame() %>%
  filter(rowSums(is.na(.)) != ncol(.))

df_map_list <-list(df_HP_map = df_HP_map, df_CHR_map = df_CHR_map, df_P_map = df_P_map, df_Mod_map = df_Mod_map) 

# Convert all variables in the data frames to character type
df_map_list <- lapply(df_map_list, function(df) {
  df[] <- lapply(df, as.character)
  return(df)
})

# Join all the data frames together using a full join
evidencemap_combined <- reduce(df_map_list, full_join)


#Create new variables and restructure dataframe (tidy)
evidencemap_combined_restructured <- evidencemap_combined %>%
  mutate(cannabis_use = ifelse(node_type_simple %in% c("Moderator", "Level"), node_label, NA)) %>%  
  fill(cannabis_use) %>%
  mutate(primary_studies = ifelse(node_type_simple == "Primary_Study", node_label, NA)) %>%
  mutate(reviews = ifelse(node_type_simple %in% c("SR","MA","SRMA","Review"), node_label, NA))  %>%
  fill(reviews) %>%
  mutate(population = ifelse(node_type_simple == "Population", node_label, NA))%>%
  fill(population) %>%
  mutate(outcome = ifelse(node_type_simple %in% c("Outcome", "Suboutcome"), node_label, NA)) %>% fill(outcome) %>%
  mutate(firstauthor = str_extract(node_label, ".*(?= \\()")) %>%
mutate(year = str_extract(node_label, "\\d{4}")) 

evidencemap_filtered <- evidencemap_combined_restructured %>% filter(node_type_simple == "Primary_Study" & !is.na(firstauthor)) 

evidencemap_tidy<- evidencemap_filtered %>%
mutate(firstauthor = str_replace_all(firstauthor, "et al", "")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Rossler", "Rössler")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Roessler", "Rössler")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Branas", "Brañas")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Degenhart", "Degenhardt")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Barrigon", "Barrigón")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Beaza", "Baeza")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Bushy", "Buchy")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Collizi", "Colizi")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Krebbs", "Krebs")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Mane", "Mané")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Martinez-Arevalo", "Martínez Arévalo")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Pelayo-Teran", "Pelayo-Terán")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Pelayo-Terran", "Pelayo-Terán")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Philips", "Phillips")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "van os", "Van Os")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Wiliiams", "Williams")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Gonzales-Pinto", "González-Pinto")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Setien-Suero", "Setién-Suero")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Caspari D", "Caspari")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Colizi", "Colizzi")) %>%
mutate(firstauthor = str_replace_all(firstauthor, "Schimmelman","Schimmelmann")) %>% 
mutate(firstauthor = str_replace_all(firstauthor, "Arsenault ", "Arseneault")) %>% 
mutate(firstauthor = str_replace_all(firstauthor, "Arsenault","Arseneault")) %>% 
mutate(firstauthor = str_replace_all(firstauthor, "Donoghue", "O’Donoghue")) %>% 
mutate(firstauthor = str_replace_all(firstauthor, "Focking", "Foecking")) %>% mutate(firstauthor = str_replace_all(firstauthor, "Oullette-Plamondon", "Oullett-Plamondon"))%>% 
mutate(firstauthor = str_replace_all(firstauthor, "Focking", "Foecking")) %>% mutate(firstauthor = str_replace_all(firstauthor, "Sorbaca", "Sorbara"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Bloeman", "Bloemen"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Kristensen and Cadenhead", "Kristensen"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Oullett-Plamondon", "Ouellet-Plamondon"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Schimmelmannn", "Schimmelmann"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Tien & Anthony", "Tien"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "van der Meer and Velthorst", "Van der Meer"))



evidencemap_tidy <- evidencemap_tidy %>%
rename(studydesign = node_type)%>%
mutate(
    Topic = as.factor(Topic),
    population = as.factor(population),
    outcome = as.factor(outcome),
    studydesign = as.factor(studydesign),
    cannabis_use = as.factor(cannabis_use)
  )




evidencemap_tidy <- evidencemap_tidy %>% select(population, Topic, primary_studies, firstauthor, year, reviews, cannabis_use, outcome, studydesign, k, N, "I-Squared", effect_size, effect_size_measure, LCI, HCI, "p-value", significance, group_1_size, Rob_label)



# Create the keys_column and broad_topic
evidencemap_tidy <- evidencemap_tidy %>%
  mutate(
    authorww = str_replace_all(firstauthor, "\\s", ""),
    broad_topic = case_when(
      population == "Healthy Population" ~ "HP",
      population == "CHR Population" ~ "CHR",
      population == "Psychosis Population" ~ "P",
      population == "Environmental Moderators" ~ "EnvMod",
      population == "Genetic  Moderators" ~ "GenMod"))

  evidencemap_tidy <- evidencemap_tidy %>% 
  mutate(study_year_psycont = paste(authorww, year, broad_topic, sep = "_"))

  evidencemap_tidy <- evidencemap_tidy %>% 
  mutate(studycode = paste(authorww, year, sep = "_"))
  


evidencemap_tidy$studydesign <- tolower(evidencemap_tidy$studydesign)

evidencemap_tidy$study_year_psycont<- tolower(evidencemap_tidy$study_year_psycont)

evidencemap_tidy$studycode<- tolower(evidencemap_tidy$studycode)

evidencemap_tidy <- evidencemap_tidy %>%
  mutate(studydesign = recode(studydesign, "cohorty" = "cohort"))

evidencemap_tidy <- evidencemap_tidy %>%
  mutate(prospective_cohort = case_when(
    studydesign == "prospective cohort" ~ "yes",
    studydesign %in% c("longitudinal", "cohort") ~ "maybe",
    TRUE ~ "no"
  ))


evidencemap_tidy <- evidencemap_tidy %>%
mutate(studycode = str_replace_all(studycode, "schimmelmann_2011", "schimmelmann_2012")) %>%
mutate(studycode = str_replace_all(studycode, "seddon_2015", "seddon_2016"))%>%
mutate(studycode = str_replace_all(studycode, "hadden_2016", "hadden_2018"))%>%
mutate(studycode = str_replace_all(studycode, "emsley_2019", "emsley_2020"))%>%
mutate(studycode = str_replace_all(studycode, "bloemen_2010", "bloemen_2009"))%>%
mutate(studycode = str_replace_all(studycode,  "bhattacharya_2020", "patel_2016"))


 evidencemap_tidy <- evidencemap_tidy %>%
  group_by(studycode) %>%
  mutate(PublicationID = cur_group_id())

evidencemap_tidy <- evidencemap_tidy %>%
  mutate(unique = ifelse(duplicated(PublicationID), 1, 0))

  evidencemap_tidy <- evidencemap_tidy %>%
  mutate(unique_study_pop= ifelse(duplicated(study_year_psycont), 1, 0))



evidencemap_tidy <- evidencemap_tidy %>%
 mutate(potstudies = case_when(
    studydesign == "prospective cohort" ~ 1,
    studydesign %in% c("longitudinal", "cohort") ~ 1,
    TRUE ~ 0))

#merge data with the manually checked data info
studies_to_check <- read_excel("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/manuallycheckeddata/studies_to_check_JMG_10.06.xlsb.xlsx")


evidencemap_tidy <- evidencemap_tidy %>%
  left_join(studies_to_check %>% select(studycode,Exclusion_coded,citation), by = "studycode")

#take the values from the exclusion coded and citation and fill them in in those cases with the same studycode
evidencemap_tidy <- evidencemap_tidy %>%
  group_by(studycode) %>%
  fill(Exclusion_coded, citation, .direction = "downup")

# Write evidencemap_studylist to an Excel file
write.xlsx(evidencemap_tidy, "02_data/cleandata/evidencemap_tidy.xlsx")

evidencemap_tidy_publicationunique <- evidencemap_tidy %>%
  distinct(PublicationID, .keep_all =TRUE)

idstocheck <- evidencemap_tidy_publicationunique %>%
select(studycode, `cannabis_use`, outcome, PublicationID)

# Write evidencemap_studylist to an Excel file
write.xlsx(idstocheck, "02_data/cleandata/idstocheck.xlsx")

names(evidencemap_tidy)

