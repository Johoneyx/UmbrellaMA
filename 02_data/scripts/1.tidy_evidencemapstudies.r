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


View(evidencemap_filtered)

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
mutate(firstauthor = str_replace_all(firstauthor, "Focking", "Foecking")) %>% 
mutate(firstauthor = str_replace_all(firstauthor, "Oullette-Plamondon", "Oullett-Plamondon"))%>% 
mutate(firstauthor = str_replace_all(firstauthor, "Sorbaca", "Sorbara"))%>%
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


#give the same studycode (firstauthor_year) the same number
 evidencemap_tidy <- evidencemap_tidy %>%
  group_by(studycode) %>%
  mutate(PublicationID = cur_group_id())%>%
  arrange(studycode)

View(evidencemap_tidy)


#**************************************************************Create_Studylist***********************************************************
# here I created a list of all the studies with info on how the 
#shortform

evidencemap_studylist <- evidencemap_tidy %>%
  group_by(studycode) %>%
  summarize(
    studydesign = paste(unique(studydesign), collapse = "; "),
    outcomes = paste(unique(Topic), collapse = "; "),
    populations = paste(unique(population), collapse = "; "),
    cannabis_levels = paste(unique(cannabis_use), collapse = "; "),
    reviews = paste(unique(reviews), collapse = "; ")
  ) %>%
  ungroup()

View(evidencemap_studylist)

#*****************************************************************************************************************************************

#code potential prospective longitudinal cohort studies 


evidencemap_studylist <- evidencemap_studylist %>%
mutate(prospective_longitudinal = case_when(
    str_detect(studydesign, "prospective cohort") ~ "yes",
    str_detect(studydesign, "longitudinal|cohort") ~ "maybe",
    TRUE ~ "no"
  ))


View(evidencemap_studylist)


table(evidencemap_studylist$Exclusion_coded)


#I checked some of the studies manually already
studies_to_check <- read_excel("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/manuallycheckeddata/studies_to_check_JMG_10.06.xlsb.xlsx")

#join with the studylist 
evidencemap_studylist <- evidencemap_studylist %>%
left_join(studies_to_check %>% select(studycode,Exclusion_coded,citation,Comment,Check), by = "studycode") 


evidencemap_studylist <- evidencemap_studylist %>%
mutate(Exclusion_coded = ifelse(prospective_longitudinal == "no", "not prospective longitudinal", Exclusion_coded))


studies_to_check <- read_excel("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/manuallycheckeddata/studies_to_check_JMG_10.06.xlsb.xlsx")


df_studylist_cohort_unique_extracted <- read_excel("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/cohort_df_clean.xlsx")


df_extracted_short <- df_studylist_cohort_unique_extracted %>%
  group_by(studycode) %>%
  select(studycode, study_type, `prospective?`) %>%
  summarize(
    studytype_extracted = paste(unique(study_type), collapse = "; "),
    prospective_extracted = paste(unique(`prospective?`), collapse = "; ")
  ) %>%
  ungroup()


View(df_extracted_short)


evidencemap_studylist <- evidencemap_studylist %>%
left_join(df_extracted_short, by="studycode")

View(evidencemap_studylist)

#**********************************get_info_from_extraction***********


filtered_studies <- evidencemap_studylist %>%
  filter(is.na(Exclusion_coded) & prospective_longitudinal == "maybe")


filtered_studies <- filtered_studies %>%
left_join(df_extracted_short, by="studycode")

View(filtered_studies)










































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



evidencemap_tidy <- evidencemap_tidy %>%
mutate(excluded = ifelse(!is.na(Exclusion_coded), 1, 0))


# Write evidencemap_studylist to an Excel file
write.xlsx(evidencemap_tidy, "02_data/cleandata/evidencemap_tidy.xlsx")

evidencemap_tidy_publicationunique <- evidencemap_tidy %>%
  distinct(PublicationID, .keep_all =TRUE)

idstocheck <- evidencemap_tidy_publicationunique %>%
select(studycode, `cannabis_use`, outcome, PublicationID)

# Write evidencemap_studylist to an Excel file
write.xlsx(idstocheck, "02_data/cleandata/idstocheck.xlsx")


studieswithoutstudytype <- evidencemap_tidy_publicationunique %>%
filter(is.na(studydesign))

write.xlsx(studieswithoutstudytype, "02_data/cleandata/withoutstudydesign.xlsx")