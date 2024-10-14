library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(metafor)
library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)

#********************************************************Clean_and_Tidy_Evidencemap_Network_Studies**************************************************************************************

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
} )

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



#resulting dataframe evidencemap tidy includes all the systematic reviews and meta-analyses with all their primary studies listed so that duplicates are included 
#studycode = author_year
write.xlsx(evidencemap_tidy, "02_data/cleandata/evidencemap_tidy.xlsx")





#**************************************************************Create_Studylist***********************************************************
# here I created a list of all the studies collapsing those with the same studycode
# doesnt include stats info
# only includes studydesign, outcomes, populations, levels, reviews
# as the reviews gave different information on these, they may have several different values separated by semicolons 
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

#code potential prospective longitudinal cohort studies
#maybe are those studies where the authors said they are longitudinal or a cohort study but its not really sure whether those are really prospective
evidencemap_studylist <- evidencemap_studylist %>%
mutate(prospective_longitudinal = case_when(
    str_detect(studydesign, "prospective cohort") ~ "yes",
    str_detect(studydesign, "longitudinal|cohort") ~ "maybe",
    TRUE ~ "no"
  ))


View(evidencemap_studylist)


#**************************************************Combine with manually checked Info *************************************************
#those that are labelled maybe or even have no info at all on studytype need to be manually checked

#I checked some of the studies manually already some time ago
studies_to_check <- read_excel("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/manuallycheckeddata/studies_to_check_JMG_10.06.xlsb.xlsx")

#join with the studylist 
evidencemap_studylist <- evidencemap_studylist %>%
left_join(studies_to_check %>% select(studycode,Exclusion_coded,citation,Comment,Check), by = "studycode") 

view(evidencemap_studylist)


evidencemap_studylist <- evidencemap_studylist %>%
mutate(Exclusion_coded = ifelse(prospective_longitudinal == "no", "not prospective longitudinal", Exclusion_coded)) %>%
mutate(Exclusion_coded = ifelse(str_detect(cannabis_levels,"cannabis-induced psychotic disorder") , "no relevant predictor", Exclusion_coded)) %>% 
mutate(Exclusion_coded = ifelse(studycode =="henquet_2004" , "wrong citation", Exclusion_coded))  


table(evidencemap_studylist$Exclusion_coded)

#in the manual check I also excluded some studies that are double-publications(this means that the same cohort has been published in several different papers. I would decide either based on whether the number of participants is greater, or whether the reporting format of the statisticla analyses is more suitable for our purposes
#reasons for exclusion: not prospective longitudinal, no relevant outcome, double-publication

included_studies <- evidencemap_studylist %>%
filter(is.na(evidencemap_studylist$Exclusion_coded))

#N:147
view(included_studies)

#**************************************************************READ_IN_AND_CLEAN_EXTRACTED_DATA*************************************************

#which one is the right one?
#df_extracted is not fully cleaned as it has all the studies extracted not just the prospective longitudinals. i wrote a script to clean the prospective longitudinals. i can run that over the merged_df_clean later so that all the variables are cleaned a bit better
df_extracted <- read_excel("02_data/cleandata/merged_df_clean.xlsx")

View(df_extracted)

#choose the relevant variables that interest me now to get an overview and collapse them (as there are different rows for the timepoints and statistical datapoints that have been extracted)
df_extracted_short <- df_extracted %>%
  group_by(studycode) %>%
  select(all_of(c("studycode", "prospective?", "comment", "relevant", "any issues?", "titel", "cohort", "cohort more detail", "dataextraction", "extracted by", "doublechecked by", "citation", "reference", "followup duration", "follow-up duration", "kind of psychosis", "fep vs chronic", "cannabis level of use", "comparision(control-group)", "time frame (cannabis use and outcome measure time)", "time frame", "cannabis use timeframe", "continuation of use timeframe", "continued use", "discontinued use", "cannabis measure"))) %>%
  summarize(
    prospective_extracted = paste(unique(`prospective?`), collapse = "; "),
    comment_extraction = paste(unique(c(comment, relevant, `any issues?`)), collapse = "; "),
    paper_title = paste(unique(titel), collapse = "; "),
    cohort_combined = paste(unique(c(cohort, `cohort more detail`)), collapse = "; "),
    #country = paste(unique(country), collapse = "; "),
    #target_population = paste(unique(population), collapse = "; "),
    extracted_doublechecked = paste(unique(c(dataextraction, `extracted by`, `doublechecked by`)), collapse = "; "),
    reference = paste(unique(c(citation, reference)), collapse = "; "),
    `follow-up` = paste(unique(c(`followup duration`, `follow-up duration`)), collapse = "; "),
    psychosis_type = paste(unique(c(`kind of psychosis`, `fep vs chronic`)), collapse = "; "),
    cannabis_levels = paste(unique(c(`cannabis level of use`, `comparision(control-group)`, `time frame (cannabis use and outcome measure time)`,`time frame`, `cannabis use timeframe`, `continuation of use timeframe`, `continued use`, `discontinued use`, `cannabis measure`)), collapse = "; ")
  ) %>%
  ungroup()

#[8] "diagnostic tool"

View(df_extracted_short)
names(df_extracted_short)

#----------------------------------------MERGE STUDYLIST WITH EXTRACTED DATA ------------------------------------------------------
evidencemap_studylist <- evidencemap_studylist %>%
left_join(df_extracted_short, by="studycode")

View(evidencemap_studylist)

#only look at those studies that are not excluded
evidencemap_studylist %>%
filter(is.na(Exclusion_coded))%>%
view() 

#only those that are not yet extracted

evidencemap_studylist %>%
filter(is.na(Exclusion_coded) & is.na(extracted_doublechecked) & is.na(TotalStars)) %>%
view()


#---------------------------------------Read in and collapse robdata --------------------------------------------------------------------

#it seems like we extracted some of the rob double so I collapse those values to have a unique rob. worth doublechecking values that differ later
df_rob_extracted <- read_excel("02_data/cleandata/df_rob_clean.xlsx")

df_rob_extracted <- df_rob_extracted %>% 
group_by(studycode) %>% 
summarize(
  studytype_ROB = paste(unique(`Study type`), collapse = "; "),
  Q1 = paste(unique(`Q1`), collapse = "; "),
  Q2 = paste(unique(`Q2`), collapse = "; "),
  Q3 = paste(unique(`Q3`), collapse = "; "),
  Q4 = paste(unique(`Q4`), collapse = "; "),
  Q5 = paste(unique(`Q5`), collapse = "; "),
  Q6 = paste(unique(`Q6`), collapse = "; "),
  Q7 = paste(unique(`Q7`), collapse = "; "),
  Q8 = paste(unique(`Q8`), collapse = "; "),
  TotalStars = paste(unique(`TotalStars`), collapse = "; "),
)%>% 
view()

#---------------------------------------Merge STUDYLIST WITH ROB DATA ---------------------------------------------------------------------

evidencemap_studylist <- evidencemap_studylist %>%
  left_join(df_rob_extracted, by="studycode")

view(evidencemap_studylist)


evidencemap_studylist %>%
filter(is.na(Exclusion_coded) & is.na(extracted_doublechecked) & is.na(TotalStars)) %>%
view()


studylist_meta_analysis <- evidencemap_studylist %>%
filter(is.na(Exclusion_coded))%>%
arrange(populations)%>%
view()

write_xlsx(studylist_meta_analysis, "02_data/cleandata/studylist_meta-analysis.xlsx")

#check the data that has been extracted but doesnt have any rob data. Either it has not been extracted, or it has been extracted and is somewhere on the other pc, or the cleaning didnt include the data and i need to recheck the script 
# 6.10.2024
#bei 27 robs scheinen die Jahre falsch zu sein oder falsch geschrieben 2-3 scheinen nicht extrahiert worden zu sein (e.g. emsley, barrowclough)
# koennte daran liegen das am ende gar nicht relevant, weil doch niciht prospective longitudinal
# muss herausfinden ob wirklich prospective longitudinal 
#konings koning 
#hatte ich das nihct mal vor ewigkeiten kontrolliert? 
#ich koennte schauen ob das im citavi steht


#corcoran 2008

#dragt 2011

#mizrahi 2014

#dragt 2011

#buchy 2015

#weiser 2002

#bechtold 2016

#arseneault 2002

#zammit 2011

#baeza 2009

#schimmelmann 2012

#ariashorcajadas 2002

#barrowlclough 2013

#barrowclough 2015

#fond2019

#foti2010



















#----------------------------------this part is for the flowchart, check later--------------------------------

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

write_xlsx(studieswithoutstudytype, "02_data/cleandata/withoutstudydesign.xlsx")



