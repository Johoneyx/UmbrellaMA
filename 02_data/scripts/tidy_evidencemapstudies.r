library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(metafor)
library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)
library(stringr)

df_HP_map <- read_xlsx("C:/Users/johan/Documents/PhD/Umbrella_Meta-Analysis/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =1) %>%
  as.data.frame() %>%
  filter(rowSums(is.na(.)) != ncol(.))

df_CHR_map <- read_xlsx("C:/Users/johan/Documents/PhD/Umbrella_Meta-Analysis/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =2) %>%
  as.data.frame() %>%
  filter(rowSums(is.na(.)) != ncol(.))

df_P_map <- read_xlsx("C:/Users/johan/Documents/PhD/Umbrella_Meta-Analysis/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =3) %>%
  as.data.frame() %>%
  filter(rowSums(is.na(.)) != ncol(.))

df_Mod_map <- read_xlsx("C:/Users/johan/Documents/PhD/Umbrella_Meta-Analysis/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =4) %>%
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




#Create new variables (restructure dataframe)
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
mutate(year = str_extract(node_label, "(?<=\\()\\d{4}(?=\\w\\))"))

#exclude Van Os (2009) and Linscott (2013) as primary studies were not mentioned 
#evidencemap_combined_restructured<- evidencemap_combined_restructured %>%
  #filter(reviews != "Van Os(2009)", reviews != "Linscott (2013)")




evidencemap_studylist <- evidencemap_combined_restructured %>% filter(node_type_simple == "Primary_Study" & !is.na(firstauthor)) %>%
mutate(firstauthor = str_replace_all(firstauthor, "et al", "")) %>%
mutate(firstauthor = str_replace_all(firstauthor, c("Rossler","Rössler", "Roessler"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Branas", "Brañas"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Degenhart", "Degenhardt"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Barrigon", "Barrigón"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Beaza", "Baeza"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Bushy", "Buchy"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Collizi", "Colizi"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Krebbs", "Krebs"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Mane", "Mané"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Martinez-Arevalo", "Martínez Arévalo"))%>%
mutate(firstauthor = str_replace_all(firstauthor,c("Pelayo-Teran","Pelayo-Terran" ) , "Pelayo-Terán")%>%
mutate(firstauthor = str_replace_all(firstauthor, "Philips", "Phillips"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "van os", "Van Os"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Wiliiams", "Williams"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Gonzales-Pinto", "González-Pinto"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Setien-Suero", "Setién-Suero"))%>%
mutate(firstauthor = str_replace_all(firstauthor, "Caspari D", "Caspari")) %>%
rename(studydesign = node_type)

evidencemap_studylist <- evidencemap_studylist %>% select(population, Topic, primary_studies, firstauthor, year, reviews, cannabis_use, outcome, studydesign, k, N, "I-Squared", effect_size, effect_size_measure, LCI, HCI, "p-value", significance, group_1_size, Rob_label)



# Create the keys_column and broad_topic
evidencemap_studylist <- evidencemap_studylist %>%
  mutate(
    authorww = str_replace_all(firstauthor, "\\s", ""),
    keys_column = paste(authorww, year, population, sep = "_"),
    broad_topic = case_when(
      population == "Healthy Population" ~ "HP",
      population == "CHR Population" ~ "CHR",
      population == "Psychosis Population" ~ "P",
      population == "Environmental Moderators" ~ "EnvMod",
      population == "Genetic  Moderators" ~ "GenMod"
    )
  )


###factorize 
# Define the function
lower_and_factorize <- function(x) {
  x <- tolower(x)
  levels(as.factor(x))
}


evidencemap_studylist$studydesign<-tolower(evidencemap_studylist$studydesign)
levels(as.factor(evidencemap_studylist$studydesign))

evidencemap_studylist <- evidencemap_studylist %>%
  mutate(studydesign = recode(studydesign, "cohorty" = "cohort"))

evidencemap_studylist_unique <- evidencemap_studylist %>%
  distinct(keys_column, .keep_all = TRUE)


# Write evidencemap_studylist_unique to an Excel file
write.xlsx(evidencemap_studylist_unique, "02_data/cleandata/evidencemap_studylist_unique.xlsx")

# Write evidencemap_studylist to an Excel file
write.xlsx(evidencemap_studylist, "02_data/cleandata/evidencemap_studylist.xlsx")



studylist_yearNA <- evidencemap_studylist %>%
  filter(is.na(year))
  View(studylist_yearNA)

