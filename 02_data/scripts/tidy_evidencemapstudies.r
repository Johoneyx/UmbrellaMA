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

#read in dataframes fro the evidencemap excel
df_HP_map <- as.data.frame((read_xlsx("C:/Users/johan/Documents/PhD/Umbrella_Meta-Analysis/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =1)))
df_CHR_map <- as.data.frame((read_xlsx("C:/Users/johan/Documents/PhD/Umbrella_Meta-Analysis/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =2)))
df_P_map <- as.data.frame((read_xlsx("C:/Users/johan/Documents/PhD/Umbrella_Meta-Analysis/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =3)))
df_Mod_map <- as.data.frame((read_xlsx("C:/Users/johan/Documents/PhD/Umbrella_Meta-Analysis/02_data/rawdata/evidencemapdata_Feb24.xlsx", sheet =4)))

df_map_list <-list(df_HP_map = df_HP_map, df_CHR_map = df_CHR_map, df_P_map = df_P_map, df_Mod_map = df_Mod_map) 

# Convert all variables in the data frames to character type
df_map_list <- lapply(df_map_list, function(df) {
  df[] <- lapply(df, as.character)
  return(df)
})

# Join all the data frames together using a full join
evidencemap_combined <- reduce(df_map_list, full_join)

summary(evidencemap_combined)
view(evidencemap_combined)

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
  mutate(author = str_extract(node_label, ".*(?= \\()"), year = str_extract(node_label, "(?<=\\()\\d{4}(?=\\))"),
    .keep = "unused") %>%
  mutate_all(as.factor)

view(evidencemap_combined_restructured)
names(evidencemap_combined_restructured)


evidencemap_studylist <- evidencemap_combined_restructured %>% filter(node_type_simple == "Primary_Study" & !is.na(author)) %>%
mutate(author = str_replace_all(author, "et al", "")) %>%
rename(studydesign = node_type) 


evidencemap_studylist <- evidencemap_studylist %>%
select(population,Topic,primary_studies,author,year,reviews,cannabis_use,outcome,studydesign,k,N,"I-Squared",effect_size,effect_size_measure,LCI,HCI, "p-value",significance,group_1_size,Rob_label)

names(evidencemap_studylist)

# Create the keys_column
evidencemap_studylist <- evidencemap_studylist %>%
  mutate(population = case_match(
    population,
    "Healthy Population" ~ "HP",
    "CHR Population" ~ "CHR",
    "Psychosis Population" ~ "P",
    "Environmental Moderators" ~ "EnvMod",
    "Genetic  Moderators" ~ "GenMod"
  ), 
  authorww = str_replace_all(author, "\\s", ""),
  keys_column = paste(authorww, year, population, sep = "_")
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

view(evidencemap_studylist_unique)
  summary(evidencemap_studylist_unique)


# Write evidencemap_studylist_unique to an Excel file
write.xlsx(evidencemap_studylist_unique, "evidencemap_studylist_unique.xlsx")

# Write evidencemap_studylist to an Excel file
write.xlsx(evidencemap_studylist, "evidencemap_studylist.xlsx")


