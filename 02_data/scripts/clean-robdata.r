library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)

df_rob_merged<- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/mergeddata/merged_dt_rob.xlsx")

df_rob_merged <-df_rob_merged  %>% 
mutate(firstauthor = ifelse(str_detect(Primary, "et al"), 
str_extract(Primary, ".*(?= et al)"), 
str_extract(Primary, ".*(?=\\()"))) %>%
mutate(First_author = str_replace_all(firstauthor, "Schibart", "Schubart"))%>%
mutate(First_author = str_replace_all(firstauthor,"romer thomsen","rømerthomsen"))

# Create a new variable "Year"
df_rob_merged <- df_rob_merged %>% 
  mutate(Year = str_extract(Primary, "\\b\\d{4}\\b"))


# Johannas robs have already studycode, only fill if na
df_rob_merged$studycode <- paste(gsub("\\s", "", df_rob_merged$firstauthor), df_rob_merged$Year, sep = "_")


df_rob_merged <- df_rob_merged %>%
  mutate(studycode = tolower(studycode))

View(df_rob_merged)




#*****************************manually checked the following years*************************************

#only those that are cohort or prospective longitudinal for now
#i decided to take the years of those 

df_rob_merged_clean <-df_rob_merged%>% 
mutate(studycode = str_replace_all(studycode, "koning_na", "koning_2008"))%>%
mutate(studycode = str_replace_all(studycode,"miettunen_na","miettunen_2008"))%>%
mutate(studycode = str_replace_all(studycode,"wiles_na","wiles_2006"))%>%
mutate(studycode = str_replace_all(studycode,"riecher-rössler_na","riecher-rössler_2009"))%>%
mutate(studycode = str_replace_all(studycode,"bloemen_na","bloemen_2009"))%>%
mutate(studycode = str_replace_all(studycode,"devylder_na","devylder_2014"))%>%
mutate(studycode = str_replace_all(studycode,"phillips_na","philips_2002"))%>%
mutate(studycode = str_replace_all(studycode,"gill_na","gill_2015"))%>%
mutate(studycode = str_replace_all(studycode,"vantricht_na","vantricht_2013"))%>%
mutate(studycode = str_replace_all(studycode,"vandijk_na","vandijk_2012"))%>%
mutate(studycode = str_replace_all(studycode,"addingtonandaddington_na","addingtonandaddington_2007"))%>%
mutate(studycode = str_replace_all(studycode,"bechtold_na","bechtold_2015"))%>%
mutate(studycode = str_replace_all(studycode,"berger_na","berger_2016"))%>%
mutate(studycode = str_replace_all(studycode,"bersani_na","bersani_2002"))%>%
mutate(studycode = str_replace_all(studycode,"bousman_na","bousman_2013"))%>%
mutate(studycode = str_replace_all(studycode,"bugra_na","bugra_2013"))%>%
mutate(studycode = str_replace_all(studycode,"ferdinand_na","ferdinand_2005"))%>%
mutate(studycode = str_replace_all(studycode,"fergusson_na","fergusson_2003"))%>%
mutate(studycode = str_replace_all(studycode,"henquet_na","henquet_2005"))%>%
mutate(studycode = str_replace_all(studycode,"hides_na","hides_2006"))%>%
mutate(studycode = str_replace_all(studycode,"mackie_na","mackie_2013"))%>%
mutate(studycode = str_replace_all(studycode,"machielsen_na","machielsen_2010"))%>%
mutate(studycode = str_replace_all(studycode,"velthorst_na","velthorst_2009"))%>%
mutate(studycode = str_replace_all(studycode,"ziermans_na","ziermans_2011"))%>%
mutate(studycode = str_replace_all(studycode,"valmaggia_na","valmaggia_2014"))%>%
mutate(studycode = str_replace_all(studycode,"riecher-rossler_na","riecher-rössler_2009"))%>%
mutate(studycode = str_replace_all(studycode,"mustonen_na","mustonen_2018"))%>%
mutate(studycode = str_replace_all(studycode,"kristensen_na","kristensen_2007"))%>%
mutate(studycode = str_replace_all(studycode,"korver_na","korver_2010"))%>%
mutate(studycode = str_replace_all(studycode,"kuepper_na","kuepper_2011"))%>%
mutate(studycode = str_replace_all(studycode,"gage_na","gage_2014"))%>%
mutate(studycode = str_replace_all(studycode,"leadbeater_na","leadbeater_2019"))%>%
mutate(studycode = str_replace_all(studycode,"tien_na","tien_1990"))%>%
mutate(studycode = str_replace_all(studycode,"mcgrath_na","mcgrath_2010"))%>%
mutate(studycode = str_replace_all(studycode,"vanos_na","vanos_2002"))%>%
mutate(studycode = str_replace_all(studycode,"labad_na","labad_2015"))%>%
mutate(studycode = str_replace_all(studycode,"lavoie_na","lavoie_2014"))%>%
mutate(studycode = str_replace_all(studycode,"foecking_na","foecking_2016"))%>%
mutate(studycode = str_replace_all(studycode,"seidman_na","seidman_2016"))%>%
mutate(studycode = str_replace_all(studycode,"koning_2008","konings_2012"))%>%
mutate(studycode = str_replace_all(studycode,"emsley_2019","emsley_2020"))%>%
mutate(studycode = str_replace_all(studycode,"hadden_2016","hadden_2018"))%>%
mutate(studycode = str_replace_all(studycode,"manrique-garcia_na","manrique-garcia_2012"))%>%
mutate(studycode = str_replace_all(studycode,"martinez-arevalo_1994","martínezarévalo_1994"))%>%
mutate(studycode = str_replace_all(studycode,"romerthomsen_2018","rømerthomsen_2018"))%>%  
mutate(studycode = str_replace_all(studycode,"rossler_na","rössler_2012"))%>%
mutate(studycode = ifelse(str_detect(Primary, "Schoeler et al\\. \\(2016a\\)"), "schoeler_2016", studycode))%>%
mutate(studycode = ifelse(str_detect(Primary, "Schoeler et al\\. \\(2016b\\)"), "schoeler_2016b", studycode))%>%
mutate(studycode = ifelse(str_detect(Primary, "Addington and Addington"), "addington_2007", studycode))%>%
mutate(studycode = ifelse(str_detect(Review, "Hosseini") & str_detect(Primary, "Zammit et al"), "zammit_2002", studycode))%>%
mutate(studycode = ifelse(str_detect(Review, "Oliver") & str_detect(Primary, "Nieman"), "nieman_2014", studycode))%>%
mutate(studycode = ifelse(str_detect(Review, "Carney") & str_detect(Primary, "Nieman"), "nieman_2016", studycode))%>%
mutate(studycode = str_replace_all(studycode,"mchugh_na","mchugh_2017"))%>%
mutate(studycode = str_replace_all(studycode,"philips_2002","phillips_2002"))%>%
mutate(studycode = str_replace_all(studycode,"stefanis_na","stefanis_2004"))%>%
mutate(studycode = str_replace_all(studycode,"corcoran_na","corcoran_2008"))%>%
mutate(studycode = str_replace_all(studycode,"mizrahi_na","mizrahi_2014"))%>%
mutate_all(~ str_replace_all(., "\\*", "1"))


View(df_rob_merged_clean)

#corcoran existiert nicht
#dragt existiert nicht
#mizrahi 2014 existiert nicht
#buchy 2015 existiert nicht
#henquet 2004 und 2005 ist glaube ich gleiche studie 
#buchy 2015 gibts nicht nur 2016 
#weiser 2002 gibt es nicht
#arseneault 2002 gibts nicht
#zammit 2011 Le Bec (2009); Moore (2007); Kiburi (2021); Vaessen (2018); van der Steur(2020) gibts nicht, oder ist das gelabelt unter hosseini
#baeza 2009 gibts nicht, hab ich das irgendwo extrahiert?
#schimmelmann 2012 does not exist in roblist
#ariashorcajadas does not exist in roblist
#barrowclough 2013,2015 do not exist 
#there are two schoelers but as values are the same i just choose one of them 
#fond doesnt exist
#foti doesnt exist

#*****************************impute years*************************************************************

#evidencemap_studylist <- read_xlsx("02_data/cleandata/evidencemap_studylist.xlsx")


#evidencemap_studylist <- evidencemap_studylist %>%mutate(reviews = str_replace(reviews, "(\\w+)(\\()", "\\1 \\2"))


# join and create Year_filledin variable that takes the imputed year values from the merged df
 #merged_dt_rob<- df_rob_merged %>%
 # left_join(evidencemap_studylist, by = c("First_author" = "authorww", "Review" = "reviews")) %>%
  #mutate(Year_filledin = ifelse(is.na(Year), year, Year)) 

 # View(merged_dt_rob)

#merged_dt_rob <- merged_dt_rob %>%
  #mutate(studycode = ifelse(is.na(studycode), paste0(First_author, "_", Year_filledin), studycode))


#************************************************************************************************


#clean carolinas make * to 1


write_xlsx(df_rob_merged_clean, "02_data/cleandata/df_rob_clean.xlsx")



