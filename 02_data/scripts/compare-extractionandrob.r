library(readxl)
library(dplyr)
library(stringdist)
library(tidyr)
library(writexl)



merged_dt_rob<- read_excel("02_data/cleandata/merged_dt_rob_clean.xlsx")
cohort_df_clean<- read_excel("02_data/cleandata/cohort_df_clean.xlsx")

merged_dt_rob <- merged_dt_rob %>%
mutate(studycode = tolower(studycode))



cohort_df_code_unique <- cohort_df_clean %>%
  distinct(studycode, .keep_all = TRUE)

merged_df_code_unique <-  cohort_df_code_unique  %>%
  mutate(
    rob_done = ifelse(studycode %in% merged_dt_rob$studycode, "yes", "no"),
    closest_match = ifelse(rob_done == "no", merged_dt_rob$studycode[stringdist::amatch(studycode, merged_dt_rob$studycode, maxDist = Inf)], NA)
  )

write_xlsx(merged_df_code_unique, "02_data/cleandata/dataextraction_rob_comparision.xlsx")

View(merged_df_code_unique)

table(merged_df_code_unique$rob_done)


df_authorfreq<-as.data.frame(table(merged_df_code_unique$firstauthor))

View(df_authorfreq %>% arrange(Freq))

doubleauthorlist <- 
c("addington", "andreasson", "auther", "barrowclough", "buchy", "dragt", "fergusson","nieman","zammit")

if studycode !%in% doubleauthorlist {
 
}

gage, corcoran, bugra, ariashorcajadas, baeza, setien-suero_2019, barrowclough, collizzi, fond_2019, foti_2010, hadden_2018, hides_2006, addington, atkinson, berge, schimmelmann,emsley, marino, oullet-plamondon, rentero, roemrtjomsen_2018, schoeler, clausen 

