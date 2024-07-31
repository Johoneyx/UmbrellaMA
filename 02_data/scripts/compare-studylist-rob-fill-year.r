library(readxl)
library(dplyr)
library(stringdist)
library(tidyr)
library(writexl)
library(stringr)


merged_dt_rob<- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/merged_dt_rob_clean.xlsx")

studylist_cohort <- read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/df_studylist_cohort_unique.xlsx")

View(df_rob_merged)

df_rob_merged <- merged_dt_rob %>%
select(-firstauthor)



studylist_cohort <- studylist_cohort %>%
  mutate(review_sl = str_replace_all(str_trim(tolower(reviews)), "\\s+", "")) %>%
  mutate(firstauthor_sl = tolower(firstauthor))%>%
  mutate(firstauthor_sl = str_replace_all(firstauthor_sl,"riecher-rossler", "riecher-rössler")) %>%
  mutate(firstauthor_sl = str_replace_all(firstauthor_sl,"rossler", "rössler"))


View(studylist_cohort)

df_rob_merged <- df_rob_merged %>%
  mutate(review_rob = str_replace_all(str_trim(tolower(Review)), "\\s+", "")) %>%
  mutate(firstauthor_rob = tolower(First_author)) %>%
  mutate(firstauthor_rob = str_replace_all(firstauthor_rob, "riecher-rossler", "riecher-rössler")) %>%
  mutate(firstauthor_rob = str_replace_all(firstauthor_rob,"rossler", "rössler"))

View(df_rob_merged)


df_rob_list_merged <- studylist_cohort %>%
  left_join(df_rob_merged, by = c("firstauthor_sl" = "firstauthor_rob"), relationship = "many-to-many")

View(df_rob_list_merged)

# Update the year column in merged_dt_rob with the year values from studylist_cohort
merged_dt_rob <- df_rob_list_merged %>%
  mutate(year_merged = coalesce(Year, year))


View(merged_dt_rob[is.na(merged_dt_rob$year_merged),])

merged_dt_rob <- merged_dt_rob %>% 
mutate(studycode_merged = paste(firstauthor_rob, year_merged, sep="_"))

View(merged_dt_rob)

df_rob <- merged_dt_rob %>%
  select(Review, Primary, "Study type",studycode=studycode_merged, year = year_merged, Q1, Q2, Q3, Q4, Q5, Q6,  Q7, Q8, TotalStars)
View(df_rob)


write_xlsx(df_rob,"C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/df_rob_withcompletestudycode.xlsx" )

  