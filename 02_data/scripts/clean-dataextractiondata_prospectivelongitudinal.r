library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)


merged_df_clean <- (read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/merged_df_clean.xlsx"))

studylist_cohort <- (read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/df_studylist_cohort_unique.xlsx"))

merged_df_clean <- merged_df_clean %>% 
mutate(relevant = ifelse(studycode %in% studylist_cohort$studycode, 1, 0 ))

table(merged_df_clean$relevant)

df_plcohort <- merged_df_clean %>% 
  filter(relevant == 1)

names(df_plcohort)

#clean cannabis use data

#clean lifetime variable 
df_plcohort$"lifetime cannabis use" <-  tolower(df_plcohort$"lifetime cannabis use")


sort(levels(as.factor(df_plcohort$"lifetime cannabis use")))

df_plcohort <- df_plcohort %>%
  mutate(`lifetime cannabis use` = recode(`lifetime cannabis use`, "age of use" = "age of onset", "current cannabis use" = "current", "current use"="current", "last 12-month use" = "last 12 months","last 6-month use"= "last 6 months","last year use"= "last 12 months", "lifetime cannabis use" = "lifetime", "lifetime use" = "lifetime", "past month" = "last month", "past 30 days cannabis use" = "last month", "past year use" = "last 12 months", "prior six months" = "last 6 months", "recent use (last month)" = "last month", "last 12-month cannabis use" = "last 12 months", "cannabis use in previous 90 days" = " last 3 months", "last month use" = "last month"  ))
         

levels(as.factor(df_plcohort$"lifetime cannabis use"))




df_plcohort <- df_plcohort %>%
  mutate(`cannabis level of use` = recode(`cannabis level of use`, ocassiaonl = "occasional"))
  
#seeing it with the other cannabis-related variables together will can let me know if they are talking about current or lifetime use 
#find all values that have time something following and ersetze diese durch (times)
#ersetze alle cannabis_use durch use_

#clean level of use variable
df_plcohort$"cannabis level of use" <-  tolower(df_plcohort$"cannabis level of use")

df_plcohort <- df_plcohort %>%
# Create level_coded variable based on "cannabis level of use"
  mutate(level_coded = `cannabis level of use`) %>%
  # Remove all round brackets
  mutate(level_coded = str_replace_all(level_coded, "\\(|\\)", ""))%>%
 # Replace "less than" with "<" and "at least" with ">"
  mutate(level_coded = str_replace_all(level_coded, "less than", "<")) %>%
  mutate(level_coded = str_replace_all(level_coded, "at least", ">="))%>%
# Replace "per week" with "/week" and "prior to" with "<"
  mutate(level_coded = str_replace_all(level_coded, "per week", "/week")) %>%
  mutate(level_coded = str_replace_all(level_coded, "prior to", "<"))
# Replace "weekly" with "/week", "monthly" with "/month", and "daily" with "/day"
df_plcohort <- df_plcohort %>%
  mutate(level_coded = str_replace_all(level_coded, "weekly", "1/week")) %>%
  mutate(level_coded = str_replace_all(level_coded, "monthly", "1/month")) %>%
  mutate(level_coded = str_replace_all(level_coded, "daily", "1/day")) %>%
  # Replace number not preceded by "onset " or "age " with [number], and number preceded by "onset " or "age " with *number*
  mutate(level_coded = ifelse(str_detect(level_coded, "(?<!onset |age )([0-9]+-?[0-9]*)"), str_replace_all(level_coded, "([0-9]+-?[0-9]*)", "[\\1]"), 
    str_replace_all(level_coded, "(onset |age )([0-9]+)", "*\\2*"))) 

# Include comparison operator in brackets if it precedes a number that is already in brackets
# If it's followed by a number in brackets, delete the brackets around the number
df_plcohort <- df_plcohort %>%
  mutate(level_coded = str_replace_all(level_coded, "(>|>=|<=|<)\\s*\\[(\\d+)\\]", "[\\1 \\2]"))


# Replace "lifetime", "ever", "current" with *word*
df_plcohort <- df_plcohort %>%
  mutate(level_coded = str_replace_all(level_coded, "(lifetime|ever|current|early|late)", "*\\1*"))

# Replace "frequent", "infrequent" with [word]
df_plcohort <- df_plcohort %>%
  mutate(level_coded = str_replace_all(level_coded, "(frequent|infrequent|occasional|abuse or dependence|abuse| abuse / dependence|dependence|misuse| mild or heavy|heavy|light|any|moderate|regular|substance use disorder|cud)", "[\\1]"))

# Replace "times", "days" with (word)
df_plcohort <- df_plcohort %>%
  mutate(level_coded = str_replace_all(level_coded, "(times|days|years)", "(\\1)"))

# Replace "once" with "[1](time)"
df_plcohort <- df_plcohort %>%
  mutate(level_coded = str_replace_all(level_coded, "once", "[1](time)"))

#clean controlgroup variable 
sort(levels(as.factor(df_plcohort$"level_coded")))

View(df_plcohort)