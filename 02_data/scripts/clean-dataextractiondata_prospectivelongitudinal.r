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
  mutate(level_coded = str_replace_all(level_coded, "(frequent|infrequent|occasional|abuse or dependence|abuse| abuse / dependence|dependence|misuse|mild or heavy|heavy|light|any|moderate|regular|substance use disorder|cud)", "[\\1]"))

# Replace "times", "days" with (word)
df_plcohort <- df_plcohort %>%
  mutate(level_coded = str_replace_all(level_coded, "(times|days|years)", "(\\1)"))

# Replace "once" with "[1](time)"
df_plcohort <- df_plcohort %>%
  mutate(level_coded = str_replace_all(level_coded, "once", "[1](time)"))

#clean controlgroup variable 
sort(levels(as.factor(df_plcohort$"level_coded")))




names(df_plcohort)

#clean cannabis measure variable
sort(levels(as.factor(df_plcohort$"cannabis measure")))

df_plcohort$"cannabis measure"<-tolower(df_plcohort$"cannabis measure")

#clean cannabis measure variable
#sort(levels(as.factor(df_plcohort$"cannabis measure")))

##df_plcohort <- df_plcohort %>%
 #mutate(measure_coded ="cannabis measure") %>%
  #mutate(measure_coded = str_replace_all#(measure_coded, "(|days|years)", "(\\1)"))


  df_plcohort$"cannabis measure"<-tolower(df_plcohort$"cannabis measure")

  df_plcohort <- df_plcohort %>%
 mutate(measure_coded ="cannabis measure") %>%

# Replace all spaces in column names with underscores
# Replace all spaces in column names with underscores
colnames(df_plcohort) <- gsub(" ", "_", colnames(df_plcohort))

sort(levels(as.factor(df_plcohort$outcome)))

  
df_plcohort <- df_plcohort %>%
 mutate(outcome_coded= outcome) 


df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all(outcome coded, "(symtoms|symtomes|symptomes|symtpoms|symtomes|symptomes)", "symptoms"))


# Print words starting with "sym" 
df_plcohort %>%
  filter(str_detect(outcome_coded, "\\bschiz\\w*")) %>%
 pull(outcome_coded) %>%
 unique() %>%
 str_split(" ") %>%
  unlist() %>%
 .[str_detect(., "\\bschiz\\w*")] %>%
 unique() %>%
 print()

df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all(`outcome_coded`, "(symptoms|symtpoms|symtoms|symtomes)", "symptoms"))


  # Print words starting with "sym" 
df_plcohort %>%
  filter(str_detect(outcome_coded, "\\bgen\\w*")) %>%
 pull(outcome_coded) %>%
 unique() %>%
 str_split(" ") %>%
  unlist() %>%
 .[str_detect(., "\\bgen\\w*")] %>%
 unique() %>%
 print()

df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all(`outcome_coded`, "(dimensios|dimention)", "dimension"))%>%
  mutate(outcome_coded = str_replace_all(`outcome_coded`, "(ngtaive|negtaive)", "negative"))


table(as.factor(df_plcohort$outcome_coded))


df_plcohort %>%
  filter(str_detect(outcome_coded, "\\bpsych\\w*")) %>%
  pull(outcome_coded) %>%
  unique() %>%
  print()

#view levels as a table so that you can decide which expression to take and which values you have to rename
View(as.data.frame(table(as.factor(df_plcohort$outcome_coded))))

df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all(`outcome_coded`, "(negative psychosis|negative dimension|negative schizophrenia symptoms|negative symptoms)", "negative psychotic symptoms"))%>% 
  mutate(outcome_coded = str_replace_all(`outcome_coded`, "negative psychotic symptoms of psychotic symptoms", "negative psychotic symptoms"))


df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all(`outcome_coded`, "(negative psychosis|negative dimension|negative schizophrenia symptoms|negative symptoms)", "negative psychotic symptoms"))

df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all(`outcome_coded`, "(positive dimension|positive psychosis|positive schziphrenia symptoms|positive symptoms)", "positive psychotic symptoms"))

  #view levels as a table so that you can decide which expression to take and which values you have to rename
View(as.data.frame(table(as.factor(df_plcohort$outcome_coded))))

# Load the necessary package
library(dplyr)

# Create a table and filter it
table_data <- as.data.frame(table(as.factor(df_plcohort$outcome_coded)))
filtered_table_data <- filter(table_data, Freq > 5)

# Display the filtered table
View(filtered_table_data)

df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all(`outcome_coded`, "(schizophrenia symptoms (0-58) |mean psychotic symptoms)", "psychotic symptoms"))


df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all(`outcome_coded`, "(icd-8|psychosis onset|psychosis onset|non-affective psychosis)", "development of a psychotic disorder"))

# Create a table from the outcome_coded column
table_data <- as.data.frame(table(as.factor(df_plcohort$outcome_coded)))

filtered_table_data <- filter(table_data, Freq < 5)
# Display the filtered table
View(filtered_table_data)

# Create a table from the outcome_coded column
table_data <- as.data.frame(table(as.factor(df_plcohort$country)))

filtered_table_data <- filter(table_data, Freq > 3)
# Display the filtered table
View(filtered_table_data)

names(df_plcohort)


#variables who might have info psychosis type
#fep_vs_chronic 
#target_population
#cohort
#cohort_more_detail
#diagnostic_tool
#dt_name
#kind_of_psychosis

#cannabis
#lifetime_cannabis_use
#cannabis_level_of_use
#comparision(control-group)
#cannabis_measure
#recall?


#factors accounted for 
#baseline_differences_between_group?
#factors_accounted_for

#age
#age_sd
#age_range
#mean_age



#time
#follow-up_duration
#cannabis__&outcome_analysis_timeframe
#time_frame
#recall?
#followup_duration
#study_type
#survival_curve?

#sample_size_(total_n)
#total_n 


#%_male_in_cannabis_group 
#%_male_in_no_cannabis_group


#reference
#citation
#title
 


