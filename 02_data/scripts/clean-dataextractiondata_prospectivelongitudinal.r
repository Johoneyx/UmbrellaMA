library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
library(readxl)
library(stringr)
library(gridExtra)
library(rlang)


merged_df_clean <- (read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/merged_df_clean.xlsx"))

studylist_cohort <- (read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/studylist_meta-analysis.xlsx"))

merged_df_clean <- merged_df_clean %>% 
mutate(relevant = ifelse(studycode %in% studylist_cohort$studycode, 1, 0 ))


df_plcohort <- merged_df_clean %>% 
  filter(relevant == 1)


view(df_plcohort)

colnames(df_plcohort) <- gsub(" ", "_", colnames(df_plcohort))



#**************************************CLEAN CANNABIS USE DATA****************************************************

#clean lifetime variable 
df_plcohort$"lifetime_cannabis_use" <-  tolower(df_plcohort$"lifetime_cannabis_use")

df_plcohort <- df_plcohort %>%
  mutate(`lifetime_cannabis_use` = str_replace_all(`lifetime_cannabis_use`, c("age of use" = "age of onset", "current cannabis use" = "current", "current use"="current", "last 12-month use" = "last year","last 6-month use"= "last six months", "lifetime_cannabis_use" = "lifetime", "lifetime use" = "lifetime", "past month" = "last month", "past 30 days cannabis use" = "last month", "past year use" = "last year use", "prior six months" = "last six months", "recent use (last month)" = "last month", "last 12-month cannabis use" = "last year", "cannabis use in previous 90 days" = " last three months", "last month use" = "last month")))

#clean level of use data
df_plcohort <- df_plcohort %>%
  mutate(cannabis_level_of_use = str_replace_all(cannabis_level_of_use, c(
    "15 years or less" = "by age fifteen", 
    "16 years or more" = "after age sixteen", 
    "prior to age 15" = "by age fifteen",  
    "prior to age 14" = "by age fourteen", 
    "at age 17-18" = "at age seventeen to eighteen", 
    "at age 20-21" = "at age twenty to twentieone", 
    "Used once" = "1 times", 
    "before 15" = "by age fifteen", 
    "by age 15" = "by age fifteen", 
    "by age 16" = "by age sixteen",
    "by age 18" = "by age eighteen"
  )))


#combine data in new variable to get all info
df_plcohort <- df_plcohort %>%
mutate(cannabis_all = str_c(lifetime_cannabis_use, cannabis_level_of_use, `comparison(control-group)`, sep = "; "))

df_plcohort$cannabis_all <-  tolower(df_plcohort$cannabis_all)

#clean cannabis_all variable
df_plcohort <- df_plcohort %>%
  mutate(`cannabis_all` = str_replace_all(`cannabis_all`, ("ocassiaonl|ocassiaonl"), "occasional"))

df_plcohort <- df_plcohort %>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "less than", "<")) %>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "at least", ">="))%>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "at least", ">="))%>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "nearly", "almost"))%>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "or more", ">="))%>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "more than", ">"))%>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "per week", "/week")) %>%
   mutate(cannabis_all = str_replace_all(cannabis_all, "in a month", "/month")) %>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "prior to", "<"))


df_plcohort <- df_plcohort %>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "weekly", "1/week")) %>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "monthly", "1/month")) %>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "once a month", "1/month")) %>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "every day", "1/day"))%>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "daily", "1/day"))


#mark cannabis use time-frame related words with *word*
df_plcohort <- df_plcohort %>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "(lifetime|continued|continuation| ever |past|current|early|late|onset|at age|age of|after age|by age fourteen|by age|fourteen|fifteen|sixteen|seventeen|eighteen|twenty|twentieone|last year|last three months|last six months|prior|last 12 months|prior to age|age of onset|last month|baseline|t2|not)", "*\\1*"))

#mark cannabis use level or frequency related words with [word]
df_plcohort <- df_plcohort %>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "(severity|several|frequent|frequency|infrequent|occasional|abuse|dependence|abstinent|cannabis use disorder|misuse|mild or heavy|heavy|light|any|moderate|regular|substance use disorder|cud|cannabis use disorder|without impairment|with cannabis-induced aps|/week|/day|/month|/year|years|times|days|five|sistematic|almost|in remission|<|>|=|-|\\d+)", "[\\1]"))


#put all the star-words into the recall_cannabis_use_timeframe variable 
df_plcohort <- df_plcohort %>%
  mutate(recall_cannabis_use_timeframe = sapply(str_extract_all(cannabis_all, "\\*([^*]+)\\*"), function(x) paste(x, collapse = " ")))

#View(as.data.frame(df_plcohort$recall_cannabis_use_timeframe))

#put all the words in brackets in the cannabis use frequency variable
df_plcohort <- df_plcohort %>%
  mutate(cannabis_use_frequency = sapply(str_extract_all(cannabis_all, "\\[([^\\]]+)\\]"), function(x) paste(x, collapse = " ")))


#View(as.data.frame(df_plcohort$cannabis_use_frequency))

#View(df_plcohort)

#look at the levels that occur most in the recall timeframe variable
table_lifetime <- as.data.frame(table(as.factor(df_plcohort$recall_cannabis_use_timeframe)))
filtered_table_lifetime <- filter(table_lifetime, Freq > 3)

#View(filtered_table_lifetime)

# Convert the data frame to a grid table
table_grob<- gridExtra::tableGrob(filtered_table_lifetime)

# Save the table as a PDF
pdf("C:/Users/johan/Documents/PhD/UmbrellaMA/04_visualization/filtered_table_lifetime.pdf")
grid::grid.draw(table_grob)
dev.off()

#look at the levels that occur the most in the cannabis use frequency variable
table_freq <- as.data.frame(table(as.factor(df_plcohort$cannabis_use_frequency)))
filtered_table_freq <- filter(table_freq, Freq > 3)

View(filtered_table_freq)


# Convert the data frame to a grid table
table_grob<- gridExtra::tableGrob(filtered_table_freq)

# Save the table as a PDF
pdf("C:/Users/johan/Documents/PhD/UmbrellaMA/04_visualization/filtered_table_freq.pdf")
grid::grid.draw(table_grob)
dev.off()


df_cannabis <- df_plcohort %>%
select(cannabis_all,cannabis_use_frequency,recall_cannabis_use_timeframe,cannabis_control)

View(df_cannabis)

View(data.frame(table(df_plcohort$recall_cannabis_use_timeframe)))
View(data.frame(table(df_plcohort$cannabis_use_frequency)))
View(data.frame(table(df_plcohort$recall_cannabis_use_timeframe)))





#clean cannabis comparision variable 

df_plcohort$cannabis_control <- tolower(df_plcohort$"comparision(control-group)")


df_plcohort <- df_plcohort %>%
 mutate(comparision_coded =cannabis_control)


df_plcohort <- df_plcohort %>%
 mutate(comparision_coded = str_replace_all(comparision_coded, "(no cannbis use during the study|no-use|no cannabis use|no use of marijuana|no use or dependence|non-users|non consumers|non users)", "no use"))


df_plcohort <- df_plcohort %>%
  mutate(comparision_coded = str_replace_all(comparision_coded, "(neverd|neverrs \\(negative urine test\\)|never of cannabis|never users \\(negative urine test\\))", "never")) %>%
  select(-"comparision(control-group)")


table_comp <- as.data.frame(table(as.factor(df_plcohort$comparision_coded)))
filtered_table_comp <- filter(table_comp, Freq > 3)

View(filtered_table_comp)


#********************CANNABISCONTINUATION***********************


















  
#**********CLEAN OUTCOME DATA***************************************************************************


df_plcohort$outcome <- tolower(df_plcohort$outcome)

df_plcohort <- df_plcohort %>%
 mutate(outcome_clean= outcome) 


#correct all words with symp that are spelled wrong
df_plcohort <- df_plcohort %>%
  mutate(outcome_clean = str_replace_all(outcome_clean, "(symtoms|symtomes|symtpoms)", "symptoms"))

#see if it worked
df_plcohort %>%
  filter(str_detect(outcome_clean, "\\bsym\\w*")) %>%
 pull(outcome_clean) %>%
 unique() %>%
 str_split(" ") %>%
  unlist() %>%
 .[str_detect(., "\\bsym\\w*")] %>%
 unique() %>%
 print()


#see if it worked
df_plcohort %>%
  filter(str_detect(outcome_clean, "\\bsym\\w*")) %>%
 pull(outcome_clean) %>%
 unique() %>%
 str_split(" ") %>%
  unlist() %>%
 .[str_detect(., "\\bsym\\w*")] %>%
 unique() %>%
 print()

#i repeated it for other common syllabus such as dim, neg, pos etc.

# Print words end with "phrenia" 
df_plcohort %>%
  filter(str_detect(outcome_clean, "\\w*phrenia\\b")) %>%
 pull(outcome_clean) %>%
 unique() %>%
 str_split(" ") %>%
  unlist() %>%
 .[str_detect(., "\\w*phrenia\\b")] %>%
 unique() %>%
 print()

df_plcohort <- df_plcohort %>%
  mutate(outcome_clean = str_replace_all(outcome_clean, "(dimensios|dimention)", "dimension"))%>%
  mutate(outcome_clean = str_replace_all(outcome_clean, "(ngtaive|negtaive)", "negative"))%>%
  mutate(outcome_clean = str_replace_all(outcome_clean, "(schziphrenia|negtaive)", "negative"))

df_plcohort %>%
  filter(str_detect(outcome_clean, "\\bpsych\\w*")) %>%
  pull(outcome_clean) %>%
  unique() %>%
  print()




df_plcohort <- df_plcohort %>%
mutate("outcome_measure_clean"= toupper(outcome_measure))


df_plcohort <- df_plcohort %>%
  mutate(outcome_measure_coded = case_when(
    str_detect(outcome_measure_clean, "SOPS") ~ "SOPS",
    str_detect(outcome_measure_clean, "SIPS") ~ "SIPS",
    str_detect(outcome_measure_clean, "YSR") ~ "YSR",
    str_detect(outcome_measure_clean, "KSADS") ~ "KSADS",
    str_detect(outcome_measure_clean, "BECK") ~ "BAI",
    str_detect(outcome_measure_clean, "CAPE") ~ "CAPE",
    str_detect(outcome_measure_clean, "PHYSICAL ANHEDONIA") ~ "PA",
     str_detect(outcome_measure_clean, "SOCIAL ANHEDONIA") ~ "SA",
      str_detect(outcome_measure_clean, "CIDI") ~ "CIDI",
       str_detect(outcome_measure_clean, "DIA-X") ~ "DIA-X",
        str_detect(outcome_measure_clean, "DIS") ~ "DIS",
        str_detect(outcome_measure_clean, "K-SADS_PL") ~ "K-SADS_PL",
        str_detect(outcome_measure_clean, "GAF") ~ "GAF",
        str_detect(outcome_measure_clean, "FUPPHS") ~ "FUPPHS",
        str_detect(outcome_measure_clean, "GAF") ~ "GAF",
        str_detect(outcome_measure_clean, "HAMILTON") ~ "HDRS",
        str_detect(outcome_measure_clean, "ICD") ~ "ICD",
        str_detect(outcome_measure_clean, "LIFE CHAT") ~ "Life Chart Method",
        str_detect(outcome_measure_clean, "LIFE CHART") ~ "Life Chart Method",
        str_detect(outcome_measure_clean, "SCL-90") ~ "SCL-90",
        str_detect(outcome_measure_clean, "PACE") ~ "PACE",
        str_detect(outcome_measure_clean, "PETERS") ~ "PDI",
        str_detect(outcome_measure_clean, "PLIKS") ~ "PLIKS",
str_detect(outcome_measure_clean, "POPS") ~ "POPS",
str_detect(outcome_measure_clean, "SOPS") ~ "SOPS",
str_detect(outcome_measure_clean, "SIPS") ~ "SIPS",
str_detect(outcome_measure_clean, "PROD") ~ "PROD-Screen",
str_detect(outcome_measure_clean, "SANS") ~ "SANS",
str_detect(outcome_measure_clean, "SPIKE") ~ "SPIKE",
str_detect(outcome_measure_clean, "YSR") ~ "YSR",
str_detect(outcome_measure_clean, "BPRS") ~ "BPRS",
str_detect(outcome_clean, "icd") ~ "ICD",
str_detect(outcome_measure_clean, "DSM") ~ "DSM",
str_detect(outcome_measure_clean, "PSQ") ~ "PSQ",
str_detect(outcome_measure_clean, "SCID") ~ "SCID",
str_detect(outcome_measure_clean, "CAARMS") ~ "CAARMS",
str_detect(outcome_measure_clean, "SELF-REPORT QUESTIONNAIRE") ~ "SELF-REPORT QUESTIONNAIRE",
str_detect(outcome_measure_clean, "BSABS") ~ "BSABS",
str_detect(outcome_measure_clean, "YUNG ET AL") ~ "CAARMS",
str_detect(outcome_measure_clean, "PANSS") ~ "PANNS",
str_detect(outcome_measure_clean, "PANNS") ~ "PANNS",
str_detect(outcome_measure_clean, "SAPS") ~ "SAPS",
str_detect(outcome_measure_clean, "INTERVIEW") ~ "Interview",
str_detect(outcome_measure_clean, "NOT CLEAR") ~ "Not clear",
str_detect(outcome_measure_clean, "YMRS") ~ "YMRS",
str_detect(outcome_measure_clean, "NUMBER OF HOSPITAL READMISSIONS") ~ "Number of Hospital Readmissions",
str_detect(outcome_measure_clean, "MEAN NUMBER OF HOSPITALIZATIONS") ~ "Number of Hospitalizations",
str_detect(outcome_measure_clean, "NUMBER OF HOSPITALIZATION DAYS") ~ "Number of Hospitalization days",
str_detect(outcome_measure_clean, "SELF-REPORTED PSYCHOTIC ACUTE EPISODE") ~ "Number of Hospitalization days",
str_detect(outcome_measure_clean, "BREAKTHROUGH PSYCHOTIC SYMPTOMS") ~ "BAMM",
str_detect(outcome_measure_clean, " ADMISSION TO A PSYCHIATRIC INPATIENT UNIT AFTER EXACERBATION OF SYMTOMS") ~ "Admission Inpatient",
str_detect(outcome_measure_clean, "NUMER OF DAYS IN SUPPORTED PSYCHIATRIC HOUSING") ~ "Number of suppor",
str_detect(outcome_measure_clean, "ANY HOSPITAL ADMISSION") ~ "Any Hospital Admission",
str_detect(outcome_measure_clean, "ANY HOSPITAL ADMISSION") ~ "Any Hospital Admission",
str_detect(outcome_measure_clean, "ANY HOSPITAL ADMISSION") ~ "Any Hospital Admission",
str_detect(outcome_measure_clean, "ANY HOSPITAL ADMISSION") ~ "Any Hospital Admission",
str_detect(outcome_measure_clean, "ANY HOSPITAL ADMISSION") ~ "Any Hospital Admission",
    TRUE ~ NA_character_ 
  ))

selectionNA <-df_plcohort %>%
select(outcome_clean,outcome_measure_coded)

selectionNA <-df_plcohort %>%
filter(is.na(outcome_measure_coded))%>%
select(outcome_clean,outcome_measure_clean,outcome_measure_coded)%>%
View()


df_plcohort <- df_plcohort %>%
  mutate(outcome_clean = str_replace_all(outcome_clean, "(negative psychosis|negative dimension|negative schizophrenia symptoms|negative symptoms)", "negative psychotic symptoms"))%>% 
  mutate(outcome_clean = str_replace_all(outcome_clean, "negative psychotic symptoms of psychotic symptoms", "negative psychotic symptoms"))

df_plcohort <- df_plcohort %>%
  mutate(outcome_clean = case_when(
    !is.na(outcome_clean) ~ outcome_clean,  
    is.na(outcome_clean) & str_detect(outcome_measure_clean, regex("positive", ignore_case = TRUE)) ~ "positive psychotic symptoms",
    is.na(outcome_clean) & str_detect(outcome_measure_clean, regex("Negative", ignore_case = TRUE)) ~ "negative psychotic symptoms",
    is.na(outcome_clean) & str_detect(outcome_measure_clean, regex("General", ignore_case = TRUE)) ~ "general psychotic symptoms",
     is.na(outcome_clean) & str_detect(outcome_measure_clean, regex("total", ignore_case = TRUE)) ~ "total psychotic symptoms",
    is.na(outcome_clean) & str_detect(outcome_measure_clean, regex("hospitalizations|hospitalization", ignore_case = TRUE)) ~ "hospitalizations",
    TRUE ~ NA_character_  # Keeps NA as is if none of the above conditions are met
  ))


#outcome_coded
df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = case_when(
    str_detect(outcome_clean, regex("symptoms|symptom|first-rank|hallucination|grandiosity|paranoia|unusual thought|psychotic experiences|anhedonia|avolition|expression of emotion|ideational richness|functioning|ideas|suspiciousness|items|disorganized|score|delusions|bamm|perceptual", ignore_case = TRUE)) ~ "symptoms",
    str_detect(outcome_clean, regex("icd-8|psychosis onset|non-affective psychosis|t1-t2 psychosis", ignore_case = TRUE)) ~ "psychosis onset",
    str_detect(outcome_clean, regex("transition", ignore_case = TRUE)) ~ "transition to psychosis",
    str_detect(outcome_clean, regex("chr\\+", ignore_case = TRUE)) ~ "CHR state",
    str_detect(outcome_clean, regex("relapse|hospitalizations\\+", ignore_case = TRUE)) ~ "relapse",
    TRUE ~ NA_character_ # For rows that do not match any of the above conditions
  ))

View(as.data.frame(table(df_plcohort$outcome_coded)))


View(df_plcohort) %>% 
filter(is.na(outcome_coded)))
 
View(as.data.frame(table(df_plcohort$outcome_measure_coded)))

# Convert the data frame to a grid table
outcome_measure_coded<- gridExtra::tableGrob(outcome_measure_coded)

# Save the table as a PDF
pdf("C:/Users/johan/Documents/PhD/UmbrellaMA/04_visualization/outcome_measure_coded.pdf")
grid::grid.draw(outcome_measure_coded)
dev.off()


#******************************************CLEAN FOLLOW-UP TIME*********************************************

#time
#follow-up_duration
#cannabis__&outcome_analysis_timeframe
#time_frame
#recall?
#followup_duration
#study_type
#survival_curve?




#create a new variable followup that is a combination of follow-up_duration and followup_duration

df_plcohort$followup <- df_plcohort$"follow-up_duration"


df_plcohort$followup[is.na(df_plcohort$followup)] <- df_plcohort$followup_duration[is.na(df_plcohort$followup)]

#still needs to be checked: time_frame, time_frame_(cannabis_use_and_outcome_measure_time)
#repeated cross-sectional or longitudinal?
#followup-duration in months when there is no unit?

#studytype needs to be checked very often cohort but dont know if prospective 

df_plcohort <- df_plcohort %>%
  select(-`follow-up_duration`, -`followup_duration`)


#***************************************CLEAN COUNTRY*****************************************************

View(as.data.frame(df_plcohort$country_clean, df_plcohort$country))

df_plcohort$country_clean <- df_plcohort$country

df_plcohort$country_clean <- df_plcohort$country %>%
  str_replace_all("\\+", ",") %>%  
  str_replace_all("\\bthe\\b", "") %>%  
  str_replace_all("\\band\\b", "") %>%  
  str_replace_all("United Kingdom|Uk|England", "UK") %>%  
  str_replace_all("London", "") %>% 
  str_replace_all("\\(monreal, Quebec\\)", "") %>%  # 
  str_replace_all("Copenhagen", "")%>%  
  str_replace_all("United States", "USA") %>% 
  str_replace_all("-", "") %>%
  str_replace_all("Navarre", "") %>%
  str_replace_all(",(?!\\w)", "")%>% 
  str_trim() 

country_clean <-as.data.frame(table(as.factor(df_plcohort$country_clean)))

# Convert the data frame to a grid table
country_clean<- gridExtra::tableGrob(country_clean)

# Save the table as a PDF
pdf("C:/Users/johan/Documents/PhD/UmbrellaMA/04_visualization/country_clean.pdf")
grid::grid.draw(country_clean)
dev.off()

 
 #**************************GENDER*************************************

#%_male_in_cannabis_group 
#%_male_in_no_cannabis_group
#"%_male"
#cohort

df_gender <- df_plcohort %>%
  select(`%_male`, `%_male_in_cannabis_group`, `%_male_in_no_cannabis_group`, cohort)

View(df_gender)

unique(df_plcohort$`%_male`)


#sometimes weird values, is it worth it to go back and double-check? (ask Sagnik)


#*******************************************AGE*****************************
#"mean_age_outcome_group"
#"mean_age_non_outcome_group"
#"mean_age_in_cannabis_group"
#"mean_age_in_no-cannabis_group"
#"age_range"
#"mean_age"
#age_sd
#age

df_age <- df_plcohort %>%
  select(`mean_age_outcome_group`, `mean_age_non_outcome_group`, `mean_age_in_cannabis_group`,`mean_age_in_no-cannabis_group`,`age_range`,`mean_age`)

View(df_age)
unique(df_plcohort$age_range)

#does it make sense to categorize the cohorts according to age? (ask Sagnik)

#**********************************************************






#variables who might have info psychosis type
#fep_vs_chronic 
#target_population
#cohort
#cohort_more_detail
#diagnostic_tool
#dt_name
#kind_of_psychosis

#factors accounted for 
#baseline_differences_between_group?
#factors_accounted_for



#********************POPULATION*****************]

df_plcohort <- df_plcohort %>%
    mutate(population = case_when(
    str_detect(target_population, "CHR") ~ "CHR",
    str_detect(target_population, "HP") ~ "HP",
    str_detect(target_population, "P") ~ "P",
    str_detect(target_population, "APS") ~ "P",
    str_detect(target_population, regex("Chronic", ignore_case = TRUE)) ~ "P",
    str_detect(target_population, "Adolescents") ~ "HP",
    str_detect(target_population, "NPS") ~ "P",
     str_detect(target_population, "SCZ") ~ "P",
      str_detect(target_population, "UHR") ~ "CHR",
      str_detect(target_population, "ARMS") ~ "CHR",
       str_detect(cohort, "Avon") ~ "HP",
       str_detect(target_population, "NEMESIS") ~ "HP",
       str_detect(cohort, "inpatients") ~ "P",
       str_detect(cohort, "patients") ~ "P",
       str_detect(cohort, "Birth") ~ "HP",
    TRUE ~ NA_character_ 
  ))

View(as.data.frame(table(df_plcohort$population)))


#chronic vs FEP
df_plcohort <- df_plcohort %>%
  mutate(fepvschronic_coded = case_when(
    (str_detect(cohort, regex("FEP|first episode", ignore_case = TRUE)) | 
     str_detect(target_population, regex("FEP|first episode", ignore_case = TRUE))) & 
    (str_detect(cohort, regex("chronic", ignore_case = TRUE)) | 
     str_detect(target_population, regex("chronic", ignore_case = TRUE))) ~ "FEP, chronic",
    
    str_detect(fep_vs_chronic, regex("FEP|first episode", ignore_case = TRUE)) | 
    str_detect(cohort, regex("FEP|first episode", ignore_case = TRUE)) | 
    str_detect(target_population, regex("FEP|first episode", ignore_case = TRUE)) ~ "FEP",
    
    str_detect(cohort, regex("chronic", ignore_case = TRUE)) | 
    str_detect(target_population, regex("chronic", ignore_case = TRUE)) ~ "chronic",
    
    TRUE ~ NA_character_ # For rows that do not match any of the above conditions
  ))

View(as.data.frame(table(df_plcohort$fepvschronic_coded)))



##create psychosis_type_coded
#df_plcohort <- df_plcohort %>%
  #mutate(psychosis_type = case_when(
    #(str_detect(outcome, regex("APS|affective", ignore_case = TRUE)) ~ 
     


#reference
#citation
#title
 
 #tidy data 



write_xlsx(df_plcohort, "02_data/cleandata/cohort_df_clean.xlsx")

# Convert the data frame to a grid table
comp<- gridExtra::tableGrob(filtered_table_comp)

# Save the table as a PDF
pdf("C:/Users/johan/Documents/PhD/UmbrellaMA/04_visualization/filtered_table_comp.pdf")
grid::grid.draw(comp)
dev.off()


#*****************************************CLEAN STATISTICALDATA********************************************************

#combine the variables that indicate the total samplesize N 
#sample_size_(total_n)
#total_n
#into the new variable N

df_plcohort <- df_plcohort %>%
mutate(N = coalesce(`sample_size_(total_n)`,
total_n))


df_plcohort <- df_plcohort %>%
  select(-`sample_size_(total_n)`, -total_n)

print(df_plcohort$N)



# Function to evaluate expressions in a string and keep the original value if not an expression
evaluate_or_keep <- function(x) {
  if (is.na(x)) {
    return(NA)
  }
  result <- tryCatch({
    eval(parse(text = x))
  }, error = function(e) {
    x
  })
  return(result)
}

# Apply the function to the N column and store the result in a new column N_calculated
df_plcohort <- df_plcohort %>%
  mutate(N_calculated = sapply(N, evaluate_or_keep))%>%
  mutate(n_cu_calculated = sapply(n_cu, evaluate_or_keep))%>%
  mutate(n_ncu_calculated = sapply(n_ncu, evaluate_or_keep))%>%
  mutate(n_outcome_calculated = sapply(n_outcome , evaluate_or_keep))%>%
  mutate(n_no_outcome_calculated = sapply(n_no_outcome, evaluate_or_keep))%>%
  mutate(cu_p_calculated = sapply(cu_p, evaluate_or_keep))%>%
  mutate(ncu_p_calculated = sapply(ncu_p, evaluate_or_keep))%>%
   mutate(cu_np_calculated = sapply(cu_np, evaluate_or_keep))%>%
  mutate(ncu_np_calculated = sapply(ncu_np, evaluate_or_keep))%>%
 mutate(cu_np_calculated = sapply(cu_np, evaluate_or_keep))%>%
  mutate(ncu_np_calculated = sapply(ncu_np, evaluate_or_keep))




# View the selected columns
View(df_plcohort %>%
  select(
    N,
    N_calculated,
    n_outcome,
    n_outcome_calculated,
    n_no_outcome,
    n_no_outcome_calculated,
    n_cu,
    n_cu_calculated,
    n_ncu,
    n_ncu_calculated,
    cu_p,
    cu_p_calculated,
    ncu_p,
    ncu_p_calculated,
    cu_np,
    cu_np_calculated,
    ncu_np,
    ncu_np_calculated
  ))



df_plcohort <- df_plcohort %>%
  mutate(across(everything(), ~ str_replace_all(., "Invalid Number", "NA")))

write_xlsx(df_plcohort, "02_data/cleandata/cohort_df_clean.xlsx")
#sample_size_(total_n)
#total_n
#"n_outcome"
#"n_no_outcome"
#"n_cu"
 #"n_ncu"
#"cu_p"
# "ncu_p"
#"cu_np" "ncu_np" "or" "lci_or" "uci_or" "p_or" "rr" "lci_rr" "uci_rr" "p_rr" "hr" "lci_hr" "uci_hr" "p_hr" "timeframe_hr" "aor" "lci_aor" "uci_aor" "p_aor" "adjusted_factors_aor" "arr" "lci_arr" "uci_arr" "p_arr" "rr_direction" "adjusted_factors_arr" "ahr" "lci_ahr" "uci_ahr" "p_ahr""timeframe_ahr""covariates_ahr" "mean_in__outcome_group" "sd_in_outcome_group" "mean_in_no-outcome_group" "sd_in_no-outcome_group" "mean_c" "sd_c" "mean_nc" "sd_nc" "smd" "smd_measure" "lci_smd" "uci_smd"
#"p(smd)"
# "b"
 #"se_b"
 #"p_b"
#"ab"
# "se_ab"
# "p_ab"
# "covariates_ab"
# "other_statistical_method"
# "factor"
# "statistical_parameter"
# "p-value"
# "factors_accounted_for"
 #"time_frame"
 #"%_outcome"
#"or_direction"
#"hr_direction"
#"aor_direction"
#"arr_direction"
#"direction_ahr"
#"statistical_method"
#"extracted_from"
#"corr"
#"p_corr"
#"acorr"
#"p_acorr"
#"covariates_acorr"
#"%_of_chr_transition_to_fep_in_cannabis_grups"
#"se_of_%_transition_cu"
# "%_of_chr_transition_to_fep_in_no-cannabis_grups"
#"se_of_%_transition_ncu"
#"survival_curve?"
#"icluded_in_meta-analysis"
#"correlation_method"
#"%_in_outcome_group"
#"%_in_no-outcome_group"
#"n_dcu"
#"mean_dc"
#"sd_dc"
#"β_lci"
#"β_uci"
# "f-value"


names(df_plcohort)

#*******************CLEAN_ADJUSTED_FACTORS*********************
#factors accounted for another relevant cell?

# Select the specified columns
covariates <- df_plcohort %>%
  select("adjusted_factors_aor", "adjusted_factors_arr", "covariates_ahr", "covariates_ab", "covariates_acorr")

# View the selected columns
View(covariates)

# Combine all values in each row into a single string and tokenize
tokens <- covariates %>%
  unite("combined", everything(), sep = " ") %>%
  mutate(tokens = str_split(combined, ",|;|\\+|\\band\\b")) %>%
  unnest(tokens) %>%
  mutate(tokens = str_trim(tokens)) %>%
  filter(tokens != "" & != "NA") %>%
  mutate(tokens = tolower(tokens))

# Table the frequencies of the tokens
token_frequencies <- table(tokens$tokens)

# View the token frequencies as a data frame
View(as.data.frame(token_frequencies))

# Combine all values in each row into a single string and tokenize by spaces
words <- covariates %>%
  unite("combined", everything(), sep = " ") %>%
  mutate(words = str_split(combined, "\\s+")) %>%
  unnest(words) %>%
  mutate(words = str_trim(words)) %>%
  filter(words != "" & words != "NA") %>%
  mutate(words = tolower(words))

# Table the frequencies of the words
word_frequencies <- table(words$words)

# Convert to data frame and sort by frequency in descending order
word_frequencies_df <- as.data.frame(word_frequencies) %>%
  arrange(desc(Freq))

# View the sorted word frequencies
View(word_frequencies_df)
# Print the token frequencies
View(as.data.frame(token_frequencies))
#********************EXPLORE****************************************

#very helpful table i could use this to decide whether study is relevant *e.g. plamondon incorporates metval metmet level does it make sense to combine those levels into one effectsize
summary <- df_plcohort %>%
group_by(studycode) %>%
summarise(
  outcomes =paste(unique(outcome_coded), collapse =" & "),
  cannabis_frequency = paste(unique(cannabis_use_frequency), collapse =" & "),
  cannabis_levels = paste(unique(cannabis_level_of_use), collapse =" & "),
  timepoints = paste(unique(followup), collapse =" & "),
  populations = paste(unique(population), collapse =" & "),
  datarows = n(),
)

View(summary)



write_xlsx(summary, "04_visualization/dataexploration.xlsx")

#********************ROBDATA***************************************

View(df_plcohort %>%
select(studycode,q1, q2, q3, q4, q5, q6, q7, q9,totalstars))

#********************Create_Follow-Up_Code*********************



#********************Create_Comparision_Code******************

#********************Create_Outcome_Code***********************



#**************************************double-check_rob_manually******************************************************
#double-check missing years manually and add in

