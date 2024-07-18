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

merged_df_clean <- (read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/merged_df_clean.xlsx"))

studylist_cohort <- (read_xlsx("C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/df_studylist_cohort_unique.xlsx"))

merged_df_clean <- merged_df_clean %>% 
mutate(relevant = ifelse(studycode %in% studylist_cohort$studycode, 1, 0 ))


df_plcohort <- merged_df_clean %>% 
  filter(relevant == 1)

colnames(df_plcohort) <- gsub(" ", "_", colnames(df_plcohort))



#clean cannabis use data

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
mutate(cannabis_all = str_c(lifetime_cannabis_use, cannabis_level_of_use,  sep = " "))

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


#df_plcohort <- df_plcohort %>%
  #mutate(cannabis_all = gsub("(\\b(?!14\\b|15\\b|16\\b|18\\b|17-18\\b|20-21\\b)[0-9]+\\b)(?<!T )", "[\\1]", cannabis_all, perl = TRUE)) %>%
  #mutate(cannabis_all = gsub("(\\b(?!17-18\\b|20-21\\b)([0-9]+-[0-9]+)\\b)(?<!T )", "[\\1]", cannabis_all, perl = TRUE))

#mark cannabis use level or frequency related words with [word]
df_plcohort <- df_plcohort %>%
  mutate(cannabis_all = str_replace_all(cannabis_all, "(severity|several|frequent|frequency|infrequent|occasional|abuse|dependence|abstinent|cannabis use disorder|misuse|mild or heavy|heavy|light|any|moderate|regular|substance use disorder|cud|cannabis use disorder|without impairment|with cannabis-induced aps|/week|/day|/month|/year|years|times|days|five|sistematic|almost|in remission|<|>|=|-)", "[\\1]"))


#put all the star-words into the recall_cannabis_use_timeframe variable 
df_plcohort <- df_plcohort %>%
  mutate(recall_cannabis_use_timeframe = sapply(str_extract_all(cannabis_all, "\\*([^*]+)\\*"), function(x) paste(x, collapse = " ")))

View(as.data.frame(df_plcohort$recall_cannabis_use_timeframe))

#put all the words in brackets in the cannabis use frequency variable
df_plcohort <- df_plcohort %>%
  mutate(cannabis_use_frequency = sapply(str_extract_all(cannabis_all, "\\[([^\\]]+)\\]"), function(x) paste(x, collapse = " ")))


View(as.data.frame(df_plcohort$cannabis_use_frequency))

View(df_plcohort)

#look at the levels that occur most in the recall timeframe variable
table_lifetime <- as.data.frame(table(as.factor(df_plcohort$recall_cannabis_use_timeframe)))
filtered_table_lifetime <- filter(table_lifetime, Freq > 3)

View(filtered_table_lifetime)

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


#clean cannabis comparision variable 

df_plcohort$cannabis_control <- tolower(df_plcohort$"comparision(control-group)")


df_plcohort <- df_plcohort %>%
 mutate(comparision_coded =cannabis_control)


df_plcohort <- df_plcohort %>%
 mutate(comparision_coded = str_replace_all(`comparision_coded`, "(no cannbis use during the study|no-use|no cannabis use|no use of marijuana|no use or dependence|non-users|non consumers|non users)", "no use"))


 df_plcohort <- df_plcohort %>%
 mutate(comparision_coded = str_replace_all(`comparision_coded`, "(used 0 times|no use (never cannabis)|0 times us of cannabis|never use|never-users|never used|never users (negative urine test)|use of marijuana|no use or dependence|non-users|non consumers|non users)", "never"))


 df_plcohort <- df_plcohort %>%
 mutate(comparision_coded = str_replace_all(`comparision_coded`, "(neverd|neverrs \\(negative urine test\\)|never of cannabis|never users \\(negative urine test\\)|use of marijuana|no use or dependence|non-users|non consumers|non users)", "never"))

table_comp <- as.data.frame(table(as.factor(df_plcohort$comparision_coded)))
filtered_table_comp <- filter(table_comp, Freq > 3)

View(filtered_table_comp)


  
#clean outcome variable 


df_plcohort$outcome <- tolower(df_plcohort$outcome)

df_plcohort <- df_plcohort %>%
 mutate(outcome_clean= outcome) 

# Print words starting with "sym" 
df_plcohort %>%
  filter(str_detect(outcome_clean, "\\bsym\\w*")) %>%
 pull(outcome_clean) %>%
 unique() %>%
 str_split(" ") %>%
  unlist() %>%
 .[str_detect(., "\\bsym\\w*")] %>%
 unique() %>%
 print()

#correct all words with symp that are spelled wrong
df_plcohort <- df_plcohort %>%
  mutate(outcome_clean = str_replace_all(outcomeclean, "(symtoms|symtomes|symtpoms)", "symptoms"))

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

#i repeated it for other comon syllabus such as dim, neg, pos etc.

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
  mutate(outcome_clean = str_replace_all(`outcome_clean`, "(dimensios|dimention)", "dimension"))%>%
  mutate(outcome_clean = str_replace_all(`outcome_clean`, "(ngtaive|negtaive)", "negative"))%>%
  mutate(outcome_clean = str_replace_all(`outcome_clean`, "(schziphrenia|negtaive)", "negative"))

df_plcohort %>%
  filter(str_detect(outcome_clean, "\\bpsych\\w*")) %>%
  pull(outcome_clean) %>%
  unique() %>%
  print()

# Select and view the specified variables
df_selected <- df_plcohort %>%
  select(outcome_clean, outcome_measure)

View(df_selected)

df_outcome_stats <- df_plcohort %>%
  select(outcome_clean, outcome_measure, outcome_measure_coded,mean_c,sd_c, mean_nc, sd_nc, or, lci_or,uci_or,aor,lci_aor,uci_aor,cu_p,ncu_p,cu_np,ncu_np)

View(df_outcome_stats)

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

#kind of psychotic symptoms and kind of relapse 
df_plcohort <- df_plcohort %>%
  mutate(suboutcome = case_when(
    str_detect(outcome_clean, ) ~ ,




























df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all("outcome_coded", "(negative psychosis|negative dimension|negative schizophrenia symptoms|negative symptoms)", "negative psychotic symptoms"))%>% 
  mutate(outcome_coded = str_replace_all("outcome_coded", "negative psychotic symptoms of psychotic symptoms", "negative psychotic symptoms"))


df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all("outcome_coded", "(negative psychosis|negative dimension|negative schizophrenia symptoms|negative symptoms)", "negative psychotic symptoms"))

df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all("outcome_coded", "(positive dimension|positive psychosis|positive schziphrenia symptoms|positive symptoms)", "positive psychotic symptoms"))

#view levels as a table so that you can decide which expression to take and which values you have to rename
View(as.data.frame(table(as.factor(df_plcohort$outcome_coded))))

selection <-df_plcohort[df_plcohort$outcome_coded,]

View(selection)

df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all(outcome_coded, "(schizophrenia symptoms (0-58) |mean psychotic symptoms)", "grandiosity"|"hallucinations"|"disorganized symptoms"|"general symptoms"|"any cidi hallucination item"|"paranoia"|"first-rank)", "psychotic symptoms"))


df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all(`outcome_coded`, "(icd-8|psychosis onset|psychosis onset|non-affective psychosis)", "development of a psychotic disorder"))


df_plcohort <- df_plcohort %>%
  mutate(outcome_coded = str_replace_all('outcome_coded', "(chr+", "chr state"))

# Create a table from the outcome_coded column
table_data <- as.data.frame(table(as.factor(df_plcohort$outcome_coded)))

filtered_table_data <- filter(table_data, Freq > 5)
# Display the filtered table
View(filtered_table_data)




#variables who might have info psychosis type
#fep_vs_chronic 
#target_population
#cohort
#cohort_more_detail
#diagnostic_tool
#dt_name
#kind_of_psychosis

df_cohort <- df_plcohort %>%
select(study_year_psycont,fep_vs_chronic, target_population,cohort,cohort_more_detail,diagnostic_tool,kind_of_psychosis,outcome, outcome_measure, outcome_clean, outcome_measure, outcome_measure_coded,mean_c,sd_c, mean_nc, sd_nc, or, lci_or,uci_or,aor,lci_aor,uci_aor,cu_p,ncu_p,cu_np,ncu_np)

View(df_cohort)


df_pop<- df_plcohort %>%
select(target_population,population)

View(df_pop)
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

View(as.data.frame(table(df_plcohort$population)))
View(as.data.frame(table(df_plcohort$target_population)))

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

View(df_plcohort)

#sample_size_(total_n)
#total_n 


#%_male_in_cannabis_group 
#%_male_in_no_cannabis_group


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



