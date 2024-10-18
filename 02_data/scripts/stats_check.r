library(metafor)
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


df_plcohort <- read_xlsx("02_data/cleandata/cohort_df_clean.xlsx")

names(df_plcohort)

#***********Check_SMD_DATA***************

#"mean_c" "sd_c","mean_dc",sd_c,sd_nc "mean_ndc","mean_nc" "sd_nc" "smd" "smd_measure" "lci_smd" "uci_smd" #"p(smd)"#"n_cu"
 #"n_ncu"

# List of columns to check for non-NA values
columns_to_check <- c("mean_c", "sd_c", "n_dcu", "mean_dc", "sd_dc", "continued_use", "continued_use_n", "sd_continueduse", "mean_nc", "sd_nc", "smd", "smd_measure", "lci_smd","uci_smd", "p(smd)","statistical_method","other_statistical_method","statistical_parameter","factor", "b", "se_b", "p_b", "ab", "se_ab","p_ab", "mean-starter", "sd_starter","β_lci", "β_uci")

# Filter rows where at least one of the specified columns is not NA
df_continous <- df_plcohort %>%
  filter(if_any(all_of(columns_to_check), ~ !is.na(.))) %>%
  select(reference,comment,population,studycode,cannabis_all, N, n_cu, mean_c, sd_c, n_dcu, mean_dc, sd_dc, continued_use, continued_use_n, sd_continueduse, mean_nc, sd_nc, smd, smd_measure, lci_smd, uci_smd, 'p(smd)', statistical_method,other_statistical_method, b, se_b, p_b, ab, se_ab, p_ab,'mean-starter', sd_starter, β_lci, β_uci, followup, outcome)


View(df_continous)


#***********Check_OR_DATA****************




#***********Check_Other_data************

# Filter rows where 'b' is not NA, get distinct studycode values, and count them
df_continous %>%
  filter(!is.na(b)|!is.na(ab)) %>%
  distinct(studycode) %>%
  count() %>%
  
to_check <- df_continous %>%
  filter(!is.na(b)|!is.na(ab)|!is.na(other_statistical_method)|!is.na(statistical_method))


# Add an empty column 'standardized?' and conditionally fill it
df_continous <- df_continous %>%
  mutate(`standardized` = if_else(studycode %in% c("","") "standardized", NA_character_)) %>%
  mutate("unstandardized" = if_else(studycode %in% c("","")
  "unstandardized",NA_character_))

write_xlsx(to_check,"02_data/tocheck24.09.2024_continous.xlsx")
