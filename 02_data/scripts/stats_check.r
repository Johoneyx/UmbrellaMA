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

View(df_plcohort)

#***********Check_SMD_DATA***************

#"mean_c" "sd_c","mean_dc",sd_c,sd_nc "mean_ndc","mean_nc" "sd_nc" "smd" "smd_measure" "lci_smd" "uci_smd" #"p(smd)"#"n_cu"
 #"n_ncu"

View(df_plcohort %>% 
  select(cannabis_all, N, n_cu, mean_c, sd_c, n_dcu, mean_dc, sd_dc, 
         continued_use, continued_use_n, sd_continueduse, mean_nc, sd_nc, 
         smd, smd_measure, lci_smd, uci_smd, 'p(smd)', statistical_method, 
         other_statistical_method, b, se_b, p_b, ab, se_ab, p_ab, 
         'mean-starter', sd_starter, β_lci, β_uci, followup, outcome))



#***********Check_OR_DATA****************




#***********Check_Other_data************


