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

#***********************************************************Primary_Studies***********************************************************************************************
print(df_plcohort$other_statistical_method)
print(df_plcohort$statistical_method)













#*******************************************************