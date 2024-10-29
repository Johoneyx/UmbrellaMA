
library(tidyverse)
library(readxl)
library(writexl)
library(metafor)
library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)
library(meta)


df_meta_analysis <- read_excel("02_data/cleandata/cohort_df_clean.xlsx")
df_rob_clean <- read_excel("02_data/cleandata/df_rob_clean.xlsx")

############################################
#                 DATA PREPARATION         #
############################################

#inspect relevant data 
#relevant data: 

df_meta_analysis %>%
  select(
    studycode,
    population,
    #cannabis_all,
    outcome_coded,
    outcome_measure_coded,
    followup, n_outcome_calculated,
    n_no_outcome_calculated,
    n_cu_calculated,
    n_ncu_calculated,
    mean_c,
    sd_c,
    n_dcu, 
    mean_dc,
    sd_dc,
    continued_use,
    #mean_starter,
    #sd_starter,
    continued_use_n,
    sd_continueduse,
    mean_nc,
    sd_nc,
    smd,
    smd_measure,
    lci_smd,
    uci_smd,
    #p_smd,
    statistical_method,
    other_statistical_method,
    statistical_parameter,
    factor, 
    b,
    se_b,
    p_b,
    ab,
    se_ab,
    p_ab,
    β_lci,
    β_uci,
    `f-value`,
    t_value 
  )%>%
  arrange(studycode)%>%
  view()


#transform relevant rows to numeric, take the values from the n-columns that I calculated before
df_meta_analysis <- df_meta_analysis %>%
mutate(across(c(mean_c, sd_c, n_cu_calculated, mean_nc, sd_nc, n_ncu_calculated, smd,t_value,discontinued_use_n,mean_dc,sd_dc), as.numeric))

#Other requirements:
#pre-calculated SMDs must be uncorrected, if hedges g, backtransform to SMD
#t-values must stem from independent student's t-test 



###########################################
#         SMD CALCULATION                 #
###########################################


#******Calculate SMD from Mean and SD*******

# Count how many complete raw SMD data exist
n_complete_SMDraw <- df_meta_analysis %>%
  filter(!is.na(mean_c) & !is.na(sd_c) & !is.na(n_cu_calculated) & !is.na(mean_nc) & !is.na(n_ncu_calculated)) %>%
  summarise(count = n())

# Print the count
print(n_complete_SMDraw)

#transform relevant rows to numeric , take the values from the n-columns that I calculated 
df_meta_analysis <- df_meta_analysis %>%
mutate(across(c(mean_c, sd_c, n_cu_calculated, mean_nc, sd_nc, n_ncu_calculated, smd,t_value), as.numeric))



#count now again how many complete raw SMD data exist
n_complete_SMDraw_after <- df_meta_analysis %>%
  filter(!is.na(mean_c) & !is.na(sd_c) & !is.na(n_cu_calculated) & !is.na(mean_nc) & !is.na(n_ncu_calculated)) %>%
  summarise(count = n())

# Print the count
print(n_complete_SMDraw_after) #perfect, all data is still there


#Calculate SMD from mean and sd
df_meta_analysis <- escalc(measure = "SMD", m1i = mean_c, sd1i = sd_c, n1i = n_cu_calculated, 
m2i = mean_nc, sd2i = sd_nc, n2i = n_ncu_calculated, data = df_meta_analysis, slab=df_meta_analysis$studycode)

#how many have been calculated?
nSMD <- sum(!is.na(df_meta_analysis$yi))
print(nSMD)

View(df_meta_analysis)

df_meta_analysis <- df_meta_analysis %>%
  rename(vi_SMDraw = vi, yi_SMDraw = yi)

table(df_meta_analysis$yi)

#**************************Calculate_SMD_from_T***************************

df_meta_analysis <- escalc(measure = "SMD", 
n1i = n_cu_calculated, n2i = n_ncu_calculated, data = df_meta_analysis, ti= t_value, slab=df_meta_analysis$studycode)

df_meta_analysis <- df_meta_analysis %>%
  rename(vi_SMD_t = vi, yi_SMD_t= yi)


#****************Calculate from Precalculated SMD *****************

df_meta_analysis <- escalc(measure = "SMD", 
 n1i = n_cu_calculated, n2i =n_ncu_calculated, di=smd, data = df_meta_analysis, slab=df_meta_analysis$studycode)

df_meta_analysis <- df_meta_analysis %>%
  rename(vi_SMD_precalc = vi, yi_SMD_precalc= yi)



#*****************Calculate from mean,sd continued use vs discontinued use ***********

df_meta_analysis <- escalc(measure = "SMD", 
m1i = mean_c, sd1i = sd_c, n1i = n_cu_calculated, 
m2i = mean_dc, sd2i = sd_dc, n2i = discontinued_use_n, data=df_meta_analysis)

df_meta_analysis <- df_meta_analysis %>%
  rename(vi_SMD_cVSdcRaw = vi, yi_SMD_cVSdcRaw= yi)


###########################################
#         OR CALCULATION                 #
###########################################


#transform relevant rows to numeric , take the values from the n-columns that I calculated 
df_meta_analysis<- df_meta_analysis %>%
mutate(across(c(cu_p_calculated, n_cu_calculated,  n_ncu_calculated,ncu_np_calculated, cu_np_calculated, ncu_p_calculated, N_calculated, or,uci_or,lci_or, p_or,aor,p_aor,uci_aor,lci_aor), as.numeric))



#count now again how many complete raw OR data exist
n_complete_ORraw_after <- df_meta_analysis %>%
  filter(!is.na(cu_p_calculated) & !is.na(cu_np_calculated) & !is.na(ncu_p_calculated) & !is.na(ncu_np_calculated))%>%
  summarise(count = n())

# Print the count
print(n_complete_ORraw_after) #perfect, all data is still there

#************Calculate_OR_From_2x2Counts************

df_meta_analysis <- escalc(measure="OR", ai=cu_p_calculated, bi=cu_np_calculated, ci=ncu_p_calculated, di= ncu_np_calculated, data = df_meta_analysis)

df_meta_analysis <- df_meta_analysis %>%
  rename(vi_ORraw = vi, yi_ORraw= yi)

#***********Calculate_OR_From_OR********************

df_meta_analysis<- conv.wald(out=or, ci.lb=lci_or, ci.ub=uci_or, pval=p_or, n=N_calculated, data=df_meta_analysis, transf=log)

df_meta_analysis <- df_meta_analysis %>%
  rename(vi_ORprecalc = vi, yi_ORprecalc= yi)


#**********Calculate_OR_From_aOR********************

df_meta_analysis<- conv.wald(out=aor, ci.lb=lci_aor, ci.ub=uci_aor, pval=p_aor, n=N_calculated, data=df_meta_analysis, transf=log)

df_meta_analysis <- df_meta_analysis %>%
  rename(vi_aORprecalc = vi, yi_aORprecalc= yi)


#**********Calculate_OR_From_RR**************




#**********Calculate_OR_From_RR**************








###########################################
#         ALL COMBINED                    #
###########################################

View(df_meta_analysis)

#combine all OR
df_meta_analysis <- df_meta_analysis %>%
  mutate(OR_combined = coalesce(yi_aORprecalc, yi_ORraw, yi_ORprecalc))

  df_meta_analysis <- df_meta_analysis %>%
  mutate(vi_OR_combined = coalesce(vi_aORprecalc, vi_ORraw, vi_ORprecalc))

#combine all SMD
df_meta_analysis <- df_meta_analysis %>%
  mutate(SMD_combined = coalesce(yi_SMD_precalc, yi_SMDraw, yi_SMD_t, yi_SMD_cVSdcRaw))

  df_meta_analysis <- df_meta_analysis %>%
  mutate(vi_SMD_combined = coalesce(vi_SMD_precalc, vi_SMDraw, vi_SMD_t, vi_SMD_cVSdcRaw))

#convert OR to SMD
#SMD = squareroot(3)/pi * lnOR
#0.5513

df_meta_analysis <- df_meta_analysis %>%
  mutate(SMD_OR=OR_combined * 0.5513) %>%
  mutate(vi_SMD_OR =vi_OR_combined * 0.5513)


#combine all 

df_meta_analysis <- df_meta_analysis %>%
mutate(vi_SMD_all = coalesce(vi_SMD_combined, vi_SMD_OR)) %>%
mutate(yi_SMD_all = coalesce(SMD_combined, SMD_OR))


#*************************merge with Rob******************************
df_meta_analysis <- df_meta_analysis %>%
left_join(df_rob_clean)


#*****************************save datafile***************************

write_xlsx(df_meta_analysis, "C:/Users/johan/Documents/PhD/UmbrellaMA/02_data/cleandata/df_meta_analysis.xlsx")
