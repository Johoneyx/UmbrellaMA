library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(metafor)
library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)


df_meta_analysis <- read_excel("02_data/cleandata/cohort_df_clean.xlsx")

#-------------------------------------------------------------------Data_Preparation_Continous_Meta-Analysis----------------------------------

#view relevant data and check for strange values 
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
  )%>%
  arrange(studycode)%>%
  view()


#relevant variables are 




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
    mean_continueduse,
    sd_continueduse,
    mean_nc,
    sd_nc,
    smd,
    smd_measure,
    lci_smd,
    uci_smd)%>%
    View()

#count how many complete raw SMD data exist
n_complete_SMDraw <- df_meta_analysis %>%
  filter(!is.na(mean_c) & !is.na(sd_c) & !is.na(n_cu) & !is.na(mean_nc)) %>%
  summarise(count = n())

print(n_complete_SMDraw)


#transform relevant rows to numeric
dat_continous <- df_meta_analysis %>%
mutate(across(c(mean_c, sd_c, n_cu, mean_nc, sd_nc, n_ncu), as.numeric))

#count now again how many complete raw SMD data exist
count_non_na_rows <- dat_continous %>%
  filter(!is.na(mean_c) & !is.na(sd_c) & !is.na(n_cu) & !is.na(mean_nc)) %>%
  summarise(count = n())

print(count_non_na_rows) #perfect, all data is still there

 dat_continous  <-  dat_continous  %>%
  mutate(study = as.numeric(factor(studycode, levels = unique(studycode))))


dat_continous <- dat_continous %>%
  group_by(study) %>%
  mutate(esid = row_number()) %>%
  ungroup()


#now check again how many 

#***************calculate_effectsize(SMD)*************************************************************

nSMD <- sum(!is.na(dat_continous$yi))
print(nSMD)


# Calculate effect sizes
dat_continous <- escalc(measure = "SMD", 
                      m1i = mean_c, sd1i = sd_c, n1i = n_cu, 
                      m2i = mean_nc, sd2i = sd_nc, n2i = n_ncu, 
                      data = dat_continous, slab=paste("Study", study, "Estimate", esid)) 

         
    

#**************************fit_Meta-Analysis_Model_SMD**********

### assume that the effect sizes within studies are correlated with rho=0.6
V <- vcalc(vi, cluster=study, obs=esid, data=dat_continous, rho=0.6)
 
### fit multilevel model using this approximate V matrix
res <- rma.mv(yi, V, random = ~ 1 | study/esid, data=dat_continous, digits=3)
res

#count how many values were fitted in the model (any NAs introduced?)
nSMD <- sum(!is.na(dat_continous$yi))                                      #there were 198 - 172 NAs introduced or smd could not be calculated it seems 
print(nSMD)                                          


dat_continous <- dat_continous %>%
filter(!is.na(yi)) %>%
group_by 

View(dat_continous)

#*****************************Create_Forestplot_HP_Continous****
par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))


 dat_continous_plot  <-  dat_continous  %>%
  mutate(study = as.numeric(factor(studycode, levels = unique(studycode))))


dat_continous_plot <- dat_continous %>%
  group_by(study) %>%
  mutate(esid = row_number()) %>%
  ungroup()

 
dd <- c(0,diff(dat_continous_plot$study))
rows <- (1:res$k) + cumsum(dd)

print(dat_continous_plot$study)

forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-10,14), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")

view(dat_continous)


#********************************Create Forestplot with aggregated values****************************


