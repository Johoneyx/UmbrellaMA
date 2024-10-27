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
dat_continous <- df_meta_analysis %>%
mutate(across(c(mean_c, sd_c, n_cu_calculated, mean_nc, sd_nc, n_ncu_calculated, smd,t_value), as.numeric))

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
dat_continous <- df_meta_analysis %>%
mutate(across(c(mean_c, sd_c, n_cu_calculated, mean_nc, sd_nc, n_ncu_calculated, smd,t_value), as.numeric))



#count now again how many complete raw SMD data exist
n_complete_SMDraw_after <- dat_continous %>%
  filter(!is.na(mean_c) & !is.na(sd_c) & !is.na(n_cu_calculated) & !is.na(mean_nc) & !is.na(n_ncu_calculated)) %>%
  summarise(count = n())

# Print the count
print(n_complete_SMDraw_after) #perfect, all data is still there


#Calculate SMD from mean and sd
dat_continous <- escalc(measure = "SMD", m1i = mean_c, sd1i = sd_c, n1i = n_cu_calculated, 
m2i = mean_nc, sd2i = sd_nc, n2i = n_ncu_calculated, data = dat_continous, slab=dat_continous$studycode)

#how many have been calculated?
nSMD <- sum(!is.na(dat_continous$yi))
print(nSMD)

View(dat_continous)

dat_continous <- dat_continous %>%
  rename(vi_SMDraw = vi, yi_SMDraw = yi)

table(dat_continous$yi)

#**************************Calculate_SMD_from_T***************************

dat_continous <- escalc(measure = "SMD", 
n1i = n_cu_calculated, n2i = n_ncu_calculated, data = dat_continous, ti= t_value, slab=dat_continous$studycode)

dat_continous <- dat_continous %>%
  rename(vi_SMD_t = vi, yi_SMD_t= yi)


#****************Calculate from Precalculated SMD *****************

dat_continous <- escalc(measure = "SMD", 
 n1i = n_cu_calculated, n2i =n_ncu_calculated, di=smd, data = dat_continous, slab=dat_continous$studycode)

dat_continous <- dat_continous %>%
  rename(vi_SMD_precalc = vi, yi_SMD_precalc= yi)

table(dat_continous$yi)

#*****************Calculate from mean,sd continued use vs discontinued use ***********


dat_continous <- escalc(measure = "SMD", m1i = mean_c, sd1i = sd_c, n1i =  
m2i = mean_dc, sd2i = sd_dc, n2i = n_dcu data = dat_continous, slab=dat_continous$studycode)







#***********Calculate from mean,sd continous use vs discontinued use*******************



    n_dcu, 
    mean_dc,
    sd_dc,
    print(dat_continous$continued_use),
    #mean_starter,
    #sd_starter,
    continued_use_n,
    sd_continueduse,
    print(dat_continous$continued_use_n)
    print(dat_continous$n_dcu)
    print(dat_continous$mean_dc)
 print(dat_continous$continued_use)



#**************************prepare_data_for_modelfit************************







#take only those rows that have SMD data
dat_continous <- dat_continous %>%
filter(!is.na(yi))

#define a variable that numerates effect estimates and one that numerates studycode 
 dat_continous  <-  dat_continous  %>%
  mutate(study = as.numeric(factor(studycode, levels = unique(studycode))))


dat_continous <- dat_continous %>%
  group_by(study) %>%
  mutate(esid = row_number()) %>%
  ungroup()

View(dat_continous)

###########################################
#MODEL FITTING
###########################################

### assume that the effect sizes within studies are correlated with rho=0.6
V <- vcalc(vi, cluster=study, obs=esid, data=dat_continous, rho=0.6)

### fit multilevel model using this approximate V matrix
res <- rma.mv(yi, V, random = ~ 1 | study/esid, data=dat_continous, digits=3)
res

                                
                                      
#*****************************Create_Forestplot_Continous_DATA****

par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))

dd <- c(0,diff(dat_continous$study))
rows <- (1:res$k) + cumsum(dd)



forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-10,14), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")


#********************************Create Forestplot with aggregated values****************************


dat_continous <- escalc("SMD", yi=yi, vi=vi, 
data =dat_continous,
var.names=c("yi","vi"))


agg <- aggregate(dat_continous, cluster=study, V=vcov(res, type="obs"), addk=TRUE)


res <- rma(yi, vi, method="EE", data=agg, digits=3)
res

forest(res, xlim=c(-4,5), mlab="Pooled Estimate", header=TRUE, slab=studycode,
       ilab=ki, ilab.lab="Estimates", ilab.xpos=-2)


############################################
#Forestplots 
############################################


