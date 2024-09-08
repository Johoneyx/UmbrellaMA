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

#*******************************************SMD*******************************************
#Number of participants in both groups
# mean and standard deviation 
#other option SMD  
#option t-value and p-value 
#option only p-value and knowledge of effectdirection(sign)

#"mean_c" "sd_c" "mean_nc" "sd_nc" "smd" "smd_measure" "lci_smd" "uci_smd" #"p(smd)"#"n_cu"
 #"n_ncu"

#*****************P*********************

dat <- df_plcohort %>%
filter(population=="P")

View(dat)


dat <- dat %>%
mutate(across(c(mean_c, sd_c, n_cu, mean_nc, sd_nc, n_ncu), as.numeric))

View(dat)

dat <- escalc(measure="SMD", m1i=mean_c, sd1i=sd_c, n1i=n_cu,m2i=mean_nc, sd2i=sd_nc, n2i=n_ncu, data=dat)

dat <- dat %>%
filter(!yi =="Invalid Number" )

dat <- dat %>%
  mutate(study = as.numeric(factor(studycode, levels = unique(studycode))))

  View(dat)

dat <- dat %>%
  group_by(study) %>%
  mutate(esid = row_number()) %>%
  ungroup()



# Calculate effect sizes
dat <- escalc(measure = "SMD", 
                      m1i = mean_c, sd1i = sd_c, n1i = n_cu, 
                      m2i = mean_nc, sd2i = sd_nc, n2i = n_ncu, 
                      data = dat, slab=paste("Study", study, "Estimate", esid)) 
                      
                      
                      

### assume that the effect sizes within studies are correlated with rho=0.6
V <- vcalc(vi, cluster=study, obs=esid, data=dat, rho=0.6)
 
### fit multilevel model using this approximate V matrix
res <- rma.mv(yi, V, random = ~ 1 | study/esid, data=dat, digits=3)
res

par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))
 
dd <- c(0,diff(dat$study))
rows <- (1:res$k) + cumsum(dd)


forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-5,7), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")





















#******************************************RAW_ODS*********************************************
#transitioned not transitioned in cannabis use and no use (4-felder)
#odds ratios  


#****************************************ADJUSTED_ODS*****************************************


#****************************************HAZARD*********************************************

#****************************************RISKRATIO********************************************



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


[192] "followup"
[25] "followup_duration"

 [98] "time_frame"


#"lifetime_cannabis_use"
# [20] "cannabis_level_of_use"
 #[21] "comparision(control-group)"
 #22] "cannabis_measure"

#"study_type"
# [24] "prospective?"


 #"study_type"
 #[24] "prospective?"
 #[25] "followup_duration"
# [183] "cannabis_all"
#[184] "recall_cannabis_use_timeframe"
#[185] "cannabis_use_frequency"
#[186] "cannabis_control"
#[187] "comparision_coded"
#[188] "outcome_clean"
#[189] "outcome_measure_clean"
#[190] "outcome_measure_coded"
#[191] "outcome_coded"
#[192] "followup"
#[193] "country_clean"
#[194] "population"
#[195] "fepvschronic_coded"
#171] "discontinued_use_+_outcome"
#[172] "discontinued_use_+_no_outcome"
#[173] "mean_continueduse"
#[174] "sd_continueduse"
#[175] "mean-starter"
#[167] "mean_age_discontinued_(starter)_group"
#[168] "sample_size_(total_n)"
#[169] "starters_use"
#[170] "starter___n"
#[161] "cannabis__&_outcome_analysis_timeframe"
#[131] "cannabis_use_timeframe"
#[132] "continuation_of_use_timeframe"
#[133] "continued_use"
#[134] "discontinued_use"