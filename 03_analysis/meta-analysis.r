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


#****************Multilevel Model for all populations*****************
#question: is there a difference in the association of cannabis use and psychosis based on the vulnerability-level? (HP vs CHR vs P)
#all data taken together
#unit-of-analysis problem because one study investigates multiple outcomes, cannabis-use-levels and time-points
#application of a multilevel model with subgroups or metaregression moderatoranalysis? 
#one analysis for SMDs, one for Log Odds ratios or transforming Odds to SMD or point-biseral correlations?
#unadjusted values 


#***********************SMD*******************************************
#Number of participants in both groups
# mean and standard deviation 
#other option SMD  
#option t-value and p-value 
#option only p-value and knowledge of effectdirection(sign)

#"mean_c" "sd_c" "mean_nc" "sd_nc" "smd" "smd_measure" "lci_smd" "uci_smd" #"p(smd)"#"n_cu"
 #"n_ncu"
































dat <- df_plcohort 

View(dat)

dat <- dat %>%
mutate(across(c(mean_c, sd_c, n_cu, mean_nc, sd_nc, n_ncu), as.numeric))

View(dat)

dat <- escalc(measure="SMD", m1i=mean_c, sd1i=sd_c, n1i=n_cu,m2i=mean_nc, sd2i=sd_nc, n2i=n_ncu, data=dat)



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
forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-5,7), cex=0.4,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")


agg <- aggregate(dat, cluster=study, V=vcov(res, type="obs"), addk=TRUE)
agg <- agg[c(1,4,5,9)]
agg

res <- rma(yi, vi, method="EE", data=agg, digits=3)
res

forest(res, xlim=c(-4,5), mlab="Pooled Estimate", header=TRUE,
       ilab=ki, ilab.lab="Estimates", ilab.xpos=-2)


















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
#"included_in_meta-analysis"
#"correlation_method"
#"%_in_outcome_group"
#"%_in_no-outcome_group"
#"n_dcu"
#"mean_dc"
#"sd_dc"
#"β_lci"
#"β_uci"
# "f-value"