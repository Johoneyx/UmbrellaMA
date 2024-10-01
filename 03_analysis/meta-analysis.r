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


#*****************DF_CONTINOUS_OUTCOME*****************************************
#Number of participants in both groups
#mean and standard deviation 
#other option SMD  
#option t-value and p-value 
#option only p-value and knowledge of effectdirection(sign)

#"mean_c" "sd_c" "mean_nc" "sd_nc" "smd" "smd_measure" "lci_smd" "uci_smd" #"p(smd)"#"n_cu"
 #"n_ncu"


df_continous <- df_plcohort %>%
  select(studycode,
    population,
    study_type,
    `prospective?`,
    cannabis_all,
    recall_cannabis_use_timeframe,
    cannabis_use_frequency,
    cannabis_control,
    comparision_coded,
    outcome_clean,
    outcome_coded,
    followup,
    N_calculated,
    n_outcome,
    n_no_outcome,
    n_cu,
    n_ncu,
    statistical_method,
    mean_in_outcome_group = `mean_in__outcome_group`,
    sd_in_outcome_group = `sd_in_outcome_group`,
    mean_in_no_outcome_group = `mean_in_no-outcome_group`,
    sd_in_no_outcome_group = `sd_in_no-outcome_group`,
    mean_c,
    sd_c,
    mean_nc,
    sd_nc,
    smd,
    smd_measure,
    lci_smd,
    uci_smd,
    p_smd = `p(smd)`,
    b,
    se_b,
    p_b,
    ab,
    se_ab,
    p_ab,
    covariates_ab,
    `β_lci`,
    `β_uci`,
    f_value = `f-value`,
    other_statistical_method,
    factor,
    statistical_parameter,
    p_value = `p-value`,
    factors_accounted_for,
    time_frame,
  )

View(df_continous)
#***************calculate_effectsize(SMD)*************************************************************

# Helper function to convert to numeric and capture warnings
coerce_to_numeric <- function(x, col_name) {
  result <- suppressWarnings(as.numeric(x))
  if (any(is.na(result) & !is.na(x))) {
    warning(paste("NAs introduced by coercion in column", col_name))
    print(df_continous[is.na(result) & !is.na(x), col_name, drop = FALSE])
  }
  return(result)
}

# Convert specified columns to numeric and capture warnings
df_continous <- df_continous %>%
  mutate(across(c(mean_c, sd_c, n_cu, mean_nc, sd_nc, n_ncu), ~ coerce_to_numeric(., cur_column())))

# View the updated dataframe
View(df_continous)

#df_continous <- escalc(measure="SMD", m1i=mean_c, sd1i=sd_c, n1i=n_cu, m2i=mean_nc, sd2i=sd_nc, n2i=n_ncu, data=df_continous)



#df_continous <- escalc(measure="SMD", m1i=mean_c, sd1i=sd_c, n1i=n_cu,m2i=mean_nc, sd2i=sd_nc, n2i=n_ncu, data=df_continous)


df_continous  <- df_continous  %>%
  mutate(study = as.numeric(factor(studycode, levels = unique(studycode))))

View(df_continous)

df_continous  <- df_continous  %>%
  group_by(study) %>%
  mutate(esid = row_number()) %>%
  ungroup()

View(df_continous)

# Calculate effect sizes
df_continous  <- escalc(measure ="SMD", m1i = mean_c, sd1i = sd_c, n1i = n_cu, m2i = mean_nc, sd2i = sd_nc, n2i = n_ncu, 
data = df_continous , slab=paste("Study", study, "Estimate", esid)) 
                                      

### assume that the effect sizes within studies are correlated with rho=0.6
V <- vcalc(vi, cluster=study, obs=esid, data=df_continous, rho=0.6)
 
### fit multilevel model using this approximate V matrix
res <- rma.mv(yi, V, random = ~ 1 | study/esid, data=df_continous , digits=3)
res

par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))
 
dd <- c(0,diff(df_continous $study))
rows <- (1:res$k) + cumsum(dd)


forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-5,7), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")



#**************************fit_Meta-Analysis_Model_SMD**********


### assume that the effect sizes within studies are correlated with rho=0.6
V <- vcalc(vi, cluster=study, obs=esid, data=df_continous, rho=0.6)
 
### fit multilevel model using this approximate V matrix
res <- rma.mv(yi, V, random = ~ 1 | study/esid, data=df_continous, digits=3)
res

par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))
 
dd <- c(0,diff(df_continous$study))
rows <- (1:res$k) + cumsum(dd)



#*****************************Create_Forestplot_Continous****


forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-5,7), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")


#************************Create_Forestplot_aggregated_Continous**************************

agg <- aggregate(df_continous, cluster=study, V=vcov(res, type="obs"), addk=TRUE)

res <- rma(yi, vi, method="EE", data=agg, digits=3)
res

forest(res, xlim=c(-4,5), mlab="Pooled Estimate", header=TRUE,
       ilab=ki, ilab.lab="Estimates", ilab.xpos=-2)













#**********************DF_BINARY_OUTCOME*********************


df_binary <- df_plcohort %>%
  select(studycode,
    population,
    study_type,
    `prospective?`,
    cannabis_all,
    recall_cannabis_use_timeframe,
    cannabis_use_frequency,
    cannabis_control,
    comparision_coded,
    outcome_clean,
    outcome_coded,
    followup,
    N_calculated,
    n_outcome,
    n_no_outcome,
    n_cu,
    n_ncu,
    cu_p,
    ncu_p,
    cu_np,
    ncu_np,
    or,
    lci_or,
    uci_or,
    p_or,
    or_direction,
    aor,
    lci_aor,
    uci_aor,
    p_aor,
    adjusted_factors_aor,
    aor_direction,
    rr,
    lci_rr,
    uci_rr,
    p_rr,
    rr_direction,
    arr,
    lci_arr,
    uci_arr,
    p_arr,
    arr_direction,
    adjusted_factors_arr,
  )


View(df_binary)



df_or <- df_plcohort %>%
filter(!is.na(or)) %>%
summarize(count = n())



# Count the number of rows with "aor" filled
df_aor <- df_plcohort %>%
  filter(!is.na(aor))


summarize(count = n(df_aor))
view(df_aor)

# Count the number of rows with "rr" filled
df_rr <- df_plcohort %>%
  filter(!is.na(rr))


summarize(count = n(df_rr))
view(df_rr)

# Count the number of rows with all of "cu_p", "ncu_p", "cu_np", "ncu_np" filled
df_all_cu_ncu <- df_plcohort %>%
  filter(!is.na(cu_p) & !is.na(ncu_p) & !is.na(cu_np) & !is.na(ncu_np))

summarize(count = n(df_all_cu_ncu))
view(df_all_cu_ncu)

# Count the number of rows with all of "or", "aor", "rr", "cu_p", "ncu_p", "cu_np", "ncu_np" filled
df_all <- df_plcohort %>%
  filter(!is.na(or) & !is.na(aor) & !is.na(rr) & !is.na(cu_p) & !is.na(ncu_p) & !is.na(cu_np) & !is.na(ncu_np))

summarize(count = n(df_all))
view(df_all)



#******************************************Calculate_Binary_Outcome_Effectsize*********************************************
#transitioned not transitioned in cannabis use and no use (4-felder)
#odds ratios  


df_binary <- df_binary %>%
  mutate_at(vars(cu_p, cu_np, ncu_p, ncu_np,or,lci_or,uci_or,p_or), as.numeric)


df_binary <- escalc(measure="OR", ai=cu_p, bi=cu_np, ci=ncu_p, di= ncu_np, data = df_binary)

View(df_binary)



df_binary<- conv.wald(out=or, ci.lb=lci_or, ci.ub=uci_or, pval=p_or, n=N_calculated, data=df_binary, transf=log)


View(df_binary)

#*******************DATA-PREP*********************************#

df_binary <- df_binary%>%
  mutate(study = as.numeric(factor(studycode, levels = unique(studycode))))

  
df_binary<- df_binary %>%
  group_by(study) %>%
  mutate(esid = row_number()) %>%
  ungroup()







#**************************fit_Meta-Analysis_Model_OR**********

### turn 'dat' into an 'escalc' object (and add study labels)
df_binary <- escalc(measure="OR", yi=yi, vi=vi, data=df_binary, slab=paste( "Study",studycode, "Estimate", esid))
 
### assume that the effect sizes within studies are correlated with rho=0.6
V <- vcalc(vi, cluster=study, obs=esid, data=df_binary, rho=0.6)
 

### fit multilevel model using this approximate V matrix
res <- rma.mv(yi, V, random = ~ 1 | study/esid, data=df_binary, digits=3)
res


#**************************Create_Forestplot_OR**************

par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))
 
dd <- c(0,diff(df_binary$study))
rows <- (1:res$k) + cumsum(dd)
forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-5,7), cex=0.4,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")


#***************aggregated forestplot_OR*****************

agg <- aggregate(df_binary, cluster=study, V=vcov(res, type="obs"), addk=TRUE)

res <- rma(yi, vi, method="EE", data=agg, digits=3)
res

forest(res, xlim=c(-4,5), mlab="Pooled Estimate", header=TRUE,
       ilab=ki, ilab.lab="Estimates", ilab.xpos=-2)



#-----------------------------df_Other-----------------------------------

df_binary <- df_plcohort %>%
  select(
    population,
    study_type,
    `prospective?`,
    cannabis_all,
    recall_cannabis_use_timeframe,
    cannabis_use_frequency,
    cannabis_control,
    comparision_coded,
    outcome_clean,
    outcome_coded,
    followup,
    N_calculated,
    n_outcome,
    n_no_outcome,
    n_cu,
    n_ncu,
    cu_p,
    ncu_p,
    cu_np,
    ncu_np,
    hr,
    lci_hr,
    uci_hr,
    p_hr,
    timeframe_hr,
    hr_direction,
    ahr,
    lci_ahr,
    uci_ahr,
    p_ahr,
    timeframe_ahr,
    covariates_ahr,
    direction_ahr,
    statistical_method,
    b,
    se_b,
    p_b,
    ab,
    se_ab,
    p_ab,
    covariates_ab,
    `β_lci`,
    `β_uci`,
    f_value = `f-value`,
    other_statistical_method,
    factor,
    statistical_parameter,
    p_value = `p-value`,
    factors_accounted_for,
    time_frame,
    percent_outcome = `%_outcome`,
    statistical_method,
    percent_chr_transition_to_fep_in_cannabis_groups = `%_of_chr_transition_to_fep_in_cannabis_grups`,
    se_of_percent_transition_cu = `se_of_%_transition_cu`,
    percent_chr_transition_to_fep_in_no_cannabis_groups = `%_of_chr_transition_to_fep_in_no-cannabis_grups`,
    `se_of_%_transition_ncu`,
    `%_in_outcome_group`,
    `%_in_no-outcome_group`
  )