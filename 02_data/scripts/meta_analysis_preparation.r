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


#---------------------------------------------------------------------------------------------------------------------------------------------
#we need an ID variable for the different datarows of the same paper
#the problem is right now I am not sure whether some of the data have been double-extracted 

df_meta_analysis <- df_meta_analysis %>%
  group_by(studycode) %>%
  mutate(row_id = row_number())

# Add a suffix to the ID variable to distinguish rows of the same paper


View(df_meta_analysis)



df_meta_analysis %>%
  select(
    studycode,
    population,
    cannabis_all,
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
    cu_p_calculated,
    ncu_p_calculated,
    cu_np_calculated,
    ncu_np_calculated,
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
    direction_ahr#,
    #percent_of_chr_transition_to_fep_in_cannabis_groups,
    #se_of_percent_transition_cu,
    #percent_of_chr_transition_to_fep_in_no_cannabis_groups,
    #se_of_percent_transition_ncu
  )%>%
  arrange(studycode)%>%
  view()

names(df_meta_analysis)   


#***************************************************

# Convert selected columns to numeric data type
numeric_columns <- c(
  "followup", "n_outcome_calculated", "n_no_outcome_calculated",
  "n_cu_calculated", "n_ncu_calculated", "mean_c", "sd_c", "n_dcu", 
  "mean_dc", "sd_dc", "continued_use", "continued_use_n", "sd_continueduse",
  "mean_nc", "sd_nc", "smd", "lci_smd", "uci_smd", "b", "se_b", "p_b", "ab",
  "se_ab", "p_ab", "β_lci", "β_uci", "f-value", "cu_p_calculated", "ncu_p_calculated",
  "cu_np_calculated", "ncu_np_calculated", "or", "lci_or", "uci_or", "p_or",
  "aor", "lci_aor", "uci_aor", "p_aor", "rr", "lci_rr", "uci_rr", "p_rr",
  "arr", "lci_arr", "uci_arr", "p_arr", "hr", "lci_hr", "uci_hr", "p_hr",
  "ahr", "lci_ahr", "uci_ahr", "p_ahr"
)

df_meta_analysis <- df_meta_analysis %>%
  mutate_at(vars(one_of(numeric_columns)), as.numeric)


df_meta_analysis <- df_meta_analysis %>%
  mutate(study = as.numeric(factor(studycode, levels = unique(studycode))))

df_meta_analysis <- df_meta_analysis %>%
  group_by(studycode) %>%
  mutate(esid = row_number()) %>%
  ungroup()



# Calculate effect sizes
dat <- escalc(measure = "SMD", 
                      m1i = mean_c, sd1i = sd_c, n1i = n_cu_calculated, 
                      m2i = mean_nc, sd2i = sd_nc, n2i = n_ncu_calutated, 
                      data = df_meta_analysis, slab=paste("Study", studycode, "Estimate", esid)) 
                                      
                      

### assume that the effect sizes within studies are correlated with rho=0.6
V <- vcalc(vi, cluster=study, obs=esid, data=df_meta_analysis, rho=0.6)
 
### fit multilevel model using this approximate V matrix
res <- rma.mv(yi, V, random = ~ 1 | study/esid, data=dat, digits=3)
res

par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))
 
dd <- c(0,diff(dat$study))
rows <- (1:res$k) + cumsum(dd)


forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-5,7), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")

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

