library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(metafor)
library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)


merged_df_clean <- read_excel("02_data/cleandata/merged_df_clean.xlsx")
df_studylist <- read_excel("02_data/cleandata/studylist_meta-analysis.xlsx")





#---------------------------------------------------------------------------------------------------------------------------------------------
#we need an ID variable for the different datarows of the same paper
#the problem is right now I am not sure whether some of the data have been double-extracted 

# Filter df_meta_analysis to include only those studycodes present in df_studylist
df_meta_analysis <- merged_df_clean %>%
  semi_join(df_studylist, by = "studycode")

# View the filtered dataframe
view(df_meta_analysis)


df_meta_analysis <- df_meta_analysis %>%
  group_by(studycode) %>%
  mutate(row_id = row_number())

# Add a suffix to the ID variable to distinguish rows of the same paper

colnames(df_meta_analysis) <- gsub(" ", "_", colnames(df_meta_analysis))

df_meta_analysis %>%
  select(
    studycode,
    n_no_outcome,
    n_cu,
    n_ncu,
    n_outcome,
    mean_c,
    sd_c, 
    n_dcu, 
    mean_dc,
    sd_dc,
    continued_use,
    #mean_starter,
    sd_starter,
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

df_meta_analysis %>%
  select(
    studycode,
    cannabis_level_of_use,
    `comparision(control-group)`
    n_no_outcome,
    n_cu,
    n_ncu,
    n_outcome,
    mean_c,
    sd_c, 
    n_dcu, 
    mean_dc,
    sd_dc,
    continued_use,
    #mean_starter,
    sd_starter,
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