
df_HP_continous <- df_plcohort %>%
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
    percent_outcome = `%_outcome`,
    statistical_method,
    percent_chr_transition_to_fep_in_cannabis_groups = `%_of_chr_transition_to_fep_in_cannabis_grups`,
    se_of_percent_transition_cu = `se_of_%_transition_cu`,
    percent_chr_transition_to_fep_in_no_cannabis_groups = `%_of_chr_transition_to_fep_in_no-cannabis_grups`,
    `se_of_%_transition_ncu`,
    `%_in_outcome_group`,
    `%_in_no-outcome_group`
  )


#***************calculate_effectsize(SMD)*************************************************************

dat <- df_plcohort %>%
filter(population=="HP")

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



#**************************fit_Meta-Analysis_Model_SMD**********


### assume that the effect sizes within studies are correlated with rho=0.6
V <- vcalc(vi, cluster=study, obs=esid, data=dat, rho=0.6)
 
### fit multilevel model using this approximate V matrix
res <- rma.mv(yi, V, random = ~ 1 | study/esid, data=dat, digits=3)
res

par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))
 
dd <- c(0,diff(dat$study))
rows <- (1:res$k) + cumsum(dd)



#*****************************Create_Forestplot_HP_Continous****


forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-5,7), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")



#**********************DF_HP_BINARY_OUTCOME*********************


df_HP_binary <- df_plcohort_HP %>%
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