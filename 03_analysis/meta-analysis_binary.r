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

#--------Data_Preparation_binary_Meta-Analysis-----------------------

#view relevant data and check for strange values 
#relevant data: 

df_meta_analysis %>%
  select(
    studycode,
    population,
    cannabis_all,
    outcome_coded,
    outcome_measure_coded,
    followup, n_outcomecu_p_calculated,
    n_no_outcomecu_p_calculated,
    n_cucu_p_calculated,
    n_ncucu_p_calculated,
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
    cu_p_calculated,
    ncu_p_calculated,
    cu_npcu_p_calculated,
    ncu_npcu_p_calculated,
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
  )%>%
  arrange(studycode)%>%
  view()
   




# Count how many complete raw SMD data exist
n_complete_ORraw <- df_meta_analysis %>%
  filter(!is.na(cu_p_calculated) & !is.na(cu_np_calculated) & !is.na(ncu_p_calculated) & !is.na(ncu_np_calculated))%>%
  summarise(count = n())

# Print the count
print(n_complete_ORraw)


#transform relevant rows to numeric , take the values from the n-columns that I calculated 
dat_binary<- df_meta_analysis %>%
mutate(across(c(cu_p_calculated, n_cu_calculated,  n_ncu_calculated,ncu_np_calculated, cu_np_calculated, ncu_p_calculated, N_calculated, or,uci_or,lci_or, p_or), as.numeric))



#count now again how many complete raw SMD data exist
n_complete_ORraw_after <- df_meta_analysis %>%
  filter(!is.na(cu_p_calculated) & !is.na(cu_np_calculated) & !is.na(ncu_p_calculated) & !is.na(ncu_np_calculated))%>%
  summarise(count = n())

# Print the count
print(n_complete_ORraw_after) #perfect, all data is still there



#***************calculate_effectsize(SMD)*************************************************************

dat_binary <- escalc(measure="OR", ai=cu_p_calculated, bi=cu_np_calculated, ci=ncu_p_calculated, di= ncu_np_calculated, data = dat_binary)

View(dat_binary)



dat_binary<- conv.wald(out=or, ci.lb=lci_or, ci.ub=uci_or, pval=p_or, n=N_calculated, data=dat_binary, transf=log)


View(dat_binary)
###ATTENTION! NAs introduced, somevalues not halfway between confidence itnervall

#**************************prepare_data_for_modelfit****************

#take only those rows that have OR data
dat_binary<- dat_binary%>%
filter(!is.na(yi) & !is.na(vi))

#define a variable that numerates effect estimates and one that numerates studycode 
 dat_binary <-  dat_binary %>%
  mutate(study = as.numeric(factor(studycode, levels = unique(studycode))))


dat_binary<- dat_binary%>%
  group_by(study) %>%
  mutate(esid = row_number()) %>%
  ungroup()

View(dat_binary) #some vi were not calculated 


#+++++++++++++++++MODEL_FITTING++++++++++++++++++++++++++++++++++++

### assume that the effect sizes within studies are correlated with rho=0.6
V <- vcalc(vi, cluster=study, obs=esid, data=dat_binary, rho=0.6)

### fit multilevel model using this approximate V matrix
res <- rma.mv(yi, V, random = ~ 1 | study/esid, data=dat_binary, digits=3)
res

                                
                                      
#*****************************Create_Forestplot_binary_DATA****

par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))

dd <- c(0,diff(dat_binary$study))
rows <- (1:res$k) + cumsum(dd)


forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-10,14), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")



#********************************Create Forestplot with aggregated values****************************


dat_binary<- escalc("OR", yi=yi, vi=vi, 
data =dat_binary,
var.names=c("yi","vi"))



agg <- aggregate(dat_binary, cluster=study, V=vcov(res, type="obs"), addk=TRUE)


res <- rma(yi, vi, method="EE", data=agg, digits=3)
res

forest(res, xlim=c(-4,5), mlab="Pooled Estimate", header=TRUE,
       ilab=ki,slab=studycode, ilab.lab="Estimates", ilab.xpos=-2)


#***************with modelfit********

forest(res, xlim=c(-4,5), mlab=mlabfun("model results", res), header=TRUE,
       ilab=ki,slab=studycode, ilab.xpos=-2)
 
### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, x) {
   list(bquote(paste(.(text),
      " (Q = ", .(fmtx(x$QE, digits=2)),
      ", df = ", .(x$k - x$p), ", ",
      .(fmtp2(x$QEp)), "; ",
      I^2, " = ", .(fmtx(x$I2, digits=1)), "%, ",
      tau^2, " = ", .(fmtx(x$tau2, digits=2)), ")")))}
 


 mlabfun <- function(text, res) {
  paste(text, " (Q = ", round(res$QE, 2), ", p = ", round(res$QEp, 3), ")", sep="")
}
