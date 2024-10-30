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

View(df_meta_analysis)


df_rob_clean <- read_excel("02_data/cleandata/df_rob_clean.xlsx")

View(df_rob_clean)

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
   

df_meta_analysis <- df_meta_analysis %>% 
left_join(df_rob_clean, by= "studycode")

View(df_meta_analysis)

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
res_binary <- rma.mv(yi, V, random = ~ 1 | study/esid, data=dat_binary, digits=3)
res_binary

typeof(res_binary)
View(res_binary)                       
                                      
#*****************************Create_Forestplot_binary_DATA****

par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))

dd <- c(0,diff(dat_binary$study))
rows <- (1:res_binary$k) + cumsum(dd)


forest(res_binary, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-10,14), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")



#********************************Create Forestplot with aggregated values****************************

# 
dat_binary<- escalc("OR", yi=yi, vi=vi, 
data =dat_binary,
var.names=c("yi","vi"))



agg_binary <- aggregate(dat_binary, cluster=study, V=vcov(res_binary, type="obs"), addk=TRUE)

#fit model with aggregated data 
res_agg_binary <- rma(yi, vi, method="EE", data=agg_binary, digits=3)
res_agg_binary

make forestplot with aggreagated data
forest(res_agg_binary, xlim=c(-4,5), mlab="Pooled Estimate", header=TRUE,
       ilab=ki,slab=studycode, ilab.lab="Estimates", ilab.xpos=-2)
 

#***************make forest-plot with modelfit********
### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, x) {
   list(bquote(paste(.(text),
      " (Q = ", .(fmtx(x$QE, digits=2)),
      ", df = ", .(x$k - x$p), ", ",
      #.(fmtp2(x$QEp)), "; ",
      I^2, " = ", .(fmtx(x$I2, digits=1)), "%, ",
      tau^2, " = ", .(fmtx(x$tau2, digits=2)), ")")))}
 


forest(res_agg_binary, xlim=c(-4,5), mlab=mlabfun("model results", res_agg_binary), header=TRUE,
       ilab=ki,slab=studycode, ilab.xpos=-2)
 

#}}}}}}}}}}}}}}}}}}}}


### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)
forest(res_agg_binary, xlim=c(-16, 14.6), at=seq(-5, 7, by=2), 
ilab=cbind(ki,population),slab=studycode, ilab.lab=c("Estimates","Population"),ilab.xpos=c(-9.5,-6.5), cex=0.75, ylim=c(-10,res_agg_binary$k+4), top=4, order=population, #rows=c(3:4,9:15,20:23), 
mlab="Pooled Estimate",psize=1, header="Author(s) and Year")
 
### set font expansion factor (as in forest() above)
op <- par(cex=0.75)
 
 
### add text for the subgroups
text(-16, c(24,16,5), pos=4, c("HP",
                               "CHR",
                               "P"), font=4)
 
### set par back to the original settings
par(op)
 
### fit random-effects model in the three subgroups
res.hp <- rma(yi, vi, subset=(population=="HP"), data=agg_binary)
res.chr <- rma(yi, vi, subset=(population=="CHR"),     data=agg_binary)
res.p <- rma(yi, vi, subset=(population=="P"),  data=agg_binary)
 
### add summary polygons for the three subgroups
addpoly(res.hp, mlab=mlabfun("RE Model for Subgroup", res.hp))
addpoly(res.chr, mlab=mlabfun("RE Model for Subgroup", res.chr))
addpoly(res.p, mlab=mlabfun("RE Model for Subgroup", res.p))
 
### fit meta-regression model to test for subgroup differences
res_popdiff <- rma(yi, vi, mods = ~ population, data=dat_binary)
 
### add text for the test of subgroup differences
text(-16, -1.8, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
     Q[M], " = ", .(fmtx(res_popdiff$QM, digits=2)),
     ", df = ", .(res_popdiff$p - 1), ", "#, .(fmtp2(res$QMp)
     )))


#***********************Forestplot with Risk of Bias **************
#******************************************************************
View(agg_binary)


library(dplyr)

dat_agg_binary <- agg_binary %>%
  mutate(across(c(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8), ~ case_when(
    . == 1 ~ "+",
    . == 2 ~ "++",
    . == 0 ~ "-",
    is.na(.) ~ "?"
  )))


### estimated average odds ratio (and 95% CI/PI)
pred <- predict(res_agg_binary, transf=exp, digits=2)
pred
 
############################################################################
