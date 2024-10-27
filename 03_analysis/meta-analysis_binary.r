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
 
### need the rounded estimate and CI bounds further below
pred <- fmtx(c(pred$pred, pred$ci.lb, pred$ci.ub), digits=2)
 
### total number of studies
k <- nrow(dat_agg_binary)
 
### set na.action to "na.pass" (instead of the default, which is "na.omit"),
### so that even study 3 (with a missing log odds ratio) will be shown in the
### forest plot
options(na.action = "na.pass")
 
### get the weights and format them as will be used in the forest plot
weights <- paste0(fmtx(weights(res_agg_binary), digits=1), "%")
weights[weights == "NA%"] <- ""
 
### adjust the margins
par(mar=c(10.9,0,1.8,1.3), mgp=c(3,0.2,0), tcl=-0.2)
 
### forest plot with extra annotations
sav <- forest(res_agg_binary, atransf=exp, at=log(c(0.01, 0.10, 1, 10, 100)), xlim=c(-30,11),
       xlab="", efac=c(0,4), textpos=c(-30,-4.7), lty=c(1,1,0), refline=NA,
       ilab.xpos=c(-20.6,-18.6,-16.1,-14.1,-10.8), ilab.pos=2,
       cex=0.78, header=c("Study or Subgroup","IV, Random, 95% CI"), mlab="")
 
### add horizontal line at the top
segments(sav$xlim[1]+0.5, k+1, sav$xlim[2], k+1, lwd=0.8)
 
### add vertical reference line at 0
segments(0, -2, 0, k+1, lwd=0.8)
 
### now we add a bunch of text; since some of the text falls outside of the
### plot region, we set xpd=NA so nothing gets clipped
par(xpd=NA)
 
### adjust cex as used in the forest plot and use a bold font
par(cex=sav$cex, font=2)
 
text(sav$ilab.xpos, k+2, pos=2, c("Events","Total","Events","Total","Weight"))
text(c(mean(sav$ilab.xpos[1:2]),mean(sav$ilab.xpos[3:4])), k+3, pos=2, c("Cannabis","No Cannabis"))
text(sav$textpos[2], k+3, "Odds ratio", pos=2)
text(0, k+3, "Odds ratio")
text(sav$xlim[2]-0.6, k+3, "Risk of Bias", pos=2)
text(0, k+2, "IV, Random, 95% CI")
text(c(sav$xlim[1],sav$ilab.xpos[c(2,4,5)]), -1, pos=c(4,2,2,2,2),
     c("Total (95% CI)", sum(dat_agg_binary$n_outcome_calculated), sum(dat_agg_binary$n_no_outcome_calculated), "100.0%"))
text(sav$xlim[1], -7, pos=4, "Risk of bias legend")
 
### first hide the non-bold summary estimate text and then add it back in bold font
rect(sav$textpos[2], -1.5, sav$ilab.xpos[5], -0.5, col="white", border=NA)
text(sav$textpos[2], -1, paste0(pred[1], " [", pred[2], ",  ", pred[3], "]"), pos=2)
 
### use a non-bold font for the rest of the text
par(cex=sav$cex, font=1)
 
### add 'Favours caffeine'/'Favours decaf' text below the x-axis
text(log(c(0.01, 100)), -4, c("Favours caffeine","Favours decaf"), pos=c(4,2), offset=-0.5)
 
### add 'Not estimable' for the study with missing log odds ratio
text(sav$textpos[2], k+1-which(is.na(dat_agg_binary$yi)), "Not estimable", pos=2)
 
### add text for total events
text(sav$xlim[1], -2, pos=4, "Total events:")
text(sav$ilab.xpos[c(1,3)], -2, c(sum(dat_agg_binary$ai),sum(dat_agg_binary$ci)), pos=2)
 
### add text with heterogeneity statistics
text(sav$xlim[1], -3, pos=4, bquote(paste("Heterogeneity: ",
   "Tau"^2, " = ", .(fmtx(res$tau2, digits=2)), "; ",
   "Chi"^2, " = ", .(fmtx(res$QE, digits=2)),
   ", df = ", .(res$k - res$p),
   " (", .(fmtp(res$QEp, digits=2, pname="P", add0=TRUE, sep=TRUE, equal=TRUE)), "); ",
   I^2, " = ", .(round(res$I2)), "%")))
 
### add text for test of overall effect
text(sav$xlim[1], -4, pos=4, bquote(paste("Test for overall effect: ",
   "Z = ", .(fmtx(res$zval, digits=2)),
   " (", .(fmtp(res$pval, digits=2, pname="P", add0=TRUE, sep=TRUE, equal=TRUE)), ")")))
 
### add text for test of subgroup differences
text(sav$xlim[1], -5, pos=4, bquote(paste("Test for subgroup differences: Not applicable")))
 
### add risk of bias points and symbols
cols <- c("#00cc00", "#cc0000", "#eeee00","#eeee00")
syms <- levels(dat_agg_binary$Q1)
pos  <- seq(sav$xlim[2]-5.5,sav$xlim[2]-0.5,length=6)
for (i in 1:6) {
   points(rep(pos[i],k), k:1, pch=19, col=cols[dat_agg_binary[[6+i]]], cex=2.2)
   text(pos[i], k:1, syms[dat_agg_binary[[6+i]]], font=2)
}
text(pos, k+2, c("A","B","C","D","E","F"), font=2)
 
### add risk of bias legend
text(sav$xlim[1], -8:-13, pos=4, c(
   "(A) Random sequence generation (selection bias)",
   "(B) Allocation concealment (selection bias)",
   "(C) Blinding of participants and personnel (performance bias)",
   "(D) Incomplete outcome data (attrition bias)",
   "(E) Selective reporting (reporting bias)",
   "(F) Other bias"))





   library(dplyr)
library(dplyr)

# Example data
dat_agg_binary <- data.frame(
  Q1 = c(1, 2, 0, NA, 1),
  Q2 = c(2, 1, 0, NA, 2),
  Q3 = c(0, 1, 2, NA, 0),
  Q4 = c(NA, 0, 1, 2, 1),
  Q5 = c(1, 2, 0, NA, 1),
  Q6 = c(2, 1, 0, NA, 2),
  Q7 = c(0, 1, 2, NA, 0),
  Q8 = c(NA, 0, 1, 2, 1)
)

# Replace values in specified columns
dat_agg_binary <- dat_agg_binary %>%
  mutate(across(c(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8), ~ case_when(
    . == 1 ~ "+",
    . == 2 ~ "++",
    . == 0 ~ "-",
    is.na(.) ~ "?"
  )))

# Define colors and symbols
cols <- c("+" = "#00cc00", "++" = "#cc0000", "-" = "#eeee00", "?" = "#000000")
syms <- levels(factor(c("+", "++", "-", "?")))
pos  <- seq(0, 5, length = 6)  # Adjust positions as needed

# Set up the plotting area
plot(1, type = "n", xlim = c(-1, 6), ylim = c(0, k + 3), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")

# Add risk of bias points and symbols
k <- nrow(dat_agg_binary)
for (i in 1:6) {
   points(rep(pos[i], k), k:1, pch = 19, col = cols[dat_agg_binary[[i]]], cex = 2.2)
   text(pos[i], k:1, dat_agg_binary[[i]], font = 2)
}
text(pos, k + 2, c("A", "B", "C", "D", "E", "F"), font = 2)

# Add labels for risk of bias categories
text(-1, -8:-13, pos = 4, c(
   "(A) Random sequence generation (selection bias)",
   "(B) Allocation concealment (selection bias)",
   "(C) Blinding of participants and personnel (performance bias)",
   "(D) Incomplete outcome data (attrition bias)",
   "(E) Selective reporting (reporting bias)",
   "(F) Other bias"))