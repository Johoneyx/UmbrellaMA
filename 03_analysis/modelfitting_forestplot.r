
library(tidyverse)
library(readxl)
library(writexl)
library(metafor)
library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)
library(meta)


df_meta_analysis <- read_excel("02_data/cleandata/df_meta_analysis.xlsx")


#**************************prepare_data_for_modelfit************************

#take only those rows that have SMD data
df_meta_analysis <- df_meta_analysis %>%
filter(!is.na(yi_SMD_all)&!is.na(vi_SMD_all))

#define a variable that numerates effect estimates and one that numerates studycode 
df_meta_analysis <-  df_meta_analysis  %>%
  mutate(study = as.numeric(factor(studycode, levels = unique(studycode))))


df_meta_analysis  <- df_meta_analysis  %>%
  group_by(study) %>%
  mutate(esid = row_number()) %>%
  ungroup()

View(df_meta_analysis )

###########################################
#MODEL FITTING
###########################################

### assume that the effect sizes within studies are correlated with rho=0.6
V <- vcalc(vi_SMD_all, cluster=study, obs=esid, data=df_meta_analysis, rho=0.6)

rem_SMD_all <- rma.mv(yi_SMD_all, V, random = ~1 | study/esid, data = df_meta_analysis, digits=3)


View(df_meta_analysis)

############################################
#Forestplots
############################################

                               
par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))

dd <- c(0,diff(df_meta_analysis$study))
rows <- (1:rem_SMD_all$k) + cumsum(dd)


forest(rem_SMD_all, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-10,14), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")


#********************************Create Forestplot with aggregated values****************************

dat<- escalc("SMD", yi=yi_SMD_all, vi=vi_SMD_all, 
data=df_meta_analysis)

View(dat)

agg <- aggregate(dat, cluster=study, V=vcov(rem_SMD_all, type="obs"), addk=TRUE)


res_agg <- rma(yi, vi, method="EE", data=agg, digits=3)
res_agg


forest(res_agg, xlim=c(-5,5), mlab="Pooled Estimate", header=TRUE, slab=studycode, ilab= ki, ilab.lab="Estimates",  ylim=c(-10,res_agg$k+20), order=population, rows=c(75:52,47:27,21:4))
text(-5, c(76,48,22), c("CHR", "HP",
                        "P"), font=4, pos=4)



### fit random-effects model in the three subgroups
res.hp <- rma(yi_SMD_all,vi_SMD_all, subset=(population=="HP"),data=df_meta_analysis)
res.chr <- rma(yi_SMD_all,vi_SMD_all,subset=(population=="CHR"),data=df_meta_analysis)
res.p <- rma(yi_SMD_all,vi_SMD_all, subset=(population=="P"),data=df_meta_analysis)


### add summary polygons for the three subgroups
addpoly(res.chr, row=50.5)
addpoly(res.hp, row= 24.5)
addpoly(res.p, row= 1.5)



############################################
#   MODEL WITH HP, CHR, P Groups
############################################



  