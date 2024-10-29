
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
V <- vcalc(vi_SMD_all, cluster=study, obs=esid, data=df_meta_analysis , rho=0.6)

### fit multilevel model using this approximate V matrix
res <- rma.mv(yi_SMD_all, V, random = ~ 1 | study/esid, data=df_meta_analysis, digits=3)
res

                                
                                      
#*****************************Create_Forestplot_Continous_DATA****

par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))

dd <- c(0,diff(df_meta_analysis$study))
rows <- (1:res$k) + cumsum(dd)



forest(res, rows=rows, ylim=c(2,max(rows)+3), xlim=c(-10,14), cex=0.8,
       efac=c(0,1), header=TRUE, mlab="Pooled Estimate")
abline(h = rows[c(1,diff(rows)) == 2] - 1, lty="dotted")


#********************************Create Forestplot with aggregated values****************************

dat<- escalc("SMD", yi=yi_SMD_all, vi=vi_SMD_all, 
data =df_meta_analysis)

View(dat)

agg <- aggregate(dat, cluster=study, V=vcov(res, type="obs"), addk=TRUE)



res <- rma(yi, vi, method="EE", data=agg, digits=3)
res




############################################
#Forestplots 
############################################


forest(res, xlim=c(-4,5), mlab="Pooled Estimate", header=TRUE, slab=studycode,
       ilab=ki, ilab.lab="Estimates", ilab.xpos=-2)


