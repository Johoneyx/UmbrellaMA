library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(metafor)
library(dplyr)
library(stringr)
library(purrr)
library(openxlsx)


#-------------------------------------------------------------------Data_Preparation_Continous_Meta-Analysis----------------------------------




















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
                      data = dat, slab=paste("Study", studycode, "Estimate", esid)) 
                                      
                      

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

