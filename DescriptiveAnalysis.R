##..................
## 0. Libraries ####
##..................

library(tidyverse)
library(ggsignif)
library(data.table)


##............................
## 1. Read data from zero ####
##............................

### 1.1. read phenotypes

### bw
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/db_pheno/repr")
bw<-read.delim("pheno_data_eur_zbw21.tsv")
bw1<-bw[,c(2,42,43,48,50,51)]
table(duplicated(bw1$FID)) #1119 (the ones that are EUR and have zBW21)

### bmi
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/db_pheno/bmi_metab")
bmi<-read.delim("pheno_data.tsv")
bmi1<-bmi[,c(1:8,15:25,26,28,29,35,36)]
table(duplicated(bmi1$FID))

setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/db_pheno")
bmi2<-read.csv("georgina_prs_30jul.2021.csv")
dim(bmi2) # 10980
bmi22<-bmi2[,c(1,8,10,11,17,18)]
table(duplicated(bmi22$HelixID))

### smoking preg
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_analyses/PGRS_smok_GF/db")
smk<-read.table("HELIX_smok.txt", header=T, sep="\t")
dim(smk)
head(smk)
table(duplicated(smk$HelixID))# yes!!!
smk<-smk[!duplicated(smk$HelixID),]
smk1<-smk[,c(1,20,22,23)]
table(duplicated(smk1$HelixID)) # no

### smoking post
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_analyses/PGRS_smok_GF/PAPER/Data/db")
smk2 <- read.csv("20190905_helix_tobacco_data_MB_request.csv")
smk3<-smk2[,c(2,16)]
dim(smk3)
table(duplicated(smk3$HelixID))
smk13<-merge(smk1, smk3, by="HelixID", all.x=T)
dim(smk13)

### covariates and other bmi
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_analyses/PGRS_smok_GF/db")
cov<-read.csv("prs_smok_01mar.2022.csv")
dim(cov) #1623
names(cov)
cov1<-cov[,c(1,3,4,5,7,8,10,11,14)]
table(duplicated(cov1$HelixID))
cov2<-read.delim("pheno_with_maternaleducation.tsv")
dim(cov2) #1304
cov22<-cov2[,c(1,3)]
table(duplicated(cov22$FID))
cov12<-merge(cov1, cov22, by.x="HelixID", by.y="FID")
table(duplicated(cov12$HelixID))
dim(cov12)


### 1.2. read PRS

### bw
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/002_fBW/fBW21_EUR")
PRSbw21<-read.table("scaled_PRSvalues.txt", header=T, sep=" ")
dim(PRSbw21) 
#select 0.001
names(PRSbw21)
PRSbw21<-PRSbw21[,c(1,6)]
colnames(PRSbw21)[[2]]<-"PRSbw21"
PRSbw21$PRSbw21.t<- ntile(PRSbw21$PRSbw21, 4)
PRSbw21$PRSbw21.g<-rep(NA,nrow(PRSbw21))
PRSbw21$PRSbw21.g[PRSbw21$PRSbw21.t==1]<-"1_PRS_low"
PRSbw21$PRSbw21.g[PRSbw21$PRSbw21.t==2|PRSbw21$PRSbw21.t==3]<-"2_PRS_mid"
PRSbw21$PRSbw21.g[PRSbw21$PRSbw21.t==4]<-"3_PRS_high"
table(PRSbw21$PRSbw21.g)
PRSbw21$PRSbw21.g2<-rep(NA,nrow(PRSbw21))
PRSbw21$PRSbw21.g2[PRSbw21$PRSbw21.t==1]<-"1_PRS_ref"
PRSbw21$PRSbw21.g2[PRSbw21$PRSbw21.t==2|PRSbw21$PRSbw21.t==3]<-"1_PRS_ref"
PRSbw21$PRSbw21.g2[PRSbw21$PRSbw21.t==4]<-"2_PRS_high"
table(PRSbw21$PRSbw21.g2)

### bw-uk
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/002_fBW/fBW21_EUR_panUK")
PRSbw21uk<-read.table("scaled_PRSvalues.txt", header=T, sep=" ")
dim(PRSbw21uk)
# sel 0.05
names(PRSbw21uk)
PRSbw21uk<-PRSbw21uk[,c(1,8)]
colnames(PRSbw21uk)[[2]]<-"PRSbw21uk"
PRSbw21uk$PRSbw21uk.t<- ntile(PRSbw21uk$PRSbw21uk, 4)
PRSbw21uk$PRSbw21uk.g<-rep(NA,nrow(PRSbw21uk))
PRSbw21uk$PRSbw21uk.g[PRSbw21uk$PRSbw21uk.t==1]<-"1_PRS_low"
PRSbw21uk$PRSbw21uk.g[PRSbw21uk$PRSbw21uk.t==2|PRSbw21uk$PRSbw21uk.t==3]<-"2_PRS_mid"
PRSbw21uk$PRSbw21uk.g[PRSbw21uk$PRSbw21uk.t==4]<-"3_PRS_high"
table(PRSbw21uk$PRSbw21uk.g)
PRSbw21uk$PRSbw21uk.g2<-rep(NA,nrow(PRSbw21uk))
PRSbw21uk$PRSbw21uk.g2[PRSbw21uk$PRSbw21uk.t==1]<-"1_PRS_ref"
PRSbw21uk$PRSbw21uk.g2[PRSbw21uk$PRSbw21uk.t==2|PRSbw21uk$PRSbw21uk.t==3]<-"1_PRS_ref"
PRSbw21uk$PRSbw21uk.g2[PRSbw21uk$PRSbw21uk.t==4]<-"2_PRS_high"
table(PRSbw21uk$PRSbw21uk.g2)

### bmi
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/028_BMI/BMI_EUR")
PRSbmi<-read.table("scaled_PRSvalues.txt", header=T, sep=" ")
dim(PRSbmi)
# sel 0.05
names(PRSbmi)
PRSbmi<-PRSbmi[,c(1,8)]
colnames(PRSbmi)[[2]]<-"PRSbmi"
PRSbmi$PRSbmi.t<- ntile(PRSbmi$PRSbmi, 4)
PRSbmi$PRSbmi.g<-rep(NA,nrow(PRSbmi))
PRSbmi$PRSbmi.g[PRSbmi$PRSbmi.t==1]<-"1_PRS_low"
PRSbmi$PRSbmi.g[PRSbmi$PRSbmi.t==2|PRSbmi$PRSbmi.t==3]<-"2_PRS_mid"
PRSbmi$PRSbmi.g[PRSbmi$PRSbmi.t==4]<-"3_PRS_high"
table(PRSbmi$PRSbmi.g)
PRSbmi$PRSbmi.g2<-rep(NA,nrow(PRSbmi))
PRSbmi$PRSbmi.g2[PRSbmi$PRSbmi.t==1]<-"1_PRS_ref"
PRSbmi$PRSbmi.g2[PRSbmi$PRSbmi.t==2|PRSbmi$PRSbmi.t==3]<-"1_PRS_ref"
PRSbmi$PRSbmi.g2[PRSbmi$PRSbmi.t==4]<-"3_PRS_high"
table(PRSbmi$PRSbmi.g2)

### wc
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/042_WC/WC_EUR")
PRSwc<-read.table("scaled_PRSvalues.txt", header=T, sep=" ")
dim(PRSwc)
# sel 0.001
names(PRSwc)
PRSwc<-PRSwc[,c(1,6)]
colnames(PRSwc)[[2]]<-"PRSwc"
PRSwc$PRSwc.t<- ntile(PRSwc$PRSwc, 4)
PRSwc$PRSwc.g<-rep(NA,nrow(PRSwc))
PRSwc$PRSwc.g[PRSwc$PRSwc.t==1]<-"1_PRS_low"
PRSwc$PRSwc.g[PRSwc$PRSwc.t==2|PRSwc$PRSwc.t==3]<-"2_PRS_mid"
PRSwc$PRSwc.g[PRSwc$PRSwc.t==4]<-"3_PRS_high"
table(PRSwc$PRSwc.g)
PRSwc$PRSwc.g2<-rep(NA,nrow(PRSwc))
PRSwc$PRSwc.g2[PRSwc$PRSwc.t==1]<-"1_PRS_ref"
PRSwc$PRSwc.g2[PRSwc$PRSwc.t==2|PRSwc$PRSwc.t==3]<-"1_PRS_ref"
PRSwc$PRSwc.g2[PRSwc$PRSwc.t==4]<-"3_PRS_high"
table(PRSwc$PRSwc.g2)

### fm
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/041_FM/FM_EUR")
PRSfm<-read.table("scaled_PRSvalues.txt", header=T, sep=" ")
dim(PRSfm)
# sel 0.05
names(PRSfm)
PRSfm<-PRSfm[,c(1,8)]
colnames(PRSfm)[[2]]<-"PRSfm"
PRSfm$PRSfm.t<- ntile(PRSfm$PRSfm, 4)
PRSfm$PRSfm.g<-rep(NA,nrow(PRSfm))
PRSfm$PRSfm.g[PRSfm$PRSfm.t==1]<-"1_PRS_low"
PRSfm$PRSfm.g[PRSfm$PRSfm.t==2|PRSfm$PRSfm.t==3]<-"2_PRS_mid"
PRSfm$PRSfm.g[PRSfm$PRSfm.t==4]<-"3_PRS_high"
table(PRSfm$PRSfm.g)
PRSfm$PRSfm.g2<-rep(NA,nrow(PRSfm))
PRSfm$PRSfm.g2[PRSfm$PRSfm.t==1]<-"1_PRS_ref"
PRSfm$PRSfm.g2[PRSfm$PRSfm.t==2|PRSfm$PRSfm.t==3]<-"1_PRS_ref"
PRSfm$PRSfm.g2[PRSfm$PRSfm.t==4]<-"3_PRS_high"
table(PRSfm$PRSfm.g2)

### merge PRS
prs0<-merge(PRSbw21, PRSbw21uk, by="FID", all.x=T, all.y=T)
prs1<-merge(prs0, PRSbmi, by="FID", all.x=T, all.y=T)
prs2<-merge(prs1, PRSwc, by="FID", all.x=T, all.y=T)
prs3<-merge(prs2, PRSfm, by="FID", all.x=T, all.y=T)
table(duplicated(prs3$FID))#1155


### 1.3. merging
### PRS + cov
m1<-merge(prs3, cov12, by.x="FID", by.y="HelixID", all.x=T)
dim(m1)

### previous + smk1
m2<-merge(m1, smk13, by.x="FID", by.y="HelixID", all.x=T)
dim(m2)

### previous + bmi1
m3<-merge(m2, bmi1, by="FID", all.x=T)
dim(m3)

### previous + bw1
m4<-merge(m3, bw1, by="FID", all.x=T)
dim(m4) # 1155


### select IDs of the study
#### EUR
table(m4$FINAL_ancestry, useNA="ifany")#1155

#### with smok
m6<-m4[!is.na(m4$msmok_a),]
dim(m6) # 1095

#### with mat edu
m7<-m6[!is.na(m6$h_edumc),]
dim(m7) # 1086



### 1.4. save objects
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_analyses/PGRS_smok_GF/db")
write.table(m7, "db_revFron_20220308.txt", 
            row.names=F, quote=F, sep="\t")
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_analyses/PGRS_smok_GF/db")
m7<-read.table("db_revFron_20220308.txt", 
               header=T, sep="\t")

m7x<-m7[,c(2,6,10,14,18,53,54,55,62)]
colnames(m7x)<-c("PRS.BW.EGG", "PRS.BW.PanUK", "PRS.BMI", "PRS.WC", 
                 "PRS.FM", "zBMI", "zWC", "zFM", "zBW")

setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_analyses/PGRS_smok_GF/db")
write.table(m7x, "db_revFron_correlation_20220308.txt", 
            row.names=F, quote=F, sep="\t")



##.............................
## 2. Descriptive analysis ####
##.............................


### for 1086
# preterm
table(m7$preterm, useNA="ifany")
table(m7$preterm, m7$cohort, useNA="ifany")
table(m7$msmok_s, useNA="ifany")
table(m7$globalexp2, useNA="ifany")

# ses
table(m7$e3_ses, useNA="ifany")
table(m7$e3_ses, m7$h_edumc, useNA="ifany")
res<-chisq.test(m7$e3_ses, m7$h_edumc) #p-value 1.960197e-22
res$p.value

# maternal bmi
summary(m7$e3_mbmi)
mean(m7$e3_mbmi)
sd(m7$e3_mbmi)
res<-lm(e3_mbmi~msmok_a, m7)
summary(res)#pval 0.759
res<-lm(e3_mbmi~msmok_s, m7)
summary(res)#pval ns
# parity - no info
summary(m7$e3_numpreg)# many missings

# mat age
summary(m7$e3_age)
res<-lm(e3_age~msmok_a, m7)
summary(res)#pval  0.0115 *
res<-lm(e3_age~msmok_s, m7)
summary(res)#pval 0.015 (sustained)
res<-lm(e3_age~h_edumc, m7)
summary(res)#pval 1.93e-09 

# mat edu
table(m7$h_edumc, m7$msmok_a, useNA="ifany")
res<-chisq.test(m7$h_edumc, m7$msmok_a)# < 2.55822e-20
summary(res)



### for traits
### for BW: 1080
dd.bw<-m7[!is.na(m7$zBW21),]
dim(dd.bw)
summary(dd.bw$zBW21, useNA="ifany")
summary(dd.bw$zBW, useNA="ifany")
mean(dd.bw$zBW21)
sd(dd.bw$zBW21)
mean(dd.bw$e3_bw)
sd(dd.bw$e3_bw)

# by cohort
aggregate(dd.bw$zBW21, list(dd.bw$cohort), FUN=mean)
aggregate(dd.bw$zBW21, list(dd.bw$cohort), FUN=sd)
aggregate(dd.bw$e3_bw, list(dd.bw$cohort), FUN=mean)
aggregate(dd.bw$e3_bw, list(dd.bw$cohort), FUN=sd)

# by smoking
aggregate(dd.bw$zBW21, list(dd.bw$msmok_a), FUN=mean)
aggregate(dd.bw$zBW21, list(dd.bw$msmok_a), FUN=sd)
aggregate(dd.bw$zBW21, list(dd.bw$msmok_s), FUN=mean)
aggregate(dd.bw$zBW21, list(dd.bw$msmok_s), FUN=sd)

# by PRS EGG
aggregate(dd.bw$zBW21, list(as.factor(dd.bw$PRSbw21.g)), FUN=mean)
aggregate(dd.bw$zBW21, list(as.factor(dd.bw$PRSbw21.g)), FUN=sd)

# by PRS PanUK
aggregate(dd.bw$zBW21, list(as.factor(dd.bw$PRSbw21uk.g)), FUN=mean)
aggregate(dd.bw$zBW21, list(as.factor(dd.bw$PRSbw21uk.g)), FUN=sd)

### for BMI: 1063
m8<-m7[!is.na(m7$globalexp2),]
dim(m8)#1063
dd.bmi<-m8[!is.na(m8$hs_zbmi_who),]
dim(dd.bmi)#1063
mean(dd.bmi$hs_zbmi_who)
sd(dd.bmi$hs_zbmi_who)
mean(dd.bmi$hs_c_bmi)
sd(dd.bmi$hs_c_bmi)
table(dd.bw$FID%in%dd.bmi$FID)
table(dd.bmi$FID%in%dd.bw$FID)

# by cohort
aggregate(dd.bmi$hs_c_bmi, list(dd.bmi$cohort), FUN=mean)
aggregate(dd.bmi$hs_c_bmi, list(dd.bmi$cohort), FUN=sd)
aggregate(dd.bmi$hs_zbmi_who, list(dd.bmi$cohort), FUN=mean)
aggregate(dd.bmi$hs_zbmi_who, list(dd.bmi$cohort), FUN=sd)

# by smoking
aggregate(dd.bmi$hs_zbmi_who, list(dd.bmi$msmok_a), FUN=mean)
aggregate(dd.bmi$hs_zbmi_who, list(dd.bmi$msmok_a), FUN=sd)
aggregate(dd.bmi$hs_zbmi_who, list(dd.bmi$msmok_s), FUN=mean)
aggregate(dd.bmi$hs_zbmi_who, list(dd.bmi$msmok_s), FUN=sd)

# by PRS
aggregate(dd.bmi$hs_zbmi_who, list(as.factor(dd.bmi$PRSbmi.g)), FUN=mean)
aggregate(dd.bmi$hs_zbmi_who, list(as.factor(dd.bmi$PRSbmi.g)), FUN=sd)

### for WC: 1060
dd.wc<-m8[!is.na(m8$hs_z_waist),]
dim(dd.wc)#1060
dim(dd.wc)
mean(dd.wc$hs_z_waist)
sd(dd.wc$hs_z_waist)
mean(dd.wc$hs_waist)
sd(dd.wc$hs_waist)

# by cohort
aggregate(dd.wc$hs_z_waist, list(dd.wc$cohort), FUN=mean)
aggregate(dd.wc$hs_z_waist, list(dd.wc$cohort), FUN=sd)
aggregate(dd.wc$hs_waist, list(dd.wc$cohort), FUN=mean)
aggregate(dd.wc$hs_waist, list(dd.wc$cohort), FUN=sd)

# by smoking
aggregate(dd.wc$hs_z_waist, list(dd.wc$msmok_a), FUN=mean)
aggregate(dd.wc$hs_z_waist, list(dd.wc$msmok_a), FUN=sd)
aggregate(dd.wc$hs_z_waist, list(dd.wc$msmok_s), FUN=mean)
aggregate(dd.wc$hs_z_waist, list(dd.wc$msmok_s), FUN=sd)

# by PRS
aggregate(dd.wc$hs_z_waist, list(as.factor(dd.wc$PRSwc.g)), FUN=mean)
aggregate(dd.wc$hs_z_waist, list(as.factor(dd.wc$PRSwc.g)), FUN=sd)

### for FM:
dd.fm<-m8[!is.na(m8$hs_z_fatprop_bia),]
dim(dd.fm)#1052
dd.fmx<-m8[!is.na(m8$hs_fatmass),]
dim(dd.fmx)#1052
mean(dd.fm$hs_z_fatprop_bia)
sd(dd.fm$hs_z_fatprop_bia)
mean(dd.fmx$hs_fatmass)
sd(dd.fmx$hs_fatmass)

# by cohort
aggregate(dd.fm$hs_z_fatprop_bia, list(dd.fm$cohort), FUN=mean)
aggregate(dd.fm$hs_z_fatprop_bia, list(dd.fm$cohort), FUN=sd)
aggregate(dd.fmx$hs_fatmass, list(dd.fmx$cohort), FUN=mean)
aggregate(dd.fmx$hs_fatmass, list(dd.fmx$cohort), FUN=sd)

# by smoking
aggregate(dd.fm$hs_z_fatprop_bia, list(dd.fm$msmok_a), FUN=mean)
aggregate(dd.fm$hs_z_fatprop_bia, list(dd.fm$msmok_a), FUN=sd)
aggregate(dd.fm$hs_z_fatprop_bia, list(dd.fm$msmok_s), FUN=mean)
aggregate(dd.fm$hs_z_fatprop_bia, list(dd.fm$msmok_s), FUN=sd)

# by PRS
aggregate(dd.fm$hs_z_fatprop_bia, list(as.factor(dd.fm$PRSfm.g)), FUN=mean)
aggregate(dd.fm$hs_z_fatprop_bia, list(as.factor(dd.fm$PRSfm.g)), FUN=sd)


### descriptive d3
d3<-m7

# main
table(d3$sex, useNA="ifany") # no missings
table(d3$h_edumc, useNA="ifany") # no missings
table(d3$msmok_a, useNA="ifany") # no missings
table(d3$Cohort, useNA="ifany") # no missings
table(d3$msmok_d, useNA="ifany") # 21
table(d3$msmok_s, useNA="ifany") # 16
table(d3$globalexp2, useNA="ifany") # 23

# age and gestational age
summary(d3$e3_gac, useNA="ifany")#no missings
table(d3$preterm, useNA="ifany")
summary(d3$age_sample_months, useNA="ifany")#no missings

# scores
summary(d3$PRSbw21, useNa="ifany")#no missings
summary(d3$PRSbw21uk, useNa="ifany")#no missings
summary(d3$PRSbmi, useNa="ifany")#no missings
summary(d3$PRSfm, useNa="ifany")#no missings
summary(d3$PRSwc, useNa="ifany")#no missings



save.image("Descriptives.R")
