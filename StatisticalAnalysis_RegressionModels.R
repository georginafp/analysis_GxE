####################################
####### STATISTICAL ANALYSIS #######
####################################

load("Descriptives.R")
source("DescriptiveAnalysis.R")


##.....................
## 3. Main and GxE ####
##.....................

### 3.1. PRS continous and any smoking

### zBW - EGG
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             PRSbw21 + as.factor(msmok_a), data=dd.bw)
summary(res1)$coefficients
res1x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              PRSbw21 * as.factor(msmok_a), data=dd.bw)
summary(res1x)$coefficients
anova(res1, res1x)

### zBW - PanUK
res2 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbw21uk +   as.factor(msmok_a) + as.factor(h_edumc), data=dd.bw)
summary(res2)$coefficients
res2x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbw21uk *   as.factor(msmok_a) + as.factor(h_edumc), data=dd.bw)
summary(res2x)$coefficients
anova(res2, res2x)

### zBMI
res3 <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbmi +  as.factor(msmok_a)+ as.factor(h_edumc)+ as.factor(globalexp2), data=dd.bmi)
summary(res3)$coefficients
res3x <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbmi *  as.factor(msmok_a)+ as.factor(h_edumc)+ as.factor(globalexp2), data=dd.bmi)
summary(res3x)$coefficients
anova(res3, res3x)

### zFM 
res4 <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSfm + as.factor(msmok_a) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.fm)
summary(res4)$coefficients
res4x <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSfm * as.factor(msmok_a) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.fm)
summary(res4x)$coefficients
anova(res4, res4x)

### zWC
res5 <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSwc +  as.factor(msmok_a) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.wc)
summary(res5)$coefficients
res5x <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSwc *  as.factor(msmok_a) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.wc)
summary(res5x)$coefficients
anova(res5, res5x)




### 3.2. PRS continous and sustained smoking
### BW
res1 <- lm(e3_bw ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) + e3_gac + as.factor(e3_sex) +
             PRSbw21.g + as.factor(msmok_s), data=dd.bw)
summary(res1)$coefficients

### zBW - EGG
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             PRSbw21 + as.factor(msmok_s), data=dd.bw)
summary(res1)$coefficients
res1x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              PRSbw21 * as.factor(msmok_s), data=dd.bw)
summary(res1x)$coefficients
anova(res1, res1x)

### zBW - PanUK
res2 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbw21uk +   as.factor(msmok_s) + as.factor(h_edumc), data=dd.bw)
summary(res2)$coefficients
res2x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbw21uk *   as.factor(msmok_s) + as.factor(h_edumc), data=dd.bw)
summary(res2x)$coefficients
anova(res2, res2x)

### BMI
res1 <- lm(hs_c_bmi ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) + age_sample_months + as.factor(e3_sex) +
             PRSbmi.g + as.factor(msmok_s), data=dd.bmi)
summary(res1)$coefficients

### zBMI

res3 <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbmi +  as.factor(msmok_s)+ as.factor(h_edumc)+ as.factor(globalexp2), data=dd.bmi)
summary(res3)$coefficients
res3x <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbmi *  as.factor(msmok_s)+ as.factor(h_edumc)+ as.factor(globalexp2), data=dd.bmi)
summary(res3x)$coefficients
anova(res3, res3x)

### zFM 
res4 <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSfm + as.factor(msmok_s) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.fm)
summary(res4)$coefficients
res4x <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSfm * as.factor(msmok_s) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.fm)
summary(res4x)$coefficients
anova(res4, res4x)

### zWC
res5 <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSwc +  as.factor(msmok_s) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.wc)
summary(res5)$coefficients
res5x <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSwc *  as.factor(msmok_s) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.wc)
summary(res5x)$coefficients
anova(res5, res5x)




### 3.3. PRS 3 groups and any smoking 
### zBW - EGG
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             PRSbw21.g + as.factor(msmok_a), data=dd.bw)
summary(res1)$coefficients
res1x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              PRSbw21.g * as.factor(msmok_a), data=dd.bw)
summary(res1x)$coefficients
anova(res1, res1x)

### zBW - PanUK
res2 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbw21uk.g +   as.factor(msmok_a) + as.factor(h_edumc), data=dd.bw)
summary(res2)$coefficients
res2x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbw21uk.g *   as.factor(msmok_a) + as.factor(h_edumc), data=dd.bw)
summary(res2x)$coefficients
anova(res2, res2x)

### zBMI
res3 <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbmi.g +  as.factor(msmok_a)+ as.factor(h_edumc)+ as.factor(globalexp2), data=dd.bmi)
summary(res3)$coefficients
res3x <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbmi.g *  as.factor(msmok_a)+ as.factor(h_edumc)+ as.factor(globalexp2), data=dd.bmi)
summary(res3x)$coefficients
anova(res3, res3x)

### zFM 
res4 <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSfm.g + as.factor(msmok_a) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.fm)
summary(res4)$coefficients
res4x <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSfm.g * as.factor(msmok_a) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.fm)
summary(res4x)$coefficients
anova(res4, res4x)

### zWC

res5 <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSwc.g +  as.factor(msmok_a) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.wc)
summary(res5)$coefficients
res5x <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSwc.g *  as.factor(msmok_a) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.wc)
summary(res5x)$coefficients
anova(res5, res5x)



### 3.4. PRS 3 groups and sustained smoking 
### zBW - EGG
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             PRSbw21.g + as.factor(msmok_s), data=dd.bw)
summary(res1)$coefficients
res1x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              PRSbw21.g * as.factor(msmok_s), data=dd.bw)
summary(res1x)$coefficients
anova(res1, res1x)

### zBW - PanUK
res2 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbw21uk.g +   as.factor(msmok_s) + as.factor(h_edumc), data=dd.bw)
summary(res2)$coefficients
res2x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbw21uk.g *   as.factor(msmok_s) + as.factor(h_edumc), data=dd.bw)
summary(res2x)$coefficients
anova(res2, res2x)

### zBMI

res3 <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbmi.g +  as.factor(msmok_s)+ as.factor(h_edumc)+ as.factor(globalexp2), data=dd.bmi)
summary(res3)$coefficients
res3x <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbmi.g *  as.factor(msmok_s)+ as.factor(h_edumc)+ as.factor(globalexp2), data=dd.bmi)
summary(res3x)$coefficients
anova(res3, res3x)

### zFM 
res4 <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSfm.g + as.factor(msmok_s) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.fm)
summary(res4)$coefficients
res4x <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSfm.g * as.factor(msmok_s) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.fm)
summary(res4x)$coefficients
anova(res4, res4x)

### zWC
res5 <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSwc.g +  as.factor(msmok_s) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.wc)
summary(res5)$coefficients
res5x <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSwc.g *  as.factor(msmok_s) + as.factor(h_edumc)+ as.factor(globalexp2), data=dd.wc)
summary(res5x)$coefficients
anova(res5, res5x)




##....................
## 4. Sensitivity ####
##....................


### 4.1. PRS 3 groups and sustained smoking - without preterm 
dd.bw.p<-dd.bw[dd.bw$preterm=="1"&!is.na(dd.bw$preterm),]
dim(dd.bw.p)


### zBW - egg
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             PRSbw21.g + as.factor(msmok_s), data=dd.bw.p)
summary(res1)$coefficients
res1x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              PRSbw21.g * as.factor(msmok_s), data=dd.bw.p)
summary(res1x)$coefficients
anova(res1, res1x)

### zBW - panuk
res2 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbw21uk.g +   as.factor(msmok_s) + as.factor(h_edumc), data=dd.bw.p)
summary(res2)$coefficients
res2x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbw21uk.g *   as.factor(msmok_s) + as.factor(h_edumc), data=dd.bw.p)
summary(res2x)$coefficients
anova(res2, res2x)



### 4.2. PRS 3 groups and sustained smoking - without adjusting postnatal second hand smoke
### zBMI
res3 <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbmi.g +  as.factor(msmok_s)+ as.factor(h_edumc), data=dd.bmi)
summary(res3)$coefficients
res3x <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbmi.g *  as.factor(msmok_s)+ as.factor(h_edumc), data=dd.bmi)
summary(res3x)$coefficients

anova(res3, res3x)

### zFM 

res4 <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSfm.g + as.factor(msmok_s) + as.factor(h_edumc), data=dd.fm)
summary(res4)$coefficients
res4x <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSfm.g * as.factor(msmok_s) + as.factor(h_edumc), data=dd.fm)
summary(res4x)$coefficients
anova(res4, res4x)

### zWC

res5 <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSwc.g +  as.factor(msmok_s) + as.factor(h_edumc), data=dd.wc)
summary(res5)$coefficients
res5x <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSwc.g *  as.factor(msmok_s) + as.factor(h_edumc), data=dd.wc)
summary(res5x)$coefficients
anova(res5, res5x)




### 4.3. PRS 3 groups and sustained smoking - without children exposed to second hand smoke
table(dd.bmi$globalexp2)
dd.bmi.f<-dd.bmi[dd.bmi$globalexp2=="no exposure",]
dim(dd.bmi.f)
dd.fm.f<-dd.fm[dd.fm$globalexp2=="no exposure",]
dim(dd.fm.f)
dd.wc.f<-dd.wc[dd.wc$globalexp2=="no exposure",]
dim(dd.wc.f)

### zBMI
res3 <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSbmi.g +  as.factor(msmok_s)+ as.factor(h_edumc), data=dd.bmi.f)
summary(res3)$coefficients
res3x <- lm(hs_zbmi_who ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSbmi.g *  as.factor(msmok_s)+ as.factor(h_edumc), data=dd.bmi.f)
summary(res3x)$coefficients
anova(res3, res3x)

### zFM 
res4 <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSfm.g + as.factor(msmok_s) + as.factor(h_edumc), data=dd.fm.f)
summary(res4)$coefficients
res4x <- lm(hs_z_fatprop_bia~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSfm.g * as.factor(msmok_s) + as.factor(h_edumc), data=dd.fm.f)
summary(res4x)$coefficients
anova(res4, res4x)

### zWC
res5 <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             PRSwc.g +  as.factor(msmok_s) + as.factor(h_edumc), data=dd.wc.f)
summary(res5)$coefficients
res5x <- lm(hs_z_waist  ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              PRSwc.g *  as.factor(msmok_s) + as.factor(h_edumc), data=dd.wc.f)
summary(res5x)$coefficients
anova(res5, res5x)



### 4.4. PRS 3 groups and sustained smoking - without INMA
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/002_fBW/fBW21_EUR")
scl<-read.table("scaled_PRSvalues_noSAB.txt", header=T, sep="")
dim(scl)
#select 0.001
scl1<-scl[,c(1,6)]
dd.bw.ns<-merge(dd.bw, scl1, by="FID")
dim(dd.bw.ns)

### zBW - EGG
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             PRSbw21.g + as.factor(msmok_s), data=dd.bw.ns)
summary(res1)$coefficients
res1x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              PRSbw21.g * as.factor(msmok_s), data=dd.bw.ns)
summary(res1x)$coefficients
anova(res1, res1x)


### 4.5. PRS 3 groups and sustained smoking - without INMA and MoBA
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/002_fBW/fBW21_EUR")
rm(scl)
scl<-read.table("scaled_PRSvalues_noSABnoMOB.txt", header=T, sep="")
dim(scl)
#select 0.001
scl1<-scl[,c(1,6)]
rm(dd.bw.ns)
dd.bw.ns<-merge(dd.bw, scl1, by="FID")
dim(dd.bw.ns)

### zBW - EGG
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             PRSbw21.g + as.factor(msmok_s), data=dd.bw.ns)
summary(res1)$coefficients
res1x <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
              PRSbw21.g * as.factor(msmok_s), data=dd.bw.ns)
summary(res1x)$coefficients
anova(res1, res1x)




### 4.6. Stratification by PRS group - zBW
### zBW - EGG
table(dd.bw$PRSbw21.g)#1_PRS_low  2_PRS_mid 3_PRS_high
dd.bw.1<-dd.bw[dd.bw$PRSbw21.g=="1_PRS_low",]
dim(dd.bw.1)
dd.bw.2<-dd.bw[dd.bw$PRSbw21.g=="2_PRS_mid",]
dim(dd.bw.2)
dd.bw.3<-dd.bw[dd.bw$PRSbw21.g=="3_PRS_high",]
dim(dd.bw.3)
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             as.factor(msmok_s), data=dd.bw.1)
summary(res1)$coefficients
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             as.factor(msmok_s), data=dd.bw.2)
summary(res1)$coefficients
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             as.factor(msmok_s), data=dd.bw.3)
summary(res1)$coefficients


### zBW - PanUK
table(dd.bw$PRSbw21uk.g)#1_PRS_low  2_PRS_mid 3_PRS_high
rm(dd.bw.1, dd.bw.2, dd.bw.3)
dd.bw.1<-dd.bw[dd.bw$PRSbw21uk.g=="1_PRS_low",]
dim(dd.bw.1)
dd.bw.2<-dd.bw[dd.bw$PRSbw21uk.g=="2_PRS_mid",]
dim(dd.bw.2)
dd.bw.3<-dd.bw[dd.bw$PRSbw21uk.g=="3_PRS_high",]
dim(dd.bw.3)

res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             as.factor(msmok_s), data=dd.bw.1)
summary(res1)$coefficients
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             as.factor(msmok_s), data=dd.bw.2)
summary(res1)$coefficients
res1 <- lm(zBW21 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + as.factor(h_edumc) +
             as.factor(msmok_s), data=dd.bw.3)
summary(res1)$coefficients


### 4.7. PRS tertiles - any smoking
# zBW 
M1.4 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*BWg + 
              as.factor(sex) + e3_gac, data=d3)
summary(M1.4)$coefficients

# BMI 
M3.4 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*BMIg
            + as.factor(globalexp2), data=d3.bmi)
summary(M3.4)$coefficients

# FM
M9.4 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*FMg
            + as.factor(globalexp2), data=d3.fm)
summary(M9.4)$coefficients

# WC
M10.4 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               as.factor(h_edumc) + as.factor(msmok_a)*WCg + 
               as.factor(globalexp2), data=d3.wc)
summary(M10.4)$coefficients




##.............................................
## 5. GxE with any mat smoking (Table TS5) ####
##.............................................

### 5.1. PRS continious - sustained smoking 

# zBW 
M1.4 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_s)*Pt_0.01_BW + 
              as.factor(sex) + e3_gac, data=d3)
summary(M1.4)$coefficients

# BMI 
M3.4 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_s)*Pt_0.05_BMI
            + as.factor(globalexp2), data=d3.bmi)
summary(M3.4)$coefficients

# FM
M9.4 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_s)*Pt_0.05_FM
            + as.factor(globalexp2), data=d3.fm)
summary(M9.4)$coefficients

# WC
M10.4 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               as.factor(h_edumc) + as.factor(msmok_s)*Pt_0.001_WC + 
               as.factor(globalexp2), data=d3.wc)
summary(M10.4)$coefficients



### 5.2. PRS tertiles - sustained smoking 
# zBW 
M1.4 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_s)*BWg + 
              as.factor(sex) + e3_gac, data=d3)
summary(M1.4)$coefficients

# BMI 
M3.4 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_s)*BMIg
            + as.factor(globalexp2), data=d3.bmi)
summary(M3.4)$coefficients

# FM
M9.4 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_s)*FMg
            + as.factor(globalexp2), data=d3.fm)
summary(M9.4)$coefficients

# WC
M10.4 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               as.factor(h_edumc) + as.factor(msmok_s)*WCg + 
               as.factor(globalexp2), data=d3.wc)
summary(M10.4)$coefficients




##..........................................................................
## 6. Association PRS adjusted for maternal smoking (Table TS3 and TS4) ####
##..........................................................................

### 6.1. PRS continuous - any smoking 

# zBW 
M1.3 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + Pt_0.01_BW + as.factor(h_edumc)
            + as.factor(sex) + e3_gac, data=d3)
summary(M1.3)$coefficients

# BMI
M3.3 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + Pt_0.05_BMI + as.factor(h_edumc)
            + as.factor(globalexp2), data=d3.bmi)
summary(M3.3)$coefficients

# FM
M9.3 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + Pt_0.05_FM + as.factor(h_edumc)
            + as.factor(globalexp2), data=d3.fm)
summary(M9.3)$coefficients

# WC
M10.3 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               as.factor(msmok_a) + Pt_0.001_WC + as.factor(h_edumc)+ 
               as.factor(globalexp2), data=d3.wc)
summary(M10.3)$coefficients



### 6.2. PRS continuous - sustained smoking 

# zBW 
M1.3 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_s) + Pt_0.01_BW + as.factor(h_edumc)
            + as.factor(sex) + e3_gac, data=d3)
summary(M1.3)$coefficients

# BMI
M3.3 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_s) + Pt_0.05_BMI + as.factor(h_edumc)
            + as.factor(globalexp2), data=d3.bmi)
summary(M3.3)$coefficients


# FM
M9.3 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_s) + Pt_0.05_FM + as.factor(h_edumc)
            + as.factor(globalexp2), data=d3.fm)
summary(M9.3)$coefficients

# WC
M10.3 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               as.factor(msmok_s) + Pt_0.001_WC + as.factor(h_edumc)+ 
               as.factor(globalexp2), data=d3.wc)
summary(M10.3)$coefficients




### 6.3. PRS tertiles - any smoking 

# zBW 
M1.3 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + BWg + as.factor(h_edumc)
            + as.factor(sex) + e3_gac, data=d3)
summary(M1.3)$coefficients


# BMI
M3.3 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + BMIg + as.factor(h_edumc)
            + as.factor(globalexp2), data=d3.bmi)
summary(M3.3)$coefficients

# FM
M9.3 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + FMg + as.factor(h_edumc)
            + as.factor(globalexp2), data=d3.fm)
summary(M9.3)$coefficients

# WC
M10.3 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               as.factor(msmok_a) + WCg + as.factor(h_edumc)+ 
               as.factor(globalexp2), data=d3.wc)
summary(M10.3)$coefficients




### 6.4. PRS tertiles - sustained smoking 

# zBW 
M1.3 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_s) + BWg + as.factor(h_edumc)
            + as.factor(sex) + e3_gac, data=d3)
summary(M1.3)$coefficients

# BMI
M3.3 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_s) + BMIg + as.factor(h_edumc)
            + as.factor(globalexp2), data=d3.bmi)
summary(M3.3)$coefficients

# FM
M9.3 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_s) + FMg + as.factor(h_edumc)
            + as.factor(globalexp2), data=d3.fm)
summary(M9.3)$coefficients

# WC
M10.3 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               as.factor(msmok_s) + WCg + as.factor(h_edumc)+ 
               as.factor(globalexp2), data=d3.wc)
summary(M10.3)$coefficients



##...........................................................
## 7. Testing global interaction any smoking (T4 and T5) ####
##...........................................................

### 7.1. PRS continious - any smoking 

# zBW
M1.3 <- lm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_a) + Pt_0.01_BW + as.factor(h_edumc)
           + as.factor(sex) + e3_gac, data=d3)
M1.4 <- lm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.01_BW + 
             as.factor(sex) + e3_gac, data=d3)
anova(M1.3, M1.4)

# BMI 
M1.3 <- lm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_a) + Pt_0.05_BMI + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.bmi)
M1.4 <- lm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.05_BMI + 
             as.factor(globalexp2), data=d3.bmi)
anova(M1.3, M1.4)

# FM
M1.3 <- lm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_a) + Pt_0.05_FM + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.fm)
M1.4 <- lm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.05_FM + 
             as.factor(globalexp2), data=d3.fm)
anova(M1.3, M1.4)

# WC
M1.3 <- lm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_a) + Pt_0.001_WC + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.wc)
M1.4 <- lm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.001_WC + 
             as.factor(globalexp2), data=d3.wc)
anova(M1.3, M1.4)



### 7.2. PRS tertiles - any smoking 

# zBW
M1.3 <- lm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_a) + BWg + as.factor(h_edumc)
           + as.factor(sex) + e3_gac, data=d3)
M1.4 <- lm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_a)*BWg + 
             as.factor(sex) + e3_gac, data=d3)
anova(M1.3, M1.4)

# BMI 
M1.3 <- lm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_a) + BMIg + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.bmi)
M1.4 <- lm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_a)*BMIg + 
             as.factor(globalexp2), data=d3.bmi)
anova(M1.3, M1.4)

# FM
M1.3 <- lm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_a) + FMg + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.fm)
M1.4 <- lm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_a)*FMg + 
             as.factor(globalexp2), data=d3.fm)
anova(M1.3, M1.4)

# WC
M1.3 <- lm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_a) + WCg + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.wc)
M1.4 <- lm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_a)*WCg + 
             as.factor(globalexp2), data=d3.wc)
anova(M1.3, M1.4)




### 7.3. PRS continious - sustained smoking 

# zBW
M1.3 <- lm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + Pt_0.01_BW + as.factor(h_edumc)
           + as.factor(sex) + e3_gac, data=d3)
M1.4 <- lm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*Pt_0.01_BW + 
             as.factor(sex) + e3_gac, data=d3)
anova(M1.3, M1.4)

# BMI 
M1.3 <- lm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + Pt_0.05_BMI + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.bmi)
M1.4 <- lm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*Pt_0.05_BMI + 
             as.factor(globalexp2), data=d3.bmi)
anova(M1.3, M1.4)

# FM
M1.3 <- lm(hs_z_fatprop_bia ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + PRSfm + as.factor(h_edumc)
           + as.factor(globalexp2), data=dd.fm)
M1.4 <- lm(hs_z_fatprop_bia ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*PRSfm + 
             as.factor(globalexp2), data=dd.fm)
anova(M1.3, M1.4)

# WC
M1.3 <- lm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + Pt_0.001_WC + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.wc)
M1.4 <- lm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*Pt_0.001_WC + 
             as.factor(globalexp2), data=d3.wc)
anova(M1.3, M1.4)



### 7.4. PRS tertiles - sustained smoking 

# zBW
M1.3 <- lm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + BWg + as.factor(h_edumc)
           + as.factor(sex) + e3_gac, data=d3)
M1.4 <- lm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*BWg + 
             as.factor(sex) + e3_gac, data=d3)
anova(M1.3, M1.4, refit = FALSE)

# BMI 
M1.3 <- lm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + BMIg + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.bmi)
M1.4 <- lm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*BMIg + 
             as.factor(globalexp2), data=d3.bmi)
anova(M1.3, M1.4)

# FM
M1.3 <- lm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + FMg + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.fm)
M1.4 <- lm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*FMg + 
             as.factor(globalexp2), data=d3.fm)
anova(M1.3, M1.4)

# WC
M1.3 <- lm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + WCg + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.wc)
M1.4 <- lm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*WCg + 
             as.factor(globalexp2), data=d3.wc)
anova(M1.3, M1.4)
