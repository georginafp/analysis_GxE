####################################
####### DESCRIPTIVE ANALYSIS #######
####################################



#### packages 
library(tidyverse)
library(ggsignif)


#### set working directory
setwd("D:/Georgina/Documents/GxE_analysis")

#### read phenotypic data
d1<-read.table("pheno_data.tsv", header=T, sep="\t")
dim(d1) # 1304

#### read matrix of results (Created in previous scripts)
d2<-read.table("M5_results.txt", header=T)
dim(d2) # 1086
names(d2)

# [1] "FID"               "IID"               "Pt_0.05_BMI"
# [4] "Pt_0.01_BW"        "Pt_0.05_FM"        "Pt_0.001_WC"
# [7] "PC1"               "PC2"               "PC3"
#[10] "PC4"               "PC5"               "PC6"
#[13] "PC7"               "PC8"               "PC9"
#[16] "PC10"              "sex"               "age_sample_months"
#[19] "msmok_a"           "h_edumc"           "msmok_s"
#[22] "msmok_d"           "Cohort"            "BW"
#[25] "BMI"               "FM"                "WC"


#### read covariate files 
smok2 <- read.csv("20190905_helix_tobacco_data_MB_request.csv")


####  create PRS tertiles
d2$BWt<- ntile(d2$Pt_0.01_BW, 4)
d2$BWg<-rep(NA,nrow(d2))
d2$BWg[d2$BWt==1]<-"1-PRS-low"
d2$BWg[d2$BWt==2|d2$BWt==3]<-"2-PRS-mid"
d2$BWg[d2$BWt==4]<-"3-PRS-high"
table(d2$BWg)

d2$BMIt<- ntile(d2$Pt_0.05_BMI, 4)
d2$BMIg<-rep(NA,nrow(d2))
d2$BMIg[d2$BMIt==1]<-"1-PRS-low"
d2$BMIg[d2$BMIt==2|d2$BMIt==3]<-"2-PRS-mid"
d2$BMIg[d2$BMIt==4]<-"3-PRS-high"
table(d2$BMIg)

d2$FMt<- ntile(d2$Pt_0.05_FM, 4)
d2$FMg<-rep(NA,nrow(d2))
d2$FMg[d2$FMt==1]<-"1-PRS-low"
d2$FMg[d2$FMt==2|d2$FMt==3]<-"2-PRS-mid"
d2$FMg[d2$FMt==4]<-"3-PRS-high"
table(d2$FMg)

d2$WCt<- ntile(d2$Pt_0.001_WC, 4)
d2$WCg<-rep(NA,nrow(d2))
d2$WCg[d2$WCt==1]<-"1-PRS-low"
d2$WCg[d2$WCt==2|d2$WCt==3]<-"2-PRS-mid"
d2$WCg[d2$WCt==4]<-"3-PRS-high"
table(d2$WCg)


####  add gestational age
d11<-d1[,c(2,43)]
smok21<-smok2[,c(2,16)]
table(smok21$globalexp2, useNA="ifany")


#### keep only complete cases of second hand smoke variable 
smok21<-smok21[complete.cases(smok21$globalexp2),]


#### merge with rest of covariates and data 
m<-merge(d11, smok21, by.x="IID", by.y="HelixID", all.x=T, all.y=T)
dim(m)
d3<-merge(d2, m, by.x="IID", all.x=T)
dim(d3)

####  descriptive d3
table(d3$sex, useNA="ifany")          # no missings
table(d3$h_edumc, useNA="ifany")      # no missings
table(d3$msmok_a, useNA="ifany")      # no missings
table(d3$Cohort, useNA="ifany")       # no missings
table(d3$msmok_d, useNA="ifany")      # 21
table(d3$msmok_s, useNA="ifany")      # 16
table(d3$globalexp2, useNA="ifany")   # 23

summary(d3$BW, useNA="ifany")         # no missings
summary(d3$BMI, useNA="ifany")        # no missings
summary(d3$FM, useNA="ifany")         # 11
summary(d3$WC, useNA="ifany")         # 3

summary(d3$e3_gac, useNA="ifany")     # no missings
summary(d3$age_sample_months, 
        useNA="ifany")                # no missings
summary(d3$Pt_0.01_BW, 
        useNa="ifany")                # no missings
summary(d3$Pt_0.05_BMI, 
        useNa="ifany")                # no missings
summary(d3$Pt_0.05_FM, 
        useNa="ifany")                # no missings
summary(d3$Pt_0.001_WC, 
        useNa="ifany")                # no missings


####  for BW: 1086
####  for BMI:
d3.bmi<-d3[complete.cases(d3$globalexp2),] # 1063
mean(d3.bmi$BMI)
sd(d3.bmi$BMI)

####  for WC:
d3.wc<-d3.bmi[complete.cases(d3.bmi$WC),] # 1060
dim(d3.wc)
mean(d3.wc$WC)
sd(d3.wc$WC)

####  for FM:
d3.fm<-d3.bmi[complete.cases(d3.bmi$FM),] # 1052
dim(d3.fm)
mean(d3.fm$FM)
sd(d3.fm$FM)

#### save image of the work to load it on further analysis
save.image("DescriptiveAnalysis.RData")
