##...........................................................
## 7. Testing global interaction any smoking (T4 and T5) ####
##...........................................................


load("RegressionModels.R)
source("StatisticalAnalysis_RegressionModels.R")


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
