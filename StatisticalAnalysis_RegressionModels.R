####################################
####### STATISTICAL ANALYSIS #######
####################################


#### packages 
library(imager)
library(stats)


#### load image from previous analysis 
load("DescriptiveAnalysis.RData")



#### Run regression models 


##### 1. PRS --------

    ##### in continious

# zBW
M1.2 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              Pt_0.01_BW + as.factor(h_edumc) + e3_gac + as.factor(sex), data=d3)
summary(M1.2)$coefficients

# BMI
M3.2 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              Pt_0.05_BMI+ as.factor(h_edumc), data=d3.bmi)
summary(M3.2)$coefficients

# FM 
d <- d3.bmi[d3.bmi$msmok_s==1,]
M4.2 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              Pt_0.05_FM + as.factor(h_edumc), data=d3.fm)
summary(M4.2)$coefficients

# WC
d <- d3.bmi[d3.bmi$msmok_s==1,]
M10.2 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               Pt_0.001_WC + as.factor(h_edumc), data=d3.wc)
summary(M10.2)$coefficients


      ##### in tertiles

# zBW
M1.2 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              BWg + as.factor(h_edumc) + e3_gac + as.factor(sex),
            data=d3)
summary(M1.2)$coefficients

# BMI
M3.2 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              BMIg + as.factor(h_edumc), data=d3.bmi)
summary(M3.2)$coefficients

# FM 
M4.2 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              FMg + as.factor(h_edumc), data=d3.fm)
summary(M4.2)$coefficients

# WC
M10.2 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               WCg + as.factor(h_edumc), data=d3.wc)
summary(M10.2)$coefficients






##### 2. SMOKING --------
    
    ##### with ANY variable 

# zBW 
M1.1 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + as.factor(h_edumc) + as.factor(sex) + e3_gac,
            data=d3)
summary(M1.1)

# BMI
M3.1 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + as.factor(h_edumc) + as.factor(globalexp2), 
            data=d3.bmi)
summary(M3.1)

# FM
M9.1 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_a) + as.factor(h_edumc) + as.factor(globalexp2), 
            data=d3.fm)
summary(M9.1)

# WC
M10.1 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               as.factor(msmok_a) + as.factor(h_edumc) + as.factor(globalexp2), 
             data=d3.wc)
summary(M10.1)



        ##### with SUSTAINED variable 

# zBW 
M1.1 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_s) + as.factor(h_edumc) + as.factor(sex) + e3_gac, 
            data=d3)
summary(M1.1)

# BMI
M3.1 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_s) + as.factor(h_edumc)+ as.factor(globalexp2), 
            data=d3.bmi)
summary(M3.1)

# FM
M9.1 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(msmok_s) + as.factor(h_edumc)
            + as.factor(globalexp2), data=d3.fm)
summary(M9.1)

# WC
M10.1 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               as.factor(msmok_s) + as.factor(h_edumc) 
             + as.factor(globalexp2), data=d3.wc)
summary(M10.1)






##### 3. GxE ANALYSIS --------

      ##### with ANY variable 
      ####  PRS continious 

# zBW 
M1.4 <- glm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.01_BW + 
              as.factor(sex) + e3_gac, data=d3)
summary(M1.4)$coefficients

# BMI 
M3.4 <- glm(BMI ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.05_BMI
            + as.factor(globalexp2), data=d3.bmi)
summary(M3.4)$coefficients

# FM
M9.4 <- glm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
              as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.05_FM
            + as.factor(globalexp2), data=d3.fm)
summary(M9.4)$coefficients

# WC
M10.4 <- glm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
               as.factor(h_edumc) + as.factor(msmok_a)*Pt_0.001_WC + 
               as.factor(globalexp2), data=d3.wc)
summary(M10.4)$coefficients



        ##### with ANY variable 
        ####  PRS tertiles 

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





      ##### with SUSTAINED variable 
      ####  PRS continious 

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


        ##### with SUSTAINED variable 
        ####  PRS tertiles 

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







##### 4. ASSOCIATION PRS ADJUSTED FOR MATERNAL SMOKING 

        ##### with ANY variable 
        ####  PRS continious 

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



    
        ##### with SUSTAINED variable 
        ####  PRS continious 

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





        ##### with ANY variable 
        ####  PRS tertiles 


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




        ##### with SUSTAINED variable 
        ####  PRS tertiles 

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





##### 4. TESTING GLOBAL INTERACTION 

        ##### with ANY variable 
        ####  PRS continious 

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



        ##### with ANY variable 
        ####  PRS tertiles 


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



        
        ##### with SUSTAINED variable 
        ####  PRS continious 

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
M1.3 <- lm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + Pt_0.05_FM + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.fm)
M1.4 <- lm(FM ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*Pt_0.05_FM + 
             as.factor(globalexp2), data=d3.fm)
anova(M1.3, M1.4)

# WC
M1.3 <- lm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + Pt_0.001_WC + as.factor(h_edumc)
           + as.factor(globalexp2), data=d3.wc)
M1.4 <- lm(WC ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*Pt_0.001_WC + 
             as.factor(globalexp2), data=d3.wc)
anova(M1.3, M1.4)




        ##### with SUSTAINED variable 
        ####  PRS tertiles 

# zBW
M1.3 <- lm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(msmok_s) + BWg + as.factor(h_edumc)
           + as.factor(sex) + e3_gac, data=d3)
M1.4 <- lm(BW ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
             as.factor(h_edumc) + as.factor(msmok_s)*BWg + 
             as.factor(sex) + e3_gac, data=d3)
anova(M1.3, M1.4)

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
