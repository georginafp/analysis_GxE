#################################
####### INTERACTION PLOTS #######
#################################


#### packages 
library(imager)
library(stats)
library(ggplot2)




#### load image from previous analysis 
load("DescriptiveAnalysis.RData")



#### creating variables
##### any smoking and BW
table(d3$msmok_a)
d3$anysmok<-rep(NA,nrow(d3))
d3$anysmok[d3$msmok_a==0]<-"non-smoker"
d3$anysmok[d3$msmok_a==1]<-"smoker"


#### plot with ggplot
pdf("GxE_zBW_PRSc_asmok_MB.pdf.pdf")
ggplot(d3, aes(x=Pt_0.01_BW, y=BW, color=anysmok)) + 
  geom_point (size=2, shape=23)+ geom_smooth(method = "lm") + 
  labs(x = "PRS zBW", y = "Birth weight (zBW)")+ 
  scale_fill_discrete(name="Maternal smoking",
                      breaks=c("non-smoker", "smoker"),
                      labels=c("Non-smoker", "Smoker"))
dev.off()

pdf("GxE_zBW_PRS3_asmok_MB.pdf")
ggplot(d3, aes(x=BWg, y=BW, fill=anysmok), ylim(c(-5,7))) + 
  geom_boxplot() +
  labs(x = "PRS level", y = "Birth weight (zBW)")+ 
  scale_fill_discrete(name="Maternal smoking",
                      breaks=c("non-smoker", "smoker"),
                      labels=c("Non-smoker", "Smoker"))
dev.off()


pdf("GxE_zBW_PRS2_asmok_MB.pdf")
ggplot(d3, aes(x=BWg2, y=BW, fill=anysmok)) + 
  geom_boxplot() +
  labs(x = "PRS level", y = "Birth weight (zBW)")+ 
  scale_fill_discrete(name="Maternal smoking",
                      breaks=c("non-smoker", "smoker"),
                      labels=c("Non-smoker", "Smoker"))
dev.off()

##### sustained smoking and BW
m2<-d3[!is.na(d3$msmok_s),]
dim(m2)

pdf("GxE_zBW_PRSc_sustsmok_MB.pdf")
ggplot(m2, aes(x=Pt_0.01_BW, y=BW, color=msmok_s)) + 
  geom_point (size=2, shape=23)+ geom_smooth(method = "lm") + 
  labs(x = "PRS zBW", y = "Birth weight (zBW)") + 
  scale_fill_discrete(name="Maternal smoking",
                      breaks=c("no", "yes_ns", "yes_s"),
                      labels=c("Non-smoker", "Non-sustained smoker", "Sustained smoker"))
dev.off()

pdf("GxE_zBW_PRS3_sustsmok_MB.pdf")
ggplot(m2, aes(x=BWg, y=BW, fill=msmok_s), ylim(c(-5,7))) + 
  geom_boxplot() +
  labs(x = "PRS level", y = "Birth weight (zBW)") + 
  scale_fill_discrete(name="Maternal smoking",
                      breaks=c("no", "yes_ns", "yes_s"),
                      labels=c("Non-smoker", "Non-sustained smoker", "Sustained smoker"))
dev.off()

pdf("GxE_zBW_PRS2_sustsmok_MB.pdf")
ggplot(m2, aes(x=BWg2, y=BW, fill=msmok_s)) + 
  geom_boxplot() +
  labs(x = "PRS level", y = "Birth weight (zBW)")+ 
  scale_fill_discrete(name="Maternal smoking",
                      breaks=c("no", "yes_ns", "yes_s"),
                      labels=c("Non-smoker", "Non-sustained smoker", "Sustained smoker"))
dev.off()


