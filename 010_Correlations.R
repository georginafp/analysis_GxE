######################
#### Correlations ####
######################


## set up and packages
load("Descriptives.RData")
source("DescriptiveAnalysis.R")
library(data.table)
library(ggcorrplot)


## load database
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_analyses/PGRS_smok_GF/db")
M1 <- fread("db_revFron_correlation_20220308.txt")


## select and label columns of interest 
PRS <- M1[,c(1,2,3,5,4,9,6,8,7)]
colnames(PRS) <- c("PRS zBW EGG", "PRS zBW PanUK", "PRS zBMI", "PRS zFM", 
                   "PRS zWC", "zBW", "zBMI", "zFM", "zWC")
PRS <- PRS[complete.cases(PRS),]


# Compute a correlation matrix
corr <- cor(PRS)
head(corr[, 1:6])
write.table(corr, "correlation_matrix.csv")


# Compute a matrix of correlation p-values
p.mat <- cor_pmat(PRS)
head(p.mat[, 1:4])
write.table(p.mat, "correlation_pvalues.csv")


## correlation plot
tiff("CorrelationPlot.png", units="in", width=5, height=5, res=300)
ggcorrplot(corr, method = "circle")
dev.off()

