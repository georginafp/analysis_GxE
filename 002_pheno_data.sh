#####################################################
#### Preparing pheno data for BW, BMI, FM and WC ####
#####################################################


# In R: 
R


# Read the file containing all the metadata codebook from final data of the GWAS from HELIX:
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs")
dat <- read.table("HELIX_GWAS_FINAL_ALLmeta_upd_codebook.csv", sep=";", header=TRUE, dec=",")
head(dat)

# Assigning the working directory, the one that contains the block of traits of HELIX
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/db_pheno/repr/")

# Read and save the pheno information from reproductive block and metabolic-related
db <- read.table("db_egg.csv", sep=",", header=TRUE, dec=",")
head(db)

# Merge both objects by HelixID column keeping all the variables that did not match between both
# It assigns NA when no match between x and y
m=merge(dat,db,by="HelixID", all.x=T)
dim(m) # 1450

# Remove duplicated samples
m = m[!duplicated(m$HelixID),]
dim(m) # 1304


# Replace missing values (NA) in the columns were phenotypes and covariates are so that a zscore can be calculated in further lines
	
	# Determine which columns present NAs 
	apply(m, MARGIN=2, function(x) sum(is.na(x)))
	
	# Replace NA values of the column for the mean of the column
	#m$e3_bw[is.na(m$e3_bw)]<-mean(m$e3_bw, na.rm=TRUE)	
	#m$e3_gac[is.na(m$e3_gac)]<-mean(m$e3_gac, na.rm=TRUE)




# For calculating the reproductive and metabolic-related traits (MR), one needs: 

	# (1) BW and MR in terms of a z-score (*BW Z-score = [BW value – BW mean]/standard deviation) 
		
	# MEAN 
	mean_bw <- mean(m$e3_bw, na.rm=TRUE)
		
	# STANDARD DEVIATION 
	sd_bw <- sd(m$e3_bw, na.rm=TRUE)
	
	m$zBW<-rep(NA, nrow(m))
	class(m$zBW)
	
	m$zBW<-as.numeric(m$zBW)
	m$zBW<-(m$e3_bw-mean_bw)/sd_bw
	head(m)


	# (2) GA transformation (normalized) 

	m$qGA<-m$e3_gac
	class(m$qGA) # numeric --> OK!
	m$qGA<-qnorm((rank(m$qGA,na.last="keep")-0.375)/(sum(!is.na(m$qGA))+0.25))
	head(m$qGA)
	# check if the data is behaving as if it was normally distributed
	png("Hist_pheno.png")
	hist(m$qGA)	
	dev.off() 

	# (3) Creating the preterm variable
	m$preterm<-rep(NA, nrow(m))
	m$preterm[m$e3_gac > 37] <- 1
	m$preterm[m$e3_gac < 37] <- 2


# Before writing the object in a file REMEMBER:
	# delete the first column (HelixID) or change the order
	# so that the first columns of the file correspond do FID and IID
	# Otherwise PRSice cannot make contrasts 
	dat <- dat[,-1]


# Writing a .tsv file 
library(readr)
library(dplyr)
write_tsv(dat, "pheno_data_transformed.tsv")


# Adding more variables (traits) that were stored in a different file 
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/db_pheno/")
other <- fread("georgina_prs.csv")

# Repeat procedure of merging by HelixID 
.... ## from line 24 to 97


###### FILTERING THE DATA FOR ONLY EUROPEAN ANCESTRY 
europeans <- which(m$FINAL_ancestry == "EUR")
only_eur <- m[europeans, ]
write_tsv(only_eur, "pheno_data_eur.tsv")
