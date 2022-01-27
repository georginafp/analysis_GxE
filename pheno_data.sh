########################################################################
#### Aging, reproductive, metabolic, respiratory_allergy phenotypes ####
########################################################################


# In R: 
R


# Read the file containing all the metadata codebook from final data of the GWAS from HELIX:
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs")
dat <- read.table("HELIX_GWAS_FINAL_ALLmeta_upd_codebook.csv", sep=";", header=TRUE, dec=",")
head(dat)

# Assigning the working directory, the one that contains the block of traits of HELIX
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/db_pheno/repr/")

# Read and save the pheno information from reproductive block (for instance)
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




# For calculating the reproductive PRS and TL, one needs: 

	# (1) BW and TL in terms of a z-score (*BW Z-score = [BW value – BW mean]/standard deviation) 
		
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
....


###### FILTERING THE DATA FOR ONLY EUROPEAN ANCESTRY 
europeans <- which(m$FINAL_ancestry == "EUR")
only_eur <- m[europeans, ]
write_tsv(only_eur, "pheno_data_eur.tsv")







###################################
#### protein relate phenotypes ####
###################################


# Call the needed libraries
library(minfi)
library(readr)
library(dplyr)

# Load the proteome object which is an ExpressionSet containing proteome data from HELIX 
load("proteome_subcohort_v5.Rdata")

# Get the expression raw data from proteome
# See that individuals are in columns while proteins in row
dd<-exprs(proteome_subcohort)

# Need the transposed matrix to have individuals in rows and proteins in columns 
dd1 <- data.frame(t(dd))
colnames(db1)[which(names(db1) == "V1")] <- "LabID"

# Write a tab separated file from the previous object so that one can merge it with metadata from HELIX
write.table(dd1, "db_proteins.txt", row.names=TRUE, col.names=TRUE, quote=F)

# Now, one can proceed with the commands from above






#################################
#### neurological phenotypes ####
#################################


# READING CONNER'S DATABASE
library(haven)
dat_con <- read_dta("Helix all Conners_2017_06_15_v2r.dta")

# READING RAVEN'S DATABASE
dat_rav <- read_dta("neurov6.dta")

# READING CBCL'S DATABASE
dat_cbcl <- read_dta("CBCL_directas_2017_06_15v4r.dta")

# Only dat_con and dat_cbcl contain HELIX ID, so merge them together by the mentioned ID
m=merge(dat_con, dat_rav,by="hs_idnum_helix", all.x=T)


# Change the label of the already created object (m) so that a merge between this and the RAVEN'S database can be done: 
colnames(m)[which(names(m) == "hs_idnum.x")] <- "hs_idnum"


# Merge both by id number
m1=merge(m, dat_rav, by="hs_idnum", all.x=T)


# The HELIX ID is a combination of the cohort + the id number, so one is creating a new variable (hs_idnum_helix) joining both cohort and id number: 
dat_rav$hs_idnum_helix<- paste(dat_rav$hs_cohort, dat_rav$hs_idnum)


# Remove the space between them: 
dat_rav$hs_idnum_helix <- gsub('\\s+', '', dat_rav$hs_idnum_helix)


# Merge the RAVEN'S database with the already created CONNER'S + CBCL'S database by the HELIX ID (created above in RAVEN'S)
m1=merge(m,dat_rav,by="hs_idnum_helix", all.x=T)


# Remove duplicates 
m1 = m1[!duplicated(m1$hs_idnum_helix),]


# Load the metadata/codebook from Helix final GWAS:
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/")
dat <- read.table("HELIX_GWAS_FINAL_ALLmeta_upd_codebook.csv", sep=";", header=TRUE, dec=",")


# Merge it with the already created object containing all the database for neuro traits, but first, change the label of the HelixID column in the neuro database object for HelixID which is the one that appears in the metadata object:
colnames(m1)[which(names(m1) == "hs_idnum_helix")] <- "HelixID"


# Proceed with the merging: 
m2=merge(dat,m1,by="HelixID", all.x=T)
dim(m2) # 1303 x 166 --> no duplicates


###### FILTERING THE DATA FOR ONLY EUROPEAN ANCESTRY 
europeans <- which(m2$FINAL_ancestry == "EUR")
only_eur <- m2[europeans, ]
write_tsv(only_eur, "pheno_data_eur.tsv")