####################################################
##### SUMMARY STATISTIC COMING FROM GENE ATLAS ##### 
####################################################

# Define trait key and download data
trait_code="clinical_c_G35" # as an example

# Downloading data 
echo "Downloading base data: $trait_code...";
wget ftp://ftp.igmm.ed.ac.uk/pub/GeneATLAS/${trait_code}.v2.tar

# unzip data
tar xf ${trait_code}.v2.tar

cd results/${trait_code}

# Extract whole data by chr
echo "Extracting base data...";
for i in {1..22}; do     gunzip imputed.allWhites.$trait_code.chr$i.csv.gz; done
gunzip imputed.allWhites.combined.$trait_code.chrX.csv.gz

# Add data in a unique file 
cat imputed.allWhites.$trait_code.chr1.csv imputed.allWhites.$trait_code.chr2.csv imputed.allWhites.$trait_code.chr3.csv imputed.allWhites.$trait_code.chr4.csv imputed.allWhites.$trait_code.chr5.csv imputed.allWhites.$trait_code.chr6.csv imputed.allWhites.$trait_code.chr7.csv imputed.allWhites.$trait_code.chr8.csv imputed.allWhites.$trait_code.chr9.csv imputed.allWhites.$trait_code.chr10.csv imputed.allWhites.$trait_code.chr11.csv imputed.allWhites.$trait_code.chr12.csv imputed.allWhites.$trait_code.chr13.csv imputed.allWhites.$trait_code.chr14.csv imputed.allWhites.$trait_code.chr15.csv imputed.allWhites.$trait_code.chr16.csv imputed.allWhites.$trait_code.chr17.csv imputed.allWhites.$trait_code.chr18.csv imputed.allWhites.$trait_code.chr19.csv imputed.allWhites.$trait_code.chr20.csv imputed.allWhites.$trait_code.chr21.csv imputed.allWhites.$trait_code.chr22.csv imputed.allWhites.combined.$trait_code.chrX.csv > outputfile.csv 



######################################################
##### SUMMARY STATISTIC COMING FROM GWAS CATALOG ##### 
######################################################

# Insert the link where the corresponding summary statistic (FTP link) is accessible for downloading
wget http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST90018001-GCST90019000/GCST90018120/GCST90018120_buildGRCh37.tsv.gz

	# If the file is a compressed file, decompress it by inserting the name of the already downloaded file 
	gunzip file

	# If the file is a .txt file (uncompressed format), continue with the next commands

	# Plus, if the summary statistic is composed of several files 
		# Merge them: 	
			# Consider only having one header 
				tail -n +2 file1 > outputfile1
				tail -n +2 file2 > outputfile2
				tail -n +2 file3 > outputfile3

			# Merging them:

				# In R: 
				R

				# Call the needed library
				library(data.table)

				# See the files present in the working directory
				list.files()

				# Reading the files using the fread function 
				dat1 <- fread("outputfile1")
				dat2 <- fread("outputfile2")
				dat3 <- fread("outputfile3")

				# install.packages("gtools")
				# Calling the needed library 
				library(gtools)

				# Merging using the smartbind function
				total <- smartbind(dat1, dat2, dat3)
	
					# If there are a lot of missings and you CAN NOT compute the PRS: 
						total <- na.omit(total)

				
# In R: 
R

# Needed library for reading the file contatining the summary statistics  
library(data.table)

# reading the summary statistic 
dat <- fread("outputfile.csv")

# Make sure you check for: 


	# 1. Format for PRSice (labels) 
		# OPTIONAL:
		# Selecting for reordering columns of the file 
		# So that the same structure is present in all the summary statistics for all the traits) 
		dat <- subset(dat, select=c(3,4,6,7,8,1,2,....))

	# Labelling columns so that PRSice software can run 
	names(dat) <- c("SNP", "A1", "BETA", "SE", "P", "imp_quality")
	
	# 2. Remove duplicated SNPs 
	dat = dat[!duplicated(dat$SNP),]

	# 3. Effect allele column (A1) needs to be in UPPER CASE for the PRSIce to properly run 
	dat$A1 <- toupper(dat$A1)


# Once everything is checked, write a file containing the "new" ordered and labeled summary statistic file that will be used as input (base data) for PRSice 
write.table(dat, "base_data.txt", row.names=F, quote=F)

# Exit without saving the working environment
q("no")
