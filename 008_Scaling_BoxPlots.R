###########################################
#### Scaling the PRS (.all_score file) ####
########################################### 


# As the selected file is containing the ID columns, and for scaling are not needed
# One will be dealing with only the columns containing the PRS at each threshold

# Open R environment:
R

# load the needed libraries
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(tibble)

# Working directory:
setwd("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results")

# Convert to factor each of the folders from the directory
fold <- as.factor(list.files())

# Because inside the directory there is a README file that is interferring the loop iteration from above
n<-length(fold)-1

# For each i value inside the directory, iterate to set the working directory as the actual folder in the i index

for (i in 1:n){
	setwd(paste0("/home/isglobal.lan/gfuentes/data/WS_HELIX/HELIX_preproc/pgrs/results/", fold[[i]]))

		fold1 <- gsub("[^a-zA-Z]", "", fold)				# Keep only the letters from the folders
		fold1 <- fold1[1:n]						# Remove the README file present in the directory
		
		if (length(list.files(path=paste0(getwd(), "/", fold1[[i]], 
						  "_EUR"), 
				      all.files=TRUE, 
				      pattern="^[^\\.]|\\.[^\\.]")) > 0){   	# Keep only those directories where files exists (are not empty) 
			
			setwd(paste0(getwd(), "/", fold1[[i]], "_EUR"))  	# Assign the new working directory for the trait_EUR folder
			
			fileNames <- list.files(pattern = ".all_score")	 	# Take the files whose pattern is .all_score 
			
			dat <- fread(fileNames)					# Read the files (.all_score) 

			escalats <- dat 
    			escalats <- scale(escalats[,3:ncol(escalats)]) 		# Apply the scale function to each file but only from the 3rd column
 			escalats <- data.frame(escalats)
    			ids <- dat[,1:2]					# save the ids in a new variable called 'ids'
    			escalats1 <- cbind(ids, escalats)			# Adding the ids another time		
			names(escalats1) <- names(dat)
    			write.table(escalats1, "scaled_PRSvalues.txt", 
				    row.names=F, quote=F)			# Write a file with the scaled values 
		
			data <- escalats					# Copy scaled data to a new auxiliar variable
	
			DF <- rownames_to_column(data, var = "threshold")	# The next three lines of code are: 	
			columns <- names(DF[,2:ncol(DF)])			# Reshaping the data so that the column names 	
			DF <- DF %>% pivot_longer(cols = all_of(columns), 
						  names_to = "Threshold", 
						  values_to = "Value")		# become one column and the values are in one column.	


			pdf("simple_boxplot.pdf")
			boxplot(escalats)
			dev.off()
	
			png("simple_boxplot.png")
			boxplot(escalats)
			dev.off()


##############
### ggplot ###
##############

			DF %>%														
			  ggplot( aes(x=Threshold, y=Value, 
				      fill=Threshold)) +			# boxplot using ggplot tools (PDF extension)
			  geom_boxplot() +
			  scale_fill_viridis(discrete = TRUE, alpha=0.9) +
			  geom_jitter(color="black", size=0.4, alpha=0.5) +
			 #theme_ipsum() +
			  theme(
			    legend.position="none",
			    plot.title = element_text(size=11)
			  ) +
			  ggtitle("Boxplot of the PRS scaled values") +
			  xlab("Thresholds")
			
			ggsave("ggplot_boxplot.pdf")				# save it in the current directory (each folder) 





			DF %>%														
			  ggplot( aes(x=Threshold, y=Value, 
				      fill=Threshold)) +			# boxplot using ggplot tools (PNG extension)
			  geom_boxplot() +
			  scale_fill_viridis(discrete = TRUE, alpha=0.9) +
			  geom_jitter(color="black", size=0.4, alpha=0.5) +
			  #theme_ipsum() +
			  theme(
			    legend.position="none",
			    plot.title = element_text(size=11)
			  ) +
			  ggtitle("Boxplot of the PRS scaled values") +
			  xlab("Thresholds")

			ggsave("ggplot_boxplot.png")				# save it in the current directory (each folder) 

				
		}

}
