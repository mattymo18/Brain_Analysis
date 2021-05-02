
library(tidyverse)
# So there are 133 subject


# Calculate coupling : Pearson's correlation of each region
# We have 68 ROI (Region of Interest) in each SC and FC
# SC and FC matrix is 67*67 triangle matrix except diagonals


FC.clean <- read.csv("derived_data/FC.clean.csv")
SC.clean <- read.csv("derived_data/SC.clean.csv")

#Remove first column x in each csv file
FC.clean <- FC.clean[,-1]
SC.clean <- SC.clean[,-1]

# Leave only FC and SC value for coupling
FC <- FC.clean[,1:2279]
SC <- SC.clean[,1:2279]

#Check the na in FC
#Checked both dimension 1058*2279
FC <- na.omit(FC)
SC <- SC[SC$Subject %in% FC$Subject,]


#Divide SC with ROI's volume in each subject 
#Calculate correlation only nonzero SC and corresponding FC
# Calculate pearson's correlation except the first Subject id column
# SC has only positive value so compute with absolute value of FC

#The first column is subject number
SC.2 <- as.matrix(SC[,-1])
SC.treat <- matrix(0, nrow = nrow(SC.2), ncol = ncol(SC.2))

for(i in 1:nrow(SC)){
  
  #Divide each row with its rowsums
  SC.treat[i,] <-SC.2[i,] / (rowSums(SC.2)[i])
}

# Make regional coupling matrix for 133 subjects
regional.cp = matrix(0,nrow = nrow(SC))
FC.2 <- as.matrix(FC[,-1])

for( i in 1: nrow(FC)){
  
  #Non zero SC
  temp.nzero <- which(SC.treat[i,] != 0)
  
  #correlation between nonzero SC and corresponding FC
  
  regional.cp[i] <- cor.test(abs(FC.2[i,temp.nzero]),SC.treat[i,temp.nzero],method = "pearson")$estimate
}
regional.cp <- cbind(FC[,1],regional.cp) 
colnames(regional.cp) <- c("Subject", "coupling")

write.csv(regional.cp, "derived_data/Druguser.cp.csv", row.names = F)
