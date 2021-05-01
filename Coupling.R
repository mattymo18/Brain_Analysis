
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

# Extract subject that we're interested in. (133 subject)
FC.drug <- FC.clean %>% filter(Cocaine == "true" | THC == "true" | Opiates == "true" | Amphetamines == "true" | MethAmphetamine =="true" | Oxycontin == "true") 
SC.drug <- SC.clean %>% filter(Cocaine == "true" | THC == "true" | Opiates == "true" | Amphetamines == "true" | MethAmphetamine =="true" | Oxycontin == "true") 

# After subtraction, leave only FC and SC value for coupling
FC.drug <- FC.drug[,1:2278]
SC.drug <- SC.drug[,1:2278]

#Subject number '995174' has NA in FC so I removed this one
#Checked both dimension is 132*2278

FC.drug <-na.omit(FC.drug)
SC.drug <- SC.drug %>% filter(Subject !="995174")


  

#Divide SC with ROI's volume in each subject 
#Calculate correlation only nonzero SC and corresponding FC
# Calculate pearson's correlation except the first Subject id column
# SC has only positive value so compute with absolute value of FC

#The first column is subject number
SC.treat <- matrix(0, nrow = 132, ncol = ncol(SC.drug[,-1]))
SC.drug.2 <- as.matrix(SC.drug[,-1])
for(i in 1:132){
  
  #Divide each row with its rowsum
  
  SC.treat[i,] <-SC.drug.2[i,] / (rowSums(SC.drug.2[,-1])[i])
}

# Make regional coupling matrix for 133 subjects
regional.cp = matrix(0,nrow = 132)
FC.drug.2 <- as.matrix(FC.drug[,-1])

for( i in 1: nrow(FC.drug)){

  #Non zero SC
  temp.nzero <- which(SC.treat[i,] != 0)

  #correlation between nonzero SC and corresponding FC
  
   regional.cp[i] <- cor.test(abs(FC.drug.2[i,temp.nzero]),SC.treat[i,temp.nzero],method = "pearson")$estimate
}
regional.cp <- cbind(FC.drug[,1],regional.cp) 
colnames(regional.cp) <- c("Subject", "coupling")

write.csv(regional.cp, "derived_data/Druguser.cp.csv")
