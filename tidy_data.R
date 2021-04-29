#load data and libs
library(tidyverse)
library(R.matlab)

FC <- readMat("Source_Data/FC/HCP_cortical_DesikanAtlas_FC.mat")
SC <- readMat("Source_Data/SC/HCP_cortical_DesikanAtlas_SC.mat")
TNPCA.FC <- readMat("Source_Data/TNPCA_Result/TNPCA_Coeff_HCP_Functional_Connectome.mat")
TNPCA.SC <- readMat("Source_Data/TNPCA_Result/TNPCA_Coeff_HCP_Structural_Connectome.mat")
Traits.clean <- read.csv("derived_data/Clean.Traits.csv")

#start with PCA matrices since these will be easier and join with the Traits data

#we want them as vectors then we can make the DF
FC.IDs <- c(TNPCA.FC$network.subject.ids)
SC.IDs <- c(TNPCA.SC$sub.id)

#save the PCA coef matrices as Data frames
FC.PCA.MAT <- as.data.frame(TNPCA.FC$PCA.Coeff[1, , ])
SC.PCA.MAT <- as.data.frame(TNPCA.SC$PCA.Coeff[1, , ])

#Lets rename these columns
names(SC.PCA.MAT) <- paste0("PC", 1:60)
names(FC.PCA.MAT) <- paste0("PC", 1:60)

#now we can add in the subject IDs
FC.PCA.MAT$Subject <- FC.IDs
SC.PCA.MAT$Subject <- SC.IDs

#clean it up a bit
FC.PCA.clean.temp <- FC.PCA.MAT %>% 
  select(Subject, everything())

SC.PCA.clean.temp <- SC.PCA.MAT %>% 
  select(Subject, everything())

#perfect, now all that is left is joining the traits, we want to left join this
FC.PCA.clean <- left_join(FC.PCA.clean.temp, Traits.clean, by = "Subject")
SC.PCA.clean <- left_join(SC.PCA.clean.temp, Traits.clean, by = "Subject")

#now save the clean dataframes

write.csv(FC.PCA.clean, "derived_data/FC.PCA.clean.csv")
write.csv(SC.PCA.clean, "derived_data/SC.PCA.clean.csv")