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

print("TNPCA Data Cleaned")
#ok now we will work on FC and SC

#start with FC first, FC and SC are very different

#we can easily extract the subject ids and vectorize it
FC.Subjects.list <- c(FC$subj.list)

#set up an empty matrix to fill, should not contain the 
#diag entries so we need to add 1-67 instead of 1-68 for 
#the number of cols needed for the whole upp triangle matrix as a vector 
#minus the diag part which has length 68
FC.MAT <- matrix(0, nrow = length(FC.Subjects.list), ncol = sum(1:67))
#then try to populate the matrix with the upper triangle
for (i in 1:length(FC.Subjects.list)) {
  mat = matrix(unlist(FC$hcp.cortical.fc[[i]]), ncol = 68)
  #found this issue where some of the hcp.cortical.fc matrices are empty
  #we can use an if to handle it and just put NAs
  if (nrow(mat) == 0) {
    FC.MAT[i, ] = rep(NA, sum(1:67))
  } else {
    #now we don't want the diag entries
    mat[lower.tri(mat, diag = T)] = NA
    upp.tri = mat[!is.na(mat)]
    FC.MAT[i, ] = upp.tri
  }
}

FC.DF <- as.data.frame(FC.MAT)

FC.DF$Subject <- FC.Subjects.list

#clean up a bit
FC.clean.temp <- FC.DF %>% 
  select(Subject, everything())

#finally join with traits
#found some NA's we can remove them here
FC.clean <- na.omit(left_join(FC.clean.temp, Traits.clean, by = "Subject"))

#now similar thing for SC

#first we need the subjects ids
SC.Subjects.list <- c(SC$all.id)

#SC is in a different form, but we should be able to follow a very similar process
#here is how we extract one 68x68 matrix SC$hcp.sc.count[, , 1]
#I don't think the dig entries are needed here since they are all 0

SC.MAT <- matrix(0, nrow = length(SC.Subjects.list), ncol = sum(1:67))
for (i in 1:length(SC.Subjects.list)) {
  mat = matrix(SC$hcp.sc.count[, , i], ncol = 68)
  #fix missing data issue with NAs
  if(nrow(mat) == 0) {
    SC.MAT[i, ] = rep(NA, sum(1:67))
  } else {
    #no need for diag
    mat[lower.tri(mat, diag = T)] = NA 
    upp.tri = mat[!is.na(mat)]
    SC.MAT[i, ] = upp.tri
  }
}
SC.DF <- as.data.frame(SC.MAT)

SC.DF$Subject <- SC.Subjects.list

#clean up a bit
SC.clean.temp <- SC.DF %>% 
  select(Subject, everything())

#finally join with traits and remove NAs
SC.clean <- na.omit(left_join(SC.clean.temp, Traits.clean, by = "Subject"))

#now save the clean dataframes

write.csv(FC.clean, "derived_data/FC.clean.csv")
write.csv(SC.clean, "derived_data/SC.clean.csv")

print("SC and FC Matrix Data Cleaned")