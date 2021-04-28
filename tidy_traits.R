#load data and libs
library(tidyverse)
library(R.matlab)
library(readxl)

#only need the traits from thr 175 

Traits <- readMat("Source_Data/traits/175traits/HCP_175Traits.mat")

#we need the column headers from here
Trait.Details <- read_excel("Source_Data/traits/175traits/Details_175_Traits.xls")
#get the headers and the index
Trait.Headers <- Trait.Details %>% 
  select(Indx, Column_Header)

#now we want to flip thise so every row is a participant and the columns are vars
traits.175 <- as.data.frame(t(Traits$traits.175))
#change the col names to the headers we get from the .xls
names(traits.175) <- Trait.Headers$Column_Header
#add an ID column to identify participants
traits.175$ID <- Traits$hcp.subj.id 

#clean up the order and save
Traits.clean <- traits.175 %>% 
  select(ID, everything())

write.csv(Traits.clean, "derived_data/Clean.Traits.csv")
