#load data and libs all we need is coupling and traits so FC and SC doesn't matter
library(tidyverse)

set.seed(893) #doesn't matter but stay consistent

DF <- read.csv("derived_data/model_data_TNPCA_FC.csv")

#split up by user group
Alc <- DF %>% 
  filter(Alc == 1)
Drug <- DF %>% 
  filter(hard.drug == 1)
MJ <- DF %>% 
  filter(mari.user == 1)
Control <- DF %>% 
  filter(Alc == 0 & hard.drug == 0 & mari.user == 0)

Alc.couple <- as.numeric(Alc$coupling)
Drug.couple <- as.numeric(Drug$coupling)
MJ.couple <- as.numeric(MJ$coupling)
CTRL.couple <- as.numeric(Control$coupling)

print("Alc Summary")
summary(Alc.couple)

print("Drug Summary")
summary(Drug.couple)

print("MJ Summary")
summary(MJ.couple)

print("CTRL Summary")
summary(CTRL.couple)

paste("T.Test for differnce of means, Alc and Ctrl:", t.test(Alc.couple, CTRL.couple)$p.value)
paste("T.Test for differnce of means, Drug and Ctrl:", t.test(Drug.couple, CTRL.couple)$p.value)
paste("T.Test for differnce of means, MJ and Ctrl:", t.test(MJ.couple, CTRL.couple)$p.value)
