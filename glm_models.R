## 
set.seed(893)

library(tidyverse)
library(caret)
library(epiDisplay)
library(pROC)

clean.traits<-read.csv("derived_data/Clean.Traits.csv")

##Modeling 


##Get clean data first 
##FC,SC,TNPCA.SC, TNPCA.FC, Coupling, traits
##Remove all the first column

FC<-read.csv("derived_data/FC.clean.csv")
SC<-read.csv("derived_data/SC.clean.csv")


FC.tnpca<-read.csv("derived_data/FC.PCA.clean.csv")
SC.tnpca<-read.csv("derived_data/SC.PCA.clean.csv")

coupling<-read.csv("derived_data/coupling.csv")

FC<-FC[,-1]
SC<-SC[,-1]
FC.tnpca<-FC.tnpca[,-1]
SC.tnpca<-SC.tnpca[,-1]
coupling<-coupling[,-1]


# Make a data for modeling with Subject, coupling, TNPCA.FC, TNPCA.SC, dummy variables for traits
mod.data<-left_join(coupling,FC.tnpca, by = "Subject")

mod.data.sc<-left_join(coupling,SC.tnpca,by = "Subject")

hard.drug.user <- mod.data %>% filter(Cocaine == "true" | Opiates == "true"|Amphetamines == "true"
                                      | MethAmphetamine == "true" | Oxycontin == "true")

mari.user <- mod.data %>% filter(SSAGA_Mj_Use == 1)
alc <- mod.data %>% dplyr::select(Subject, SSAGA_Alc_D4_Ab_Dx, SSAGA_Alc_D4_Dp_Dx)


model.data<-mod.data[,1:62]
model.data$hard.drug <-0
model.data$hard.drug[model.data$Subject %in% hard.drug.user$Subject] = 1
model.data$hard.drug <- as.factor(model.data$hard.drug) #132 users

model.data$mari.user<-0
model.data$mari.user[model.data$Subject %in% mari.user$Subject] = 1
model.data$mari.user <- as.factor(model.data$mari.user) #581 users

model.data <- left_join(model.data, alc, by = "Subject") # levels 
model.data$SSAGA_Alc_D4_Ab_Dx<-as.factor(model.data$SSAGA_Alc_D4_Ab_Dx)
model.data$SSAGA_Alc_D4_Dp_Dx<-as.factor(model.data$SSAGA_Alc_D4_Dp_Dx)


#Make model data with tnpca.sc
temp <- model.data %>% dplyr::select(Subject, hard.drug, mari.user, SSAGA_Alc_D4_Ab_Dx,SSAGA_Alc_D4_Dp_Dx)
model.data.sc <-left_join(mod.data.sc[,1:62],temp, by = "Subject") 

#Save the data for model first

write.csv(model.data, "derived_data/model_data_TNPCA_FC.csv",row.names = FALSE)
write.csv(model.data.sc, "derived_data/model_data_TNPCA_SC.csv",row.names = FALSE)

# GLM

model.data<-na.omit(model.data) #1 NA 
#Hard drug model 
hard.drug.dat <- model.data[,-(64:66)]
hard.drug.mod <- glm(hard.drug~.-Subject,data =hard.drug.dat,family = binomial )
summary(hard.drug.mod) # coupling PC3,15,33


##Marijuana model 
mari.dat <- model.data[,-c(63,65,66)]
mari.mod <- glm(mari.user ~ .-Subject, data = mari.dat, family = binomial)
summary(mari.mod)

#Alcohol user 
alc.dat <- model.data[,-c(63,64)]

alc.abuse.mod <-glm(SSAGA_Alc_D4_Ab_Dx~.-Subject-SSAGA_Alc_D4_Dp_Dx,data = alc.dat, family = binomial)
summary(alc.abuse.mod)

alc.dep.mod <- glm(SSAGA_Alc_D4_Dp_Dx~.-Subject-SSAGA_Alc_D4_Ab_Dx,data = alc.dat, family = binomial)
summary(alc.dep.mod)


#AIC

model.aic<-c(hard.drug.mod$aic,mari.mod$aic,alc.abuse.mod$aic,alc.dep.mod$aic)
model.aic

## It seems that there are striking difference in hard drug user and alcohol dp.
## Find Auc for four models  

#Auc for hard drug user model
res.hard <-lroc(hard.drug.mod)

res.mari <-lroc(mari.mod)

res.alc.ab<-lroc(alc.abuse.mod)

#Auc for alc dependence model 
res.alc.dep <- lroc(alc.dep.mod)

model.auc<-c(res.hard$auc, res.mari$auc,res.alc.ab$auc,res.alc.dep$auc)
model.auc




##############################################glm model with coupling and TNPCA SC 
model.data.sc<-na.omit(model.data.sc) 
#Hard drug model 
hard.drug.dat.sc <- model.data.sc[,-(64:66)]
hard.drug.mod.sc <- glm(hard.drug~.-Subject,data =hard.drug.dat.sc,family = binomial )
summary(hard.drug.mod.sc) # coupling PC3,15,33


##Marijuana model 
mari.dat.sc <- model.data.sc[,-c(63,65,66)]
mari.mod.sc <- glm(mari.user ~ .-Subject, data = mari.dat.sc, family = binomial)
summary(mari.mod.sc)

#Alcohol user 
alc.dat.sc <- model.data.sc[,-c(63,64)]

alc.abuse.mod.sc <-glm(SSAGA_Alc_D4_Ab_Dx~.-Subject-SSAGA_Alc_D4_Dp_Dx,data = alc.dat.sc, family = binomial)
summary(alc.abuse.mod.sc)

alc.dep.mod.sc <- glm(SSAGA_Alc_D4_Dp_Dx~.-Subject-SSAGA_Alc_D4_Ab_Dx,data = alc.dat.sc, family = binomial)
summary(alc.dep.mod.sc)


#AIC

model.aic.sc<-c(hard.drug.mod.sc$aic,mari.mod.sc$aic,alc.abuse.mod.sc$aic,alc.dep.mod.sc$aic)
model.aic.sc

## It seems that there are striking difference in hard drug user and alcohol dp.
## Find Auc for four models  

#Auc for hard drug user model
res.hard.sc <-lroc(hard.drug.mod.sc)

res.mari.sc <-lroc(mari.mod.sc)

res.alc.ab.sc<-lroc(alc.abuse.mod.sc)

#Auc for alc dependence model 
res.alc.dep.sc <- lroc(alc.dep.mod.sc)

model.auc.sc<-c(res.hard.sc$auc, res.mari.sc$auc,res.alc.ab.sc$auc,res.alc.dep.sc$auc)
model.auc.sc






#################### Final conclusion
print("Final conclusion with coupling & TNPCA.FC")
model.aic
model.auc

print("Final conclusion with coupling & TNPCA.SC")
model.aic.sc
model.auc.sc




###################Get subject list 


## min ( 185947 coupling 0.1188 ) Max (182032 coupling 0.3310)
model.data %>% filter(hard.drug == 0 )%>% dplyr::select(Subject,coupling) %>% arrange((coupling)) %>% head()

##min(972566 coupling 0.1637 ) Max(358144 coupling 0.3079)
model.data %>% filter(hard.drug == 1 )%>% dplyr::select(Subject,coupling) %>% arrange(desc(coupling)) %>% head()


## min ( 185947 coupling 0.1188 ) Max (182032 coupling 0.3310)
model.data %>% filter(SSAGA_Alc_D4_Dp_Dx == 1 )%>% dplyr::select(Subject,coupling) %>% arrange(desc(coupling)) %>% head()

##min(767464 coupling 0.14747 ) Max(131722 0.30280)
model.data %>% filter(SSAGA_Alc_D4_Dp_Dx == 5 )%>% dplyr::select(Subject,coupling) %>% arrange((coupling)) %>% head()

