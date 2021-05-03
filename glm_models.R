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

# Join and make big data frame 

full.data <- left_join(coupling, FC ,by="Subject")

# Make a data for modeling with Subject, coupling, TNPCA.FC, TNPCA.SC, dummy variables for traits
mod.data<-left_join(coupling,FC.tnpca, by = "Subject")

hard.drug.user <- mod.data %>% filter(Cocaine == "true" | THC == "true"| Opiates == "true"|Amphetamines == "true"
                                      | MethAmphetamine == "true" | Oxycontin == "true")

mari.user <- mod.data %>% filter(SSAGA_Mj_Use == 1)
alc <- mod.data %>% select(Subject, SSAGA_Alc_D4_Ab_Dx, SSAGA_Alc_D4_Dp_Dx)


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

#Save the data for model first

write.csv(model.data, "derived_data/model_data.csv")

# GLM

model.data<-na.omit(model.data) #1 NA 
#Hard drug model 
hard.drug.dat <- model.data[,-(64:66)]
hard.drug.mod <- glm(hard.drug~.-Subject,data =drug.dat,family = binomial )
summary(hard.drug.mod) # coupling PC3,15,33


##Marijuana model 
mari.dat <- model.data[,-c(63,65,66)]
mari.mod <- glm(mari.user ~ .-Subject, data = mari.dat, family = binomial)
summary(mari.mod)

#Alcohol user 
acl.dat <- model.data[,-c(63,64)]

acl.abuse.mod <-glm(SSAGA_Alc_D4_Ab_Dx~.-Subject-SSAGA_Alc_D4_Dp_Dx,data = acl.dat, family = binomial)
summary(acl.abuse.mod)

acl.dep.mod <- glm(SSAGA_Alc_D4_Dp_Dx~.-Subject-SSAGA_Alc_D4_Ab_Dx,data = acl.dat, family = binomial)
summary(acl.dep.mod)


#AIC

model.aic<-c(hard.drug.mod$aic,mari.mod$aic,acl.abuse.mod$aic,acl.dep.mod$aic)
model.aic





