library(tidyverse)
library(pROC)
library(caret)

set.seed(893)

## Glm modeling with resampling method for hard drug users and alc dep subjects

## load data first 

model.data <-read.csv("derived_data/model_data_TNPCA_FC.csv")
model.data.sc<-read.csv("derived_data/model_data_TNPCA_SC.csv")

## Make response variables as a factor
model.data$hard.drug<-as.factor(model.data$hard.drug)
model.data$SSAGA_Alc_D4_Dp_Dx<-as.factor(model.data$SSAGA_Alc_D4_Dp_Dx)

model.data.sc$hard.drug<-as.factor(model.data.sc$hard.drug)
model.data.sc$SSAGA_Alc_D4_Dp_Dx<-as.factor(model.data.sc$SSAGA_Alc_D4_Dp_Dx)


## test and training data with undersampling method
## Foun NA from Alc dep. 
model.data <- na.omit(model.data) #1057 subjects
## make balanced data first for hard drug users (0 : 1030 / 1 :27)

balanced.drug <- NULL

index<-sample(c(1:1030),size = 27*1.5)
temp <- model.data %>% filter(hard.drug == 0) 
balanced.drug <- temp[index,]
balanced.drug.dat <- model.data %>% filter(hard.drug ==1) %>% rbind(.,balanced.drug)

balanced.drug.dat %>% dplyr::select(hard.drug) %>% summary()

#lets make a split (small subjects so I used 0.6)
trainIndex <- balanced.drug.dat$hard.drug %>%
  createDataPartition(p = .6, list = F)
train.drug.fc <- balanced.drug.dat[trainIndex, ]
test.drug.fc <- balanced.drug.dat[-trainIndex, ]

##Check number of 0 and 1
train.drug.fc %>% select(hard.drug) %>% summary() 
test.drug.fc %>% select(hard.drug) %>% summary()


## Similar work to alcohol data (1: 996 / 5 : 61)
balanced.alc <- NULL

index<-sample(c(1:996),size= 61)
temp <- model.data %>% filter(SSAGA_Alc_D4_Dp_Dx == 1)
balanced.alc <- temp[index,]
balanced.alc.dat <- model.data %>% filter(SSAGA_Alc_D4_Dp_Dx ==5) %>% rbind(.,balanced.alc)

balanced.alc.dat %>% dplyr::select(SSAGA_Alc_D4_Dp_Dx) %>% summary()


##make a split 
trainIndex <- balanced.alc.dat$SSAGA_Alc_D4_Dp_Dx %>%
  createDataPartition(p = .75, list = F)
train.alc.fc <- balanced.alc.dat[trainIndex, ]
test.alc.fc <- balanced.alc.dat[-trainIndex, ]

##Check number of 0 and 1
train.alc.fc %>% select(SSAGA_Alc_D4_Dp_Dx) %>% summary() 
test.alc.fc %>% select(SSAGA_Alc_D4_Dp_Dx) %>% summary()




####Glm model auc 

##hard drug 

drug.model <- glm(hard.drug ~.-Subject -mari.user -SSAGA_Alc_D4_Dp_Dx -SSAGA_Alc_D4_Ab_Dx, data = train.drug.fc, family = binomial )

p<-predict(drug.model,newdata = test.drug.fc, type = "response")

# If p exceeds threshold of 0.5
result<- ifelse(p > 0.5, "1", "0")
# Convert to factor: p_class
p_class <- factor(result, levels = levels(test.drug.fc[["hard.drug"]]))
# Create confusion matrix
confusionMatrix(p_class,test.drug.fc[["hard.drug"]])


##Alcohol model

acl.dep.model <- glm(SSAGA_Alc_D4_Dp_Dx~.-Subject -hard.drug -mari.user -SSAGA_Alc_D4_Ab_Dx,data = train.alc.fc, family = binomial)



