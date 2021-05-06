#load data libs and set seed
library(epiDisplay)
library(tidyverse)
library(caret)
set.seed(893)
FC.model <- read.csv("derived_data/model_data_TNPCA_FC.csv")

################# drug user GLM FC #####################
hard.drugs.fc <- FC.model %>%
  select(-c(Subject, Alc, mari.user))
#now we want to split the data into train and test
trainIndex <- hard.drugs.fc$hard.drug %>%
  createDataPartition(p = .75, list = F)
hard.drugs.train.fc <- hard.drugs.fc[trainIndex, ]
hard.drugs.test.fc <- hard.drugs.fc[-trainIndex, ]

#build full glm with train
hard.drug.fc.glm <- glm(hard.drug ~., data = hard.drugs.train.fc, family = "binomial")
hard.drugs.fc.probs <- predict(hard.drug.fc.glm,
                               newdata = hard.drugs.test.fc %>% select(-hard.drug),
                               type = "response")
hard.drugs.fc.preds <- ifelse(hard.drugs.fc.probs >=.5, 1, 0)

print("TNPCA_FC GLM results on Hard Drug Users")
confusionMatrix(as.factor(hard.drugs.fc.preds), as.factor(hard.drugs.test.fc$hard.drug))

paste("Drug Users AIC:", hard.drug.fc.glm$aic)
res.hard.drug <- lroc(hard.drug.fc.glm)
paste("Drug Users AUC:", res.hard.drug$auc)

Sys.sleep(10)

################# MJ user GLM FC #####################
MJ.fc <- FC.model %>% 
  select(-c(Subject, hard.drug, Alc))
#now we want to split the data into train and test
trainIndex <- MJ.fc$mari.user %>% 
  createDataPartition(p = .75, list = F)
MJ.train.fc <- MJ.fc[trainIndex, ]
MJ.test.fc <- MJ.fc[-trainIndex, ]

#build full glm with train
MJ.fc.glm <- glm(mari.user ~., data = MJ.train.fc, family = "binomial")
MJ.fc.probs <- predict(MJ.fc.glm, 
                       newdata = MJ.test.fc %>% select(-mari.user), 
                       type = "response")
MJ.fc.preds <- ifelse(MJ.fc.probs >=.5, 1, 0)

print("TNPCA_FC GLM results on Marijuana Users")
confusionMatrix(as.factor(MJ.fc.preds), as.factor(MJ.test.fc$mari.user))

paste("MJ Users AIC:", MJ.fc.glm$aic)
res.MJ <- lroc(MJ.fc.glm)
paste("MJ Users AUC:", res.MJ$auc)

Sys.sleep(10)

################# Alc user GLM FC #####################
Alc.dep.fc <- FC.model %>% 
  select(-c(Subject, hard.drug, mari.user))
#now we want to split the data into train and test
trainIndex <- Alc.dep.fc$Alc %>% 
  createDataPartition(p = .75, list = F)
Alc.dep.train.fc <- Alc.dep.fc[trainIndex, ]
Alc.dep.test.fc <- Alc.dep.fc[-trainIndex, ]

#build full glm with train
Alc.dep.fc.glm <- glm(Alc ~., data = Alc.dep.train.fc, family = "binomial")
Alc.dep.fc.probs <- predict(Alc.dep.fc.glm, 
                            newdata = Alc.dep.test.fc %>% select(-Alc), 
                            type = "response")
Alc.dep.fc.preds <- ifelse(Alc.dep.fc.probs >=.5, 1, 0)

print("TNPCA_FC GLM results on Alcohol Users")
confusionMatrix(as.factor(Alc.dep.fc.preds), as.factor(Alc.dep.test.fc$Alc))

paste("Alc Users AIC:", Alc.dep.fc.glm$aic)
res.Alc <- lroc(Alc.dep.fc.glm)
paste("Alc Users AUC:", res.Alc$auc)

Sys.sleep(10)

#SAVE ROC plots

png(file = "derived_graphics/FC_ROC_Curve_Drug.png")
lroc(hard.drug.fc.glm)
dev.off()

png(file = "derived_graphics/FC_ROC_Curve_MJ.png")
lroc(MJ.fc.glm)
dev.off()

png(file = "derived_graphics/FC_ROC_Curve_Alc.png")
lroc(Alc.dep.fc.glm)
dev.off()