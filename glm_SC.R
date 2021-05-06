#load data libs and set seed
library(epiDisplay)
library(tidyverse)
library(caret)
set.seed(893)
SC.model <- read.csv("derived_data/model_data_TNPCA_SC.csv")

################# drug user GLM FC #####################
hard.drugs.sc <- SC.model %>%
  select(-c(Subject, Alc, mari.user))
#now we want to split the data into train and test
trainIndex <- hard.drugs.sc$hard.drug %>%
  createDataPartition(p = .75, list = F)
hard.drugs.train.sc <- hard.drugs.sc[trainIndex, ]
hard.drugs.test.sc <- hard.drugs.sc[-trainIndex, ]

#build full glm with train
hard.drug.sc.glm <- glm(hard.drug ~., data = hard.drugs.train.sc, family = "binomial")
# summary(hard.drug.fc.glm)
hard.drugs.sc.probs <- predict(hard.drug.sc.glm,
                               newdata = hard.drugs.test.sc %>% select(-hard.drug),
                               type = "response")
hard.drugs.sc.preds <- ifelse(hard.drugs.sc.probs >=.5, 1, 0)

print("TNPCA_SC GLM results on Hard Drug Users")
confusionMatrix(as.factor(hard.drugs.sc.preds), as.factor(hard.drugs.test.sc$hard.drug))

paste("Drug Users AIC:", hard.drug.sc.glm$aic)
res.hard.drug <- lroc(hard.drug.sc.glm)
paste("Drug Users AUC:", res.hard.drug$auc)

Sys.sleep(10)

################# MJ user GLM FC #####################
MJ.sc <- SC.model %>% 
  select(-c(Subject, hard.drug, Alc))
#now we want to split the data into train and test
trainIndex <- MJ.sc$mari.user %>% 
  createDataPartition(p = .75, list = F)
MJ.train.sc <- MJ.sc[trainIndex, ]
MJ.test.sc <- MJ.sc[-trainIndex, ]

#build full glm with train
MJ.sc.glm <- glm(mari.user ~., data = MJ.train.sc, family = "binomial")
# summary(Alc.dep.fc.glm)
#significant are coupling, PC17, PC25, PC32, 49, 53, 58
MJ.sc.probs <- predict(MJ.sc.glm, 
                       newdata = MJ.test.sc %>% select(-mari.user), 
                       type = "response")
MJ.sc.preds <- ifelse(MJ.sc.probs >=.5, 1, 0)

print("TNPCA_SC GLM results on Marijuana Users")
confusionMatrix(as.factor(MJ.sc.preds), as.factor(MJ.test.sc$mari.user))

paste("MJ Users AIC:", MJ.sc.glm$aic)
res.MJ <- lroc(MJ.sc.glm)
paste("MJ Users AUC:", res.MJ$auc)

Sys.sleep(10)

################# Alc user GLM FC #####################
Alc.dep.sc <- SC.model %>% 
  select(-c(Subject, hard.drug, mari.user))
#now we want to split the data into train and test
trainIndex <- Alc.dep.sc$Alc %>% 
  createDataPartition(p = .75, list = F)
Alc.dep.train.sc <- Alc.dep.sc[trainIndex, ]
Alc.dep.test.sc <- Alc.dep.sc[-trainIndex, ]

#build full glm with train
Alc.dep.sc.glm <- glm(Alc ~., data = Alc.dep.train.sc, family = "binomial")
# summary(Alc.dep.fc.glm)
#significant are coupling, PC17, PC25, PC32, 49, 53, 58
Alc.dep.sc.probs <- predict(Alc.dep.sc.glm, 
                            newdata = Alc.dep.test.sc %>% select(-Alc), 
                            type = "response")
Alc.dep.sc.preds <- ifelse(Alc.dep.sc.probs >=.5, 1, 0)

print("TNPCA_SC GLM results on Alcohol Users")
confusionMatrix(as.factor(Alc.dep.sc.preds), as.factor(Alc.dep.test.sc$Alc))

paste("Alc Users AIC:", Alc.dep.sc.glm$aic)
res.Alc <- lroc(Alc.dep.sc.glm)
paste("Alc Users AUC:", res.Alc$auc)

Sys.sleep(10)

#SAVE ROC plots

png(file = "derived_graphics/SC_ROC_Curve_Drug.png")
lroc(hard.drug.sc.glm)
dev.off()

png(file = "derived_graphics/SC_ROC_Curve_MJ.png")
lroc(MJ.sc.glm)
dev.off()

png(file = "derived_graphics/SC_ROC_Curve_Alc.png")
lroc(Alc.dep.sc.glm)
dev.off()