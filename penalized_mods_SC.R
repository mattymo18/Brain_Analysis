print("Estimated wait time ~9 minutes")
#SC now
library(glmnet)
library(tidyverse)
library(caret)
library(ROCR)

set.seed(893)
SC.model <- read.csv("derived_data/model_data_TNPCA_SC.csv")

######################################### Hard Drug #########################################
hard.drugs.sc <- SC.model %>%
  select(-c(Subject, Alc, mari.user))
#now we want to split the data into train and test
trainIndex <- hard.drugs.sc$hard.drug %>%
  createDataPartition(p = .75, list = F)
hard.drugs.train.sc <- hard.drugs.sc[trainIndex, ]
hard.drugs.test.sc <- hard.drugs.sc[-trainIndex, ]

# we need a model matrix
x <- model.matrix(hard.drug~., hard.drugs.train.sc)[, -1]
y <- hard.drugs.train.sc$hard.drug

################## Ridge ##################
cv.ridge.drug <- cv.glmnet(x, y, alpha = 0, family = "binomial")
ridge.drug <- glmnet(x, y, alpha = 0, family = "binomial", lambda = cv.ridge.drug$lambda.min)

# make predictions need new matrix
newx.ridge.drug <- model.matrix(hard.drug~. , hard.drugs.test.sc)[, -1]
probs.ridge.drug <- ridge.drug %>% predict(newx = newx.ridge.drug, type = "response")
preds.ridge.drug <- ifelse(probs.ridge.drug >= .5, 1, 0)
# model prediction performance
conmat.ridge.drug <- confusionMatrix(as.factor(preds.ridge.drug), as.factor(hard.drugs.test.sc$hard.drug))

#AUC calculation
pred.object.ridge.drug <- prediction(probs.ridge.drug, hard.drugs.test.sc$hard.drug)
auc.ridge.drug <- attr(performance(pred.object.ridge.drug, "auc"), "y.values")[[1]]

################## LASSO ##################
cv.lasso.drug <- cv.glmnet(x, y, alpha = 1, family = "binomial")
lasso.drug <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso.drug$lambda.min)

# make predictions need new matrix
newx.lasso.drug <- model.matrix(hard.drug~. , hard.drugs.test.sc)[, -1]
probs.lasso.drug <- lasso.drug %>% predict(newx = newx.lasso.drug, type = "response")
preds.lasso.drug <- ifelse(probs.ridge.drug >= .5, 1, 0)
# model prediction performance
conmat.lasso.drug <- confusionMatrix(as.factor(preds.lasso.drug), as.factor(hard.drugs.test.sc$hard.drug))

#AUC calculation
pred.object.lasso.drug <- prediction(probs.lasso.drug, hard.drugs.test.sc$hard.drug)
auc.lasso.drug <- attr(performance(pred.object.lasso.drug, "auc"), "y.values")[[1]]

################## Elastic ##################
#10-fold cv
trainctrl <- trainControl(method = "cv", number = 10)

elastic.drug <- train(
  as.factor(hard.drug)~.,
  data = hard.drugs.train.sc,
  metod = "glmnet",
  trControl = trainctrl,
  #try 10 options
  tuneLength = 10
)

preds.elastic.drug <- elastic.drug %>% predict(newdata = hard.drugs.test.sc %>% 
                                                 select(-hard.drug), type = "raw")
# model prediction performance
conmat.elastic.drug <- confusionMatrix(as.factor(preds.elastic.drug), as.factor(hard.drugs.test.sc$hard.drug))

#AUC calculation
probs.elastic.drug <- elastic.drug %>% predict(newdata = hard.drugs.test.sc %>% 
                                                 select(-hard.drug), type = "prob") %>% 
  mutate(prob = pmax(`0`, `1`)) %>% 
  select(prob)
pred.object.elastic.drug <- prediction(probs.elastic.drug, hard.drugs.test.sc$hard.drug)
auc.elastic.drug <- attr(performance(pred.object.elastic.drug, "auc"), "y.values")[[1]]

print("SC Hard Drugs Models Complete")

######################################### MJ #########################################
MJ.sc <- SC.model %>% 
  select(-c(Subject, hard.drug, Alc))
trainIndex <- MJ.sc$mari.user %>% 
  createDataPartition(p = .75, list = F)
MJ.train.sc <- MJ.sc[trainIndex, ]
MJ.test.sc <- MJ.sc[-trainIndex, ]

x <- model.matrix(mari.user~., MJ.train.sc)[, -1]
y <- MJ.train.sc$mari.user

################## Ridge ##################
cv.ridge.mj <- cv.glmnet(x, y, alpha = 0, family = "binomial")
ridge.mj <- glmnet(x, y, alpha = 0, family = "binomial", lambda = cv.ridge.mj$lambda.min)

# make predictions need new matrix
newx.ridge.mj <- model.matrix(mari.user~. , MJ.test.sc)[, -1]
probs.ridge.mj <- ridge.mj %>% predict(newx = newx.ridge.mj, type = "response")
preds.ridge.mj <- ifelse(probs.ridge.mj >= .5, 1, 0)
# model prediction performance
conmat.ridge.mj <- confusionMatrix(as.factor(preds.ridge.mj), as.factor(MJ.test.sc$mari.user))

#AUC calculation
pred.object.ridge.mj <- prediction(probs.ridge.mj, MJ.test.sc$mari.user)
auc.ridge.mj <- attr(performance(pred.object.ridge.mj, "auc"), "y.values")[[1]]

################## LASSO ##################
cv.lasso.mj <- cv.glmnet(x, y, alpha = 1, family = "binomial")
lasso.mj <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso.mj$lambda.min)

# make predictions need new matrix
newx.lasso.mj <- model.matrix(mari.user~. , MJ.test.sc)[, -1]
probs.lasso.mj <- lasso.mj %>% predict(newx = newx.lasso.mj, type = "response")
preds.lasso.mj <- ifelse(probs.lasso.mj >= .5, 1, 0)
# model prediction performance
conmat.lasso.mj <- confusionMatrix(as.factor(preds.lasso.mj), as.factor(MJ.test.sc$mari.user))

#AUC calculation
pred.object.lasso.mj <- prediction(probs.lasso.mj, MJ.test.sc$mari.user)
auc.lasso.mj <- attr(performance(pred.object.lasso.mj, "auc"), "y.values")[[1]]

################## Elastic ##################
#10-fold cv
trainctrl <- trainControl(method = "cv", number = 10)

elastic.mj <- train(
  as.factor(mari.user)~.,
  data = MJ.train.sc,
  metod = "glmnet",
  trControl = trainctrl,
  #try 10 options
  tuneLength = 10
)

preds.elastic.mj <- elastic.mj %>% predict(newdata = MJ.test.sc %>% select(-mari.user), type = "raw")
# model prediction performance
conmat.elastic.mj <- confusionMatrix(as.factor(preds.elastic.mj), as.factor(MJ.test.sc$mari.user))

#AUC calculation
probs.elastic.mj <- elastic.mj %>% predict(newdata = MJ.test.sc %>% 
                                             select(-mari.user), type = "prob") %>% 
  mutate(prob = pmax(`0`, `1`)) %>% 
  select(prob)
pred.object.elastic.mj <- prediction(probs.elastic.mj, MJ.test.sc$mari.user)
auc.elastic.mj <- attr(performance(pred.object.elastic.mj, "auc"), "y.values")[[1]]

print("SC Marijuana Models Complete")

######################################### Alc #########################################
Alc.dep.sc <- SC.model %>% 
  select(-c(Subject, hard.drug, mari.user))
trainIndex <- Alc.dep.sc$Alc %>% 
  createDataPartition(p = .75, list = F)
Alc.dep.train.sc <- Alc.dep.sc[trainIndex, ]
Alc.dep.test.sc <- Alc.dep.sc[-trainIndex, ]

x <- model.matrix(Alc~., Alc.dep.train.sc)[, -1]
y <- Alc.dep.train.sc$Alc

################## Ridge ##################
cv.ridge.alc <- cv.glmnet(x, y, alpha = 0, family = "binomial")
ridge.alc <- glmnet(x, y, alpha = 0, family = "binomial", lambda = cv.ridge.alc$lambda.min)

# make predictions need new matrix
newx.ridge.alc <- model.matrix(Alc~. , Alc.dep.test.sc)[, -1]
probs.ridge.alc <- ridge.drug %>% predict(newx = newx.ridge.alc, type = "response")
preds.ridge.alc <- ifelse(probs.ridge.alc >= .5, 1, 0)
# model prediction performance
conmat.ridge.alc <- confusionMatrix(as.factor(preds.ridge.alc), as.factor(Alc.dep.test.sc$Alc))

#AUC calculation
pred.object.ridge.alc <- prediction(probs.ridge.alc, Alc.dep.test.sc$Alc)
auc.ridge.alc <- attr(performance(pred.object.ridge.alc, "auc"), "y.values")[[1]]

################## LASSO ##################
cv.lasso.alc <- cv.glmnet(x, y, alpha = 1, family = "binomial")
lasso.alc <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso.alc$lambda.min)

# make predictions need new matrix
newx.lasso.alc <- model.matrix(Alc~. , Alc.dep.test.sc)[, -1]
probs.lasso.alc <- lasso.drug %>% predict(newx = newx.lasso.alc, type = "response")
preds.lasso.alc <- ifelse(probs.lasso.alc >= .5, 1, 0)
# model prediction performance
conmat.lasso.alc <- confusionMatrix(as.factor(preds.lasso.alc), as.factor(Alc.dep.test.sc$Alc))

#AUC calculation
pred.object.lasso.alc <- prediction(probs.lasso.alc, Alc.dep.test.sc$Alc)
auc.lasso.alc <- attr(performance(pred.object.lasso.alc, "auc"), "y.values")[[1]]

################## Elastic ################
#10-fold cv
trainctrl <- trainControl(method = "cv", number = 10)

elastic.alc <- train(
  as.factor(Alc)~.,
  data = Alc.dep.train.sc,
  metod = "glmnet",
  trControl = trainctrl,
  #try 10 options
  tuneLength = 10
)

preds.elastic.alc <- elastic.alc %>% predict(newdata = Alc.dep.test.sc %>% select(-Alc), type = "raw")
# model prediction performance
conmat.elastic.alc <- confusionMatrix(as.factor(preds.elastic.alc), as.factor(Alc.dep.test.sc$Alc))

#AUC calculation
probs.elastic.alc <- elastic.alc %>% predict(newdata = Alc.dep.test.sc %>% 
                                               select(-Alc), type = "prob") %>% 
  mutate(prob = pmax(`0`, `1`)) %>% 
  select(prob)
pred.object.elastic.alc <- prediction(probs.elastic.alc, Alc.dep.test.sc$Alc)
auc.elastic.alc <- attr(performance(pred.object.elastic.alc, "auc"), "y.values")[[1]]

print("SC Alcohol Models Complete")

print("SC Results:")
paste("Hard Drug Ridge Accuracy:", as.numeric(conmat.ridge.drug$overall[1]))
paste("Hard Drug Ridge AUC:", auc.ridge.drug)
paste("Hard Drug LASSO Accuracy:", as.numeric(conmat.lasso.drug$overall[1]))
paste("Hard Drug LASSO AUC:", auc.lasso.drug)
paste("Hard Drug Elastic Accuracy:", as.numeric(conmat.elastic.drug$overall[1]))
paste("Hard Drug Elastic AUC:", auc.elastic.drug)
paste("MJ Ridge Accuracy:", as.numeric(conmat.ridge.mj$overall[1]))
paste("MJ Ridge AUC:", auc.ridge.mj)
paste("MJ LASSO Accuracy:", as.numeric(conmat.lasso.mj$overall[1]))
paste("MJ LASSO AUC:", auc.lasso.mj)
paste("MJ Elastic Accuracy:", as.numeric(conmat.elastic.mj$overall[1]))
paste("MJ Elastic AUC:", auc.elastic.mj)
paste("Alc Ridge Accuracy:", as.numeric(conmat.ridge.alc$overall[1]))
paste("Alc Ridge AUC:", auc.ridge.alc)
paste("Alc LASSO Accuracy:", as.numeric(conmat.lasso.alc$overall[1]))
paste("Alc LASSO AUC:", auc.lasso.alc)
paste("Alc Elastic Accuracy:", as.numeric(conmat.elastic.alc$overall[1]))
paste("Alc Elastic AUC:", auc.elastic.alc)