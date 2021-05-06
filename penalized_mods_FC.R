print("Estimated wait time ~9 minutes")
#start FC, need ridge, lasso, and elastic net for each Hard Drug, MJ, Alc
library(glmnet)
library(tidyverse)
library(caret)

set.seed(893)
FC.model <- read.csv("derived_data/model_data_TNPCA_FC.csv")

######################################### Hard Drug #########################################
hard.drugs.fc <- FC.model %>%
  select(-c(Subject, Alc, mari.user))
#now we want to split the data into train and test
trainIndex <- hard.drugs.fc$hard.drug %>%
  createDataPartition(p = .75, list = F)
hard.drugs.train.fc <- hard.drugs.fc[trainIndex, ]
hard.drugs.test.fc <- hard.drugs.fc[-trainIndex, ]

# we need a model matrix
x <- model.matrix(hard.drug~., hard.drugs.train.fc)[, -1]
y <- hard.drugs.train.fc$hard.drug

################## Ridge ##################
cv.ridge.drug <- cv.glmnet(x, y, alpha = 0, family = "binomial")
ridge.drug <- glmnet(x, y, alpha = 0, family = "binomial", lambda = cv.ridge.drug$lambda.min)

# make predictions need new matrix
newx.ridge.drug <- model.matrix(hard.drug~. , hard.drugs.test.fc)[, -1]
probs.ridge.drug <- ridge.drug %>% predict(newx = newx.ridge.drug, type = "response")
preds.ridge.drug <- ifelse(probs.ridge.drug >= .5, 1, 0)
# model prediction performance
conmat.ridge.drug <- confusionMatrix(as.factor(preds.ridge.drug), as.factor(hard.drugs.test.fc$hard.drug))

################## LASSO ##################
cv.lasso.drug <- cv.glmnet(x, y, alpha = 1, family = "binomial")
lasso.drug <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso.drug$lambda.min)

# make predictions need new matrix
newx.lasso.drug <- model.matrix(hard.drug~. , hard.drugs.test.fc)[, -1]
probs.lasso.drug <- lasso.drug %>% predict(newx = newx.lasso.drug, type = "response")
preds.lasso.drug <- ifelse(probs.ridge.drug >= .5, 1, 0)
# model prediction performance
conmat.lasso.drug <- confusionMatrix(as.factor(preds.lasso.drug), as.factor(hard.drugs.test.fc$hard.drug))

################## Elastic ##################
#10-fold cv
trainctrl <- trainControl(method = "cv", number = 10)

elastic.drug <- train(
  as.factor(hard.drug)~.,
  data = hard.drugs.train.fc,
  metod = "glmnet",
  trControl = trainctrl,
  #try 10 options
  tuneLength = 10
)

preds.elastic.drug <- elastic.drug %>% predict(newdata = hard.drugs.test.fc %>% 
                                                 select(-hard.drug), type = "raw")
# model prediction performance
conmat.elastic.drug <- confusionMatrix(as.factor(preds.elastic.drug), as.factor(hard.drugs.test.fc$hard.drug))

print("FC Hard Drugs Models Complete")

######################################### MJ #########################################
MJ.fc <- FC.model %>% 
  select(-c(Subject, hard.drug, Alc))
trainIndex <- MJ.fc$mari.user %>% 
  createDataPartition(p = .75, list = F)
MJ.train.fc <- MJ.fc[trainIndex, ]
MJ.test.fc <- MJ.fc[-trainIndex, ]

x <- model.matrix(mari.user~., MJ.train.fc)[, -1]
y <- MJ.train.fc$mari.user

################## Ridge ##################
cv.ridge.mj <- cv.glmnet(x, y, alpha = 0, family = "binomial")
ridge.mj <- glmnet(x, y, alpha = 0, family = "binomial", lambda = cv.ridge.mj$lambda.min)

# make predictions need new matrix
newx.ridge.mj <- model.matrix(mari.user~. , MJ.test.fc)[, -1]
probs.ridge.mj <- ridge.mj %>% predict(newx = newx.ridge.mj, type = "response")
preds.ridge.mj <- ifelse(probs.ridge.mj >= .5, 1, 0)
# model prediction performance
conmat.ridge.mj <- confusionMatrix(as.factor(preds.ridge.mj), as.factor(MJ.test.fc$mari.user))

################## LASSO ##################
cv.lasso.mj <- cv.glmnet(x, y, alpha = 1, family = "binomial")
lasso.mj <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso.mj$lambda.min)

# make predictions need new matrix
newx.lasso.mj <- model.matrix(mari.user~. , MJ.test.fc)[, -1]
probs.lasso.mj <- lasso.mj %>% predict(newx = newx.lasso.mj, type = "response")
preds.lasso.mj <- ifelse(probs.lasso.mj >= .5, 1, 0)
# model prediction performance
conmat.lasso.mj <- confusionMatrix(as.factor(preds.lasso.mj), as.factor(MJ.test.fc$mari.user))

################## Elastic ##################
#10-fold cv
trainctrl <- trainControl(method = "cv", number = 10)

elastic.mj <- train(
  as.factor(mari.user)~.,
  data = MJ.train.fc,
  metod = "glmnet",
  trControl = trainctrl,
  #try 10 options
  tuneLength = 10
)

preds.elastic.mj <- elastic.mj %>% predict(newdata = MJ.test.fc %>% select(-mari.user), type = "raw")
# model prediction performance
conmat.elastic.mj <- confusionMatrix(as.factor(preds.elastic.mj), as.factor(MJ.test.fc$mari.user))

print("FC Marijuana Models Complete")

######################################### Alc #########################################
Alc.dep.fc <- FC.model %>% 
  select(-c(Subject, hard.drug, mari.user))
trainIndex <- Alc.dep.fc$Alc %>% 
  createDataPartition(p = .75, list = F)
Alc.dep.train.fc <- Alc.dep.fc[trainIndex, ]
Alc.dep.test.fc <- Alc.dep.fc[-trainIndex, ]

x <- model.matrix(Alc~., Alc.dep.train.fc)[, -1]
y <- Alc.dep.train.fc$Alc

################## Ridge ##################
cv.ridge.alc <- cv.glmnet(x, y, alpha = 0, family = "binomial")
ridge.alc <- glmnet(x, y, alpha = 0, family = "binomial", lambda = cv.ridge.alc$lambda.min)

# make predictions need new matrix
newx.ridge.alc <- model.matrix(Alc~. , Alc.dep.test.fc)[, -1]
probs.ridge.alc <- ridge.drug %>% predict(newx = newx.ridge.alc, type = "response")
preds.ridge.alc <- ifelse(probs.ridge.alc >= .5, 1, 0)
# model prediction performance
conmat.ridge.alc <- confusionMatrix(as.factor(preds.ridge.alc), as.factor(Alc.dep.test.fc$Alc))

################## LASSO ##################
cv.lasso.alc <- cv.glmnet(x, y, alpha = 1, family = "binomial")
lasso.alc <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso.alc$lambda.min)

# make predictions need new matrix
newx.lasso.alc <- model.matrix(Alc~. , Alc.dep.test.fc)[, -1]
probs.lasso.alc <- lasso.drug %>% predict(newx = newx.lasso.alc, type = "response")
preds.lasso.alc <- ifelse(probs.lasso.alc >= .5, 1, 0)
# model prediction performance
conmat.lasso.alc <- confusionMatrix(as.factor(preds.lasso.alc), as.factor(Alc.dep.test.fc$Alc))

################## Elastic ################
#10-fold cv
trainctrl <- trainControl(method = "cv", number = 10)

elastic.alc <- train(
  as.factor(Alc)~.,
  data = Alc.dep.train.fc,
  metod = "glmnet",
  trControl = trainctrl,
  #try 10 options
  tuneLength = 10
)

preds.elastic.alc <- elastic.alc %>% predict(newdata = Alc.dep.test.fc %>% select(-Alc), type = "raw")
# model prediction performance
conmat.elastic.alc <- confusionMatrix(as.factor(preds.elastic.alc), as.factor(Alc.dep.test.fc$Alc))

print("FC Alcohol Models Complete")

print("FC Results:")
paste("Hard Drug Ridge Accuracy:", as.numeric(conmat.ridge.drug$overall[1]))
paste("Hard Drug LASSO Accuracy:", as.numeric(conmat.lasso.drug$overall[1]))
paste("Hard Drug Elastic Accuracy:", as.numeric(conmat.elastic.drug$overall[1]))
paste("MJ Ridge Accuracy:", as.numeric(conmat.ridge.mj$overall[1]))
paste("MJ LASSO Accuracy:", as.numeric(conmat.lasso.mj$overall[1]))
paste("MJ Elastic Accuracy:", as.numeric(conmat.elastic.mj$overall[1]))
paste("Alc Ridge Accuracy:", as.numeric(conmat.ridge.alc$overall[1]))
paste("Alc LASSO Accuracy:", as.numeric(conmat.lasso.alc$overall[1]))
paste("Alc Elastic Accuracy:", as.numeric(conmat.elastic.alc$overall[1]))