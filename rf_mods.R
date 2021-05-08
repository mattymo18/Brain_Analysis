#load data and libs
library(tidyverse)
library(MLeval)
library(caret)
library(ROSE)
library(MLmetrics)

print("Estimated wait time ~15 minutes")
#set a seed for random things
set.seed(893)

#load data 
fc_pca = read.csv("derived_data/FC.PCA.clean.csv")
mod_fc_data = read.csv("derived_data/model_data_TNPCA_FC.csv")

#star with modeling alcohol with FC data

#wrapper for calculating F1 score during cross validation

f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- MLmetrics::F1_Score(y_pred = data$pred,
                                y_true = data$obs,
                                positive = lev[2])
  c(F1 = f1_val)
}


# wrapper to control the portion of minority class during over sampling
less_rose <- list(name = "Less ROSE",
                  func = function (x, y) {
                    dat <- if (is.data.frame(x)) x else as.data.frame(x)
                    dat$.y <- y
                    dat <- ROSE(.y ~ ., data = dat, p = 0.5)$data
                    list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                         y = dat$.y)
                  },
                  first = TRUE)

#create train test split
fc_train <- mod_fc_data %>%
  select(-c(Subject,hard.drug,mari.user))%>%
  mutate(Alc = factor(Alc))%>%
  mutate(Alc = recode(Alc,"0" = "No","1"="Yes"))
train = createDataPartition(y = fc_train$Alc, p = 0.8, list = FALSE)
alc_train = fc_train[train,]
alc_test = fc_train[-train,]
# table(alc_train$Alc)

#model below seems to have issues and takes a while to run, lets remove it
#train a random forest first?
#5-fold cv
#random search over parameters mtry, which is the 
#number of predictors given to each tree
#also calculate F1 score for each param, note that the positive class is "yes"
# control <- trainControl(method = "cv",
#                         number = 5,
#                         summaryFunction = f1,
#                         classProbs = TRUE,
#                         search = 'random')
# 
# alc_rf_random <- train(Alc~.,
#                        data = alc_train,
#                        method = 'rf',
#                        metric = 'F1',
#                        tuneLength = 15,
#                        trControl = control)
# print(alc_rf_random)

#use ROSE during re-sampling i.e in each iteration of CV

#ROSE(randomly over-sampling examples) creates a sample of 
#synthetic data by enlarging the features space of minority 
#and majority class examples. Operationally, the new examples
#are drawn from a conditional kernel density estimate of the 
#two classes, as described in Menardi and Torelli (2013).

#over sample with the entire data set
#do it inside each CV iteration
print("Alcohol Modeling Begins")

ROSE_control <- trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 3,
                             summaryFunction = f1,
                             classProbs = TRUE,
                             sampling = "rose")

alc_rf_ROSE <- train(Alc~.,
                     data = alc_train,
                     method = 'rf',
                     metric = 'F1',
                     tuneLength = 5,
                     trControl = ROSE_control)
print("RF Alc response with ROSE sampling result:")
print(alc_rf_ROSE)

#see training precision and recall for the best model
#compared to validation F1 which is 0.30, seems to be overfitting
rf_pred = predict(alc_rf_ROSE$finalModel,alc_test[,-ncol(alc_test)])
confusionMatrix(rf_pred,alc_test[,ncol(alc_test)],
                positive = "Yes", mode = "prec_recall")
paste("F1 Score:", MLmetrics::F1_Score(alc_test[,ncol(alc_test)],
                                       rf_pred,positive = "Yes"))

Sys.sleep(15)

# variable importance and selection with the rose model, 
#maybe that would help with overfitting

var_imp_fc_rose <- varImp(alc_rf_ROSE$finalModel) %>%
  tibble::rownames_to_column()%>%
  arrange(desc(Overall))

#very different result compared to not using rose
#extract names of the 20 most important factors for subsequent use
imp_var_fc_rose <- (var_imp_fc_rose %>% head(20))$rowname

ROSE_control <- trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 3,
                             summaryFunction = f1,
                             classProbs = TRUE,
                             sampling = "rose",
                             search = 'random')

#retrain the model with only "important" variables and rose
alc_rf_ROSE_imp_var <- train(Alc~.,
                             data = alc_train[,c(imp_var_fc_rose,"Alc")],
                             method = 'rf',
                             metric = 'F1',
                             tuneLength = 15,
                             trControl = ROSE_control)

print("Retrained RF Alc reponse model with important variables results:")
print(alc_rf_ROSE_imp_var)

confusionMatrix(alc_rf_ROSE_imp_var)

print("trainning accuracy with only 20 important variables")
train_pred <- predict(alc_rf_ROSE_imp_var$finalModel,alc_train[,imp_var_fc_rose])
confusionMatrix(train_pred,alc_train$Alc, mode = "prec_recall", positive = "Yes")

print("test accuracy with only 20 important variables")
test_pred <- predict(alc_rf_ROSE_imp_var$finalModel,alc_test[,imp_var_fc_rose])
confusionMatrix(test_pred,alc_test$Alc,mode = "prec_recall",positive = "Yes")

pred <- predict(alc_rf_ROSE_imp_var$finalModel,alc_test[,-ncol(alc_test)])
paste("F1 Score:", MLmetrics::F1_Score(alc_test[,ncol(alc_test)],
                                       pred,positive = "Yes"))
# ROSE::roc.curve(test_pred,alc_test$Alc)

Sys.sleep(15)
#The test recall is not that bad, but at the cost of low Precision

print("Hard Drug Modeling Begins")

mod_fc_drug<-mod_fc_data%>%
  select(-c(Subject,Alc,mari.user)) %>%
  mutate(hard.drug = factor(hard.drug))%>%
  mutate(hard.drug = recode(hard.drug,"0" = "No","1" = "Yes"))

train = createDataPartition(y = mod_fc_drug$hard.drug, p = 0.8, list = FALSE)
drug_train = mod_fc_drug[train,]
drug_test = mod_fc_drug[-train,]

#repeadted cv was having issues, we can try normal cv and it won't take as long, 
#hm ok so same with normal cv...we can't use this i guess
# control <- trainControl(method = "cv",
#                         number = 5,
#                         summaryFunction = f1,
#                         classProbs = TRUE,
#                         search = 'random')
# 
# rf_drug_all <- train(hard.drug ~ .,
#                      data = drug_train,
#                      metric = 'F1',
#                      method = 'rf',
#                      tuneLength = 15,
#                      trControl = control)
# 
# print("Unbalanced drug user random forest result:")
# print(rf_drug_all)
# Sys.sleep(10)

ROSE_control <- trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 3,
                             summaryFunction = f1,
                             classProbs = TRUE,
                             sampling = "rose",
                             search = 'random')

#wrapper for calculating F1 score during cross validation 

rf_drug_rose_all <- train(hard.drug ~ .,
                          data = drug_train,
                          method = 'rf',
                          metric = 'F1',
                          tuneLength = 15,
                          trControl = ROSE_control)
print("Rose sample drug user RF results:")
print(rf_drug_rose_all)

pred <- predict(rf_drug_rose_all$finalModel,drug_test[,-ncol(drug_test)])
paste("F1 Score:", MLmetrics::F1_Score(drug_test[,ncol(drug_test)],
                                      pred,positive = "Yes"))
MLmetrics::ConfusionMatrix(pred,drug_test[,ncol(drug_test)])
# ROSE::roc.curve(drug_test[,ncol(drug_test)],pred)

# evalm(rf_drug_rose_all)

confusionMatrix(rf_drug_rose_all)

Sys.sleep(15)
