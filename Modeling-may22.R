
#Break into groups
#Option 1: 2/3 training and 1/3 test
#Option2: Kfold cross-validation(K=10)
#train the models in the training set: how many models to use (at least 2) and which models (logistic regression
#random forest, SVM)
#evalutate model performance in the testing set (calculate metrics auc-area under the curve, precision and recall)
#Spliting training set into two parts based on outcome: 75% and 25%

#to get the names of all the algoroithms use: names(getModelInfo())

#evaluate 3 methods using all variables that were significant based on EDA
#stepwise logistic regression
#compare the final set of variables with the initial set of significant

splitting_data <- function(Cancels) {
  index <- createDataPartition(Cancels$Cancelled, p = .75, list = FALSE)
 
  training <- Cancels[ index, ]
  testing <- Cancels[-index, ]
  return(list(Tr=training, Te=testing))

}
#1: make random forest and svm functions work
#2 evaluate prediction performance of all three methods

#STEPWISE:

step <- function(Cancels_train) {
  swr <- lm(Cancels$Cancelled ~ Cancels$education + Cancels$customer_age + Cancels$cost_per_vehicle +
              Cancels$marital_status + Cancels$bi_limit + Cancels$currently_insured.x +
             Cancels$gender + Cancels$lead_seller + Cancels$LQ_FB_PREM_AMT + Cancels$no_of_vehicles + 
              Cancels$number_of_accidents + Cancels$previous_policy_type +
              Cancels$risk_profile + Cancels$SameDay_vs_Followup + Cancels$State)

return(swr)
}

  
model_glm <- function(Cancels_train) {
  
  Cancels_train$Cancelled <- as.factor(Cancels_train$Cancelled)
  #modFormula <- "Cancelled ~ cost_per_vehicle + marital_status"

  modFormula <- "Cancelled ~ cost_per_vehicle + marital_status + bi_limit + currently_insured.x +
  customer_age + gender + previous_policy_type + SameDay_vs_Followup"
  
  modFormula <- as.formula(modFormula)
  
  glm.M <- train(modFormula,
               data=Cancels_train, 
               method = 'glm')
  
  return(glm.M)
}


model_random_forest <- function(Cancels_train) {
  
  
  Cancels_train$Cancelled <- as.factor(Cancels_train$Cancelled)
  modFormula <- "Cancelled ~ cost_per_vehicle + marital_status + bi_limit + currently_insured.x +
  customer_age + gender + previous_policy_type +
  SameDay_vs_Followup"
  modFormula <- as.formula(modFormula)
  
  rf.M <- train(modFormula,
                data=Cancels_train, 
                method = 'rf')
  return(rf.M)
}



model_svm <- function(Cancels_train) {

  
  Cancels_train$Cancelled <- as.factor(Cancels_train$Cancelled)
  modFormula <- "Cancelled ~ cost_per_vehicle + marital_status + bi_limit + currently_insured.x +
  customer_age + gender + previous_policy_type +
  SameDay_vs_Followup"
  modFormula <- as.formula(modFormula)
  
    train(modFormula,
  data = Cancels_train, method = 'svmLinear')
}


#MAKE PREDICTIONS ON TEST SET

svm_predictions<- function(Cancel_train) {
  predict <- predict.train(object=model_svm,testing[,modFormula],type="raw")
prediction_table <- table(predict)
confusion_matrix < confusionMatrix(prediction_table)
return(confusion_matrix)
}

AUC_metrics <- function(P, L) {
  library(ROCR)

  pred <- prediction(P, L)
  
  ## sensitivity/specificity curve (x-axis: specificity,
  ## y-axis: sensitivity)
  perf <- performance(pred,measure = "tpr", x.measure = "fpr")
  plot(perf)
  abline(a=0, b= 1)
  
  ## precision/recall curve (x-axis: recall, y-axis: precision)
  #perf1 <- performance(pred, "prec", "rec")
  #plot(perf1)

  ## AUC
  auc.perf = performance(pred, measure = "auc")
  cat("AUC = ", as.numeric(auc.perf@y.values), "\n")
}


AUC_metrics_PROC <- function(P, L) {
library(pROC)
  roc(L, P, plot=TRUE)
  
  roc.info <- roc(L, P, legacy.axes=TRUE)
  str(roc.info)
  
 # roc.df <- data.frame(
  #  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
   # fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
    #thresholds=roc.info$thresholds)

}

insurance_main <- function() {
  setwd("C:/Users/Alissa Hayes/Desktop/Springboard")
 # setwd('D:/Springboard/AH')
  
  require(ggplot2)
  require(tidyverse)
  require(reshape2)
  
  library(ryouready)
  library(Hmisc)
  library(reshape2)
  library(caret)
  library(e1071)
  library(randomForest)
  library(pROC)
  source("Wrangling.R")
  
  #Put in functions from wrangling file  
  Cancels_Read <- read_data()
  Cancels_add_columns <- add_calculated_columns(Cancels_Read) 
  Cancels_Missing <- remove_missing(Cancels_add_columns)
  Cancels_Clean <- final_cleaning(Cancels_Missing)
  
  Cancels_Clean$Cancelled <- as.factor(Cancels_Clean$Cancelled)
    
 Train_test_list <- splitting_data(Cancels_Clean)
 
 training <- Train_test_list$Tr
 glm.M <- model_glm(training)
 print(glm.M)
 ## predict for testing set
 glm.te.predictions <- predict(glm.M, newdata=Train_test_list$Te)
 #calculate accuracy for testing set
 glm.cm <- confusionMatrix(data=glm.te.predictions, reference = Train_test_list$Te$Cancelled)
 
#rf <- model_random_forest(training)
#rf.predictions <- predict(rf, newdata = Train_test_list$Te)
 #rf.cm <- confusionMatrix(data = rf.predictions, reference = Train_test_list$Te$Cancelled)
 
 
 #svm <- model_svm(training)
 #svm.predictions <- predict(svm, newdata = Train_test_list$Te)
# svm.cm <- confusionMatrix(data = svm.predictions, reference = Train_test_list$Te$Cancelled)
 #svm <- model_svm(training)
#stepwise <- step(training)
 #stepwise2 <- step2(training)
# stepwise_backward <- step2(stepwise)s
  #Testing <- tes)
 #auc.m <- AUC_metrics_PROC(as.numeric(glm.te.predictions), Train_test_list$Te$Cancelled)
   #roc(as.numeric(Train_test_list$Te$Cancelled), svm, plot=TRUE)
 
 auc.m <- AUC_metrics(as.numeric(glm.te.predictions),as.numeric(Train_test_list$Te$Cancelled))
}
