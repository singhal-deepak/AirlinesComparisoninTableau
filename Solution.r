
# please use attached EXCEL FILE for states code

#install.packages("readxl","dplyr" , "ROSE", "rpart.plot", "rpart", "nnet", "caret", "ggplot2",
#"ROCR", "PRROC", "pROC", "lift")
library(readxl)
library(ROSE)
library(rpart.plot)
library(rpart)
library(nnet)
library(caret)
library(ggplot2)
library(ROCR)
library(PRROC)
library(pROC)
library(lift)
library(dplyr)


#loading mortgage file in R
data = read_excel("~/MortgageDefaultersData.xls",
                 sheet = "MortgageDefaulters")

# remove empty column
data = data[-15]

# we have added dictionary sheet in excel 
dictionary = read_excel("~/MortgageDefaultersData.xls",
                        sheet = "dictionary")

state_data = read_excel("~/MortgageDefaultersData.xls",
                        sheet = "stateData")

# cleaning of state_data file
#1) removing column3 (empty column)
state_data = state_data[-3]
#2) removing row 1 (empty row)
state_data = state_data[-1,]

names(state_data)[1] = "State"
names(state_data)[2] = "median_income"
names(state_data)[3] = "per_of_people_in_poverty"

#join state_data table with dictionary with common attribute as state name
statedata = merge(x = state_data , y = dictionary, by = "State", all.x = TRUE)

names(statedata)[1] = "stateName"
names(statedata)[4] = "State"

# join data with statedata with common atrribute state code
final_data = merge(x = data , y = statedata, by = "State", all.x = TRUE)
#removing state name from data, as its redundant
drop <- c("stateName")
final_data = final_data[,!(names(final_data) %in% drop)]

# changing data type to as.factor
final_data$OUTCOME = as.factor(final_data$OUTCOME)
final_data$First_home = as.factor(final_data$First_home)

# changing data type to as.numeric
final_data$median_income = as.numeric(final_data$median_income)
final_data$per_of_people_in_poverty = as.numeric(final_data$per_of_people_in_poverty)

#Part2
is.null(final_data)
# as there is no null values, so no modification is required

#New Derived Attribute

# we are comparing average salary of state in which person is staying, with individual's income,
final_data$median_income1 = final_data$median_income/12 
final_data$Moninc_medInc = (final_data$median_income1) - (final_data$Tot_mthly_incm)
final_data$Moninc_medInc_ind = ifelse(final_data$Moninc_medInc>0,0,1)

final_data$Moninc_medInc = as.factor(final_data$Moninc_medInc)

#removing temporary columns whats just created above
# Moninc_medInc, median_income1
drop <- c("Moninc_medInc", "median_income1" )
final_data = final_data[,!(names(final_data) %in% drop)]

names(final_data)[14] = "UPBgeAppraisal"
#converting categorical variable to as.factor
final_data$UPBgeAppraisal = as.factor(final_data$UPBgeAppraisal)

#removing further irrelevant variables
# state code
drop <- c("State" )
final_data = final_data[,!(names(final_data) %in% drop)]

final_data$Moninc_medInc_ind = as.factor(final_data$Moninc_medInc_ind)

#creating age categories
summary(final_data$Bo_Age)

final_data$Bo_Age_ind <- ifelse(final_data$Bo_Age <30, "Young",
                                  ifelse(final_data$Bo_Age >=30 & final_data$Bo_Age <=60,"Adult", 
                                         "Retired"))

final_data$Bo_Age_ind = as.factor(final_data$Bo_Age_ind)
drop <- c("Bo_Age")
final_data = final_data[,!(names(final_data) %in% drop)]

# normalise credit score variable
final_data_num = final_data[,3]

max = max(final_data_num)
min = min(final_data_num)

final_data$Credit_score_norm = scale(final_data_num, center = min, scale = max - min)
drop <- c("Credit_score")
final_data = final_data[,!(names(final_data) %in% drop)]


names(final_data)[8] = "DTI_Ratio"

# removing status variables also
# because default case in OUTCOME and Status variables is same 
# and non default in OUTCOME is same as active and payoff in status
# thus highly correlated
drop <- c("Status")
final_data = final_data[,!(names(final_data) %in% drop)]

#Outlier detection and removal
# IQR method

# 1: Ln_Orig
outliers= boxplot(final_data$Ln_Orig)$out
final_datanoOut5 = ifelse(final_data$Ln_Orig %in% outliers, NA, final_data$Ln_Orig)
boxplot(final_datanoOut5)
summary(final_datanoOut5)
final_data$Ln_Orig_New = final_datanoOut5
final_data$Ln_Orig_New[is.na(final_data$Ln_Orig_New)] = 
  mean(final_data$Ln_Orig_New, na.rm = T)
sum(is.na(final_data$Ln_Orig_New))
drop <- c("Ln_Orig")
final_data = final_data[,!(names(final_data) %in% drop)]
names(final_data)[16] = "Ln_Orig"
outliers= boxplot(final_data$Ln_Orig)$out


#1)Var2 = Orig_LTV_Ratio_Pct
outliers= boxplot(final_data$Orig_LTV_Ratio_Pct)$out
final_datanoOut1 = ifelse(final_data$Orig_LTV_Ratio_Pct %in% outliers, NA, final_data$Orig_LTV_Ratio_Pct)
boxplot(final_datanoOut1)
summary(final_datanoOut1)
final_data$Orig_LTV_Ratio_Pct_New = final_datanoOut1
final_data$Orig_LTV_Ratio_Pct_New[is.na(final_data$Orig_LTV_Ratio_Pct_New)] = 
  mean(final_data$Orig_LTV_Ratio_Pct_New, na.rm = T)
sum(is.na(final_data$Orig_LTV_Ratio_Pct_New))
drop <- c("Orig_LTV_Ratio_Pct")
final_data = final_data[,!(names(final_data) %in% drop)]
names(final_data)[16] = "Orig_LTV_Ratio_Pct"
outliers= boxplot(final_data$Orig_LTV_Ratio_Pct)$out

# 3: Tot_mthly_debt_exp
outliers= boxplot(final_data$Tot_mthly_debt_exp)$out
final_datanoOut6 = ifelse(final_data$Tot_mthly_debt_exp %in% outliers, NA, final_data$Tot_mthly_debt_exp)
boxplot(final_datanoOut6)
summary(final_datanoOut6)
final_data$Tot_mthly_debt_exp_New = final_datanoOut6
final_data$Tot_mthly_debt_exp_New[is.na(final_data$Tot_mthly_debt_exp_New)] = 
  mean(final_data$Tot_mthly_debt_exp_New, na.rm = T)
sum(is.na(final_data$Tot_mthly_debt_exp_New))
drop <- c("Tot_mthly_debt_exp")
final_data = final_data[,!(names(final_data) %in% drop)]
names(final_data)[16] = "Tot_mthly_debt_exp"
outliers= boxplot(final_data$Tot_mthly_debt_exp)$out

# 4: Tot_mthly_incm
outliers= boxplot(final_data$Tot_mthly_incm)$out
final_datanoOut10 = ifelse(final_data$Tot_mthly_incm %in% outliers, NA, final_data$Tot_mthly_incm)
boxplot(final_datanoOut10)
final_data$Tot_mthly_incm_new = final_datanoOut10
final_data$Tot_mthly_incm_new[is.na(final_data$Tot_mthly_incm_new)] = 
  mean(final_data$Tot_mthly_incm_new, na.rm = T)
sum(is.na(final_data$Tot_mthly_incm_new))
drop <- c("Tot_mthly_incm")
final_data = final_data[,!(names(final_data) %in% drop)]
names(final_data)[16] = "Tot_mthly_incm"
outliers= boxplot(final_data$Tot_mthly_incm)$out

# 5: orig_apprd_val_amt
outliers= boxplot(final_data$orig_apprd_val_amt)$out
final_datanoOut7 = ifelse(final_data$orig_apprd_val_amt %in% outliers, NA, final_data$orig_apprd_val_amt)
boxplot(final_datanoOut7)
summary(final_datanoOut7)
final_data$orig_apprd_val_amt_new = final_datanoOut7
final_data$orig_apprd_val_amt_new[is.na(final_data$orig_apprd_val_amt_new)] = 
  mean(final_data$orig_apprd_val_amt_new, na.rm = T)
sum(is.na(final_data$orig_apprd_val_amt_new))
drop <- c("orig_apprd_val_amt")
final_data = final_data[,!(names(final_data) %in% drop)]
names(final_data)[16] = "orig_apprd_val_amt"
outliers= boxplot(final_data$orig_apprd_val_amt)$out

# 6: pur_prc_amt
outliers= boxplot(final_data$pur_prc_amt)$out
final_datanoOut9 = ifelse(final_data$pur_prc_amt %in% outliers, NA, final_data$pur_prc_amt)
boxplot(final_datanoOut9)
summary(final_datanoOut9)
final_data$pur_prc_amt_new = final_datanoOut9
final_data$pur_prc_amt_new[is.na(final_data$pur_prc_amt_new)] = 
  mean(final_data$pur_prc_amt_new, na.rm = T)
sum(is.na(final_data$pur_prc_amt_new))
drop <- c("pur_prc_amt")
final_data = final_data[,!(names(final_data) %in% drop)]
names(final_data)[16] = "pur_prc_amt"
outliers= boxplot(final_data$pur_prc_amt)$out

#1)7 DTI_Ratio
outliers= boxplot(final_data$DTI_Ratio)$out
final_datanoOut2 = ifelse(final_data$DTI_Ratio %in% outliers, NA, final_data$DTI_Ratio)
boxplot(final_datanoOut2)
summary(final_datanoOut2)
final_data$DTI_RatioNew = final_datanoOut2
final_data$DTI_RatioNew[is.na(final_data$DTI_RatioNew)] = 
  mean(final_data$DTI_RatioNew, na.rm = T)
sum(is.na(final_data$DTI_RatioNew))
drop <- c("DTI_Ratio")
final_data = final_data[,!(names(final_data) %in% drop)]
names(final_data)[16] = "DTI_Ratio"
outliers= boxplot(final_data$DTI_Ratio)$out

# 8: LoanValuetoAppraised
outliers= boxplot(final_data$LoanValuetoAppraised)$out
final_datanoOut8 = ifelse(final_data$LoanValuetoAppraised %in% outliers, NA, final_data$LoanValuetoAppraised)
boxplot(final_datanoOut8)
summary(final_datanoOut8)
final_data$LoanValuetoAppraised_new = final_datanoOut8
final_data$LoanValuetoAppraised_new[is.na(final_data$LoanValuetoAppraised_new)] = 
  mean(final_data$LoanValuetoAppraised_new, na.rm = T)
sum(is.na(final_data$LoanValuetoAppraised_new))
drop <- c("LoanValuetoAppraised")
final_data = final_data[,!(names(final_data) %in% drop)]
names(final_data)[16] = "LoanValuetoAppraised"
outliers= boxplot(final_data$LoanValuetoAppraised)$out

# 8: median_income
outliers= boxplot(final_data$median_income)$out
outliers
#no outliers

#10 = per_of_people_in_poverty
outliers= boxplot(final_data$per_of_people_in_poverty)$out
final_datanoOut3 = ifelse(final_data$per_of_people_in_poverty %in% outliers, NA, final_data$per_of_people_in_poverty)
boxplot(final_datanoOut3)
summary(final_datanoOut3)
final_data$per_of_people_in_povertyNew = final_datanoOut3
final_data$per_of_people_in_povertyNew[is.na(final_data$per_of_people_in_povertyNew)] = 
  mean(final_data$per_of_people_in_povertyNew, na.rm = T)
sum(is.na(final_data$per_of_people_in_povertyNew))
drop <- c("per_of_people_in_poverty")
final_data = final_data[,!(names(final_data) %in% drop)]
names(final_data)[16] = "per_of_people_in_poverty"
outliers= boxplot(final_data$per_of_people_in_poverty)$out

#11 Credit_score_norm
outliers= boxplot(final_data$Credit_score_norm)$out
final_datanoOut4 = ifelse(final_data$Credit_score_norm %in% outliers, NA, final_data$Credit_score_norm)
boxplot(final_datanoOut4)
summary(final_datanoOut4)
final_data$Credit_score_normNew = final_datanoOut4
final_data$Credit_score_normNew[is.na(final_data$Credit_score_normNew)] = 
  mean(final_data$Credit_score_normNew, na.rm = T)
sum(is.na(final_data$Credit_score_normNew))
drop <- c("Credit_score_norm")
final_data = final_data[,!(names(final_data) %in% drop)]
names(final_data)[16] = "Credit_score_norm"
outliers= boxplot(final_data$Credit_score_norm)$out

# transformations done - we have already done the variable transformations in the above part.
#1)Credit_score_norm
#2)Bo_Age_ind

# Variable Distribution
# we have summary for categorical variables and 
summary(final_data$UPBgeAppraisal)
summary(final_data$OUTCOME)
summary(final_data$Moninc_medInc_ind)
summary(final_data$Bo_Age_ind)
summary(final_data$First_home)

# histogram for quantitative variables
hist(final_data$median_income)
hist(final_data$Ln_Orig)
hist(final_data$Orig_LTV_Ratio_Pct)
hist(final_data$Tot_mthly_incm)
hist(final_data$Tot_mthly_debt_exp)
hist(final_data$orig_apprd_val_amt)
hist(final_data$pur_prc_amt)
hist(final_data$DTI_Ratio)
hist(final_data$LoanValuetoAppraised)
hist(final_data$per_of_people_in_poverty)
hist(final_data$Credit_score_norm)

data = final_data
# 15 independent variables , 1 dependent variable ie OUTCOME

#---------------------------------------
# CROSS VALIDATION
str(data)
#k fold 
k <- 3

data <- data %>% mutate(rand = runif(n = nrow(data)))
data <- data %>% arrange(rand)
data <- data %>% select(-rand)
fold_indices <- rep(1:k, each = (nrow(data)/k))

results <-  data.frame(fold = as.numeric(),
                       threshold = as.numeric(),
                       recall = as.numeric(),
                       precision = as.numeric(),
                       accuracy = as.numeric())

resultsB <-  data.frame(fold = as.numeric(),
                       threshold = as.numeric(),
                       recall = as.numeric(),
                       precision = as.numeric(),
                       accuracy = as.numeric())


resultsC <-  data.frame(fold = as.numeric(),
                        threshold = as.numeric(),
                        recall = as.numeric(),
                        precision = as.numeric(),
                        accuracy = as.numeric())


resultsD <-  data.frame(fold = as.numeric(),
                        threshold = as.numeric(),
                        recall = as.numeric(),
                        precision = as.numeric(),
                        accuracy = as.numeric())
                        

resultsNA <-  data.frame(fold = as.numeric(),
                         threshold = as.numeric(),
                         recall = as.numeric(),
                         precision = as.numeric(),
                         accuracy = as.numeric()
)

resultsNB <-  data.frame(fold = as.numeric(),
                         threshold = as.numeric(),
                         recall = as.numeric(),
                         precision = as.numeric(),
                         accuracy = as.numeric())
i=1
for (i in 1:k){
  print(i)
  
  set.seed(2)
  
  train <- data[fold_indices != i, ]
  test <- data[fold_indices == i, ]
  
  table(train$OUTCOME)
  
  TrgA = ovun.sample(OUTCOME~ ., data = train, method="under", p=0.3)$data
  summary(TrgA$OUTCOME)
  
  TrgB = ovun.sample(OUTCOME~ ., data = train, method="under", p=0.1)$data
  summary(TrgB$OUTCOME)
  
########## TrgA: LOGISTIC REGRESSION ######
  
  model1A <- glm(OUTCOME~ .,data = TrgA, family = "binomial")
  Train_predictionsA <- model1A$fitted.values
  test_predictionA <- predict(model1A,newdata = test, type = "response")
  test_predictionA
  HighTest <- ifelse(test$OUTCOME =="default", 1, 0)
  HighTest = as.factor(HighTest)

  #threshold
  proc <- roc.curve(scores.class0= test_predictionA, weights.class0 = as.numeric(as.character(HighTest)),
                 curve = T)
  #ROC CURVE
  plot(proc)
  roc <- roc(HighTest ,test_predictionA)
  threshold <-  as.numeric(coords(roc, "best", ret = "threshold"))
  
  Binarypred <- ifelse(test_predictionA >= threshold,1,0)
  consufion_mtrix <- table(Actual = test$OUTCOME, Pred = Binarypred)
  consufion_mtrix
  recall  <- consufion_mtrix[1,2]/ (consufion_mtrix[1,2] +consufion_mtrix[1,1])
  precision  <- consufion_mtrix[1,2]/ (consufion_mtrix[1,2] +consufion_mtrix[2,2])
  Binarypredf = as.factor(Binarypred)
  accuracy <- mean(Binarypredf == HighTest)

    results[i, ] <- c(fold =i,
                    threshold = threshold,
                    recall = recall ,
                    precision = precision,
                    accuracy = accuracy)
#LIFT CHART
  plotLift(test_predictionA, HighTest, cumulative = TRUE, 
           n.buckets = 10,main = "lift Curve by Decile", col = "blue")
  
######### TrgB LOGISTIC REGRESSION ######
  model1B <- glm(OUTCOME~ .,data = TrgB, family = "binomial")
  Train_predictionsB <- model1B$fitted.values
  test_predictionB <- predict(model1B,newdata = test, type = "response")

  #threshold
  proc <- roc.curve(scores.class0= Train_predictionsB, weights.class0 = as.numeric(as.character(HighTest)),
                    curve = T)
  plot(proc)
  #ROC CURVE
  roc1 <- roc(HighTest ,test_predictionB)
  threshold <-  as.numeric(coords(roc1, "best", ret = "threshold"))
  
  Binarypred <- ifelse(test_predictionB >= threshold,1,0)
  consufion_mtrix <- table(Actual = test$OUTCOME, Pred = Binarypred)
  recall  <- consufion_mtrix[1,2]/ (consufion_mtrix[1,2] +consufion_mtrix[1,1])
  precision  <- consufion_mtrix[1,2]/ (consufion_mtrix[1,2] +consufion_mtrix[2,2])
  Binarypredf = as.factor(Binarypred)
  accuracy <- mean(Binarypredf == HighTest)
  #record_result
  resultsB[i, ] <- c(fold =i,
                    threshold = threshold,
                    recall = recall ,
                    precision = precision,
                    accuracy = accuracy)
  
  #LIFT CHART
  plotLift(test_predictionB, HighTest, cumulative = TRUE, 
           n.buckets = 10,main = "lift Curve by Decile", col = "blue")

######### TrgA Neural Network ######
  nnA <- nnet(OUTCOME~., data= TrgA, linout = F, size = 10, decay = 0.01)
  summary(nnA)
  nnA$fitted.values
  nnA$residuals
  
  Trainpred <- predict(nnA, TrgA)
  testpred <- predict(nnA, test)
  print(Trainpred)
  print(testpred)
  
  #ROC CURVE
  proc <- roc.curve(scores.class0= Trainpred, weights.class0 = as.numeric(as.character(HighTest)),
                    curve = T)
  plot(proc)
  roc1 <- roc(HighTest ,testpred)
  threshold <-  as.numeric(coords(roc1, "best", ret = "threshold"))
  
  Binarypred <- ifelse(testpred >= threshold,1,0)
  consufion_mtrix <- table(Actual = test$OUTCOME, Pred = Binarypred)
  consufion_mtrix
  recall  <- consufion_mtrix[1,2]/ (consufion_mtrix[1,2] +consufion_mtrix[1,1])
  precision  <- consufion_mtrix[1,2]/ (consufion_mtrix[1,2] +consufion_mtrix[2,2])
  Binarypredf = as.factor(Binarypred)
  accuracy <- mean(Binarypredf == HighTest)
  #record_result
  resultsNA[i, ] <- c(fold =i,
                      threshold = threshold,
                      recall = recall ,
                      precision = precision,
                      accuracy = accuracy
  )
  
  #LIFT CHART
  plotLift(testpred, HighTest, cumulative = TRUE, 
           n.buckets = 10,main = "lift Curve by Decile", col = "blue")
  
  
######### TrgB Neural Network  ######
  nnB <- nnet(OUTCOME~., data= TrgB, linout = F, size = 10, decay = 0.01)
  summary(nnB)
  nnB$fitted.values
  nnB$residuals
  
  TrainpredB <- predict(nnB, TrgB)
  testpredB <- predict(nnB, test)
  print(TrainpredB)
  print(testpredB)
  
  
  proc <- roc.curve(scores.class0= TrainpredB, weights.class0 = as.numeric(as.character(HighTest)),
                    curve = T)
  plot(proc)
  roc1 <- roc(HighTest ,testpredB)
  threshold <-  as.numeric(coords(roc1, "best", ret = "threshold"))
  
  Binarypred <- ifelse(testpredB >= threshold,1,0)
  consufion_mtrix <- table(Actual = test$OUTCOME, Pred = Binarypred)
  recall  <- consufion_mtrix[1,2]/ (consufion_mtrix[1,2] +consufion_mtrix[1,1])
  precision  <- consufion_mtrix[1,2]/ (consufion_mtrix[1,2] +consufion_mtrix[2,2])
  Binarypredf = as.factor(Binarypred)
  accuracy <- mean(Binarypredf == HighTest)
  #record_result
  resultsNB[i, ] <- c(fold =i,
                      threshold = threshold,
                      recall = recall ,
                      precision = precision,
                      accuracy = accuracy
  )
  #LIFT CHART
  plotLift(testpredB, HighTest, cumulative = TRUE, 
           n.buckets = 10,main = "lift Curve by Decile", col = "blue")
  
######### Decision Tree TRGA#####
  tree_model <- rpart(OUTCOME ~ ., data = TrgA)
  rpart.plot(tree_model)
  print(tree_model)
  
  tree_pred_prob <- predict(tree_model, test, type = "prob")
  tree_pred_class <- predict(tree_model, test, type = "class")
  accuracy = mean(test$OUTCOME == tree_pred_class)
  
  tree_pred_class_train <- predict(tree_model, TrgA, type = "class")
  mean(TrgA$OUTCOME != tree_pred_class_train)
  
  #lift chart
  votes = as.numeric(tree_pred_class)
  pred <- prediction(votes, test$OUTCOME)
  perf <- performance(pred, "lift")
  plot(perf)
  #roc curve
  class <- ifelse(tree_pred_class =="default",1,0)
  outcome <- ifelse(test$OUTCOME =="default",1,0)
  pred = prediction(class, outcome)
  roc = performance(pred, measure="tpr", x.measure="fpr")
  plot(roc, col="orange", lwd=2) 
  lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)
  
  auc = performance(pred, 'auc')
  slot(auc, 'y.values')
  #

  table = table(test$OUTCOME, tree_pred_class)
  table 
  
  #threshold
  
  recall  <- table[1,2]/ (table[1,2] +table[1,1])
  precision  <- table[1,2]/ (table[1,2] +table[2,2])
  #record_result
  resultsC[i, ] <- c(fold =i,
                     threshold = 1,
                     recall = recall ,
                     precision = precision,
                     accuracy = accuracy)
  
######### Decision Tree TRGB#####
  
  tree_mode1l <- rpart(OUTCOME ~ ., data = TrgB)
  rpart.plot(tree_mode1l)
  print(tree_mode1l)
  
  tree_pred_prob <- predict(tree_mode1l, test, type = "prob")
  tree_pred_class <- predict(tree_mode1l, test, type = "class")
  accuracy = mean(test$OUTCOME == tree_pred_class)
  
  tree_pred_class_train <- predict(tree_mode1l, TrgA, type = "class")
  mean(TrgA$OUTCOME != tree_pred_class_train)
  
  #lift chart
  votes = as.numeric(tree_pred_class)
  pred <- prediction(votes, test$OUTCOME)
  perf1 <- performance(pred, "lift")
  plot(perf1)
  #roc curve
  class <- ifelse(tree_pred_class =="default",1,0)
  outcome <- ifelse(test$OUTCOME =="default",1,0)
  pred = prediction(class, outcome)
  roc = performance(pred, measure="tpr", x.measure="fpr")
  plot(roc, col="orange", lwd=2) 
  lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)
  
  auc = performance(pred, 'auc')
  slot(auc, 'y.values')
  #
  
  table = table(test$OUTCOME, tree_pred_class)
  table 
  
  #threshold
  recall  <- table[1,2]/ (table[1,2] +table[1,1])
  precision  <- table[1,2]/ (table[1,2] +table[2,2])
  recall
  
  #record_result
  resultsD[i, ] <- c(fold =i,
                     threshold = 1,
                     recall = recall ,
                     precision = precision,
                     accuracy = accuracy)
  
}
mean(results$recall) # Logistic Regression TRGA
mean(resultsB$recall) # Logistic Regression TRGB
mean(resultsNA$recall) # Neural Network TRGA
mean(resultsNB$recall) # Neural Network TRGB
mean(resultsC$recall) # Decision tree TRGA
mean(resultsD$recall) # Decision tree TRGB

# So, Recall is the critical evaluation parameter, 
# hence, looking at the recall from above cross validated models, 
# So, Logistic regression is the best model out of three

