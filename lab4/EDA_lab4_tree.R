cat("\f")

### Default Options 

options(scipen=999)
options(stringsAsFactors = FALSE)

### Read Data

help(iris)
Data = iris # iris is a default database in R 

########################################################
### visualisation of the data

### set the colours for the three classes
cols = c("steelblue1", "hotpink", "mediumpurple")
### set the symbols for the three classes
pchs = c(1,2,3)


pairs(iris[,1:4], pch = pchs[iris$Species],  cex = 1,
      col = cols[iris$Species],
      lower.panel=NULL)

### create a legend for the plot
par(xpd = TRUE)
legend("bottomleft",legend=c("setosa","versicolor","virginica"),
       col=cols,pch=pchs,cex=1,text.font=2)

##########################################################################################################
### Set up a model formula for the classification tree

str(Data)
names(Data)

predictors = c( "Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")

predictors = paste(predictors, collapse = "+")
formula = paste("Species",predictors, sep="~")
typeof(formula)
formula
# The formula is the one that gives the model the important columns

#############################################################################################################
### trees constructed with the library "tree"

#install.packages("tree")
library(tree)

### tree training

ClassificationTree = tree(formula, data = Data) 

### print information

print(ClassificationTree)
summary(ClassificationTree)
misclass.tree(ClassificationTree)
### Misclassification error rate: 0.02667 = 4 / 150

### plot information

plot(ClassificationTree)
text(ClassificationTree, all=TRUE, cex=.8)

################################################################################################################
### until here we have only computed the training error
### next we estimate the test error by training and testing


alpha = 0.80 ### percentage of training set
Train = sample(1:nrow(Data), alpha * nrow(Data)) ### training set
Data_test = Data[-Train,]                        ### test data
target_test  = Data$Species[-Train] ### test target

ClassificationTree_Test = tree(formula, data = Data, subset = Train)

print(ClassificationTree_Test)
summary(ClassificationTree_Test)
misclass.tree(ClassificationTree_Test)

### plot information

plot(ClassificationTree_Test)
text(ClassificationTree_Test, all=TRUE, cex=.8)

### predicted results

ClassificationTree_Test_Predict = predict(ClassificationTree_Test, Data_test, type="class")
ClassificationTree_Test_Predict

### we compare the predicted results with the results directly obtained from data

Prediction_Table = table( ClassificationTree_Test_Predict, target_test)
Prediction_Table

### we compute the rate of correct predictions
### rate of correct predictions is the same as accuracy

rate_correct_predictions = (Prediction_Table[1,1]+Prediction_Table[2,2]+Prediction_Table[3,3])/length(target_test)
rate_correct_predictions

### Note:
### variation of alpha will lead to  variation in the rate of correct predictions
### however, the numbers obtained are very high, i.e. rate_correct_predictions \in [0.9,1.00]

################################################################################################
### more sistematic approach for analysing the results  
### confusion matrix using the library "caret"

library(caret)

Confusion_Matrix = confusionMatrix(Prediction_Table, mode = "everything")
print(Confusion_Matrix)

### this generates more statistics, Accuracy is the same as the rate of correct predictions
###############################################################################################
###############################################################################################
###############################################################################################

Data_2 = as.data.frame( read.csv("lab4/Heart.csv", na.strings = " "))

str(Data_2)
dim(Data_2)

###############################################################################################
### Explicit type conversion

Data_2$ChestPain_factor = as.factor(Data_2$ChestPain)
Data_2$Ca_factor = as.factor(Data_2$Ca)
Data_2$Thal_factor = as.factor(Data_2$Thal)
Data_2$AHD_factor = as.factor(Data_2$AHD)

str(Data_2)

#############################################################################################################

ClassificationTree_2 = tree(AHD_factor~Thal_factor+Ca_factor+Age+MaxHR+           
                              Chol+Fbs+ChestPain_factor+Sex+Slope+ExAng+RestBP+             
                              Oldpeak+RestECG, data = Data_2) 

### print information

print(ClassificationTree_2)
summary(ClassificationTree_2)
misclass.tree(ClassificationTree_2)
### Misclassification error rate: 0.1221 = 37 / 303


### plot information

plot(ClassificationTree_2)
text(ClassificationTree_2, all=TRUE, cex=.8)

################################################################################################################
### next we estimate the test error by training and testing

set.seed(171)
alpha = 0.8 ### percentage of training set
Train_2 = sample(1:nrow(Data_2), alpha * nrow(Data_2)) ### training set
Data_2_test = Data_2[-Train_2,]                        ### test data
target_2_test  = Data_2$AHD_factor[-Train_2] ### test target

ClassificationTree_2_Test = tree(AHD_factor~Thal_factor+Ca_factor+Age+MaxHR+           
                                     Chol+Fbs+ChestPain_factor+Sex+             
                                     Oldpeak+RestECG, data = Data_2, subset = Train_2)

print(ClassificationTree_2_Test)
summary(ClassificationTree_2_Test)
misclass.tree(ClassificationTree_2_Test)

### plot information

plot(ClassificationTree_2_Test)
text(ClassificationTree_2_Test, all=TRUE, cex=.8)

### predicted results

ClassificationTree_2_Test_Predict = predict(ClassificationTree_2_Test, Data_2_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_2 = table(ClassificationTree_2_Test_Predict, target_2_test)
Prediction_Table_2
Prediction_Table_2 = Prediction_Table_2[2:1,2:1]
Prediction_Table_2

### we compute the rate of correct predictions

rate_correct_predictions_2 = (Prediction_Table_2[1,1]+Prediction_Table_2[2,2])/length(target_2_test)
rate_correct_predictions_2

### Note:
### variation of alpha will lead to  variation in the rate of correct predictions
### however, the numbers obtained are high, i.e. rate_correct_predictions \in [0.70,0.90]

Confusion_Matrix_2 = confusionMatrix(Prediction_Table_2, mode = "everything")
print(Confusion_Matrix_2)

###############################################################################################
###############################################################################################
###############################################################################################
### check if prunning the tree will lead to improved results
### we use the cross validating function several times

ClassificationTree_2_Cross_Validation = cv.tree(ClassificationTree_2, FUN=prune.misclass)
names(ClassificationTree_2_Cross_Validation)
ClassificationTree_2_Cross_Validation

par(mfrow=c(1,2))
plot(ClassificationTree_2_Cross_Validation$size,ClassificationTree_2_Cross_Validation$dev,type="b",
     xlab="number of leaves of the tree",ylab="CV error rate%",
     cex.lab=1.5,cex.axis=1.5)
plot(ClassificationTree_2_Cross_Validation$k,ClassificationTree_2_Cross_Validation$dev,type="b",
     xlab=expression(k),ylab="CV error rate%",
     cex.lab=1.5,cex.axis=1.5)
par(mfrow=c(1,1))

### after performing the cross validation many times we see that the tree we have is not optimal 
### k is a cost-for-complexity parameter, that can be used for tunning the model
### big k <=> small number of nodes in the tree  
### small k <=> big number of nodes in the tree

### it looks that the optimal tree is a smaller tree with only 7 nodes
### we prune the tree to get the 7 nodes tree

ClassificationTree_2_Prune_1 = prune.misclass(ClassificationTree_2_Test, best = 7)

print(ClassificationTree_2_Prune_1)
summary(ClassificationTree_2_Prune_1)
misclass.tree(ClassificationTree_2_Prune_1)

### plot information

plot(ClassificationTree_2_Prune_1)
text(ClassificationTree_2_Prune_1, all=TRUE, cex=.8)

### we check to see how this small tree performs on the test data

ClassificationTree_2_Prune_1_Predict = predict(ClassificationTree_2_Prune_1, Data_2_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_Prune_1 = table(target_2_test, ClassificationTree_2_Prune_1_Predict)
Prediction_Table_Prune_1
Prediction_Table_Prune_1 = Prediction_Table_Prune_1[2:1,2:1]
Prediction_Table_Prune_1

### we compute the rate of correct predictions

rate_correct_predictions_Prune_1 = (Prediction_Table_Prune_1[1,1]+Prediction_Table_Prune_1[2,2])/length(target_2_test)
rate_correct_predictions_Prune_1

### we see that the rate of correct predictions of the pruned tree is better 
### than the rate of correct predictions of the original tree
####################################################################################################################################
### we prune the tree to get a 5 nodes tree

ClassificationTree_2_Prune_2 = prune.misclass(ClassificationTree_2_Test, best = 5)

print(ClassificationTree_2_Prune_2)
summary(ClassificationTree_2_Prune_2)
misclass.tree(ClassificationTree_2_Prune_2)

### plot information

plot(ClassificationTree_2_Prune_2)
text(ClassificationTree_2_Prune_2, all=TRUE, cex=.8)

### we check to see how this small tree performs on the test data

ClassificationTree_2_Prune_2_Predict = predict(ClassificationTree_2_Prune_2, Data_2_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_Prune_2 = table(target_2_test, ClassificationTree_2_Prune_2_Predict)
Prediction_Table_Prune_2
Prediction_Table_Prune_2 = Prediction_Table_Prune_2[2:1,2:1]
Prediction_Table_Prune_2


### we compute the rate of correct predictions

rate_correct_predictions_Prune_2 = (Prediction_Table_Prune_2[1,1]+Prediction_Table_Prune_2[2,2])/length(target_2_test)
rate_correct_predictions_Prune_2

### we see that the rate of correct predictions of the second pruned tree is comparable
### with the rate of correct predictions of the first pruned tree, 
### but the second tree is easier to read

#########################################################################################################
#########################################################################################################
#########################################################################################################
### Classification with the Linear Model
### We compare with a Linear Model
### We estimate the test error for a Linear Model

Data_2$AHD_Bernoulli=ifelse(Data_2$AHD_factor == "Yes", 1, 0)

LM_Test = lm(AHD_Bernoulli~Thal_factor+Ca_factor+Age+MaxHR+           
               Chol+Fbs+ChestPain_factor+Sex+             
               Oldpeak+RestECG, data = Data_2, subset = Train_2)

summary(LM_Test)
### R-squared is bad, however we are doind classification here, not regression!
### in this case R-squared is not the "recommended" performance measure

### predicted results

LM_Predict = predict(LM_Test, Data_2_test)
Data_2_test$LM_Predicted = as.factor(ifelse(LM_Predict>0.5, "Yes", "No"))

### we compare the predicted results with the results directly obtained from data

Prediction_Table_LM = table(Data_2_test$LM_Predicted, target_2_test)
Prediction_Table_LM
Prediction_Table_LM = Prediction_Table_LM[2:1,2:1]
Prediction_Table_LM

### we compute the rate of correct predictions

rate_correct_predictions_LM = (Prediction_Table_LM[1,1]+Prediction_Table_LM[2,2])/length(target_2_test)
rate_correct_predictions_LM

### this is the "recommended" performance measure and it is not that bad!

Confusion_Matrix_LM = confusionMatrix(Prediction_Table_LM, mode = "everything")
print(Confusion_Matrix_LM)

#########################################################################################################
#########################################################################################################
#########################################################################################################
### Classification with the Logistic Regression
### We compare with Logistic Regression
### We estimate the test error for a Logistic Regression

GLM_Test = glm(AHD_factor~Thal_factor+Ca_factor+Age+MaxHR+           
               Chol+Fbs+ChestPain_factor+Sex+             
               Oldpeak+RestECG, family = binomial, data = Data_2, subset = Train_2)

summary(GLM_Test)

### predicted results

GLM_Predict = predict(GLM_Test, Data_2_test, type = "response")
GLM_Predict
Data_2_test$GLM_Predicted = as.factor(ifelse(GLM_Predict>0.5, "Yes", "No"))

### we compare the predicted results with the results directly obtained from data

Prediction_Table_GLM = table(Data_2_test$GLM_Predicted, target_2_test)
Prediction_Table_GLM
Prediction_Table_GLM = Prediction_Table_GLM[2:1,2:1]
Prediction_Table_GLM

### we compute the rate of correct predictions

rate_correct_predictions_GLM = (Prediction_Table_GLM[1,1]+Prediction_Table_GLM[2,2])/length(target_2_test)
rate_correct_predictions_GLM

### this is the "recommended" performance measure and it is not that bad!

Confusion_Matrix_GLM = confusionMatrix(Prediction_Table_GLM, mode = "everything")
print(Confusion_Matrix_GLM)

#########################################################################################################
#########################################################################################################
#########################################################################################################
### We attemp to remove predictors which are not significant in all models

ClassificationTree_3_Test = tree(AHD_factor~Thal_factor+Ca_factor+MaxHR+           
                                   ChestPain_factor+Sex+             
                                   Oldpeak+RestECG, data = Data_2, subset = Train_2)

print(ClassificationTree_3_Test)
summary(ClassificationTree_3_Test)
misclass.tree(ClassificationTree_3_Test)

### plot information

plot(ClassificationTree_3_Test)
text(ClassificationTree_3_Test, all=TRUE, cex=.8)

### predicted results

ClassificationTree_3_Test_Predict = predict(ClassificationTree_3_Test, Data_2_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_3 = table(ClassificationTree_3_Test_Predict, target_2_test)
Prediction_Table_3
Prediction_Table_3 = Prediction_Table_3[2:1,2:1]
Prediction_Table_3

### we compute the rate of correct predictions

rate_correct_predictions_3 = (Prediction_Table_3[1,1]+Prediction_Table_3[2,2])/length(target_2_test)
rate_correct_predictions_3

### Note:
### variation of alpha will lead to  variation in the rate of correct predictions
### however, the numbers obtained are high, i.e. rate_correct_predictions \in [0.70,0.90]

Confusion_Matrix_3 = confusionMatrix(Prediction_Table_3, mode = "everything")
print(Confusion_Matrix_3)

###############################################################################################
###############################################################################################
###############################################################################################
### we prune the tree to get the 7 nodes tree

ClassificationTree_3_Prune_1 = prune.misclass(ClassificationTree_3_Test, best = 5)

print(ClassificationTree_3_Prune_1)
summary(ClassificationTree_3_Prune_1)
misclass.tree(ClassificationTree_3_Prune_1)

### plot information

plot(ClassificationTree_3_Prune_1)
text(ClassificationTree_3_Prune_1, all=TRUE, cex=.8)

### we check to see how this small tree performs on the test data

ClassificationTree_3_Prune_1_Predict = predict(ClassificationTree_3_Prune_1, Data_2_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_Prune_31 = table(target_2_test, ClassificationTree_3_Prune_1_Predict)
Prediction_Table_Prune_31
Prediction_Table_Prune_31 = Prediction_Table_Prune_31[2:1,2:1]
Prediction_Table_Prune_31

### we compute the rate of correct predictions

rate_correct_predictions_Prune_31 = (Prediction_Table_Prune_31[1,1]+Prediction_Table_Prune_31[2,2])/length(target_2_test)
rate_correct_predictions_Prune_31

### we see that the rate of correct predictions of the pruned tree is better 
### than the rate of correct predictions of the original tree
####################################################################################################################################
### we prune the tree to get a 5 nodes tree

ClassificationTree_3_Prune_2 = prune.misclass(ClassificationTree_2_Test, best = 3)

print(ClassificationTree_3_Prune_2)
summary(ClassificationTree_3_Prune_2)
misclass.tree(ClassificationTree_3_Prune_2)

### plot information

plot(ClassificationTree_3_Prune_2)
text(ClassificationTree_3_Prune_2, all=TRUE, cex=.8)

### we check to see how this small tree performs on the test data

ClassificationTree_3_Prune_2_Predict = predict(ClassificationTree_3_Prune_2, Data_2_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_Prune_32 = table(target_2_test, ClassificationTree_3_Prune_2_Predict)
Prediction_Table_Prune_32
Prediction_Table_Prune_32 = Prediction_Table_Prune_32[2:1,2:1]
Prediction_Table_Prune_32

### we compute the rate of correct predictions

rate_correct_predictions_Prune_32 = (Prediction_Table_Prune_32[1,1]+Prediction_Table_Prune_32[2,2])/length(target_2_test)
rate_correct_predictions_Prune_32

### we see that the rate of correct predictions of the second pruned tree is smaller
### than the rate of correct predictions of the first pruned tree, 
### but the second tree is easier to read

#########################################################################################################
#########################################################################################################
#########################################################################################################
### Classification with the Linear Model
### We compare with a Linear Model
### We estimate the test error for a Linear Model

LM_2_Test = lm(AHD_Bernoulli~Thal_factor+Ca_factor+MaxHR+           
                 ChestPain_factor+Sex+             
                 Oldpeak+RestECG, data = Data_2, subset = Train_2)

summary(LM_2_Test)
### R-squared is bad, however we are doind classification here, not regression!
### in this case R-squared is not the "recommended" performance measure

### predicted results

LM_2_Predict = predict(LM_2_Test, Data_2_test)
Data_2_test$LM_2_Predicted = as.factor(ifelse(LM_2_Predict>0.5, "Yes", "No"))

### we compare the predicted results with the results directly obtained from data

Prediction_Table_LM_2 = table(Data_2_test$LM_2_Predicted, target_2_test)
Prediction_Table_LM_2
Prediction_Table_LM_2 = Prediction_Table_LM_2[2:1,2:1]
Prediction_Table_LM_2

### we compute the rate of correct predictions

rate_correct_predictions_LM_2 = (Prediction_Table_LM_2[1,1]+Prediction_Table_LM_2[2,2])/length(target_2_test)
rate_correct_predictions_LM_2

### this is the "recommended" performance measure and it is not that bad!

Confusion_Matrix_LM_2 = confusionMatrix(Prediction_Table_LM_2, mode = "everything")
print(Confusion_Matrix_LM_2)

#########################################################################################################
#########################################################################################################
#########################################################################################################
### Classification with the Logistic Regression
### We compare with Logistic Regression
### We estimate the test error for a Logistic Regression

GLM_2_Test = glm(AHD_factor~Thal_factor+Ca_factor+MaxHR+           
                   ChestPain_factor+Sex+             
                   Oldpeak+RestECG, family = binomial, data = Data_2, subset = Train_2)

summary(GLM_2_Test)

### predicted results

GLM_2_Predict = predict(GLM_2_Test, Data_2_test, type = "response")
GLM_2_Predict
Data_2_test$GLM_2_Predicted = as.factor(ifelse(GLM_2_Predict>0.5, "Yes", "No"))

### we compare the predicted results with the results directly obtained from data

Prediction_Table_GLM_2 = table(Data_2_test$GLM_2_Predicted, target_2_test)
Prediction_Table_GLM_2
Prediction_Table_GLM_2 = Prediction_Table_GLM_2[2:1,2:1]
Prediction_Table_GLM_2

### we compute the rate of correct predictions

rate_correct_predictions_GLM_2 = (Prediction_Table_GLM_2[1,1]+Prediction_Table_GLM_2[2,2])/length(target_2_test)
rate_correct_predictions_GLM_2

### this is the "recommended" performance measure and it is not that bad!

Confusion_Matrix_GLM_2 = confusionMatrix(Prediction_Table_GLM_2, mode = "everything")
print(Confusion_Matrix_GLM_2)
###############################################################################################
###############################################################################################
###############################################################################################
### CONCLUSION

Classification_Alg = c("Tree_2",
                   "Prune_21",
                   "Prune_22",
                   "Tree_3",
                   "Prune_31",
                   "Prune_32",
                   "LM_1",
                   "LM_2",
                   "LR_1",
                   "LR_2")
Accuracy = c(rate_correct_predictions_2,rate_correct_predictions_Prune_1,rate_correct_predictions_Prune_2,
             rate_correct_predictions_3,rate_correct_predictions_Prune_31,rate_correct_predictions_Prune_32,
             rate_correct_predictions_LM, rate_correct_predictions_LM_2,
             rate_correct_predictions_GLM, rate_correct_predictions_GLM_2)

Conclusion <- data.frame(Classification_Alg,
                         Accuracy)
Conclusion



###############################################################################################
###############################################################################################
###############################################################################################


Data_3 = as.data.frame( read.csv("Hitters.csv"))

str(Data_3)
dim(Data_3)
sum(is.na(Data_3$Salary))
Data_4 = na.omit(Data_3)
str(Data_4)
dim(Data_4)
sum(is.na(Data_4$Salary))

###############################################################################################
### Explicit type conversion

Data_4$League = as.factor(Data_4$League)
Data_4$Division = as.factor(Data_4$Division)
Data_4$NewLeague = as.factor(Data_4$NewLeague)
Data_4$Salary_log = log (Data_4$Salary)

str(Data_4)
#############################################################################################################

RegressionTree = tree(Salary_log~.-Salary, data = Data_4) 


### print information

print(RegressionTree)
summary(RegressionTree)

### Misclassification error rate: 0.1221 = 37 / 303


### plot information

plot(RegressionTree)
text(RegressionTree, all=TRUE, cex=.8)

################################################################################################################
### next we estimate the test error by training and testing


alpha = 0.8 ### percentage of training set
Train_4 = sample(1:nrow(Data_4), alpha * nrow(Data_4)) ### training set
Data_4_test = Data_4[-Train_4,]                        ### test data
target_4_test  = Data_4$Salary_log[-Train_4] ### test target

RegressionTree_Test = tree(Salary_log~.-Salary, data = Data_4, subset = Train_4)

print(RegressionTree_Test)
summary(RegressionTree_Test)

### plot information

plot(RegressionTree_Test)
text(RegressionTree_Test, all=TRUE, cex=.8)

### predicted results

RegressionTree_Test_Predict = predict(RegressionTree_Test, Data_4_test)

Performance_Tree_Test = mean((RegressionTree_Test_Predict-target_4_test)^2)
Performance_Tree_Test 

### test MSE = 0.3594402

plot(RegressionTree_Test_Predict, target_4_test)
lines(lowess(RegressionTree_Test_Predict, target_4_test, f = 2/3, iter = 3), col = "red")

#########################################################################################################
### We compare with a Linear Model
### We estimate the test error for a Linear Model

LMR_Test = lm(Salary_log~.-Salary-1, data = Data_4, subset = Train_4)

summary(LMR_Test)

LMR_Test_Predict = predict(LMR_Test, Data_4_test)

Performance_LMR_Test = mean((LMR_Test_Predict-target_4_test)^2)
Performance_LMR_Test 

### test MSE = 0.4906255

plot(LMR_Test_Predict, target_4_test)


############################################################################################
#with extra feature engineering

Data_4$composed_feature_1 = log(Data_4$CHits)*Data_4$Walks
plot(Data_4$Salary, Data_4$composed_feature_1)
print(Data_4$composed_feature_1)
Data_4 = subset(Data_4, composed_feature_1 > 5)


Data_4$composed_feature_2 = Data_4$Runs*Data_4$CHmRun
plot(Data_4$Salary, Data_4$composed_feature_2)

Data_4$composed_feature_3 = Data_4$HmRun*Data_4$CWalks
plot(Data_4$Salary, Data_4$composed_feature_3)
Data_4 = subset(Data_4, composed_feature_3 < 20000)

Data_4$composed_feature_4 = log(Data_4$CRBI)*Data_4$CAtBat
plot(Data_4$Salary, Data_4$composed_feature_4)
Data_4 = subset(Data_4, composed_feature_4 < 80000)

dim(Data_4)

################################################################################


alpha = 0.8 ### percentage of training set
Train_4 = sample(1:nrow(Data_4), alpha * nrow(Data_4)) ### training set
Data_4_test = Data_4[-Train_4,]                        ### test data
target_4_test  = Data_4$Salary_log[-Train_4] ### test target

RegressionTree_Test = tree(Salary_log~.-CRuns-AtBat-Errors-Assists-NewLeague-Salary, data = Data_4, subset = Train_4)

print(RegressionTree_Test)
summary(RegressionTree_Test)

### plot information

plot(RegressionTree_Test)
text(RegressionTree_Test, all=TRUE, cex=.8)

### predicted results

RegressionTree_Test_Predict = predict(RegressionTree_Test, Data_4_test)

Performance_Tree_Test = mean((RegressionTree_Test_Predict-target_4_test)^2)
Performance_Tree_Test 

### test MSE = 0.3979843

plot(RegressionTree_Test_Predict, target_4_test)
lines(lowess(RegressionTree_Test_Predict, target_4_test, f = 2/3, iter = 3), col = "red")

#########################################################################################################
### We compare with a Linear Model
### We estimate the test error for a Linear Model

LMR_Test = lm(Salary_log~.-CRuns-AtBat-Errors-Assists-NewLeague-Salary-1, data = Data_4, subset = Train_4)

summary(LMR_Test)

LMR_Test_Predict = predict(LMR_Test, Data_4_test)

Performance_LMR_Test = mean((LMR_Test_Predict-target_4_test)^2)
Performance_LMR_Test 

### test MSE =  0.1703049

plot(LMR_Test_Predict, target_4_test)
lines(lowess(LMR_Test_Predict, target_4_test, f = 2/3, iter = 3), col = "red")




