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

#############################################################################################################
#############################################################################################################
### trees constructed with the library "tree"

#install.packages("tree")
library(tree)

### tree training

ClassificationTree = tree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data =Data) 

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

ClassificationTree_Test = tree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = Data, subset = Train)

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

rate_correct_predictions = (Prediction_Table[1,1]+Prediction_Table[2,2]+Prediction_Table[3,3])/length(target_test)
rate_correct_predictions

### Note:
### variation of alpha will lead to  variation in the rate of correct predictions
### however, the numbers obtained are very high, i.e. rate_correct_predictions \in [0.95,1.00]

################################################################################################
### more sistematic approach for analysing the results  
### confusion matrix using the library "caret"

library(caret)

Confusion_Matrix = confusionMatrix(Prediction_Table, mode = "everything")
print(Confusion_Matrix)

### this generates more stastistics, Accuracy is the same as the rate of correct predictions
###############################################################################################
###############################################################################################
###############################################################################################
### bagging and random forests constructed with the library "randomForest"
### we only estimate the test error by training and testing
### in order to see if the results obtained with the library "tree" can be improved

library(randomForest)

### bagging by adding predictors

Bag = randomForest(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
                   data = Data, subset = Train, mtry = 4, importance = TRUE, ntree=100)

### the argument "mtry = 4" forces bagging
### ntree gives the number of trees to be constructed

Bag


importance(Bag)

### plot information

varImpPlot(Bag)

### we see that the important predictors are "Petal.Length" and "Petal.Width",
### predicted results

Bag_Predict = predict(Bag, Data_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_Bag = table(Bag_Predict, target_test)
Prediction_Table_Bag

### we compute the rate of correct predictions

rate_correct_predictions_bag = (Prediction_Table_Bag[1,1]+Prediction_Table_Bag[2,2]+Prediction_Table_Bag[3,3])/length(target_test)
rate_correct_predictions_bag

### we see the rate of correct predictions obtained by bagging is comparable 
### with that of the tree

Confusion_Matrix_Bag = confusionMatrix(Prediction_Table_Bag, mode = "everything")
print(Confusion_Matrix_Bag)

####################################################################################################

### next we try various random forests 

####################################################################################################
### random forest with 3 variables

Random_Forest_3 = randomForest(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
                               data = Data, subset = Train, mtry = 3, importance = TRUE, ntree=100)

### the argument "mtry = 3" gives a random forest with 3 variables

Random_Forest_3
importance(Random_Forest_3)

### plot information

varImpPlot(Random_Forest_3)

### we see that the important predictors are "Petal.Length" and "Petal.Width",
### predicted results

Random_Forest_3_Predict = predict(Random_Forest_3, Data_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_RF3 = table(Random_Forest_3_Predict, target_test)
Prediction_Table_RF3

### we compute the rate of correct predictions

rate_correct_predictions_RF3 = (Prediction_Table_RF3[1,1]+Prediction_Table_RF3[2,2]+Prediction_Table_RF3[3,3])/length(target_test)
rate_correct_predictions_RF3

### we see the rate of correct predictions obtained by random forest with 3 variables is comparable with
### that of tree or bagging

Confusion_Matrix_RF3 = confusionMatrix(Prediction_Table_RF3, mode = "everything")
print(Confusion_Matrix_RF3)

####################################################################################################
### random forest with 2 variables

Random_Forest_2 = randomForest(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
                                       data = Data, subset = Train, mtry = 2, importance = TRUE, ntree=100)

### the argument "mtry = 2" gives a random forest with 2 variables

Random_Forest_2
importance(Random_Forest_2)

### plot information

varImpPlot(Random_Forest_2)

### we see that the important predictors are "Petal.Length" and "Petal.Width",
### predicted results

Random_Forest_2_Predict = predict(Random_Forest_2, Data_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_RF2 = table(Random_Forest_2_Predict, target_test)
Prediction_Table_RF2

### we compute the rate of correct predictions

rate_correct_predictions_RF2 = (Prediction_Table_RF2[1,1]+Prediction_Table_RF2[2,2]+Prediction_Table_RF2[3,3])/length(target_test)
rate_correct_predictions_RF2

### we see the rate of correct predictions obtained by random forest with 2 variables is comparable with
### that of tree or bagging

Confusion_Matrix_RF2 = confusionMatrix(Prediction_Table_RF2, mode = "everything")
print(Confusion_Matrix_RF2)

####################################################################################################
####################################################################################################
####################################################################################################
### we colect the data obtained in order to draw a conclusion

Method_1 = c("Bag","RF3","RF2","Tree")
RateCP_1 = c(rate_correct_predictions_bag,rate_correct_predictions_RF3,rate_correct_predictions_RF2,rate_correct_predictions)
Conclusion_1 <- data.frame(Method_1,
                         RateCP_1)
Conclusion_1

plot(Conclusion_1$RateCP_1)

####################################################################################################
####################################################################################################
####################################################################################################


Data_2 = as.data.frame( read.csv("Heart.csv", na.strings = " "))

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

rate_correct_predictions_tree = (Prediction_Table_2[1,1]+Prediction_Table_2[2,2])/length(target_2_test)
rate_correct_predictions_tree

### Note:
### variation of alpha will lead to  variation in the rate of correct predictions
### however, the numbers obtained are high, i.e. rate_correct_predictions \in [0.70,0.90]

Confusion_Matrix_2 = confusionMatrix(Prediction_Table_2, mode = "everything")
print(Confusion_Matrix_2)
Precision_tree = Confusion_Matrix_2$byClass["Pos Pred Value"]
Precision_tree
Specificity_tree = Confusion_Matrix_2$byClass["Specificity"]
Specificity_tree

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

Confusion_Matrix_Prune_1 = confusionMatrix(Prediction_Table_Prune_1, mode = "everything")
print(Confusion_Matrix_Prune_1)
Precision_Prune_1 = Confusion_Matrix_Prune_1$byClass["Pos Pred Value"]
Precision_Prune_1
Specificity_Prune_1 = Confusion_Matrix_Prune_1$byClass["Specificity"]
Specificity_Prune_1

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

### we see that the rate of correct predictions of the second pruned tree is smaller 
### than the rate of correct predictions of the first pruned tree, 
### but the second tree is easier to read

Confusion_Matrix_Prune_2 = confusionMatrix(Prediction_Table_Prune_2, mode = "everything")
print(Confusion_Matrix_Prune_2)
Precision_Prune_2 = Confusion_Matrix_Prune_2$byClass["Pos Pred Value"]
Precision_Prune_2
Specificity_Prune_2 = Confusion_Matrix_Prune_2$byClass["Specificity"]
Specificity_Prune_2

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
Precision_LM = Confusion_Matrix_LM$byClass["Pos Pred Value"]
Precision_LM
Specificity_LM = Confusion_Matrix_LM$byClass["Specificity"]
Specificity_LM

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
Precision_GLM = Confusion_Matrix_GLM$byClass["Pos Pred Value"]
Precision_GLM
Specificity_GLM = Confusion_Matrix_GLM$byClass["Specificity"]
Specificity_GLM

###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
### bagging and random forests constructed with the library "randomForest"
### we only estimate the test error by training and testing
### in order to see if the results obtained with the library "tree" and the linear model can be improved

### bagging by adding predictors

Bag_2 = randomForest(AHD_factor~Thal_factor+Ca_factor+Age+MaxHR+           
                       Chol+Fbs+ChestPain_factor+Sex+Slope+ExAng+RestBP+             
                       Oldpeak+RestECG, data = Data_2, subset = Train_2, mtry = 13, importance = TRUE, ntree=1000)

### the argument "mtry = 4" forces bagging
### ntree gives the number of trees to be constructed

Bag_2


importance(Bag_2)

### plot information

varImpPlot(Bag_2)

### we see that the important predictors are "Thal_factor" and "ChestPain_factor",
### predicted results

Bag_2_Predict = predict(Bag_2, Data_2_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_Bag_2 = table(Bag_2_Predict, target_2_test)
Prediction_Table_Bag_2
Prediction_Table_Bag_2 = Prediction_Table_Bag_2[2:1,2:1]
Prediction_Table_Bag_2

### we compute the rate of correct predictions

rate_correct_predictions_Bag_2 = (Prediction_Table_Bag_2[1,1]+Prediction_Table_Bag_2[2,2])/length(target_2_test)
rate_correct_predictions_Bag_2

### we see the rate of correct predictions obtained by bagging is better
### than that of the tree

Confusion_Matrix_Bag_2 = confusionMatrix(Prediction_Table_Bag_2, mode = "everything")
print(Confusion_Matrix_Bag_2)
Precision_Bag_2 = Confusion_Matrix_Bag_2$byClass["Pos Pred Value"]
Precision_Bag_2
Specificity_Bag_2 = Confusion_Matrix_Bag_2$byClass["Specificity"]
Specificity_Bag_2

####################################################################################################

### next we try various random forests 

####################################################################################################
### random forest with 10 variables

Random_Forest_10 = randomForest(AHD_factor~Thal_factor+Ca_factor+Age+MaxHR+           
                                 Chol+Fbs+ChestPain_factor+Sex+Slope+ExAng+RestBP+             
                                 Oldpeak+RestECG, data = Data_2, subset = Train_2, mtry = 10, importance = TRUE, ntree=1000)

### the argument "mtry = 10" gives a random forest with 10 variables

Random_Forest_10
importance(Random_Forest_10)

### plot information

varImpPlot(Random_Forest_10)

### we see that the important predictors is "ChestPain_factor",
### predicted results

Random_Forest_10_Predict = predict(Random_Forest_10, Data_2_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_RF10 = table(Random_Forest_10_Predict, target_2_test)
Prediction_Table_RF10
Prediction_Table_RF10 = Prediction_Table_RF10[2:1,2:1]
Prediction_Table_RF10

### we compute the rate of correct predictions

rate_correct_predictions_RF10 = (Prediction_Table_RF10[1,1]+Prediction_Table_RF10[2,2])/length(target_2_test)
rate_correct_predictions_RF10


### we see the rate of correct predictions obtained by random forest with 10 variables is comparable with
### that of bagging

Confusion_Matrix_RF10 = confusionMatrix(Prediction_Table_RF10, mode = "everything")
print(Confusion_Matrix_RF10)
Precision_RF10 = Confusion_Matrix_RF10$byClass["Pos Pred Value"]
Precision_RF10
Specificity_RF10 = Confusion_Matrix_RF10$byClass["Specificity"]
Specificity_RF10

####################################################################################################
### random forest with 7 variables

Random_Forest_7 = randomForest(AHD_factor~Thal_factor+Ca_factor+Age+MaxHR+           
                                  Chol+Fbs+ChestPain_factor+Sex+Slope+ExAng+RestBP+             
                                  Oldpeak+RestECG, data = Data_2, subset = Train_2, mtry = 7, importance = TRUE, ntree=1000)

### the argument "mtry = 7" gives a random forest with 10 variables

Random_Forest_7
importance(Random_Forest_7)

### plot information

varImpPlot(Random_Forest_7)

### we see that the important predictors is "ChestPain_factor",
### predicted results

Random_Forest_7_Predict = predict(Random_Forest_7, Data_2_test, type="class")

### we compare the predicted results with the results directly obtained from data

Prediction_Table_RF7 = table(Random_Forest_7_Predict, target_2_test)
Prediction_Table_RF7
Prediction_Table_RF7 = Prediction_Table_RF7[2:1,2:1]
Prediction_Table_RF7

### we compute the rate of correct predictions

rate_correct_predictions_RF7 = (Prediction_Table_RF7[1,1]+Prediction_Table_RF7[2,2])/length(target_2_test)
rate_correct_predictions_RF7


### we see the rate of correct predictions obtained by random forest with 7 variables is comparable with
### that of  bagging

Confusion_Matrix_RF7 = confusionMatrix(Prediction_Table_RF7, mode = "everything")
print(Confusion_Matrix_RF7)
Precision_RF7 = Confusion_Matrix_RF7$byClass["Pos Pred Value"]
Precision_RF7
Specificity_RF7 = Confusion_Matrix_RF7$byClass["Specificity"]
Specificity_RF7

####################################################################################################
####################################################################################################
####################################################################################################
### we colect the data obtained in order to draw a conclusion

Method_2 = c("Tree","Prune_1","Prune_2","LM","LR","Bag","RF10","RF7")
RateCP_2 = c(rate_correct_predictions_tree,rate_correct_predictions_Prune_1, 
           rate_correct_predictions_Prune_2,rate_correct_predictions_LM,rate_correct_predictions_GLM,
           rate_correct_predictions_Bag_2,rate_correct_predictions_RF10,rate_correct_predictions_RF7)
Precision_2 = c(Precision_tree,Precision_Prune_1, 
              Precision_Prune_2,Precision_LM,Precision_GLM,
              Precision_Bag_2,Precision_RF10,Precision_RF7)
Specificity_2 = c(Specificity_tree,Specificity_Prune_1, 
                Specificity_Prune_2,Specificity_LM,Specificity_GLM,
                Specificity_Bag_2,Specificity_RF10,Specificity_RF7)
Conclusion_2 <- data.frame(Method_2,
                         RateCP_2,
                         Precision_2,
                         Specificity_2)
Conclusion_2

plot(Conclusion_2$RateCP_2)
plot(Conclusion_2$Precision_2)
plot(Conclusion_2$Specificity_2)

plot(Conclusion_2$Precision_2, Conclusion_2$Specificity_2)

####################################################################################################
####################################################################################################
####################################################################################################

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

### test MSE = 0.2462889

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

### test MSE = 0.4046162

plot(LMR_Test_Predict, target_4_test)
lines(lowess(LMR_Test_Predict, target_4_test, f = 2/3, iter = 3), col = "red")

#############################################################################################################

RegressionForrest = randomForest(Salary_log~.-Salary , data = Data_4, mtry = 10, importance = TRUE, ntree=1000) 

### print information

RegressionForrest
importance(RegressionForrest)

### plot information

varImpPlot(RegressionForrest)

################################################################################################################
### next we estimate the test error by training and testing
###Random Forest

RegressionForrest_Test = randomForest(Salary_log~.-Salary , data = Data_4, subset = Train_4, mtry = 10, importance = TRUE, ntree=1000) 

### print information

RegressionForrest_Test
importance(RegressionForrest_Test)

### plot information

varImpPlot(RegressionForrest_Test)

### predicted results

RegressionForrest_Test_Predict = predict(RegressionForrest_Test, Data_4_test)

Performance_Forrest_Test = mean((RegressionForrest_Test_Predict-target_4_test)^2)
Performance_Forrest_Test 

### test MSE = 0.1558750

plot(RegressionForrest_Test_Predict, target_4_test)
lines(lowess(RegressionForrest_Test_Predict, target_4_test, f = 2/3, iter = 3), col = "red")

####################################################################################################
####################################################################################################
####################################################################################################
### we colect the data obtained in order to draw a conclusion

Method_3 = c("Regression_Tree","LMRegression","Regression_Forrest")
MSE = c(Performance_Tree_Test,Performance_LMR_Test,Performance_Forrest_Test)
Conclusion_3 <- data.frame(Method_3,
                           MSE)
Conclusion_3

plot(Conclusion_3$MSE)


####################################################################################################
####################################################################################################
####################################################################################################
### Exercise
### Read Data

Data_5 = as.data.frame( read.csv("default.csv", na.strings = " "))

str(Data_5)
dim(Data_5)

####################################################################################################
### Explicit type conversion

Data_5$default_factor = as.factor(Data_5$default)
Data_5$student_factor = as.factor(Data_5$student)

str(Data_5)
