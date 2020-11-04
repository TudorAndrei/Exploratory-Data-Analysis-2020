### clear screen

cat("\f")

### Default Options 

options(scipen=999)
options(stringsAsFactors = FALSE)

############################################################################################
############################################################################################
############################################################################################
### Read Data

Data = as.data.frame( read.csv("lab3/oil.csv"))

str(Data)
dim(Data)
sum(is.na(Data$percentage.yield))

cor(Data)

library(corrplot)
corrplot(cor(Data), method = "ellipse")

############################################################################################

Linear_Model_1 = lm(percentage.yield~., data = Data)
Linear_Model_1

#yield = -6.8208+0.2272*gravity+0.5537*pressure-0.1495*distil+0.1547*endpoint

summary(Linear_Model_1)

############################################################################################

Linear_Model_2 = lm(percentage.yield~.-1, data = Data)
Linear_Model_2

#yield = 0.1872*gravity+0.3754*pressure-0.1674*distil+0.1547*endpoint

summary(Linear_Model_2)

############################################################################################

Linear_Model_3 = lm(percentage.yield~.-vapour.pressure-1, data = Data)
Linear_Model_3

#yield = 0.2741*gravity-0.1793*distil+0.1572*endpoint

summary(Linear_Model_3)

#this is a good model

############################################################################################

Linear_Model_4 = lm(percentage.yield~.-gravity-vapour.pressure-1, data = Data)
Linear_Model_4

#yield = -0.1538*distil+0.1699*endpoint

summary(Linear_Model_4)

#this is a good model


############################################################################################

Linear_Model_5 = lm(percentage.yield~.-gravity-vapour.pressure-ten.percent.distillation.point-1, data = Data)
Linear_Model_5

#yield = 0.06125*endpoint

summary(Linear_Model_5)

#this is not really a good model

############################################################################################

anova(Linear_Model_1, Linear_Model_2, Linear_Model_3,Linear_Model_4, Linear_Model_5)

############################################################################################

Data$composed_feature_1 = 0.2741*Data$gravity-0.1793*Data$ten.percent.distillation.point+0.1572*Data$fraction.end.point
Data$composed_feature_2 = -0.1538*Data$ten.percent.distillation.point+0.1699*Data$fraction.end.point


plot(Data$composed_feature_1,Data$percentage.yield)
lines(lowess(Data$composed_feature_1, Data$percentage.yield, f = 2/3, iter = 3), col = "red")

plot(Data$composed_feature_2,Data$percentage.yield)
lines(lowess(Data$composed_feature_2, Data$percentage.yield, f = 2/3, iter = 3), col = "red")

Oil_Model = lm(percentage.yield~gravity+ten.percent.distillation.point+fraction.end.point+0, data = Data)
Oil_Model

summary(Oil_Model)

#RSS = 147.63

sqrt (147.63/29)

#residual standard error
#RSE = 2.256

standard_residuals = rstandard(Oil_Model)
plot(Data$percentage.yield, standard_residuals, xlab="yield", ylab="Standardized residuals")
hist(standard_residuals)

hist(Data$gravity)
hist(Data$ten.percent.distillation.point)
hist(Data$fraction.end.point)

x = data.frame(gravity = 40, ten.percent.distillation.point = 250, fraction.end.point = 400)
y = predict(Oil_Model, x)
y

confidence_interval_95 = predict(Oil_Model, x,  interval = c("confidence"), level = 0.95)
confidence_interval_95

# 95% confidence interval (for the mean of y)

prediction_interval_95 = predict(Oil_Model, x,  interval = c("prediction"), level = 0.95)
prediction_interval_95

# 95% prediction interval (for y)

prediction_interval_99 = predict(Oil_Model, x,  interval = c("prediction"), level = 0.99)
prediction_interval_99

# 99% prediction interval (for y)

#for details read
#https://towardsdatascience.com/how-confidence-and-prediction-intervals-work-4592019576d8

###########################################################################################
###########################################################################################
###########################################################################################
#Cobb, S., and P. Douglas, 1928, "A Theory of Production," 
#American Economic Review 18, pp. 139-165.


Data_2 = as.data.frame( read.csv("cobbdouglas.csv"))

str(Data_2)
dim(Data_2)

##########################################################################################

Data_2$Capital_log = log (Data_2$Relative.Capital.Stock..1899.100)
Data_2$Labour_log = log (Data_2$Relative.Number.of.Workers..1899.100)
Data_2$Production_log = log (Data_2$Index.of.Manufactures)

#########################################################################################

CD_Model = lm(Production_log~Labour_log+Capital_log, data = Data_2)
CD_Model    

summary(CD_Model)

Data_2$CD_feature = Data_2$Relative.Number.of.Workers..1899.100^0.8073 * Data_2$Relative.Capital.Stock..1899.100^0.2331

plot(Data_2$CD_feature,Data_2$Index.of.Manufactures)
lines(lowess(Data_2$CD_feature,Data_2$Index.of.Manufactures, f = 2/3, iter = 3), col = "red")

###########################################################################################
###########################################################################################
###########################################################################################
### Read Data

Data_3 = as.data.frame( read.csv("./lab3/Hitters.csv"))

str(Data_3)
dim(Data_3)
sum(is.na(Data_3$Salary))
Data_4 = na.omit(Data_3)
str(Data_4)
dim(Data_4)
sum(is.na(Data_4$Salary))
############################################################################################
log_years = log(Data_4$Years)
log_salary = log(Data_4$Salary)

Linear_Model = lm(log_salary~.-1-Salary-CRBI-CHmRun, data = Data_4)
Linear_Model
summary(Linear_Model)
y = predict(Linear_Model, Data_4)
plot(y, log_salary)
