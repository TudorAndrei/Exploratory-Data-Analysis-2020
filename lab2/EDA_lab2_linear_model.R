### clear screen

cat("\f")

### Default Options 

options(scipen=999)
options(stringsAsFactors = FALSE)

############################################################################################
#Hubble, E. (1929) A relation between distance and radial velocity among extra-galactic nebulae.
#Proceedings of the National Academy of Science, 15, 168 - 173.

Data_1 = as.data.frame( read.csv("lab2/Hubble.csv"))
str(Data_1)

plot( Data_1$Velocity.km.sec, Data_1$Distance.MegaParsecs, xlab = "Velocity (km/sec)", ylab = "Distance (MPc)")


cor(Data_1$Distance.MegaParsecs, Data_1$Velocity.km.sec) # check correlation

library(corrplot)
corrplot(cor(Data_1), method = "ellipse")

############################################################################################

Linear_Model_1 = lm(Distance.MegaParsecs~Velocity.km.sec, data = Data_1)
Linear_Model_1

# dist =   0.399098 + 0.001373*Velocity 

summary(Linear_Model_1)
standard_residuals_1 = rstandard(Linear_Model_1)
plot(Data_1$Velocity.km.sec, standard_residuals_1, xlab="Velocity", ylab="Standardized residuals")
hist(standard_residuals_1)

############################################################################################
#Hubble's model = Hubble's Law.
#D=TV where D=distance, T=time, V=velocity 
#The mean speed theorem. This formula was found by Nicole Oresme and used by Galileo Galilei for defining speed

Linear_Model_2 = lm(Distance.MegaParsecs~Velocity.km.sec-1, data = Data_1)
Linear_Model_2

# dist =   0.001922*Velocity 

summary(Linear_Model_2)

standard_residuals_2 = rstandard(Linear_Model_2)
plot(Data_1$Velocity.km.sec, standard_residuals_2, xlab="Velocity", ylab="Standardized residuals")
hist(standard_residuals_2)

#H = 1/T ~ 500 km/MPc*sec = Hubble Constant
#T = 0.001922 MPc*sec/km:
#1 MPc = 3.086 * 10^19 km

T = 0.001922 * 3.086 * (10^19)
T

# T = 59312920000000000 sec
# 1 year = 60*60*24*365.25 sec

T = T/(60*60*24*365.25)
T

#T = 1 879 513 017 years ~ 2*10^9 years
#T = estimated age of Universe
#linear association between distance and velocity ===> Big Bang theory. 

#############################################################################################
Data_1$Velocity_2=Data_1$Velocity.km.sec^2

Linear_Model_3 = lm(Distance.MegaParsecs~Velocity.km.sec+Velocity_2-1, data = Data_1)
Linear_Model_3

# dist =   0.00191033078*Velocity + 0.00000001479*Velocity^2

summary(Linear_Model_3)

#############################################################################################
Data_1$Velocity_3=Data_1$Velocity.km.sec^3

Linear_Model_4 = lm(Distance.MegaParsecs~Velocity.km.sec+Velocity_2+Velocity_3-1, data = Data_1)
Linear_Model_4

# dist =   0.000601951985*Velocity + 0.000005484959*Velocity^2 - 0.000000004329*Velocity^3

summary(Linear_Model_4)

#############################################################################################
Data_1$Velocity_4=Data_1$Velocity.km.sec^4

Linear_Model_5 = lm(Distance.MegaParsecs~Velocity.km.sec+Velocity_2+Velocity_3+Velocity_4-1, data = Data_1)
Linear_Model_5

# dist =   0.000702138313855*Velocity + 0.000008847102612*Velocity^2 - 0.000000014020330*Velocity^4 
#        + 0.000000000006207*Velocity^4

summary(Linear_Model_5)

#############################################################################################
Data_1$Velocity_5=Data_1$Velocity.km.sec^5
Data_1$Velocity_6=Data_1$Velocity.km.sec^6
Data_1$Velocity_7=Data_1$Velocity.km.sec^7
Data_1$Velocity_8=Data_1$Velocity.km.sec^8

Linear_Model_6 = lm(Distance.MegaParsecs~Velocity.km.sec+Velocity_2+Velocity_3+Velocity_4+
                      Velocity_5+Velocity_6+Velocity_7+Velocity_8-1, 
                    data = Data_1)
Linear_Model_6
summary(Linear_Model_6)

##############################################################################################
##############################################################################################
##############################################################################################
### Read Data

Data_2 = cars  # cars is a default data set in R. The data were recorded in the 1920s!

plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)")

str(Data_2)
dim(Data_2)

cor(cars$speed, cars$dist) # check correlation

############################################################################################

Linear_Model_7 = lm(dist~speed, data = Data_2)
Linear_Model_7

# dist = -17.579 + 3.932speed


summary(Linear_Model_7)
plot(Data_2$speed,Data_2$dist)

############################################################################################

Data_2$speed2 = Data_2$speed*Data_2$speed

Linear_Model_8= lm(dist~speed+speed2, data = Data_2)
Linear_Model_8

# dist =   2.47014 + 0.91329speed+0.09996speed*speed 

summary(Linear_Model_8)
plot(Data_2$speed2,Data_2$dist)

##############################################################################################
Data_2$distlog = log(Data_2$dist)
Data_2$speedlog = log(Data_2$speed)

Linear_Model_9 = lm(distlog~speedlog, data = Data_2)
Linear_Model_9

# log(dist) =   -0.7297 + 1.6024 log(speed) 

summary(Linear_Model_9)
plot(Data_2$speedlog,Data_2$distlog)

##############################################################################################
Data_2$speed
Data_2$dist

Data_2[2,] # outlier ???
Data_2[4,] # outlier ???

Data_3 = Data_2[-c(2, 4),]

Data_3$speed
Data_3$dist

Linear_Model_10 = lm(distlog~speedlog, data = Data_3)
Linear_Model_10

# log(dist) =   -1.458 + 1.860 log(speed) 

summary(Linear_Model_10)
standard_residuals_10 = rstandard(Linear_Model_10)
plot(Data_3$speedlog, standard_residuals_10, xlab="log (speed)", ylab="Standardized residuals")
hist(standard_residuals_10)

plot(Data_3$speedlog,Data_3$distlog)
lines(lowess(Data_3$speedlog, Data_3$distlog, f = 2/3, iter = 3), col = "red")


#############################################################################################

Linear_Model_11 = lm(distlog~.-dist-distlog, data = Data_3)
Linear_Model_11

# log(dist) =   -4.425408 -0.373501*speed +  0.005608 speed*speed + 4.573494 log(speed) 

summary(Linear_Model_11)

#############################################################################################
#############################################################################################
#############################################################################################
### Read New Data

Data_4 = as.data.frame( read.csv("lab2/Site.csv"))
str(Data_4)

plot(Data_4$Square.Feet, Data_4$Annual.Sales, xlab = "Square feet (/1000)", ylab = "Annual sales ($ millions)")


cor(Data_4$Square.Feet, Data_4$Annual.Sales) # check correlation

############################################################################################

Linear_Model_12 = lm(Annual.Sales~Square.Feet, data = Data_4)
Linear_Model_12

# Annual.Sales = 0.9645 + 1.6699*Square.Feet


summary(Linear_Model_12)

standard_residuals_12 = rstandard(Linear_Model_12)
plot(Data_4$Square.Feet, standard_residuals_12, xlab="Square feet", ylab="Standardized residuals")
hist(standard_residuals_12)

# we make predictions based on this model

x = data.frame(Square.Feet = 5.0)
predict(Linear_Model_12, x)

# we predict that a store of 5 000 square feets wil make anula sales of 9 313 785 $ 

x = data.frame(Square.Feet = c(1.0,2.0,3.0,4.0,5.0))
predict(Linear_Model_12, x)

############################################################################################

