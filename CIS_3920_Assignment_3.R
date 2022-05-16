#Sangie Cheung
#Assignment #3: Linear Regression

install.packages("ISLR") #Installed ISLR package
library(ISLR)   #Loaded ISLR package
?College #Information about "College" dataset
str(College) #Structure of "College" dataset

#Question 1
model = lm(Grad.Rate~S.F.Ratio,data=College) #Creates model between graduation rate and student-faculty ratio
summary(model) #Outputs model information
confint(model) #Outputs confidence interval for model
plot(College$S.F.Ratio,College$Grad.Rate, xlab = "Student-Faculty Ratio", 
     ylab = "Graduation Rate", main = "Student-Faculty Ratio vs Graduation Rate") #Shows plot between Grad.Rate and S.F.Ratio
abline(model, col="blue") #Inserts line of best fit in blue

quadmodel = lm(Grad.Rate~S.F.Ratio+I(S.F.Ratio^2), data = College) #Creates model with S.F.Ratio as a quadratic term
summary(quadmodel) #Outputs quadmodel information

#Question 2
multimodel = lm(Grad.Rate~Private+Top25perc+Outstate+Room.Board, data=College) #Creates multiple linear regression model for graduation rates on predictors "Private", "Top25perc", "Outstate", and "Room.Board"
summary(multimodel) #Outputs multimodel information
confint(multimodel) #Outputs confidence interval for multimodel

#Creates new observation and predicts the graduation rate with lower and upper bounds
Private = c("No")
Top25perc = c(55)
Outstate = c(25000)
Room.Board = c(4000)
new_obs = data.frame(Private,Top25perc,Outstate,Room.Board)
new_obs
predict(multimodel,new_obs,interval = "prediction")

#Outputs 4 diagnostic plots for multimodel
par(mfrow=c(2,2)) #Creates a 2 by 2 panel plot
plot(multimodel) #Shows plot of multimodel

#Question 3
modelall = lm(Grad.Rate~.,data=College) #Creates model with all the variables
summary(modelall) #Outputs modelall information

install.packages("leaps") #Installs leaps package
library(leaps)

#Applies foward selection to model with all variables 
model_fwd = regsubsets(Grad.Rate~., data=College, nvmax=NULL, method="forward")
summary(model_fwd) #Outputs model_fwd information
model_fwd_summary=summary(model_fwd) #Stores summary output
which.max(model_fwd_summary$adjr2) #Displays best subset by adjusted R-sqaured
summary(model_fwd)$which[13,] #Outputs the selected predictors for the subset

#Creates regression model for the best subset of predictors by adjusted R-sqaured
bestmodel = lm(Grad.Rate~Private+Apps+Top10perc+Top25perc+F.Undergrad+P.Undergrad
               +Outstate+Room.Board+Personal+PhD+Terminal+perc.alumni+Expend, data=College)
summary(bestmodel) #Outputs bestmodel information

par(mfrow=c(1,1)) #Resets to 1 by 1 panel plot
plot(model_fwd, scale="adjr2", main="Forward Selection: AdjR2") #Plots the ranking of subsets in the feature selection process.

