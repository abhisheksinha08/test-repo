
#__________________________________
# Functional Misspecification
#__________________________________

#1. Missing some non-linearity
#2. Important Parameter is missing
library("lmtest")

#Ramsey's RESET Test
resettest(output1, power=2:3, type=c("fitted")) 
#Checking for 2nd order and 3 order polynomial terms
#Null Hypothesis - No mis-specification

#Problem of Functional Misspecification - Unbiased Estimator

#Solution - Data Transformation. E.g. a ~ b with Resettest output for power=2 is p-value<0.05, then change it to a ~ b + b^2


#__________________________________
# Heteroskedasticity
#__________________________________

# Variance is not constant for error temp in each sample.

# Problem - Inefficient but not unbiased.

#Diagnosis

# 1.Studentized Breush-pagan test
bptest(output, a ~ , data = df)
# Null Hypothesis : Homoskedasticity

# 2.Variance Co-variance Matrix
library("sandwich")
vcovHC(output, type="")
# If diagnols are same, no heteroskedasticity

# 3. Plot - Check 1st plot
par(mfrow=c(2,2))
plot(output)
# If plot 1 (TOP LEFT) shows any pattern, heteroskedasticity is present.

# Solution: Box-Cox Transformation
distBCMod <- caret::BoxCoxTrans(cars$dist) #Generate Model
dist_new=predict(distBCMod, cars$dist) #Predict New Variable
lmMod_bc <- lm(dist_new ~ speed) #Regression with new variable



#__________________________________
# Multicollinearity
#__________________________________

round(cor(data),2) 
#This will return cor data for all variables. Looking at high values and 
#removing one variabe at a time can help with rectifying this issue.

# Variance Inflation Factor VIF
library(car)
vif(model1)
#if value >= 5 then multicollinearity exists.


#__________________________________
# Autocorrelation
#__________________________________

# Autocorelation in Error Term. Eg. e2 = e1 + rho * (random_part)

# Detection

#Durbin-Watson Test
dwtest(lm(y~x))
#Null Hypothesis: No auto-correlation.

library(timeSeries)
acf(y)
pacf(y)
# If acf is geometric and pacf is cutting the blue line, shows Autocorrelation.
# AR(n) where n can be found by looking at the number of lines that are cut by blue line on both sides.

#Dickey-Fuller test
library(urca)
ur.df(y, type="trend", selectlags = "Fixed")
#Unit Root Test. Null Hypothesis: Unit Root or Random Walk. If p-value<0.05 then AutoCorr.

#Durbin-Watson Test
library(lmtest)
dwtest(model)
# DW near 2 show no autocorrelation. Null Hypothesis: NO Autocorrelation.

#For AR(1) model
lm(diff(y) ~ diff(x))

#For AR(2), we can use diff(x) and diff(x, lag=2) and use the same fourmula.

#Ljung-Box White Noise Test
Box.test(model, lag=1)
#Null Hypothesis: White Noise (No Autocorrelation)

