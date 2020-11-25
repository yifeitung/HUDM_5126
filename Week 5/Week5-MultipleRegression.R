# Get data 
data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06FI05.txt", header = FALSE)

# Changing the variables to have more meaningful names
names(data)[1]<-paste("targpop")
names(data)[2]<-paste("dispoinc")
names(data)[3]<-paste("sales")
data
attach(data)

# Some graphs to check what is going on:
stem(dispoinc)
stem(targpop)

par(mfrow = c(2, 2)) # Splits the graph area in 2x2 grid
hist(targpop)
hist(dispoinc)
plot(targpop, sales)
plot(dispoinc, sales)
# Income might have an outlier. Both predictors have strong relationship with sales
par(mfrow = c(1, 1))

# Creating the observations y and the design matrix X
y = sales
(X = cbind(rep(1, nrow(data)), targpop, dispoinc))

# 3D plots
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(targpop, dispoinc, y)

m1 <- lm(y ~ targpop + dispoinc , data = data)
install.packages("rockchalk")
library(rockchalk)
plotPlane(m1, plotx1 = "targpop", plotx2 = "dispoinc")


# Matrix products that occur in the formulas

t(X )%*% X
# Note, for example, element 2,1 is the sum of x_i1:
sum(targpop)

t(X)%*%y

solve(t(X)%*%X) # Note it matches with equation 6.73 on p. 239

# Finally, the estimated slope and intercept
(b = solve(t(X) %*% X) %*% t(X) %*% y)


# Interpretation of b1:
# For every 1000 people increase in target population, the sales 
# increase by $1,454.60, keeping income fixed.

# Interpretation of b2:
# For every $1000 increase in disposable income, the sales 
# increase by $9,365.50, keeping target population fixed.


# Check and compare to built-in lm funcion
reg = lm(sales ~ targpop + dispoinc)
summary(reg)

# Fitted values y-hat
(y.hat = X %*% b)
# Alternatively with built-in function:
predict(reg)

# Residuals
e = y - y.hat
e

# Compare to built-in function
resid(reg)

# Residual plot for checking assumptions
plot(y.hat, e)
abline(h = 0)
# Seems assumptions are ok

# You can also check against specific predictors
plot(targpop, e)
plot(dispoinc, e)

# Checking for interaction effect
plot(targpop*dispoinc, e)
# If you see straight line pattern include the interaction

# Checking normality
par(mfrow = c(2, 2))
boxplot(e)
hist(e)
qqnorm(e)
qqline(e)
# Possible normality violation
par(mfrow = c(1, 1))

# ANOVA
t(y) %*% y
SSE = t(y)%*%y -t(b)%*%t(X)%*%y
SSE
# Check with built-in function (R calls this SSResiduals)
anova(reg)

# Total SS
n = nrow(data)
J = matrix(rep(1,n^2), n, n)
SSTO = t(y)%*%y - t(y)%*%J%*%y/n
SSTO

# Alternatively:
sum(anova(reg)[,2])


MSE = SSE/(n-ncol(data))
MSE

# To get overall F-test and R-squared

summary(reg)
# F-test statistic = 99.1
# p-value = 1.921e-10
# Reject H0: beta1 = beta2 = 0

# Test the significance of each predictor separately:
# Ho: beta1 = 0
# p-value =  2e-06
# Conclusion: Target population is a significant predictor

# Ho: beta2 = 0
# p-value = 0.033
# Conclusion: Disposable income is a significant predictor

# Confidence intervals for betas
confint(reg)
# Can also be obtained manually with the t distribution

# Variances and covariances of the coefficients b:
(s.sq.b = drop(MSE)*solve(t(X)%*%X))

# St. errors of the coefficients
sqrt(diag(s.sq.b))
# Note they match the lm output SE

# Manual 95% CI for beta1:
n = nrow(data)
coef(reg)[2] - qt(0.975, n - 3)*sqrt(diag(s.sq.b))[2]
coef(reg)[2] + qt(0.975, n - 3)*sqrt(diag(s.sq.b))[2]

# R-sq = 0.9167
# That is, 91.67% of sales variation is explained by the regression model

# Alternative formula
1 -SSE/SSTO


# Prediction
x.new = c(1, 65.4, 17.6)
s.sq.new = t(x.new)%*%s.sq.b%*%x.new
s.sq.new
# Manual 95% CI for mean response:
coef(reg)%*%x.new - qt(0.975, 18)*sqrt(s.sq.new)
coef(reg)%*%x.new + qt(0.975, 18)*sqrt(s.sq.new)

# Or with built-in function
predict(reg, data.frame(targpop=65.4,dispoinc=17.6), interval = "conf")


# 95% PI
predict(reg, data.frame(targpop=65.4,dispoinc=17.6), interval = "predict")

# Exercises
# Read in data

Data <- read.table(file.choose(), header=T)
attach(Data)

# Obtain scatterplot matrix and correlation matrix:
cor(Data[,c(2,3,5)])
pairs(Data[,c(2,3,5)])

# Exercise
# Use skin cancer data and fit a model with both latitude and longitude

# a) Obtain the estimated regression equation
reg <- lm(Mort~Lat + Long)
summary(reg)

# Predicted mortality rate = 400.68 - 5.93*Lat - 0.15*Long

# b) Interpret both slopes

# For every 1 degree increase in latitude, the mortality rate decrease by 5.93 people per 100,00 
# keeping longitude fixed.

# For every 1 degree increase in longitude, the mortality rate decrease by 0.15 people per 100,00
# keeping latitude fixed.

# c) Obtain the fitted values and residuals
# Fitted values
predict(reg)

# 1        2        3        4        5        6        7        8        9       10       11 
# 192.1990 179.6364 179.5307 160.7440 153.9009 142.0901 158.3005 158.0805 222.5864 192.7123 120.0347 
# 12       13       14       15       16       17       18       19       20       21       22 
# 150.3165 149.6143 136.6380 157.8929 164.0243 202.1706 122.4826 158.1538 139.8644 130.2918 113.9982 
# 23       24       25       26       27       28       29       30       31       32       33 
# 192.9452 158.8461 105.7209 139.9537 152.2143 130.4191 151.3301 177.5509 134.5771 178.4718 104.2220 
# 34       35       36       37       38       39       40       41       42       43       44 
# 150.1129 175.8760 122.0469 147.2877 142.2808 188.3342 120.3086 174.5238 199.4820 150.0555 129.0863 
# 45       46       47       48       49 
# 166.7568 101.2156 158.7094 123.5251 129.8842

# Residuals
e <- residuals(reg)

# d) Perform the overall F-test and state the conclusion
# F-test = 49.79, p-value = 3.101e-12
# That is, at least one predictor is useful for predicting mortality.

# e) Obtain R2 and interpret its value
# R2 = 0.684

# f) Obtain 96% CI's for the slopes

confint(reg, level = 0.96)

# g) Estimate the expected mortality rate in states with  latitude = 40 and longitude = 70
newdata <- data.frame(Lat = 40, Long = 70)

# We now apply the the predict function
predict(reg, newdata)

# h) Obtain a 95% CI for the mean estimated mortality rate in part g)

predict(reg, newdata, interval = "confidence")

# i) Obtain 91% prediction limits for new observations with X1 = 40 and X2 = 70

predict(reg, newdata, interval = "prediction", level = 0.91)


