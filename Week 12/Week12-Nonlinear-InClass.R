# Get data from Table 13.1 on p. 515

data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2013%20Data%20Sets/CH13TA01.txt", header = FALSE)

names(data)[1]<-paste("y")
names(data)[2]<-paste("x")
data
attach(data)

plot(x, y)

# Fitting exponential regression with y on x
# nls function does nonlinear least squares
mod <- nls(y ~ a*exp(b * x), data = data, start = list(a = 57, b = -0.04))
summary(mod)

# Estimates are gamma_0 = 58.60654 and gamma_1 = -0.03959

# By default nls uses the Gauss-Newton algorithm
# Other choices are plinear and port

# Iteration steps can be shown by setting trace = TRUE
# Shows the residual sum of squares and estimates at each step:
mod <- nls(y~a*exp(b*x), data = data, start = list(a = 57, b = -0.04), trace=T)

# How about the starting values?
# We can try not providing any:
mod <- nls(y~a*exp(b*x), data = data, trace=T)

# :(

# In this example we follow the suggestions on p. 521 and fit

logy = log(y)

initial = lm(logy ~ x)
summary(initial)

# Then b0 = -0.037974 & a0 = exp(4.037159) = 56.66513 (and were rounded off)
# Using exact starting values as textbook:
mod <- nls(y~a*exp(b*x), data = data, start = list(a = 56.66513, b = -0.037974), trace=T)

# This matches approximately Table 13.3 on p. 524 (book used approximate values)

# If we need just the coefficients:
coef(mod)


# Predicted values
predict(mod)

# Plot fitted curve
# Create a grid:
h = seq(0, 70, len=100)
lines(h, coef(mod)[1]*exp(coef(mod)[2]*h))

# Compare to linear
abline(lm(y~x), col = "red")

# Alternatively
lines(data$x, predict(mod), col="blue")

# Diagnostics

# First linear model
plot(x, resid(lm(y~x)))
abline(h = 0)
# Linear assumption violated!

# Now nonlinear model:
e = resid(mod)
plot(predict(mod),e)
abline(h=0)

# Normality assumption looks ok:
qqnorm(e)
qqline(e)


# SE using Bootstrap
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- nls(formula, data=d, start = list(a = 56.66513, b = -0.037974))
  return(coef(fit)) 
} 

library(boot)
results <- boot(data= data, statistic=bs, 
 R=1000, formula= y~a*exp(b*x))

plot(results, index=1)
plot(results, index=2)

# SE
sd(results$t[,1])
sd(results$t[,2])

# Bootstrap CI:
# There are many versions of bootstrap confidence intervals
# Bias-corrected and accelerated (bca) are considered the best:
# 95% BCA for a:
boot.ci(results, type="bca", index =1)

# 95% BCA for b:
boot.ci(results, type="bca", index =2)

# Other choices for type are "percent", "student", "basic", and "normal"

# Or if you trust the asymptotic normality:
confint(mod)





# Exercise: p. 534
# Get data first:

data2 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2013%20Data%20Sets/CH13TA04.txt", header = FALSE)

names(data2)[1]<-paste("x1")
names(data2)[2]<-paste("x2")
names(data2)[3]<-paste("y")
attach(data2)

# If attach doesn't work try clearing the workspace
rm(list = ls())

# Note x1 is indicator of location with x1 = 1 for location A and x1 = 0 for B

# a) Fit Y = g0 + g1*X1 + g3*exp(g2*X2)
# For starting values use 
# g0 = largest y for location B
# g1 = average difference in y between locations A & B
# g2: solve in 0.971 = g0+g3*exp(20*g2) (obtained by plugging in 23rd obs)
# g3 = -0.5

# b) Obtain the graph on p. 534
# that is y vs. x2 with the two locations overlaid and estimated curves fitted
# c) Find SSE
# d) Obtain bootstrap SE and CI for g1







# Neural networks
# Adopted from 
# https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/

# Data of housing values in Boston suburbs
library(MASS)
data <- Boston
head(data)
dim(data)

# We want to predict the median value medv by the other variables
# First try good old regression model, but first splitting 
# the dataset into training and testing to avoid overfitting

# 75% training and 25% test 
# Select randomly 75% of the indeces to be training:
index <- sample(1:nrow(data),round(0.75*nrow(data)))
# Extract the training dataset from the main dataset
train <- data[index,]
# The rest are test data

test <- data[-index,]

# Predict with linear model using all predictors:
lm.fit <- lm(medv~., data=train)
summary(lm.fit)

# How well did we predict it?
# Try the results on the test data:
pr.lm <- predict(lm.fit,test)
cbind(test$medv, pr.lm)

# Mean squared prediction error based on the test dataset
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
MSE.lm

# Now neural network!
# For the neural network we will scale the data first
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

head(train_)

install.packages("neuralnet")
library(neuralnet)

# A fancy way to write 
medv~crim+zn+indus+chas+nox+rm + age + dis + rad + tax + ptratio + black + lstat
# because y ~ . is not accepted in neuralnet package

n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))





# We will use 2 hidden layers with configuration 13:5:3:1, 
# meaning the input layer has 13 inputs (the 13 x variables), 
# the two hidden layers have 5 and 3 neurons and the output layer has a single output

nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

# hidden defines a vector with number of neurons in each layer
# linear.output = TRUE corresponds to regression
# linear.output = FALSE corresponds to classification

# Fancy plot!
plot(nn)

# Prediction with neural network
pr.nn <- compute(nn,test_[,1:13])

# Taking only final output and rescaling it
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)

# MS prediction error for neural network fit
MSE.nn <- sum((test$medv - pr.nn_)^2)/nrow(test_)


cbind(test$medv, pr.lm, pr.nn_)



print(paste(MSE.lm,MSE.nn))
# Almost half of the regression!!!!

# Visally examining the fit
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)

points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)

legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
# Note red dots are closer to the 45 degree line than blue dots


# Exercise: Redo example on p. 543
data3 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC09.txt", header=F)
data3
names(data3)[2]<-paste("y")
names(data3)[5]<-paste("x1")
names(data3)[6]<-paste("x2")
names(data3)[9]<-paste("x3")
names(data3)[8]<-paste("x4")
attach(data3)
heart = cbind(log(y), x1, x2, x3, x4)
head(heart)

# Use 5 hidden nodes and one layer