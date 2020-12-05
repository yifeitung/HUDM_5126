
# Exercise:
# Use the dataset from Q11.23 on p. 477
data1 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2011%20Data%20Sets/CH11PR23.txt", header = FALSE)
names(data1)[1]<-paste("Y")
names(data1)[2]<-paste("X1")
names(data1)[3]<-paste("X2")
names(data1)[4]<-paste("X3")
names(data1)[5]<-paste("X4")

# a) Fit the traditional OLS and state the equation
summary(lm(Y ~ ., data = data1))

# b) Standardize all data with scale() function. 
# Obtain ridge regression coefficients and R^2 for lambda = 0, 0.002, 0.06, and 0.1
data2=scale(data1)

library(MASS)
lm.ridge(Y ~ ., data = as.data.frame(data2), lambda = 0.1)
lm.ridge(Y ~ ., data = as.data.frame(data2), lambda = 0.002)
lm.ridge(Y ~ ., data = as.data.frame(data2), lambda = 0)

# c) Obtain the optimal lambda with the glmnet package using cv.glmnet() and $lamnda.min
cvridge <- cv.glmnet(x = as.matrix(data2[,-1]), y = data2[,1], alpha = 0)

(opt_lambda = cvridge$lambda.min)
