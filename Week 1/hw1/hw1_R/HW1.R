### HUDM 5126 Linear Models and Regression Analysis HW1
### 1.19

################# Data Preparation ################
library(dplyr)
library(ggplot2)
library(car)
setwd("/Users/yifei/Documents/Teachers College/Linear Models and Regression/Week 1")
getwd()

mydata<-read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR19.txt")
names(mydata)=c("Y","X")
mydata

cor(mydata$X,mydata$Y)

#a. Obtain the least squares estimates of b0 and b1, and state the estimated regression function.
linear.model<-lm(Y~X,data = mydata)
summary(linear.model)
#Y.hat=2.11405+0.03883*X

#b. Plot the estimated regression function and the data. Does the estimated regression function appear to fit the data well?
# Scatterplot
plot(mydata$X,mydata$Y,xlab = "ACT Test Scores",ylab = "Student's GPA")
abline(lm(mydata$Y~mydata$X))

#c. Obtain a point estimate of the mean freshman GPA for students with ACT test score X=30.
new.data=data.frame(X=30)
predict(linear.model,newdata=new.data)

#d. What is the point estimate of the change in the mean response when the entrance test score increases by one point?
# For every one point increase in the entrance test scores, the grade point average (GPA) increases by 0.03883.

