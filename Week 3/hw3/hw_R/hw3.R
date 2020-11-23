# Linear Models and Regression Analysis HW3

################ Data Preparation #################
setwd("/Users/yifei/Documents/Teachers College/Linear Models and Regression/Week 3")
getwd()

library(ggplot2)
library(lmtest)
library(hrbrthemes)
library(extrafont)

# 1
# This question is adapted from Q3.3. Refer to Grade Point Average data in Problem 1.19.
# a. Prepare a boxplot of the ACT scores (X variable). Are there any noteworthy features on the plot?
mydata<-read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR19.txt",header = FALSE)
names(mydata)=c("Y","X")
mydata
# Descriptive values for the variable X
summary(mydata$X)
# Boxplot of the ACT Scores
boxplot(mydata$X,main="Boxplot for Students' ACT Scores",ylab="Student's ACT scores")
# The boxplot looks "normal". It seems symmetric, which most of data clustered around the middle but with no extreme
# outliers. It looks to be from a random sample.

# b. Prepare a histogram of the residuals. What information does the plot provide?
# Fitting the Simple Linear Regression Model
reg<-lm(mydata$Y~mydata$X)
summary(reg)
# Obtain the residuals
e<-resid(reg)
mydata<-cbind(mydata,e)
# Histogram of the residuals
p1<-ggplot(mydata,aes(x=e))+geom_histogram(color="darkgreen",fill="lightgreen")+
  labs(x="Residuals",y="Frequency",title = "Histogram of the Residuals")+
  theme(plot.title = element_text(color = "black",size=11,face = "bold",hjust = 0.5))
p1
# According to the histogram of the residuals, the residuals are left-skewed, which means the mean of residuals is smaller
# than the median of residuals.

# c. Plot the residuals against the fitted value Yhat. What are your findings about departures from the regression assumption?
# Find the fitted values first
y.hat<-predict(reg)
mydata<-cbind(mydata,y.hat)
plot(mydata$y.hat,mydata$e,main = "Residuals against the Fitted Values",xlab="Fitted Values",ylab="Residuals")
abline(h=0,lty=2)

# The residuals against the fitted value plot is relatively shapeless without clear patterns in the data and be generally symmetrically distributed around the 0 line, except for those outliers.
# Therefore, the constant variance assumption is hold or we can say there is no heteroskedasticity.

# d. Prepare a normal probability plot of the residuals. Test the reasonableness of the normality assumption with the KS test using
# alpha=0.05. What do you conclude?
# A normal probability of the residuals
# We compute the standardlized residuals with the standard function first.
stdres<-rstandard(reg)
qqnorm(stdres,xlab="Normal Scores", ylab="Standardlized Residuals", main = "Normal Probability Plot of Residuals")
qqline(stdres)
# Test the reasonableness of the normality assumption: Kolmogorov-Smirnov test for normality
ks.test(rstandard(reg),"pnorm")
# Since p-value=0.8188, which is greater than 0.05, we cannot reject the null hypothesis. Therefore, normality assumption holds.

# e. Conduct the BP test to determine if the variance varies with the level of X. Use alpha=0.01. State your conclusion. Does your conclusion support your
# preliminary findings in part c)?
# Breusch-Pagan test for constant variance
bptest(reg)
# p-value=0.5877 is not less than 0.01. Do not reject H0. Conclude error variance constant. This conclusion support my preliminary findings in part c).

# Q 3.10 
mydata2<-read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%203%20Data%20Sets/CH03PR10.txt",header = FALSE)
names(mydata2)=c("Fitted Values","Semistudentized Residuals")
mydata2
# a. plot the semistudentized residuals against the fitted values. What does the plot suggest?
plot(mydata2$`Fitted Values`,mydata2$`Semistudentized Residuals`,main = "Semistudentized Residuals against Fitted Values",
     xlab="Fitted Values",ylab="Semistudentized Residuals")
abline(h=0)
abline(h=-2,lty=2)
abline(h=2,lty=2)
# Based on the plot, there is an outlier outside +-2 standard deviation in the observations. The outlier is at the largest predicted value, which is Yhat=15.6 and it can be very influencial beacuse it is far away from the 0 line,
# which could change the slope coeffieicent significantly.

# b. How many semistudentized residuals are outside +-1 standard deviation? Approximately how many would you expect to see if the normal error model is appropriate?
plot(mydata2$`Fitted Values`,mydata2$`Semistudentized Residuals`,main = "Semistudentized Residuals against Fitted Values",
     xlab="Fitted Values",ylab="Semistudentized Residuals")
abline(h=0)
abline(h=-1,lty=2)
abline(h=1,lty=2)
# 4 semistudentized residuals are outside +-1 standard deviation. If the normal error model is appropriate, there should be 4 points fall outside.

# 3
# Read the data frame from the file HW3.RData
load("HW3.RData")
mydata3<-data
# a. Prepare a scatterplot of X vs. Y overlaid with the estimated regression line.
p2<-ggplot(mydata3,aes(x=x,y=y))+geom_point(color="black")+
  xlab("X")+ylab("Y")+ggtitle("Scatterplot of X vs.Y")+theme_ipsum(plot_title_size = 12)+
  geom_smooth(method = lm,color="red",se=FALSE)
p2

# b. Calculate the correlation coefficient between X and Y and comment on the strength of the linear association,
# using the standard cutoff point of +-0.7.
cor(mydata3$x,mydata3$y)
# The correlation coefficient between X and Y is 0.664. Since it is smaller than standard cutoff point of 0.7, we can conclude that the variables are not strongly positively linearly related.

# c. Create a new variable X'=sqrt(X)
x2<-sqrt(mydata3$x)
mydata3<-cbind(mydata3,x2)

# d. Prepare a scatterplot of X' vs. Y overlaid with the estimated regression line.
p3<-ggplot(mydata3,aes(x=x2,y=y))+geom_point(color="black")+
  xlab("X'")+ylab("Y")+ggtitle("Scatterplot of X' vs.Y")+theme_ipsum(plot_title_size = 12)+
  geom_smooth(method = lm,color="red",se=FALSE)
p3

# e. Caculate the correlation coefficient between X' and Y and comment on the strength of the linear association.
cor(mydata3$x2,mydata3$y)
# The correlation coefficient between X' and Y is 0.717. Since it is larger than standard cutoff point of 0.7, we conclude that the variables are strongly positively linearly related.

# f. Obtain the estimated linear regression function for the transformed data.
reg2<-lm(mydata3$y~mydata3$x2)
summary(reg2)
# y.hat=1.7636+9.3497*x

# g. Plot the residuals vs. fitted values.
y.hat2<-predict(reg2)
e2<-resid(reg2)
mydata3<-cbind(mydata3,y.hat2,e2)
p4<-ggplot(mydata3,aes(x=y.hat2,y=e2))+geom_point(color="black")+
  xlab("fitted values")+ylab("Residuals")+ggtitle("Residuals vs. Fitted Values")+theme_ipsum(plot_title_size = 12)+
  geom_hline(yintercept = 0,linetype="dashed",color="red",size=1.5)
p4

# h. What does the plot from part g) show?
# The plot from part g) shows that after the transformation of data, we observe a constant variance for the residuals.
