# HUDM 5126 Linear Models and Regression Analysis HW2

################ Data Preparation ##############

library(ggplot2)
library(hrbrthemes)
setwd("/Users/yifei/Documents/Teachers College/Linear Models and Regression/Week 2")
getwd()

mydata<-read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR19.txt")
names(mydata)=c("Y","X")
mydata

# 2.4
## a
linearmodel<-lm(Y~X,data = mydata)
summary(linearmodel)

# Obtain a 99% Confidence Interval for beta1.
confint(linearmodel,level = 0.99)
confint(linearmodel,level = 0.99)[2,] #just for beta1

# The 99% confidence interval for beta1 is (0.0054,0.0723).

# Interpretation: We are 99% confident that for each extra 1 point increase in ACT scores,
# student's Grade Point Average increase by as little as 0.0054 to as much as 0.0723.
# It does not include zero. The director of admissions should be interested in whether the 
# confidence interval includes zero because if the confidence interval includes zero, then
# we should conclude that increase ACT scores have no contributions on GPA increase.

## b
# Using the t-statistics, test whether or not there is a linear association exists between 
# student's ACT scores (X) and GPA at the end of the freshman year (Y), using alpha=0.01.

# State the Hypothesis
# H0: beta1=0 vs. Ha: beta1 not equal 0
# Obtain critical value from the t-table
# df=n-2=120-2=118
qt(0.005,df=118,lower.tail = FALSE)
# critical value=2.618
# To obtain the T-statistics for X
t_statistic=summary(linearmodel)$coefficients[2,3]
t_statistic
# Test statistic for X is 3.040
# State the decision rule:
# Reject H0 if |t-statistic|>critical value
# Conclusion: Since 3.040>2.618, we reject H0 and conclude that beta1 is significant and not equal to 0.

## c
# Report p-value
report_p=summary(linearmodel)$coefficients[2,4]
report_p
# The p-value is 0.00292.
# Since 0.00292<0.01. It supports the conclusion reached in part (b).

##################### Extra Exercises #######################
# Confidence intervals for the mean values
predict(linearmodel,mydata,interval = "confidence",level = 0.99)

# Prediction Intervals for individual predicted values
predictions<-predict(linearmodel,mydata,interval="predict",level = 0.99)

# Use ggplot2 package to draw linear model and confidence interval
# Basic scatter plot.
g1<-ggplot(mydata,aes(x=X,y=Y))+geom_point(color="black")+
  theme_ipsum()+xlab("Student's ACT Scores")+ylab("Student's Grade Point Average")
# With Linear trend and Confidence Intervals
g2<-g1+geom_smooth(method=lm,color="red",se=TRUE,level=.99,fill="#69b3a2")

# Add lower bound prediction interval
all_data<-cbind(mydata,predictions)
g3<-g2+geom_line(aes(y=all_data$lwr),color="red",linetype="dashed")

# Add upper bound prediction interval
g4<-g3+geom_line(aes(y=all_data$upr),color="red",linetype="dashed")+
  ggtitle("Confidence Intervals for Mean Values and Prediction Intervals for New Values")+theme_ipsum(plot_title_size = 12)
g4

# 2.44
# See pdf file.
# Find critical value alpha=.10
# df=n-2=103-2=101
qt(0.05,df=101,lower.tail = FALSE)
# t-statistic=17.73321
# t(.95,101)=1.66
# Since 17.73321>1.66, we reject H0.
qnorm(1-0.1/2)
# z(0.95)=1.645

# 2.61
# See pdf file.

