# Overview example of material covered so far:

# We will use a built-in dataset called "faithful"
?faithful

# attach the data frame
attach(faithful)
plot(waiting, eruptions)
eruption.lm = lm(eruptions ~ waiting) 
abline(eruption.lm)

summary(eruption.lm)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.874016   0.160143  -11.70   <2e-16 ***
# waiting      0.075628   0.002219   34.09   <2e-16 ***

# Residual standard error: 0.4965 on 270 degrees of freedom
# Multiple R-squared:  0.8115,    Adjusted R-squared:  0.8108 
# F-statistic:  1162 on 1 and 270 DF,  p-value: < 2.2e-16

# a) Report the regression line equation:
# y.hat = -1.874 + 0.0756*x

# b) Find the correlation coefficient between waiting time and eruption time
cor(eruptions, waiting)
# [1] 0.9008112
# r = 0.90008112 > 0.7, that is, strong positive linear relationship

# c) Predict eruption duration when waiting time is 80 minutes

# Then we create a new data frame that set the waiting time value. 
newdata = data.frame(waiting=80) 

# We now apply the predict function 
predict(eruption.lm, newdata, interval="confidence")

#      fit      lwr      upr
# 1 4.17622 4.104848 4.247592
# y.hat = 4.17622

# d) Find 95% CI for average eruption duration when waiting = 80
# (4.104848, 4.247592)

# e) Find 95% PI for eruption duration when waiting = 70
newdata = data.frame(waiting=c(70,80)) 
predict(eruption.lm, newdata, interval="prediction")
#      fit      lwr      upr
# 1 3.41994 2.440608 4.399273
# 2 4.17622 3.196089 5.156351

# (2.440608, 4.399273)

# f) Find 94% PI for eruption when waiting = 80

predict(eruption.lm, newdata, level=0.94, interval="prediction")
#      fit      lwr      upr
# 1 3.41994 2.480433 4.359448
# 2 4.17622 3.235946 5.116494

# (3.235946, 5.116494)

# g) Find 95% CI for beta1
# To obtain CI for the coefficients use
confint(eruption.lm, level=0.95)
#                  2.5 %      97.5 %
#(Intercept) -2.18930436 -1.55872761
#waiting      0.07126011  0.07999579

# (0.07126011, 0.07999579)

# Or specifically for the slope
confint(eruption.lm, 'waiting', level=0.95)

# h) Test H0: beta1 = 0 vs Ha: beta1 not 0

# p-value = 0, therefore reject H0, that is, beta1 is not zero
# We may also say beta1 is significant (regression model is significant)

# F-test
# i) What is the value of the F-test statistic

# 1162

# R-squared:
# j) What percent of eruption time variability is explained by waiting time
# 81.15%

# You can also request ANOVA table in regression setup
anova(eruption.lm)


## Regression diagnostics
# Get data from Table 3.1 on p. 105

data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%203%20Data%20Sets/CH03TA01.txt", header = FALSE)
names(data)[1]<-paste("y")
names(data)[2]<-paste("x")
data

# Check for unusual x values:
stem(data$x)

# Fitting the SLR model
reg = lm(data$y ~ data$x)
summary(reg)

# Plot the data and the regression line
plot(data$x, data$y)
abline(reg)

# Obtain the residuals
(e = resid(reg))

# Residual plot vs. x
plot(data$x, e)
abline(h = 0)

# Conclusion: There is non-linear relationship
# That is, linearity assumption is violated.

# Predicted values and standardized residuals
y.hat = predict(reg)

s = sqrt(sum((data$y-y.hat)^2)/(6))

# Semi-studentized residuals:
e/s
plot(data$x, e/s)

# Studentized residuals:
plot(data$x, rstandard(reg))
abline(h = 0)
abline(h = -2, lty =2)
abline(h = 2, lty =2)
# Conclusion:
# No outliers: Since standardized (studentized) residuals are between -2 and 2.
# Linear relationship looks violated (we see parabola)
# Constant variance looks ok

# Time series plot of residuals
plot(e,type="b")
ts.plot(restandard(reg))

# No systematic patterns
# Therefore independence assumption looks ok

# Testing normality
qqnorm(rstandard(reg))
qqline(rstandard(reg))

# No visible violation of normality assumption

# Installing a library
install.packages("lmtest")
library(lmtest)

# Durbin-Watson test for autocorrelation
dwtest(reg, alternative= "two.sided")
# Since p-value = 0.4615 is not less than 0.05
# we do not reject Ho
# That is, independence assumption holds

# Breusch-Pagan test for constant variance
bptest(reg)
# 0.7961 is not less than 0.05
# Do not reject Ho
# That is, constant variance assumption holds

# Kolmogorov-Smirnov test for normality
ks.test(rstandard(reg), "pnorm")
# Do not reject Ho
# That is, normality assumption holds


# Exercise: Use data on skin cancer rates and states latitudes
# Read in data
# Perform regression diagnostics

# Read data from Week3-skincancer.txt
Data <- read.table(file.choose(), header=T)
attach(Data)
plot(Lat, Mort)
text(Lat, Mort, State, cex=0.4, pos=4)

reg = lm(Mort~Lat)
summary(reg)
abline(reg)

predict(reg)
resid(reg)

plot(Lat, resid(reg))
text(Lat, resid(reg), State, cex=0.4, pos=4)
plot(Lat, rstandard(reg))
text(Lat, rstandard(reg), State, cex=0.4, pos=4)
abline(h = 0)
abline(h = -2, lty =2)
abline(h = 2, lty =2)

# Perform DW test
dwtest(reg, alternative= "two.sided")

# Since p-value = 0.6433 is not less than 0.05
# we do not reject Ho
# That is, independence assumption holds

# Visual
ts.plot(resid(reg))


# Perform Breusch-Pagan test for constant variance
bptest(reg)


# Report the p-value
# What is the conclusion?
# 0.08392 is not less than 0.05
# Do not reject Ho
# That is, constant variance assumption holds

# Perform Kolmogorov-Smirnov test for normality
ks.test(rstandard(reg), "pnorm")

# Visual
qqnorm(rstandard(reg))