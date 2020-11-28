# Get data from Week7-bluegills.txt 
data <- read.table(file.choose(), header = T)

# Look at data and attach
head(data)
attach(data)

# Scatterplot
plot(age, length)

# Fit quadratic model:
age2 = age^2

reg2 = lm(length ~ age + age2)
summary(reg2)

# Plot the fitted parabola
agevalues = seq(1,6, len = 100)
predictedlengths <- predict(reg2, list(age=agevalues, age2=agevalues^2))
lines(agevalues, predictedlengths, col = "red")

# Compare to linear model
reg1 = lm(length ~ age)
summary(reg1)
abline(reg1)

# Residual plot
plot(age, resid(reg1))
abline(h=0)

# Which one is better?
anova(reg1, reg2)
# Conclusion: quadratic term is significant!

# Or look at p-value for age2 in reg2 which shows quadratic term is significant


# Report R^2 when using linear; R^2 when using quadratic and partial R^2 when x^2 is added:

# R-squared (quadratic) = 0.8011
# R-squared(linear) = 0.7349
# R-squared(quadratic|linear) = SSRegr(quadratic|linear)/SSE(linear) = 2972.1/11892.8 = 0.2499


# Alternative methods for fitting:
# Evaluating expressions directly inside the formula:
reg2b <- lm(length ~ age + I(age^2))
summary(reg2b)

# Fitting polynomial of a specific degree:
reg2c <- lm(length ~ poly(age, degree = 2, raw = T))
summary(reg2c)
# Note the use of raw = T, otherwise R fits orthogonal polynomials

# Do we need a cubic term?
reg3 <- lm(length ~ poly(age,3, raw = T))
summary(reg3)
# no


# Exercise: p. 300
data2 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%208%20Data%20Sets/CH08TA01.txt", header = FALSE)
data2

names(data2)[1]<-paste("Y")
names(data2)[2]<-paste("X1")
names(data2)[3]<-paste("X2")
attach(data2)

x1 = (X1-mean(X1))/0.4
x2 = (X2-mean(X2))/10
x12 = x1^2
x22 = x2^2

# Check correlations:
cor(X1, X1^2)
cor(X2, X2^2)

cor(x1, x12)
cor(x2, x22)

# Interaction term
x1x2 = x1*x2

model1 = lm(Y ~ x1+x2+x12+x22+x1x2)
summary(model1)

model2 = lm(Y ~ x1+x2)
anova(model2,model1)

# Since partial F test p-value =  0.5527
# we can't reject H0
# That is, no need of quadratic terms


# Exercise on p. 316
data3 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%208%20Data%20Sets/CH08TA02.txt", header = FALSE)
data3

names(data3)[1]<-paste("Y")
# Y = months elapsed for adoption of innovation
names(data3)[2]<-paste("X1")
# X1 = size of firm (millions)
names(data3)[3]<-paste("X2")
# X2 = 1 for stock firm, X2 = 0 for mutual fund
attach(data3)

# Regular linear multiple regression model:
regdummy = lm(Y ~ X1 + X2)
summary(regdummy)

# Fitted function:
# Yhat = 33.87 - 0.1*X1 + 8.06*X2

# Since p-value for X2 (dummy variable) is 3.74e-05 < 0.05
# there is a significant difference between stock and mutual fund firms
# The gap between the two types of companies is 8.055 months (stock higher than mutual funds)

# Plot:
# install.packages("ggplot2")
library(ggplot2)
plot <- ggplot(data = data3, aes(x = X1, y = Y, colour = factor(X2)))
plot + stat_smooth(method=lm, se = F) + geom_point()

# Write down the two separate regression equations:
# Mutual fund: y = 33.87 -0.1*x1
# Stock: y = 33.87 - 0.1*x1 + 8.06 = 41.93 - 0.1*x1

# p. 327
# Let's try interaction between dummy var and numerical var.
# (that is, nonparallel lines)

regdummy.Interaction = lm(Y ~ X1*X2)
summary(regdummy.Interaction)

# Call:
# lm(formula = Y ~ X1 * X2)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -5.7144 -1.7064 -0.4557  1.9311  6.3259 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 33.8383695  2.4406498  13.864 2.47e-10 ***
# X1          -0.1015306  0.0130525  -7.779 7.97e-07 ***
# X2           8.1312501  3.6540517   2.225   0.0408 *  
# X1:X2       -0.0004171  0.0183312  -0.023   0.9821    
# ---
# Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1
# 
# Residual standard error: 3.32 on 16 degrees of freedom
# Multiple R-squared:  0.8951,    Adjusted R-squared:  0.8754 
# F-statistic: 45.49 on 3 and 16 DF,  p-value: 4.675e-08

# Conclusion: Since interaction p-value = 0.9821 > 0.05
# the interaction effect is not significant
# That is, model with parallel lines is enough


# Challenge question: Test Ho: beta2 = 0 within the last model and also within the model without interaction.
# Are the conclusions the same? Does the conclusion have the same implication?

# Conclusions are the same: x2 is a significant predictor
# However, implications are different.
# Rule: when using interaction term always include the individual terms as well


# Exercise:
# Use the data from here: http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC03.txt
# V1 = id
# V2 = Market share (%)
# V3 = Price ($)
# V4 = index of the amount of advertising
# V5 = 1 if discount in price and 0 o/w
# V6 = 1 if package promotion and 0 o/w
# V7 = month
# V8 = year

data4 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC03.txt", header = FALSE)

# We want a regression model to predict Market share by Price (X1), Discount (X2), and Promotion (X3)

names(data4)[2]<-paste("Y")
names(data4)[3]<-paste("X1")
names(data4)[5]<-paste("X2")
names(data4)[6]<-paste("X3")

attach(data4)
data4

# Task 1
# Predict the market share based on price and one or both dummy variables. 
# Fit the following models:
# 1) Price only
reg1 <- lm(Y~X1)
# 2) Price and Discount
reg2 <- lm(Y~X1+X2)
# 3) Price, Discount and Promotion
reg3 <- lm(Y~X1+X2+X3)
# 4) Price, Discount, Promotion and all interactions between them
reg4 <- lm(Y~X1*X2+X1*X3)
# Choose which model is the best fit by adjusted R-square criterion?
# reg1: 0.007168
# reg2: 0.6376
# reg3: 0.679
# reg4: 0.664

# Task 2:
# In the best model obtain the residual plot and check the assumptions 
# The best model is the the model with Price, Discount and Promotion
# Obtained fitted values
y_hat <- predict(reg3)
e <- residuals(reg3)
mydata <-cbind(data4, y_hat, e)
mydata
# Residuals against Y_hat
library(latex2exp)
library(ggplot2)

g1 <- ggplot(mydata, aes(x = y_hat, y = e))+geom_point(color = "black") + xlab("Fitted")+
  ylab("Residual")+ggtitle(TeX("(a) Residual Plot against $\\widehat{Y}$"))+
  geom_hline(yintercept = 0, linetype = "dashed", color="red")
g1

# Task 3:
# Recall:
# X2 = 1 if discount in price and 0 o/w
# X3 = 1 if package promotion and 0 o/w
# Write down the 4 separate regression equations for each combination
# of discount and promotion

# That is, we need the regression equation for each of these:
# No discount, no promotion: y=3.18527-0.35269*X1
# No discount with promotion:y=3.18527-0.35269*X1+0.11803=3.3033-0.35269*X1
# Discount but no promotion: y=3.18527-0.35269*X1+0.39914=3.58441-0.35269*X1
# Discount and promotion:y=3.18527-0.35269*X1+0.39914+0.11803=3.70244-0.35269*X1
