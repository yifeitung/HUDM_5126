# Simple Linear Regression Inference

# Example from Lecture 1
x = 1:5 # Advertising budget in $100s
y = c(1, 1, 2, 2, 4) # Sales in $1000s

reg = lm(y~x)
summary(reg)

# Task: Test whether or not there is a linear association between 
# advertising expenditures and sales using alpha = 0.05

# State the hypotheses:
# Ho: beta1 = 0 vs. Ha: beta1 not equal 0

# Note that summary provides the SE, test statistics and p-values
summary(reg)$coefficients

# To obtain just the SE of b1:
(SEb1 = summary(reg)$coefficients[2,2])

# How do we obtain the critical value from the t-table?
qt(0.975,3)

# Equivalently,
qt(0.025,df=3,lower.tail = FALSE)

# Note we had to provide the left area, corresponding to right area alpha/2
# df = n - 2 = 5 - 2 = 3

# State the decision rule:
# Reject H0 if |t_test| > crit.value

# Conclusion: Since 3.66 > 3.18 we reject Ho and conclude beta1 is significant, that is not equal 0
# Report p-value
summary(reg)$coefficients[2,4]

# We could have reached the same conclusion using the p-value:
# Since 0.035 < 0.05 we reject Ho

# Exercise: repeat the above for alpha = 0.01
qt(0.01/2,df=3,lower.tail = FALSE)

# Conclusion: Since |3.656| is not greater than 5.84
# we don't reject Ho


# Task: Test Ha: beta1 > 0
# p-value is
0.0354/2
# [1] 0.0177
# Or 
summary(reg)$coefficients[2,4]/2

# Conclusion: Since 0.0177 < 0.05 we reject Ho and conclude beta1 > 0

# What about critical value?
qt(0.05,df=3,lower.tail = FALSE)
# Note there is no division by 2.

# TasK: Obtain 95% CI for beta1 and interpret it

# Manually from the output:

lower = reg$coeff[2] - qt(0.975,3)*SEb1
lower
upper = reg$coeff[2] + qt(0.975,3)*SEb1
upper

# The 95% CI for beta1 is (0.09, 1.31)

# Interpretation: we are 95% confident that for each extra $100 in advertising
# sales increase by as little as $90 to as much as $1310

# Using built-in funcion
confint(reg)

# Change CI
confint(reg,level = 0.99)

# Or just for beta 1
confint(reg)[2,]


# Correlation (manual)
x.bar = mean(x)
y.bar = mean(y)
SSxy = sum((x-x.bar)*(y-y.bar))
SSxx = sum((x-x.bar)^2)
SSyy = sum((y-y.bar)^2)

(r = SSxy/sqrt(SSxx*SSyy))
# Note: Strong positive linear relationship

# Easier:
cor(x,y)

# Test the significance of the correlation 
# and CI for population correlation coefficient rho:
cor.test(x,y)
# Note it is equivalent to the regression slope test (Same p-value and test statistic)
# 95% CI for rho is (0.11, 0.99)

# R-squared
r^2

# Or from the regression output:
summary(reg)$r.squared

# Predict y when x = 4
-0.1 + 0.7*4


newdata =data.frame(x=4)
# 95% CI
predict(reg, newdata, interval = "conf", level=0.95)


# 95% PI
predict(reg, newdata, interval = "predict", level=0.95)
# Entire population

# ANOVA
anova(reg)

## Fire damage example

data = read.table(file.choose(), header=T)
attach(data)
reg = lm(Damage ~ Distance)
summary(reg)

# Test if damage increases when distance increases

# Ho: beta1 = 0 vs. Ha: beta1 > 0
summary(reg)$coef[2,4]/2
# p-value = 6.239e-09
# Conclusion: Since p-value < 0.05 we reject Ho
# That is, fire damage increases as distance increases

# Confidence intervals for the coefficients
confint(reg)

plot(Distance, Damage)
abline(reg)

newdata = data.frame(Distance = 3.5)
predict(reg, newdata, interval = "confidence", level = 0.95)

predict(reg, newdata, interval = "predict", level = 0.95)

