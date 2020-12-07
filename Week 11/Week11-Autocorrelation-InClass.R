# Generate data similar to Table 12.1 on p. 482

# Sample size
n = 100

# Lag 1 autocorrelation
rho = 0.8

# Error terms
e = 0:n
u = 0:n

# Initializing the error term as in textbook
e[1] = 3
u[1] = 0
set.seed(344)
u[2:(n+1)] = rnorm(n)

# Generate the correlated errors according to AR(1) model
for (i in 2:(n+1)) e[i] = rho*e[i-1] + u[i]
e
ts.plot(e, type="b")

# Similarly, autocorrelated series can be generated with the arima.sim function
ts.plot(arima.sim(model=list(ar = 0.8), n = 100), type = "b")

# Regression coefficients for the simulation:
b0 = 2
b1 = 0.5
x = 0:n
y = b0 + b1*x + e
y
plot(x,y)
summary(lm(y~x))

# The easiest way to estimate the autocorrelation is with the built-in function acf
acf(e)$acf
# It provides estimates at all lags
# It tends to underestimate the rho

# Durbin-Watson test
# install.packages("lmtest")
library(lmtest)
dwtest(y ~ x)
# As expected, we reject Ho: No autocorrelation
# Positive autocorrelation

# Note residuals mimic the autocorrelation of the simulated errors
r = resid(lm(y~x))
acf(r)$acf

# Manual computation of DW statistic (just for fun)
numerator = 0
for (i in 2:(n+1)) numerator = numerator + (r[i]-r[i-1])^2
(D = numerator/sum(r^2))

# Seems to work on simulated data, so let's try a real data example


# Example p. 488
data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2012%20Data%20Sets/CH12TA02.txt", header = FALSE)
data
names(data)[1]<-paste("Y")
names(data)[2]<-paste("X")
attach(data)

reg = lm(Y~X)
summary(reg)
r = resid(reg)

# Time series plot of residuals
ts.plot(r,type="b")
abline(h = 0)
# Note the "sine" type of curve indicating a potential autocorrelation problem
# A good tsplot should have consecutive up-down trend.

dwtest(Y~X)
# Conclusion: there is strong evidence of autocorrelation!


# Fixing the issue by Cochrane-Orcutt method

# Estimate rho 

# If you do not follow the rho formula from the book
# Instead, easier:
acf(r, lag.max = 1)$acf
# Estimated rho = 0.626

# If you want to follow book's method:
# Manual estimation of rho
numerator = 0
n = nrow(data)
for (i in 2:n) numerator = numerator + r[i]*r[i-1]
rho2 = numerator/sum(r[1:(n-1)]^2)
rho2
# Same as on p. 493 of the textbook!

# Compute transformed variables:
n = nrow(data)
Yprime = 1:n
for (i in 2:n) Yprime[i] = Y[i]- rho2*Y[i-1]
Yprime = Yprime[-1]

Xprime = 1:n
for (i in 2:n) Xprime[i] = X[i]- rho2*X[i-1]
Xprime = Xprime[-1]

Regprime = lm(Yprime ~ Xprime)
summary(Regprime)

# Back to original model:
b0 = Regprime$coef[1]/(1-rho2)
b1 = Regprime$coef[2]

# SE(b1) same as SE(b1.prime)
summary(Regprime)$coef[4]
# Compare to OLS
summary(reg)$coef[4]
# The corrected SE is double the OLS one!!

# Notice no more autocorrelation!
dwtest(Yprime ~ Xprime)

# Alternatively, use package orcutt

install.packages("orcutt")
library(orcutt)

cochrane.orcutt(reg, convergence = 0)

# Depends on R version to use convergence = 

# Alternative method: use the function arima to fit a time series model:
(fit2 <- arima(Y, xreg = X, order=c(1,0,0)))


# Forecasting

# Y.hat = -1.068524 + 0.1737583 *X
# Obtain last residual

(e20 = Y[20]-(b0 + b1*X[20]))

# X21 = 175.3
# Obtain Y.hat

(Y.hat21 = b0 + b1*175.3)

# Adjust with correlated residual
(F21 = Y.hat21 + rho2*e20) # Forecasted Y21
detach(data)


# Exercise (12.9 on p. 502)
# A staff analyst for a manufacturer of microcomputer components has compiled monthly data for the past 16 months 
# on the value of industry production of processing that use components (X, in million dollars) and the value of 
# the firm's components used (Y. in thousand dollars). The analyst believes that a simple linear regression relation
# is appropriate but anticipates positive autocorrelation. 

data2 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2012%20Data%20Sets/CH12PR09.txt", header = FALSE)
names(data2)[1]<-paste("Y")
names(data2)[2]<-paste("X")
attach(data2)

# a) Fit a simple linear regression model by ordinary least squares and obtain the residuals. 
# Also obtain s(b0) and s(b1). 
reg2 <- lm(Y ~ X)
summary(reg2)
# SE(b0)
summary(reg2)$coefficients[3]
# [1] 7.174639
# SE(b1)
summary(reg2)$coefficients[4]
# [1] 3.519672

# b) Plot the residuals against time and explain whether you find any evidence of positive autocorrelation.
r2 = resid(reg2)
ts.plot(r2, type = "b")
abline(h = 0)
# Does find some evidences of autocrrelation.

# c) Conduct a formal test for positive autocorrelation
dwtest(Y ~ X)
# DW = 0.8566 and p-value is 0.002502. We reject H0. There is positive autocorrelation.

# d) Obtain a point estimate of the autocorrelation parameter using the manual formula or cochrane.orcutt
# Manual estimation of rho
numerator = 0
n = nrow(data2)
for (i in 2:n) numerator = numerator + r2[i]*r2[i-1]
rho3 = numerator/sum(r2[1:(n-1)]^2)
rho3
# rho is 0.5784105

# cochrane.orcutt
cochrane.orcutt(reg2, convergence = 0)
# rho 0.578411

# e) Use one iteration to obtain the estimates beta prime. Also obtain s{b.prime}
# Compute transformed variables:
n = nrow(data2)
Yprime = 1:n
for (i in 2:n) Yprime[i] = Y[i]- rho3*Y[i-1]
Yprime = Yprime[-1]

Xprime = 1:n
for (i in 2:n) Xprime[i] = X[i]- rho3*X[i-1]
Xprime = Xprime[-1]

Regprime = lm(Yprime ~ Xprime)
summary(Regprime)

# SE(b0)
(summary(Regprime)$coef[3]/(1-rho3))
# 8.908873
# Compare to OLS
summary(reg2)$coefficients[3]
# [1] 7.174639

# f) Test whether any positive autocorrelation remains
dwtest(Yprime ~ Xprime)
# DW = 1.4747, p-value = 0.1053, cannot reject H0. Conclude no autocorrelation.

# g) Restate the estimated regression function obtained in part (b) in terms of the original variables. Also obtain s{b0} and s{b1}. 
# Original OLS
# Y.hat = -7.739 + 53.953*X
# Y.hat = -1.646929 + 50.93322*X

# Back to original model:
b0 = Regprime$coef[1]/(1-rho3)
# b0 =  -1.646929 
b1 = Regprime$coef[2]
# b1 = 50.93322

# SE(b1)
summary(Regprime)$coef[4]
# [1] 4.349034
# Compare to OLS
summary(reg2)$coefficients[4]
# [1] 3.519672
# The correct SE is larger than the OLS one.



