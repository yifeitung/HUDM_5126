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

dwtest(Y~X)
# Conclusion: there is strong evidence of autocorrelation!


# Fixing the issue with Cochrane-Orcutt method

# Estimate rho 
# Here we do NOT follow the rho formula from the book!
# Instead, easier:

acf(r, lag.max = 1)$acf

# Estimated rho = 0.626

# If you want to follow book's method:
# Manual estimation of rho
numerator = 0
n = nrow(data)
for (i in 2:n) numerator = numerator + r[i]*r[i-1]
rho2 = numerator/sum(r[2:n]^2)
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

# Notice no more autocorrelation!
dwtest(Yprime ~ Xprime)

# Back to original model:
b0 = Regprime$coef[1]/(1-rho2)
b1 = Regprime$coef[2]

# SE(b1) same as SE(b1.prime)
summary(Regprime)$coef[4]
# Compare to OLS
summary(reg)$coef[4]


# Alternatively, use package orcutt

install.packages("orcutt")
library(orcutt)

cochrane.orcutt(reg, convergence = 0)

# Depends on R version to use convergence = 


# Alternative method: use the function arima to fit a time series model:
(fit2 <- arima(Y, xreg = X, order=c(1,0,0)))


# Forecasting

# Y.hat = -1.072261 + 0.1737822 *X
# Obtain last residual

(e20 = Y[20]-(b0 + b1*X[20]))


# X21 = 175.3
# Obtain Y.hat

(Y.hat21 = b0 + b1*175.3)

# Adjust with correlated residual
(F21 = Y.hat21 + rho2*e20)


# Exercise (12.9 on p. 502)
# A staff analyst for a manufacturer of microcomputer components has compiled monthly data for the past 16 months 
# on the value of industry production of processing that use components (X, in million dollars) and the value of 
# the firm's components used (Y. in thousand dollars). The analyst believes that a simple linear regression relation
# is appropriate but anticipates positive autocorrelation. 

# a) Fit a simple linear regression model by ordinary least squares and obtain the residuals. 
# Also obtain s(b0) and s(b1). 
# b) Plot the residuals against time and explain whether you find any evidence of positive autocorrelation.
# c) Conduct a formal test for positive autocorrelation
# d) Obtain a point estimate of the autocorrelation parameter
# e) Use one iteration to obtain the estimates beta prime. Also obtain s{b.prime}
# f) Test whether any positive autocorrelation remains 
# g) Restate the estimated regression function obtained in part (b) in terms of the original variables. Also obtain s{b0} and s{b1}. 


data2 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2012%20Data%20Sets/CH12PR09.txt", header = FALSE)

names(data2)[1]<-paste("Y")
names(data2)[2]<-paste("X")
attach(data2)

# a)
reg2 = lm(Y~X)
summary(reg2)
r2 = resid(reg2)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -7.739      7.175  -1.079    0.299    
# X             53.953      3.520  15.329 3.82e-10 ***


# b)
ts.plot(r2, type="b")
# Sine curve indicates presence of autocorrelation

# c)
dwtest(Y~X)

#        Durbin-Watson test
# 
# data:  Y ~ X
# DW = 0.8566, p-value = 0.002502

# Conclusion: Since p-value < 0.05 we reject H0
# That is, there is positive autocorrelation

# d) + e)

cochrane.orcutt(reg2, convergence = 0)

# Or if you use the formula :
numerator = 0
n = nrow(data2)
for (i in 2:n) numerator = numerator + r2[i]*r2[i-1]
rho3 = numerator/sum(r2[1:(n-1)]^2)
rho3

Yprime = 1:n
for (i in 2:n) Yprime[i] = Y[i]- rho3*Y[i-1]
Yprime = Yprime[-1]

Xprime = 1:n
for (i in 2:n) Xprime[i] = X[i]- rho3*X[i-1]
Xprime = Xprime[-1]

Regprime2 = lm(Yprime ~ Xprime)
summary(Regprime2)


# f)

dwtest(Yprime ~ Xprime)


# Conclusion: no more autocorrelation since p-value > 0.05

# g)
b0 = Regprime2$coef[1]/(1-rho3)
b1 = Regprime2$coef[2]

# From package with 0 iteration
# Y = -1.64692 + 50.93322X

# SE(b1)
summary(Regprime2)$coef[2,2]

# SE(b0)
summary(Regprime2)$coef[1,2]/(1-rho3)

# Alternative:
(fit3 <- arima(Y, xreg = X, order=c(1,0,0)))


