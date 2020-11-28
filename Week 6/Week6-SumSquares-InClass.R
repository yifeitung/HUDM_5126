# Get data 
data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%207%20Data%20Sets/CH07TA01.txt", header = FALSE)

data

# Changing the variables to have more meaningful names
names(data)[1]<-paste("X1")
names(data)[2]<-paste("X2")
names(data)[3]<-paste("X3")
names(data)[4]<-paste("Y")

attach(data)

# Total sum of squares:
(SST = sum((Y - mean(Y))^2))
# [1] 495.3895

# Using only X1 as a predictor:
reg1 = lm(Y ~ X1)
summary(reg1)

# Call:
# lm(formula = Y ~ X1)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6.1195 -2.1904  0.6735  1.9383  3.8523 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.4961     3.3192  -0.451    0.658    
# X1            0.8572     0.1288   6.656 3.02e-06 ***
# ---
# Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1
# 
# Residual standard error: 2.82 on 18 degrees of freedom
# Multiple R-squared:  0.7111,    Adjusted R-squared:  0.695 
# F-statistic:  44.3 on 1 and 18 DF,  p-value: 3.024e-06

# Side note: to obtain the covariance matrix of the coefficients:
vcov(reg1)

anova(reg1)

# Analysis of Variance Table
# 
# Response: Y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# X1         1 352.27  352.27  44.305 3.024e-06 ***
# Residuals 18 143.12    7.95                      
# ---
# Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1  1

# SSR(X1) = 352.27
# SSE(X1) = 143.12

# Alternative for SST:
sum(anova(reg1)[,2])

# Using only X2 as a predictor:
reg2 = lm(Y ~ X2)
anova(reg2)
# Analysis of Variance Table
# 
# Response: Y
#           Df Sum Sq Mean Sq F value  Pr(>F)    
# X2         1 381.97  381.97  60.617 3.6e-07 ***
# Residuals 18 113.42    6.30                    
# ---
# Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1 

# Therefore, SSR(X2) = 381.97


# Using both X1 and X2 as predictors:
reg12 = lm(Y ~ X1 + X2)
anova(reg12)

# Analysis of Variance Table
# 
# Response: Y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# X1         1 352.27  352.27 54.4661 1.075e-06 ***
# X2         1  33.17   33.17  5.1284    0.0369 *  
# Residuals 17 109.95    6.47                      
# ---
# Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1
 
# Note: R anova function provides sequential sums of squares, aka Type I
# Therefore,
# SSR(X2|X1) = 33.17
# SSR(X1, X2) = 352.27 + 33.17 = 385.44
# SSE(X1, X2)=109.95

# Type II SS can be obtained with the car package:
install.packages("car")
library(car)
Anova(reg12, type="II")
# Therefore,
# SSR(X1|X2) = 3.473

# Regression with all 3 predictors:
reg123 = lm(Y ~ X1+X2+X3)
anova(reg123)

# Analysis of Variance Table
# 
# Response: Y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# X1         1 352.27  352.27 57.2768 1.131e-06 ***
# X2         1  33.17   33.17  5.3931   0.03373 *  
# X3         1  11.55   11.55  1.8773   0.18956    
# Residuals 16  98.40    6.15                      
# ---

# SSR(X3|X1, X2) = 11.55
# SSR(X1, X2, X3) = 396.99

# Partial F-statistic for Ho: b3 = 0 is F* = 1.8773 
# Corresponding p-value = 0.18956. Therefore X3 is not significant if X1 and X2 are already in the model

# Partial F-test with built-in command
anova(reg1,reg123)

# Analysis of Variance Table
# 
# Model 1: Y ~ X1
# Model 2: Y ~ X1 + X2 + X3
#   Res.Df     RSS Df Sum of Sq      F  Pr(>F)  
# 1     18 143.120                              
# 2     16  98.405  2    44.715 3.6352 0.04995 *
# ---

# SSR(X2, X3|X1) = 44.715 = 396.99 - 352.27
# SSE(X1) = 143.12
# SSE(X1, X2, X3) = 98.405

# Tests

anova(reg123)

# Analysis of Variance Table
# 
# Response: Y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# X1         1 352.27  352.27 57.2768 1.131e-06 ***
# X2         1  33.17   33.17  5.3931   0.03373 *  
# X3         1  11.55   11.55  1.8773   0.18956    
# Residuals 16  98.40    6.15  

# F = 1.8773 is for testing
# Full: Y = b0 + b1*X1 + b2*X2 + b3*X3
# Reduced Y = b0 + b1*X1 + b2*X2
# 1.8773 = t^2 for b3 from 

summary(reg123)

# Call:
# lm(formula = Y ~ X1 + X2 + X3)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.7263 -1.6111  0.3923  1.4656  4.1277 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  117.085     99.782   1.173    0.258
# X1             4.334      3.016   1.437    0.170
# X2            -2.857      2.582  -1.106    0.285
# X3            -2.186      1.595  -1.370    0.190
# 
# Residual standard error: 2.48 on 16 degrees of freedom
# Multiple R-squared:  0.8014,    Adjusted R-squared:  0.7641 
# F-statistic: 21.52 on 3 and 16 DF,  p-value: 7.343e-06

(-1.37)^2
#[1] 1.8769

# Exercise: What is the square of t stat for X1 equal to?
 1.437^2
# [1] 2.064969
reg231 = lm(Y~ X2+X3+X1)
anova(reg231)

# Analysis of Variance Table
# 
# Response: Y
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# X2         1 381.97  381.97 62.1052 6.735e-07 ***
# X3         1   2.31    2.31  0.3762    0.5483    
# X1         1  12.70   12.70  2.0657    0.1699    
# Residuals 16  98.40    6.15                      

# 2.0657 is partial F stat for testing if X1 should be dropped from
# a model with X1, X2, and X3

anova(reg1,reg123)

# Analysis of Variance Table
# 
# Model 1: Y ~ X1
# Model 2: Y ~ X1 + X2 + X3
#   Res.Df     RSS Df Sum of Sq      F  Pr(>F)  
# 1     18 143.120                              
# 2     16  98.405  2    44.715 3.6352 0.04995 *

# F = 3.6352 is for testing
# Full: Y = b0 + b1*X1 + b2*X2 + b3*X3
# Reduced Y = b0 + b1*X1 

# Compute R^2(Y,X2|X1) = SSR(X2|X1)/SSE(X1) = 33.17/143.12 = 0.2317636

# Using the rsq package:
install.packages("rsq")
library(rsq)
rsq.partial(reg12, reg1)

# Exercise: Compute
# a) R^2(Y, X1 | X2)
# R^2(Y, X1 | X2) = SSR(X1|X2)/SSE(X2)=3.473/113.42=0.0306
reg21<- lm(Y~X2+X1)
summary(reg21)
anova(reg21)
rsq.partial(reg12, reg2)

# b) R^2(Y, X3 | X1, X2)
# R^2(Y, X3 | X1, X2) = SSR(X3 | X1, X2)/SSE(X1, X2)=11.55/109.95=0.1050
rsq.partial(reg123, reg12)

# c) Obtain the standardized regression model using only X1 as a predictor 
# and verify the formula for the slope.
scale(Y)
scale(X1)
standardreg <- lm(scale(Y)~scale(X1))
summary(standardreg)
# Hint: Use the scale() function to transform the predictor and the response variables