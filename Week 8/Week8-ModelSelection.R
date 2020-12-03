# Generating multivariate normal data
# Install and load the libraries

install.packages("mvtnorm")
library(mvtnorm)
install.packages("clusterGeneration")
library(clusterGeneration)

# Set the seed for replication of same results
set.seed(1790)

# Generate data with 10 predictors and an outcome related to only five of them 
# Generate a random covariance matrix with package clusterGeneration
cov1 <- genPositiveDefMat(dim = 10, covMethod = "eigen")

# Generate a random vector of 10 means from norm(0, 10)
mns1 <- rnorm(10, 0, 10)

# Generate coefficients for the output for Y from norm(0, 1)
coef1 <- rnorm(11, 0, 1/2) # First one is the intercept

# Set five of them equal to zero
# coef1[sample(2:11, 5, replace = F)] <- 0 # Generate randomly which 5
# Select X1, X4, X6, X9 and X10 to be non-informative predictors
coef1[c(2,5,7,10,11)] = 0

N = 100
# Generate the design matrix X: 
set.seed(5671)
X <- cbind(rep(1, N), rmvnorm(n = N, mean = mns1, sigma = cov1$Sigma))
Y <- X %*% coef1 + rnorm(N, 0, 1)

ss100 <- data.frame(cbind(X[,2:11], Y))
names(ss100) <- c(paste0("X", 1:10), "Y")
# Create the output Y:


lm1 <- lm(Y ~ ., data = ss100)
summary(lm1)

### Use bestglm package for best subset selection
# install.packages("bestglm")
library(bestglm)


# Best subset selection exhaustive search:
rsOut <- regsubsets(Y ~ ., data = ss100, nvmax = 10)
summary(rsOut)
plot(rsOut)
plot (rsOut, scale="Cp")

# Up to how many predictors can be controlled with nvmax option
# Default value is nvmax = 8


bs1 <- bestglm(Xy = ss100, 
               family = gaussian,
               IC = "BIC")
summary(bs1)
names(bs1)
bs1$BestModel
bs1$Subsets

bs2 <- bestglm(Xy = ss100, 
               family = gaussian,
               IC = "AIC")
summary(bs2)
bs2$BestModel
bs2$Subsets


# ECLS-K Example ############################################################
# More info about the data here:
# https://nces.ed.gov/ecls/ 

### Load the eclsk_c data by opening a browser window
load(file.choose())

### Or call the path directly (fill in your path)
# load(file = "d:\Work\Columbia\HUDM5126\Week8-eclsk_c.Rdata")
head(eclsk_c)
dim(eclsk_c)

# Some charts:
par(mfrow = c(2,2))
hist(eclsk_c$WKSESL) # Socioeconomic status
hist(eclsk_c$RIRT) # Kindergarten reading score
hist(eclsk_c$MIRT) # Kindergarten math score
hist(eclsk_c$P1EXPECT) # Parental expectations
par(mfrow = c(1,1))

# Clean the data, remove ID numbers, which are unrelated to regression
# Select C6R4MSCL (5th grade math score) as the response variable Y

eclsk1 <- eclsk_c[, !names(eclsk_c) %in% c("S1_ID", "CHILDID", "C5R4RSCL", "C6R4RSCL", "C5R4MSCL", "C6R4MSCL", "F5SPECS")]
eclsk1 <- cbind(eclsk1, F5SPECS = eclsk_c$F5SPECS, C6R4MSCL = eclsk_c$C6R4MSCL)
head(eclsk1)
# Some scatterplots
pairs(eclsk1[,3:10])
# Correlation matrix:
cor(eclsk1)

# Regression with all predictors:
summary(lm(C6R4MSCL ~ ., data = eclsk1))

# Best subset selection with timing:
st <- Sys.time()
bs3 <- bestglm(Xy = eclsk1, 
               family = gaussian,
               IC = "BIC")
en <- Sys.time()
(tm <- en - st)

bs4 <- bestglm(Xy = eclsk1, family = gaussian, IC = "AIC")

plot(0:35, bs3$Subsets$BIC, type = "b", ylab = "BIC", ylim = c(40500, 42700),
     xlab = "Number of Covariates", lwd = 3, pch = 19, main = "BIC", cex.main = 2)

plot(0:35, bs4$Subsets$AIC, type = "b", ylab = "AIC", ylim = c(40525, 41300),
     xlab = "Number of Covariates", lwd = 3, pch = 19, main = "AIC", cex.main = 2)

summary(bs3$BestModel)
which.min(bs3$Subsets$BIC) #which has minimum BIC

summary(bs4$BestModel)

### Forward selection with MASS
library(MASS)
min.model <- lm(C6R4MSCL ~ 1, data = eclsk1) # start with intercept
max.model <- lm(C6R4MSCL ~ ., data = eclsk1) # stop with model with everything
scp <- list(lower = min.model, upper = max.model)
fwd <- stepAIC(min.model, direction = 'forward', scope = scp)

fwd$coefficients

### Do the results match up with best subset selection?

(d1 = names(fwd$coefficients)[-1]) # Names of predictor variables without the intercept
(minAIC = which.min(bs4$Subsets$AIC)) # Which model is the best
(d2 = names(bs4$Subsets[minAIC, bs4$Subsets[minAIC,] == TRUE])[-1])

# Check if the two names sets are equivalent:
d1 %in% d2
d2 %in% d1
# Or:
sort(d1) == sort(d2)

### Forward selection with BIC (k = log(N)), StepAIC can do the same job for BIC
fwd2 <- stepAIC(min.model,
                direction = 'forward', 
                scope = scp,
                k = log(nrow(eclsk1)))
fwd2$coefficients

### Do the results match up with best subset selection?
d1 <- names(fwd2$coefficients)[-1]
d2 <- names(bs3$Subsets[16, bs4$Subsets[16,] == TRUE])[-1]
d1 %in% d2
d2 %in% d1

### Backward selection with MASS
library(MASS)
min.model <- lm(C6R4MSCL ~ 1, data = eclsk1)
max.model <- lm(C6R4MSCL ~ ., data = eclsk1)
scp <- list(lower = min.model, upper = max.model)
bwd <- stepAIC(max.model, 
               direction = 'backward', 
               scope = scp)
bwd$coefficients

### Do the results match up with best subset selection?
d1 <- names(bwd$coefficients)[-1]
d2 <- names(bs4$Subsets[27, bs4$Subsets[27,] == TRUE])[-1]
d1 %in% d2
d2 %in% d1

### Backward selection with BIC (k = log(N))
bwd2 <- stepAIC(max.model,
                direction = 'backward', 
                scope = scp,
                k = log(nrow(eclsk1)))
bwd2$coefficients

### Do the results match up with best subset selection?
d1 <- names(fwd2$coefficients)[-1]
d2 <- names(bs3$Subsets[16, bs3$Subsets[16,] == TRUE])[-1]
d1 %in% d2
d2 %in% d1