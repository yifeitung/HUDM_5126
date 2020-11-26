
# Excercise:
# Obtain the data from Problem 6.18:
# X1 = age of property
# x2 = operating expenses
# X3 = vacancy rate
# X4 = area in square feet
# Y = rental rate

data1 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR18.txt", header = FALSE)
colnames(data1) = c("Y", "X1", "X2", "X3", "X4")
data1

#1. Obtain correlation matrix plot and inspect visually if there are strong associations between the predictors
#2. Obtain the VIF factors and decide if you should exclude any predictors and remove them from the dataset
#3. Working with the dataset from part 2 obtain the studentized deleted residuals 
# and identify outliers using alpha = 0.1. Remove any outliers
#4. Using the dataset from part 3 obtain the hat matrix and identify any outlying X observations

#1.
cor(data1)
pairs(data1)
# x2and x4 have the highest correlation

#2.
reg.ex = lm(Y ~ ., data = data1)
vif(reg.ex)
# No issues with multicollinearity

#3. 
rstudent(reg.ex)
# Without bonferonni:
qt(0.95, 81-5-1)
which(abs(rstudent(reg.ex)) > qt(0.95,81-5-1))


data2 = data1[-c(which(abs(rstudent(reg.ex)) > qt(0.95,81-5-1))), ]
nrow(data2)

#4. 
#Cutoff point:
2*5/71
reg = lm(Y ~ ., data = data2)
im = influence.measures(reg)
which(im$infmat[,9] > 2*5/71)

# 4 hat values > 0.14

#5. Using the dataset from part 3 obtain the raw residuals and plot them against Y-hat, and each predictor variable, 
# and each interaction term. Based on the graphs, should any modifications of the model be made?
e = resid(reg)

par(mfrow=c(2,2))
plot(data2$X1, e)
plot(data2$X2, e)
plot(data2$X3, e)
plot(data2$X4, e)
