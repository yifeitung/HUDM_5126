# Note that R is case sensitive.

# COMMENTING

# As you may have guessed, '#' is the comment character in R
# Anything on a line following a '#' will be ignored by R
# R does not do multiline or block comments. Therefore, 
# each line in a comment must begin with a new '#' character.

# It is also possible to comment out the rest of a line by 
# placing a '#' in the middle. 

### In RStudio, you can add or remove "#" from the beginning of lines
### by typing shift + command + c or going to Code => Comment/Uncomment Lines in 
### the menu.

### Open up this document in RStudio if you haven't already. Then open and save
### a new source file.

### To run a line from the source into the console, place the blinking cursor
### anywhere on that line and press command and enter on a mac or control and
### enter on a pc. Try it with the next line:

5 + 5

### If it ran correctly, you should see 
# > 5 + 5
# [1] 10

# Using R as a calculator:
5 + 4
10 - 8
14/4
7^2

# Built-in arithmetic functions
sqrt(49)
exp(1)
log(exp(10))
sin(pi/2)
cos(pi)
3 + (4 - 2)
3 + 4(4 - 2) # throws an error
3 + 4*(4 - 2) # no error
1e5 - 100 # scientific notation; 1e5 = 100000

# Getting help on any function
? t.test

# ASSIGNING VALUES 
x <- 5

# Equivalently, you can use = sign
x = 5

x
x + 7
x*3
x^2
X # throws an error because case sensitive
y <- 2000
x + y
z <- y/x
z


# Define data vector x
x = c(3,1,5,10)
x
# [1]  3  1  5 10

# Find the average of x




# [1] 4.75



# Add 5 to each element of x
x + 5
# [1]  8  6 10 15

x^2
# [1]   9   1  25 100

median(x)
# [1] 4

# Enter data by the scan function, then paste the numbers 3 1  5 10 3 8 9 -3
y = scan()
# 1: 3  1  5 10 3 8 9 -3
# 9: 
# Read 8 items

y
# [1]  3  1  5 10  3  8  9 -3

mean(y)
# [1] 4.5

# Concatenate function is used to create vectors
z = c(x,0,x)
z
# [1]  3  1  5 10  0  3  1  5 10

# Finding out how many elements of vector z
length(z)
# [1] 9

# Generate 50 standard normal random variables
a = rnorm(50)
a

hist(a)
a = rnorm(500)
hist(a)

# Deterministic sequences
1:100
seq(1, 100, by=1)
seq(1, 100, by=2)

# Compare each element of x is less than 7
x < 7

# How many elements of a are negative
sum(a < 0)

# Third element of x
x[3]
# [1] 5

# Second to fourth element of x
x[2:4]
# [1]  1  5 10

# There is no fifth element so we get NA
x[2:5]
# [1]  1  5 10 NA

# Define a matrix d of size 3x2
d = matrix(c(1,2,3,4,5,6),3,2)
d
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6

# First column of d
d[,1]
# [1] 1 2 3

# Second row of d
d[2,]
# [1] 2 5

# Element in second row first column:
d[2, 1]
# [1] 2

# Exercise from Verzani link p. 11:
miles = c(65311, 65624, 65908, 66219, 66499, 66821, 67145, 67447)
diff(miles)
# [1] 313 284 311 280 322 324 302
?diff
miles
# [1] 65311 65624 65908 66219 66499 66821 67145 67447

# Say, we want to change only last observation
miles[8] = 67457
miles
# [1] 65311 65624 65908 66219 66499 66821 67145 67457

# This gives all observations, except the last
miles[-8]
# [1] 65311 65624 65908 66219 66499 66821 67145

# Categorical data (known as factor in R)
x=c("Yes","No","No","Yes","Yes")
x
# [1] "Yes" "No"  "No"  "Yes" "Yes"
x+5
# Error in x + 5 : non-numeric argument to binary operator

# Frequency distribution
table(x)

# Barchart of x
barplot(table(x))


# INSTALLING AND USING PACKAGES 
# Packages are collections of R functions, data, and compiled code bundled 
# up into a well-defined format that makes them easily importable into R.
.libPaths() # shows you where your packages are stored.
library() # shows you which packages are available.


# Search the internet for details about package "car". Then install it.
install.packages('car') 
### If you haven't done so already, R will ask you which CRAN mirror
### to download from. After that it will be your default.
### The package car (companion to applied regression) will be installed, 
### along with any dependent packages it requires. At this point, although 
### we just *installed* the package, we have not yet *loaded* it into the 
### current workspace.

# To load the package:
library(car)

### Once the packcage has been loaded you can use its functions and access
### its help and data examples (if any)
help(package = 'car')
?logit
logit(.75)
detach("package:car")
logit(.75)

data() # Shows all example data sets currently loaded.
search() # gives a list of attached packages and dataframes

# Graphs
# The commands we will be using are 
# plot for scatterplots
# hist for histograms


# Simple Linear Regression (SLR)

# Before you start a new R exercise, always a good idea to clear the environment:
rm(list=ls())


# Example from lecture slides
x = 1:5
y = c(1, 1, 2, 2, 4)

# Create a scatterplot
plot(x,y)

# Manual regression computations:
x.bar = mean(x)
y.bar = mean(y)
SSxy = sum((x-x.bar)*(y-y.bar))
SSxx = sum((x-x.bar)^2)

# Slope:
(beta1 = SSxy/SSxx)
# Note that parentheses also display the result on the screen

# Intercept:
(beta0 = y.bar - beta1*x.bar)

# Now easier, using the built-in lm function:
reg = lm(y ~ x)
summary(reg)

# Error variance
SSyy = sum((y-y.bar)^2)
SSE = SSyy - beta1*SSxy
SSE
(s = sqrt(SSE/(5-2)))

# Easier:
summary(reg)$sigma

# Clear the workspace again
rm(list = ls())

# We will use the textbook datasets from here:
http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/

# They can be read with commands like this:
# Table 1.1
t1.1 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01TA01.txt", header = FALSE)

# Column names can be changed with
names(t1.1)[1]<-paste("x")
names(t1.1)[2]<-paste("y")
t1.1

# Access only x column:
t1.1$x

# To use the column names without reference to t1.1 you need to attach the dataset:
attach(t1.1)

plot(x,y)
# How to add the regression line?
reg = lm(y~x)
abline(reg)

cor(x,y)
# [1] 0.9063848
# Strong positive linear relationship

# What is the regression equation?
reg = lm(y ~ x)
summary(reg)
# y.hat = 62.366 + 3.57*x

# What is the predicted y when x = 65
62.366 + 3.57*65
# [1] 294.416

# Or automatically:
newx = data.frame(x = 65)
predict(reg, newdata = newx)


# Interpret the value of the slope:
# For every 1 unit increase in x, the hours increase by 3.57

# Interpret the value of the intercept:
# No practical meaning, because x cannot be 0 units

# What is the estimated regression line variance?
48.82^2
# [1] 2383.392

# Note it is much smaller than sample variance of y:
var(y)
# [1] 12800.13