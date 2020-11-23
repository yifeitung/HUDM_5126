# Example (5.2 on p. 209):

# Create the matrices
(A = matrix(c(2, 3, 5, 4, 1, 5, 7, 8), 4, 2))
(B = matrix(c(6, 9, 3, 1), 4, 1))
(C = matrix(c(3, 8, 5, 2, 8, 6, 1, 4), 4, 2))

# a) A + C
A + C

# b) A - C
A - C

# c) B'A
t(B) %*% A
# Note the transpose operator is t()
# Matrix multiplication in R is %*% 

# d) AC'

A %*% t(C)

# e) C'A

t(C) %*% A


# Exercise:

# a) Create three vectors, a, b and c, with 3 elements each and combine them in a 3x3 matrix A, 
# where each vector is a column using cbind function. How about each vector is a row?
# cbind function combines vectors in a matrix by arranging them in columns
set.seed(74)
a<-rnorm(3)
b<-rnorm(3)
c<-rnorm(3)
m<-cbind(a,b,c)


# b) Check your result with the function is.matrix()
is.matrix(m)

# c) Create a vector with 12 elements and convert it to a 4x3 matrix B using matrix() function.
# The argument byrow in matrix() is set to be FALSE by default. Please change it to TRUE to see the differences.
set.seed(74)
c<-rnorm(12)
B<-matrix(c,nrow = 4,ncol = 3)
matrix(c,nrow = 4,ncol=3,byrow = TRUE)

# d) Find B transpose and name it C.
C<-t(B)
C

# e) Can you compute C*C? Why? Can you compute B*C?
C %*% C
# Error in C %*% C : non-conformable arguments
B %*% C

# f) Extract a new matrix from the last three rows of B and name it subB
subB<-B[-1,]
subB

# g) Compute 3*C, C+B', C-B'. Can we compute C+B? Why?
3*C
C+t(B)
C+B # Error in C + B : non-conformable arrays

# h) Generate a nxn matrix (square matrix) A1, then generate another n x m matrix A2. 
# If we have A1*x=A2 (Here * represents matrix multiplication), solve for x.
n=5
A1=matrix(rnorm(n^2),n,n)

m=3
A2=matrix(rnorm(n*m),n,m)

solve(A1) %*% A2

# Regression examples
# Get data from Table 1.1 on p. 19

data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01TA01.txt", header = FALSE)
data

# Creating the observations y and the design matrix X
(y = data$V2)
X = cbind(rep(1, nrow(data)), data$V1)
X

# Matrix products that occur in the formulas

# X'X
t(X) %*% X

#X'Y
t(X)%*%y

#(X'X)^(-1)
solve(t(X)%*%X)
# solve is matrix inversion
# Check element (2,2) with formula from slides
1/sum((data$V1-mean(data$V1))^2)
# [1] 5.050505e-05


# Finally, the estimated slope and intercept
# b = ((X'X)^(-1))X'Y
(b = solve(t(X)%*%X) %*% t(X) %*% y)

# Check and compare to built-in lm funcion

reg = lm(y~data$V1)
summary(reg)

# Predicted values
y.hat = X%*%b
y.hat

# Cross-check with built-in function
cbind(predict(reg), y.hat)

# Residuals
(e = y - y.hat)
resid(reg)

# Sample size
n = nrow(data)

# Identity matrix
I = diag(rep(1, n))

# Hat matrix
H = X %*% solve(t(X)%*%X) %*% t(X)	

# Predicted values
H %*% y

# Residuals
(I-H)%*%y

# Sum of squares
t(y)%*%y
SSE = t(y)%*%y - t(b)%*%t(X)%*%y
SSE
anova(reg)

MSE = SSE/(nrow(data)-2)
MSE

# J matrix
n = nrow(data)
J = matrix(rep(1, n^2), n, n)
I = diag(rep(1, n))
SSTO = t(y)%*%(I-J/n)%*%y
SSTO
# Compare to
sum((y-mean(y))^2)

# SSR
t(y) %*% (H - J/n) %*% y
# Compare to built-in function
# Cross check with SSResidual from anova:
anova(reg)

# drop() change 1x1 matrix to scalar
# Covariance matrix of b
s.sq.b = drop(MSE)*solve(t(X)%*%X)
s.sq.b

# Obtaining SE of b:
sqrt(diag(s.sq.b))

# Covariance matrix of predictions
x.new = c(1, 65)
(s.sq.new = t(x.new) %*% s.sq.b %*% x.new)


# Exercise (5.9 on p. 210):
#                       [0 1 8]
# Define the matrix A = [0 3 1]
#                       [0 5 5]

# a) What is the rank of A?
# b) Are the column vectors of A linearly independent?
# c) Calculate the determinant of A.