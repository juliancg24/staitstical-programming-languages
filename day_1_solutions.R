################################################################################ 
#################### Statistical Programming Languages #### ####################
####################           Solutions - Day 1            ####################
################################################################################

############## Introduction and Variables, Vectors, Matrices, etc. #############

######################################
### Exercise 1 (My first R script) ###
######################################

# What was your working directory? 
# 1 a)
getwd()

#Set your current working directory to “Statistical Programming”.
# 1 b)
#setwd(".../Statistical Programming")  # ... depends on your folder structure

# 1 c)
str1 <- "slumdog"
str2 <- "millionaire"
# return the objects
str1
str2
# get class
class(str1)
class(str2)
# are they vectors?
is.vector(str1)
is.vector(str2)

# 1 d)
x <- 34.9
x <- as.character(x)
x

######################################
### Exercise 2 (R as a calculator) ###
######################################

# 2 a)
sin(3 * pi + 2)

# 2 b)
log(10, base = 5)

# 2 c)
sum(1:10)

# 2 d)
exp((2 + 3i) * (4 - 1i))
# Alternative:
exp(complex(real = 2, imaginary = 3) * complex(real = 4, imaginary = -1))

# 2 e)
482^(1/3)

############################
### Exercise 3 (Vectors) ###
############################

# 3 a)
# Create a vector a with the elements 1, 3, 5, 7, 11, 13, 17 and 19.
a <- c(1, 3, 5, 7, 11, 13, 17, 19)
a

# 3 b)
# Create a vector b with 2^1, 2^2, ..., 2^8
b <- 2^(1:8)
b

# 3 c)
# Create a vector c with 1^2, 2^2, ..., 2^8
c <- (1:8)^2
c

# 3 d)
# Find the positions where elements of b and c coincide. (Hint: ?which)
which(b == c)       # check with b[b == c]; c[b == c]

# 3 e)
# Create a vector d which contains the numbers from 1 to 100 and another vector e which contains
# 100 elements equal to 7.
d <- 1:100 	        # alternatively, seq(1, 100); check with head(d)
e <- rep(7, 100)    # check with head(e)

# 3 f)
# Create a vector f with the first 30 elements of the following 
# geometric progression: an = 10x(1.5)^(n−1). Compute the cumulative
# sum of the vector elements. Give the elements and positions
# of f whose cumulative sum is greater than 10^6.
n <- 1:30
f <- 10 * (1.5)^(n - 1) 
#f2 <- 10 * (1.5)^(1:30 - 1) 
# cumulative sum
cumsum(f)  
# position
which(cumsum(f) > 10^6)
# elements
cumsum(f)[cumsum(f) > 10^6]

# 3 g)
# Compute the values of the function y= e^(-x) for an equidistant grid 
# from -3 to 3 with an stepwidth
x <- seq(-3, 3, by = 0.5)
y = exp(-x)

###################################
### Exercise 4 (Random vectors) ###
###################################

# 4 a)
# Create a vector norm of size n = 100 from a normal distribution 
# with mean 2 and standard deviation 4.
n <- 100000
set.seed(1034) # make results reproducible
norm <- rnorm(n, mean = 2, sd = 4)  # head(norm)

# Create two vectors called: norm.even and norm.odd with 
# the elements of norm in the even and odd positions, respectively. 
norm.even <- norm[(1:n %% 2) == 0]
norm.odd <- norm[(1:n %% 2) == 1]

# norm.even2 <- norm[seq(2, 100,2)]
# norm.odd2 <- norm[seq(1, 99, 2)]

# Compute the mean and standard deviation of norm.even and
# norm.odd. 
mean(norm.even)
sd(norm.even)
mean(norm.odd)
sd(norm.odd)

# Furthermore, compute the mean and standard deviation of norm.even 
# and norm.odd considering only the positive numbers.
mean(norm.even[norm.even > 0])
sd(norm.even[norm.even > 0])^2
mean(norm.odd[norm.odd > 0])
sd(norm.odd[norm.odd > 0])^2

# 4 b)
# Create a vector pois of size  n = 50 from a poisson distribution 
# with expectation 15. 
n <- 50
pois <- rpois(n, lambda = 15)  # head(pois)

# Create a new vector pois3 that contains the elements in pois that 
# are divisible by 3. 
pois3 <- pois[(pois %% 3) == 0]  # head(pois3)
n_pois3 <- length(pois3); n_pois3

# Add random noise from a standard normal distribution to pois3 
# and store it in a variable pois3.noise.
pois3.noise <- pois3 + rnorm(length(pois3), mean = 0, sd = 1)  # head(pois3.noise)

#############################
### Exercise 5 (Matrices) ###
#############################

# 5 a)
# Create a matrix M.c with the first column vector b and
M.c <- cbind(b, c)
M.c
dim(M.c)
M.c[7, ]

# 5 b)
# Create a matrix M.r with the first row vector a
# and the second row vector b (from Exercise 3). 
# Rename the rows of M.r to a, and b, and the columns 
# to S, T, ..., Y, Z
# (Hint: These are the letters 19 to 26 of the alphabet. 
# The alphabet is provided by the constants letters and LETTERS.)
M.r <- rbind(a, b)  # alternatively, M.r <- rbind(a, t(b))
rownames(M.r) <- c("a", "b")  
colnames(M.r) <- LETTERS[19:26]
M.r

# 5 c)
# Print the matrix M.r without the column W.
M.r[, colnames(M.r) != "W"]  # alternatively, M.r[, -5]

# Print the elements of M.r larger than 12.
# 5 d)
M.r[M.r > 12]

# Create a 10x10 matrix D which contains the
# numbers 1 to 100 filled by columns and another $10x10$
# matrix E which contains the numbers $1, 1/2,
# 1/3, \dots, 1/100$ filled by rows.
# 5 e)
D <- matrix(1:100, nrow = 10, ncol = 10, byrow = FALSE)
D
E <- matrix(1 / (1:100), nrow = 10, ncol = 10, byrow = TRUE)
E
# E = t(1/D)

# Calculate the sum D + E, the difference D-E, the (matrix!)
# product P = DxE and the product of the elements d_{i,j}xe_{i,j})_{i,j=1...10}.
# Print the diagonal elements of the matrix P.
# 5 f)
D + E   # Sum
D - E   # Difference
P <- D %*% E # Matrix product
D * E   # Elementwise product
diag(P) # diagonal elements of P

###########################
### Exercise 6 (Arrays) ###
###########################
UCBAdmissions


# 6 a)
# the number of students admitted and rejected
# admitted/rejected corresponds to rows (1st dimension of the array)
# -> MARGIN = 1
apply(X = UCBAdmissions, MARGIN = 1, FUN = sum) 
# Alternative (without apply()):
c("Admitted" = sum(UCBAdmissions[1, , ]), "Rejected" = sum(UCBAdmissions[2, , ]))

# 6 b) 
# the number of males and females
# male/female corresponds to columns (2nd dimension of the array) 
# -> MARGIN = 2
apply(X = UCBAdmissions, MARGIN = 2, FUN = sum) 
# Alternative (without apply()):
c("Male" = sum(UCBAdmissions[, 1, ]), "Female" = sum(UCBAdmissions[, 2, ]))

# 6 c)
# the number of students by departments
# department corresponds to 3rd dimension of the array -> MARGIN = 3
apply(X = UCBAdmissions, MARGIN = 3, FUN = sum) 
# Alternative (without apply()):
c("A" = sum(UCBAdmissions[, , 1]), "B" = sum(UCBAdmissions[, , 2]), 
  "C" = sum(UCBAdmissions[, , 3]),
  "D" = sum(UCBAdmissions[, , 4]), "E" = sum(UCBAdmissions[, , 5]), 
  "F" = sum(UCBAdmissions[, , 6]))

# 6 d)
# the number of students admitted and rejected by gender
# admitted/rejected: MARGIN = 1, male/female: MARGIN = 2 
# -> combined: MARGIN = 1:2
apply(X = UCBAdmissions, MARGIN = 1:2, FUN = sum)
# Alternative (without apply()):
Admitted <- c("Male" = sum(UCBAdmissions[1, 1, ]), "Female" = sum(UCBAdmissions[1, 2, ]))
Rejected <- c("Male" = sum(UCBAdmissions[2, 1, ]), "Female" = sum(UCBAdmissions[2, 2, ]))
rbind(Admitted, Rejected)

# 6 e)
# the number of males and females by departments
# male/female: MARGIN = 2, department: MARGIN = 3 -> combined: MARGIN = 2:3
apply(X = UCBAdmissions, MARGIN = 2:3, FUN = sum)
# Alternative (without apply()):
Male <- c("A" = sum(UCBAdmissions[, 1, 1]), "B" = sum(UCBAdmissions[, 1, 2]), 
          "C" = sum(UCBAdmissions[, 1, 3]), "D" = sum(UCBAdmissions[, 1, 4]),
          "E" = sum(UCBAdmissions[, 1, 5]), "F" = sum(UCBAdmissions[, 1, 6]))
Female <- c("A" = sum(UCBAdmissions[, 2, 1]), "B" = sum(UCBAdmissions[, 2, 2]), 
            "C" = sum(UCBAdmissions[, 2, 3]), "D" = sum(UCBAdmissions[, 2, 4]),
            "E" = sum(UCBAdmissions[, 2, 5]), "F" = sum(UCBAdmissions[, 2, 6]))
rbind(Male, Female)

##########################
### Exercise 7 (Lists) ###
##########################

# 7 a)
# Create a named list List with three components:
# 1. a vector x with 20 random numbers from a standard Normal distribution,
# 2. a 10 ×10 matrix y with 100 values from a standard Normal distribution,
# 3. a binary vector z with 10 zeros and 20 ones.
set.seed(1242) # make results reproducible
List <- list(x = rnorm(20, mean = 0, sd = 1),
             y = matrix(rnorm(100, mean = 0, sd = 1), nrow = 10, ncol = 10), 
             z = rep(c(0, 1), c(10, 20)))
lapply(X = List, FUN = sum)  
# Alternative (without lapply()):
sum(List$x); sum(List$y); sum(List$z)

# Create a vector x with 20 random values from a standard Normal distribution. 
# Create a vector y applying the following linear function on x: y= 3 + 2.5x. 
# Add to y a random noise through a centered Normal distribution with a standard 
# deviation of 0.5. Fit a simple linear model using:
# model <- lm(y~x). Verify that model is a list. 
# 7 b)
x <- rnorm(200000, mean = 0, sd = 1)
x
y <- 3 + 2.5 * x
y
y <- y + rnorm(20, mean = 0, sd = 0.5)
y
model <- lm(y ~ x)
is.list(model)

# Apply the summary function and access the list elements. 
# Execute the following command methods(class=’lm’) to see all 
# methods available for the class lm.
summary(model)
  names(summary(model))
  summary(model)[[1]]   # summary(model)$call
  summary(model)[[2]]   # summary(model)$terms
  summary(model)[[3]]   # summary(model)$residuals
  summary(model)[[4]]   # summary(model)$coefficients
  summary(model)[[5]]   # summary(model)$aliased
  summary(model)[[6]]   # summary(model)$sigma
  summary(model)[[7]]   # summary(model)$df
  summary(model)[[8]]   # summary(model)$r.squared
  summary(model)[[9]]   # summary(model)$adj.r.squared
  summary(model)[[10]]  # summary(model)$fstatistic
  summary(model)[[11]]  # summary(model)$cov.unscaled
methods(class = 'lm')   # R functions which can be applied to objects of class lm
# example
confint(model)
plot(model)

