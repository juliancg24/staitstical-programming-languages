################################################################################ 
#################### Statistical Programming Languages 2024 ####################
####################           Solutions - Day 5            ####################
################################################################################

##################
### Exercise 1 ###
##################

# 1 a)
f <- function(x) {
  y <- sin(x^2)
  return(y)
}

# 1 b)
f(c(0, 3, -2))
f(0, 3, -2)

# 1 c)
plot(f, xlim = c(-10, 10))

###################
### Exercise 2  ###
###################

# Recursive function example: Fibonacci sequence
# A *recursive function* is one that calls **itself** inside its own body.
# Recursion works by reducing a problem to smaller subproblems until a
# *base case* is reached (which stops further self-calls).
# Note: functions can always call other functions; the special thing here
# is the *self-call*. Without defining/using recursion, calling the same
# function from within itself would not make sense and would not terminate.

# Check this example out
# double_number <- function(x) {
#   # This is NOT a recursive function (it should just return 2*x)
#   # But here we accidentally call 'double_number' again.
#   # Since there is no base case, the function keeps calling itself forever,
#   # leading to infinite recursion and an error ("evaluation nested too deeply").
#   
#   result <- double_number(x)  # <- problematic self-call
#   return(result)
# }

# This will cause an error:
double_number(5)


fibonacci <- function(n) {
  if (n == 1) {
    # Fibonacci number 1
    y <- 1
  } else if (n == 2) {
    # Fibonacci number 2
    y <- 2
    } else {
      # compute Fibonacci for general case
          y <- fibonacci(n - 1) + fibonacci(n - 2)
          # Note: here 'fibonacci' calls itself with smaller inputs.
    }
  return(y) # return output
}

fibonacci(2)

sapply(1:10, fibonacci)

########################
# Alternative solution #
########################
# Check out ?Recall

# fib <- function(n) {
#   if (n == 1) {
#     # Fibonacci number 1
#     y <- 1
#   } else if (n == 2) {
#     # Fibonacci number 2
#     y <- 2
#   } else {
#     # compute Fibonacci for general case
#     y <- Recall(n - 1) + Recall(n - 2)
#   }
#   return(y) # return output
# }
# fibonacci2 <- fib
# rm(fib)
# # Renaming wouldn't work without Recall
# sapply(1:10, fibonacci2)

##################
### Exercise 3 ###
##################

# 3 a)
# The function:
# -- iteratively looks for the minimum of the vector
# -- set it aside and look for the minimum after setting aside the first one

my_sort <- function(x) {
  # generate a vector containing zeros of same length as x
  x_sort <- numeric(length(x)) 
  for (i in 1:length(x)) { 
    # take the current smallest value of x and put it in the next slot of x_sort.
    x_sort[i] <- min(x)
    # find the index of the first occurrence of that minimum in x and remove it from x.
    # which(x == min(x)) returns all positions equal to the minimum; [1] picks the first;
    # x[-idx] returns x without that element.
    x <- x[-which(x == min(x))[1]] 
  }
  x_sort
}

set.seed(1510)
x <- sample(1:10, 11, replace = TRUE)
x
my_sort(x)

# 3 b)
test_vector <- rnorm(10000)
system.time(sort(test_vector))
system.time(my_sort(test_vector))

##################
### Exercise 4 ###
##################

data(iris) 
# Apply a function to the elements of a vector, grouped by factor

tapply(X = iris$Sepal.Length, INDEX = iris$Species, 
       FUN = function(x) c(min(x), max(x)))

# Alternatively, using the function range
tapply(X = iris$Sepal.Length, INDEX = iris$Species, FUN = range)

##################
### Exercise 5 ###
##################

# 5 a)
set.seed(1144)
x <- rnorm(100, mean = 0, sd = 2)
y <- numeric(100)

for (i in 2:length(y)) {
  if (x[i - 1] >= 0) {
    y[i] <- y[i - 1] + x[i - 1]
  } else if (x[i - 1] < 0 & x[i - 1] > -1) {
    y[i] <- y[i - 1] - 2 * x[i - 1]
  } else {
    y[i] <- y[i - 1] - x[i - 1]
  }
}

# 5 b)
plot(1:100, x, type = "l", xlab = "i", ylab = expression(x[i]))
plot(1:100, y, type = "l", xlab = "i", ylab = expression(y[i]))

##################
### Exercise 6 ###
##################
library(datasets)
data("faithful")

# 6 a) 
# seq_len(3) --> 1 2 3
# 1:3 --> 1 2 3
# seq_len() (or seq_along()) is safer than 1:length(x) 
# because it behaves 
# correctly when x is empty and avoids a classic gotcha.

# Compare: 
# x <- integer(0)
# 1:length(x)
# seq_len(length(x))
# seq_along(x)

# a)
set.seed(1202)
cor_vec <- numeric(1000)
for (i in 1:1000) {
  # (i) sample from the row numbers of faithful to generate the bootstrap sample
  bootstrap <- sample(seq_len(nrow(faithful)), replace = TRUE)
  faithful_boot <- faithful[bootstrap, ]
  # (ii) compute the correlation
  cor_vec[i] <- cor(faithful_boot$eruptions, faithful_boot$waiting)
}
# (iii) compute the confidence interval
quantile(cor_vec, c(0.025, 0.975))
hist(cor_vec)
# 6 b)
set.seed(1202)

# sample from the row numbers of faithful to generate the bootstrap sample
# ?replicate
# Apply a Function over a List or Vector
# replicate is a wrapper for the common use of sapply for 
# repeated evaluation of an expression (which will usually involve 
# random number generation).

# (i) sample with replacement using replicate  
bootstrap2 <- replicate(1000, sample(seq_len(nrow(faithful)), replace = TRUE))
# apply a function on the bootstrap sample that computes the correlation
# define a funciton
my_cor <- function(vec) {
  faithful_boot2 <- faithful[vec, ]
  cor(faithful_boot2$eruptions, faithful_boot2$waiting)
}

# (ii) compute the correlation
cor_vec2 <- apply(X = bootstrap2, MARGIN = 2, FUN = my_cor)
# (iii) compute the confidence interval
quantile(cor_vec2, c(0.025, 0.975))

# Compare running times; Note that you need {} to pass several lines to system.time
system.time({set.seed(1202)
  cor_vec <- numeric(10000)
  for (i in 1:10000) {
    # (i) sample from the row numbers of faithful to generate the bootstrap sample
    bootstrap <- sample(seq_len(nrow(faithful)), replace = TRUE)
    faithful_boot <- faithful[bootstrap, ]
    # (ii) compute the correlation
    cor_vec[i] <- cor(faithful_boot$eruptions, faithful_boot$waiting)
  }
  # (iii) compute the confidence interval
  quantile(cor_vec, c(0.025, 0.975))})

system.time({set.seed(1202)
  # (i) sample from the row numbers of faithful to generate the bootstrap sample
  bootstrap2 <- replicate(10000, sample(seq_len(nrow(faithful)), replace = TRUE))
  # (ii) apply a function on the bootstrap sample that computes the correlation
  my_cor <- function(vec) {
    faithful_boot2 <- faithful[vec, ]
    cor(faithful_boot2$eruptions, faithful_boot2$waiting)
  }
  cor_vec2 <- apply(X = bootstrap2, MARGIN = 2, FUN = my_cor)
  # (iii) compute the confidence interval
  quantile(cor_vec2, c(0.025, 0.975))})

##################
### Exercise 7 ###
##################

# one_move reads input from the console and checks its validity (repeating this
# until a valid input is given). It returns a valid cell.

# Arguments:
# cells_possible: a vector containing the values of the cells that can be chosen
#   at this point in the game
one_move <- function(cells_possible) {
  cell_valid <- FALSE
  while (!cell_valid) {
    cell <- scan(what = numeric(), n = 1, quiet = TRUE)
    # Check validity of input
    if (!(cell %in% cells_possible)) {
      cat("Cell not valid. Again:")
    } else{
      cell_valid <- TRUE
    }
  }
  return(cell)
}

# check_vector checks if all elements of a vector are identical, but unequal to 0. 
# It is used to check whether one player won the game (cells marked by a player 
# are indicated by the number of the player, i.e., 1 or 2). It returns TRUE or FALSE.
# Arguments:
# vector: the vector to be checked
check_vector <- function(vector) {
  if (sum(vector) == 0) { # vector contains only 0s
    return(FALSE)
  } else if (any(vector != vector[1])) { # not all elements identical
    return(FALSE)
  } else { # all elements identical
    return(TRUE)
  }
}

# check_win checks if one player won the game. It returns TRUE or FALSE. 
# Arguments:
# board: A 3x3 matrix corresponding to the current playing field. 0s correspond
#   to empty cells, 1s to cells marked by Player 1, 2s to cells marked by Player 2.
check_win <- function(board) {
  win <- FALSE
  if (any(apply(board, 1, check_vector)) | # checks for win in rows
      any(apply(board, 2, check_vector)) | # checks for win in columns
      check_vector(diag(board)) | # checks for win in diagonal
      check_vector(diag(board[, rev(seq_len(ncol(board)))]))) { # checks for win in opposite diagonal
    win <- TRUE
  }
  return(win)
}

# tic_tac_toe enables 2 persons to play Tic-tac-toe via the console. It has no
# arguments.
tic_tac_toe <- function() {
  # keep user's par settings 
  par_original <- par(no.readonly = TRUE)
  on.exit(par(par_original))
  
  # plot empty playing field
  par(xaxs = "i", yaxs = "i") # These arguments prevent the adding of extra space at the axis intervals
  plot.new()
  plot.window(xlim = c(0.5, 3.5), ylim = c(0.5, 3.5))
  grid(nx = 3, ny = 3, col = "black")
  box(lwd = 2)
  text(rep(1:3, 3), rep(1:3, each = 3), labels = 1:9, col = "lightgray", cex = 4)
  
  # initialization
  cells_possible <- 1:9
  board <- matrix(0, nrow = 3, ncol = 3)
  move_count <- 1
  which_player  <- rep(c(1, 2), length = 9)
  
  # print start message
  cat("In each move you have to choose one cell 1-9.\n")
  
  while (length(cells_possible) > 0) { 
    # execute one move
    cat(paste0("Player ", which_player[move_count], ":"))
    cell <- one_move(cells_possible)
    # get coordinates for plot and mark cell in plot
    column <- ifelse(cell %% 3 == 0, 3, cell %% 3)
    row <- ceiling(cell / 3)
    points(column, row, cex = 8, pch = c(1, 4)[which_player[move_count]], 
           col = c("red", "green")[which_player[move_count]], adj = 1)
    # update board matrix
    board[row, column] <- which_player[move_count]
    # check if the current player won (we check for a win after every move, i.e.,
    # if win == TRUE the player who performed the last move is the winner)
    win <- check_win(board) 
    if (win) { # terminate function with corresponding message if player won
      message <- paste0("Player ", which_player[move_count], " wins!")
      return(message)
    } else { # update the valid cells and the move count, if the player didn't win
      cells_possible <- cells_possible[-which(cells_possible == cell)]
      move_count <- move_count + 1
    }
  }
  return("Game ends in a tie!") # When no valid cells are left, the game ends in a tie.
}


tic_tac_toe()

########### Time and date handling & classical time series analysis ############

##################
### Exercise 8 ###
##################

# See locale
Sys.setlocale(category = "LC_TIME")
# Change locale 
# Standardize date/ time names across machines.
# Set LC_TIME to "C" so month/weekday names are 
# English ASCII (e.g., "Jan", "Monday"), not other languages. 
# This makes name-based parsing/labels reproducible (%b/%B/%a/%A, 
# weekdays(), months()).
# Affects name-based formats like:
#   as.POSIXct("Friday, 29 May 2015 05:50", "%A, %d %B %Y %R")  
# %A, %B depend on locale
# Does NOT affect purely numeric formats like "%m/%d/%Y".
Sys.setlocale(category = "LC_TIME", locale = "C")

# Desired: "2015-05-29 CEST"

as.POSIXct("05/29/2015", format = "%m/%d/%Y")
as.POSIXct("Friday, 29 May 2015", format = "%A, %d %B %Y")
as.POSIXct("Friday, 29 May 2015 05:50", format = "%A, %d %B %Y %R")
as.POSIXct("Friday, 29 May 2015 05:50 AM", format = "%A, %d %B %Y %I:%M %p")
as.POSIXct("Friday, 29 May 2015 05:50:06", format = "%A, %d %B %Y %T")
as.POSIXct("05/29/2015 05:50", format = "%m/%d/%Y %R")
as.POSIXct("05/29/2015 5:50 AM", format = "%m/%d/%Y %I:%M %p")
as.POSIXct("05/29/2015 05:50:06", format = "%m/%d/%Y %T")
as.POSIXct("Fri, 16 May 2015 05:50:06 GMT", format = "%a, %d %B %Y %T", tz = "GMT")

##################
### Exercise 9 ###
##################
library("rio")
phone <- import("http://galbithink.org/telcos/telephones-1876-1981.xls", 
                skip=2) # skip first two lines
plot(phone$year, phone$telephones, type = "l", xlab = "year", 
     ylab = "number of telephones")

# linear trend:
# x_t  = at + b
lml <- lm(telephones~year, data=phone)

lines(phone$year, lml$fitted.values, col = "red")

# Alternative: abline(lml, col="red")

# exponential trend: 
#x_t = exp(b) exp(a)^t --> log (x_t) = at + b
lme <- lm(log(telephones)~year, data=phone)
phone_exp <- exp(fitted(lme))

lines(phone$year, phone_exp, col="blue")
# The exponential trend seems to fit better than the linear trend, however, both 
# trend functions do not fit very well

###################
### Exercise 10 ###
###################

plot.cta <- function(xt, linear = NA, figure = 0) {
  t <- 0:(length(xt) - 1)
  return_value <- NULL
  if (is.na(linear)) { 
    # mean as trend
    trend <- rep(mean(xt), length(t))
  } else if (linear) { 
    # linear trend
    model <- lm(xt ~ t)
    trend <- fitted(model)
  } else { 
    # exponential trend
    model <- lm(log(xt) ~ t)
    trend <- exp(fitted(model))
  }
  if (figure == 0) { 
    # no seasonality
    fitted_values <- trend
  } else if (figure > 0) { 
    # additive seasonality
    # slide 18 
    # %% is R’s modulus (remainder) operator.
    # 1:11 %% 4 + 1 : 2 3 4 1 2 3 4 1 2 3 4
    season <- 1 + (t %% figure)
    add <- tapply(X = xt - trend, INDEX = season, FUN = mean)
    fitted_values <- trend + add[season]
  } else { 
    # multiplicative seasonality
    season <- 1 + (t %% -figure)
    mul <- tapply(X= xt / trend, INDEX = season, FUN = mean)
    fitted_values <- trend * mul[season]
  }
  r2 <- 1 - sum((xt - fitted_values) ^ 2) / sum((xt - mean(xt)) ^ 2)
  plot(t, xt, type = "l", col = "black",
       main = sprintf("Trend: %s / Figure: %.0f / R^2=%0.4f", linear, figure, r2))
  lines(t, fitted_values, col = "red")
}

###################
### Exercise 11 ###
###################
xt <- c(14, 6, 4, 13, 12, 5, 4, 12, 11, 5, 4, 12)
par(mfrow = c(1, 1))
plot.cta(xt, linear = TRUE, figure = 4)   # linear trend with additive seasonality
plot.cta(xt, linear = TRUE, figure = -4)  # linear trend with multiplicative seasonality
plot.cta(xt, linear = FALSE, figure = 4)  # exponential trend with additive seasonality
plot.cta(xt, linear = FALSE, figure = -4) # exponential trend with multiplicative seasonality

# Best model (largest R^2): exponential trend + multiplicative seasonality;
# However, models are pretty similar
