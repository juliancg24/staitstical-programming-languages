################################################################################ 
#################### Statistical Programming Languages #########################
####################           Solutions - Day 3       #########################
################################################################################


################################### Graphics ###################################

############################################
### Exercise 1 (Testing plot parameters) ###
############################################
# On the pressure data set try the different plot parameters mentioned on slides 6 to 8.

# Add axes labels: xlab, ylab
# Change axes ranges: xlim, ylim
# Change point symbols and colours: pch, col
# Add title: title, main
# Add grid: grid(nx, ny), abline(v = , h = , lty = )


# Load pressure dataset
data(pressure)
# No parameters
plot(pressure)
# Adding axis labels (in larger size)
plot(pressure, xlab = "Temperature", ylab = "Pressure", cex.lab = 1.2)
# Changing axis ranges and point symbol
plot(pressure, xlim = c(-50, 400), ylim = c(-20, 850), pch = 23)
# Changing point symbol and color
plot(pressure, pch = "*", col = "purple")
# Add (sub)title separately to existing plot
title(main = "Vapor Pressure of Mercury", sub = "in degrees Celsius")
# Create new plot and directly specify the (sub)title
plot(pressure, pch = "*", col = "purple", cex = 2, 
     main = "Vapor Pressure of Mercury", sub = "in degrees Celsius")
# Change plot type (lines and points) and color
plot(pressure, type = "b", col = 3)
# Add grid to existing plot using grid()
grid()
# Create new plot (same as before)
plot(pressure, type = "b", col = 3)
# Add grid to existing plot using abline()
abline(v = seq(0, 350, by = 50), h = seq(0, 800, by = 100), col = "lightgray",
       lty = "dotted")
# Create new plot containing only observations with pressure larger than 200
plot(pressure[pressure[, 2] > 200, ], type = "p", pch = 15, col = 2, 
     xlim = c(0, 360), ylim = c(0, 800))
# Add remaining observations to existing plot using different color/point symbol
points(pressure[pressure[, 2] <= 200, ], pch = 16, col = 3)
# Add legend to existing plot
legend("topleft", legend = c("Pressure > 200", "Pressure <= 200"), 
       col = 2:3, pch = 15:16)

###########################################
### Exercise 2 (Distinguish variables) ###
##########################################

data(ToothGrowth)
attach(ToothGrowth)


# This has to be considered when plotting the points and adding the legend!
# First attempt
plot(dose, len)

#Change the colours and symbols
#Change the axes labels and add title
col.veq <- c("chocolate2", "cadetblue1")
pch.veq <- c(2, 4)
plot(dose, len, col = col.veq, pch = pch.veq, 
     xlab = "Dose", ylab = "Tooth length", 
     main = "Effect of Vitamin C")

# Note that in factor variables like supp, the different values are per default 
# encoded in alphabetic order ("OJ" = 1, "VC" = 2), not in the order of their 
# appearance in the data (first: "VC", second: "OJ"), see str(supp) or 
# levels(supp). 

col.veq <- c("chocolate2", "cadetblue1")[supp]
pch.veq <- c(2, 4)[supp]
plot(dose, len, col = col.veq, pch = pch.veq, xlab = "Dose", ylab = "Tooth length", 
     main = "Effect of Vitamin C")

# Add the legend

legend(0.5, 34, c("VC", "OJ"), col = c("cadetblue1", "chocolate2"), 
       text.col = c("cadetblue1", "chocolate2"), pch = c(4, 2), 
       bg = "grey")

# Alternatively, plot observations with supp == "VC" first and add "OJ" afterwards:
plot(dose[supp == "VC"], len[supp == "VC"], 
     col = "cadetblue1", pch = 4, xlab = "Dose", ylab = "Tooth length", 
     main = "Effect of Vitamin C", las = 1)
points(dose[supp == "OJ"], len[supp == "OJ"], col = "chocolate2", pch = 2)
legend(0.5, 34, c("VC", "OJ"), col = c("cadetblue1", "chocolate2"), 
       text.col = c("cadetblue1", "chocolate2"), pch = c(4, 2), bg = "grey")

detach(ToothGrowth)

#######################################
### Exercise 3 (Side by side plots) ###
#######################################

data(iris)
attach(iris)

# First attempt
par(mfrow = c(1, 3)) # Alternative: layout(matrix(1:3, nrow = 1))
hist(Sepal.Width[Species == "setosa"])
hist(Sepal.Width[Species == "versicolor"])
hist(Sepal.Width[Species == "virginica"])

# Second attempt: make adjustments
# Add different titles
# Adjust axes labels 
main <- paste0("Species: ", c("Setosa", "Versicolor", "Virginica"))
xlab <- "Sepal Width"
hist(Sepal.Width[Species == "setosa"], col = "aquamarine",
     main = main[1], xlab = xlab, freq = FALSE)
hist(Sepal.Width[Species == "versicolor"], col = "blueviolet",
     main = main[2], xlab = xlab, freq = FALSE)
hist(Sepal.Width[Species == "virginica"], col = "burlywood2",
     main = main[3], xlab = xlab, freq = FALSE)

# Third attempt: make adjustments
# Adjust breaks
breaks <- seq(2, 4.5, by = 0.25)
main <- paste0("Species: ", c("Setosa", "Versicolor", "Virginica"))
xlab <- "Sepal Width"
hist(Sepal.Width[Species == "setosa"], col = "aquamarine",
     main = main[1], xlab = xlab, breaks = breaks, freq = FALSE)
hist(Sepal.Width[Species == "versicolor"], col = "blueviolet",
     main = main[2], xlab = xlab, breaks = breaks, freq = FALSE)
hist(Sepal.Width[Species == "virginica"], col = "burlywood2",
     main = main[3], xlab = xlab, breaks = breaks, freq = FALSE)

# Change parameter mfrow back to 1x1:
par(mfrow = c(1, 1)) # Alternative: layout(1)

# Detach the dataset
detach(iris)

#############################
### Exercise 4 (ggplot2) ####
#############################

#install.packages("ggplot2")
library(ggplot2)

# a)
# data: what data are we plotting?
# aes: aesthetic mappings
# at least one layer
# scatter plot
p1 <- ggplot(data = cars, aes(x = speed, y = dist)) +
  geom_point()
p1
class(p1)
# b)
# Add labels use + labs(x = , y = )
# Add title
plot1 <- ggplot(data = cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  labs(title = "Stopping distance of cars depending on their speed (1920s)", 
       x = "Speed (mpg)", 
       y = "Stopping Distance (ft)")


# Change the size of the points
# Change the colour 

plot2 <- plot1 + geom_text(aes(label = rownames(cars)))
plot2

plot2 + geom_point(size = 5, color = "blue", shape = 3)

####################################### 
### Exercise 5 (Multivariate plots) ###
#######################################

# Recap on functions 

f <- function(x, y) {
  f_xy <- sin(x) + sqrt(y)
  return(f_xy)
}

# 5 a)
# Create a contour plot 
x <- y <- seq(0, 5, length.out = 20)

# evaluate f for all pairs in the grid (?outer)
# outer(X, Y, FUN = "*", ...)

z <- outer(x, y, f)      

z2 <- f(x, y)
# Create contour plot
contour(x, y, z, nlevels = 20)        

# 5 b)
# Use function persp 
# Adjust theta and phi to have different view 
persp(x, y, z, 
      theta = 10, 
      phi = 15, 
      col = "pink", 
      shade = 0.2)

