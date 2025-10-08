data(pressure)

plot(pressure, xlab = "Temperature (deg C)",
     ylab = "Pressure (mm of Hg)",
     main = "Pressure data: Vapor Pressure of Mercury",
     cex.lab = 1.05)

plot(pressure[pressure[, 2] > 200, ], 
       type = "l", 
       pch = 15 ,
       col = 2, 
       xlim = c(0, 360), ylim = c(0,800), xlab = "" )

title(sub = "Pressure data: Vapor Pressure of Mercury", line = 2.5)

# Exercise 2
data("ToothGrowth")
plot(ToothGrowth)

color_vector <- c("chocolate2", "cadetblue1")[ToothGrowth$supp]
pch_vector <- c(2,4)[ToothGrowth$supp]

plot(ToothGrowth$dose, ToothGrowth$len, 
     xlab = "Dose (mg)", 
     ylab = "Tooth Length", 
     main = "Effect of Vitamin C", col = color_vector, pch= pch_vector)

legend(0.5, 34,  
       legend = c("VC","OJ"), 
       pch = c(4,2), 
       col = c("cadetblue1", "chocolate2"), 
       text.col = c("cadetblue1", "chocolate2"), 
       bg = "grey")

# Exercise 3
data("iris")

plot(iris)

main <- paste0("Species: ", levels(iris$Species))
xlab <- "Sepal Width"
ylab <- "Density"
color_vector <- c("aquamarine","blueviolet", "burlywood2")
breaks <- seq(2.0, 4.5, by = 0.25)


par(mfrow = c(1,3))

for (i in 1:3) {
  hist(iris$Sepal.Width[iris$Species == levels(iris$Species)[i]], 
       main = main[i], 
       xlab = xlab, 
       ylab = ylab, 
       col = color_vector[i], 
       border = "black", 
       prob = TRUE,  
       breaks = breaks)
}

# Exercise 4
library("ggplot2")

data("cars")

# reset par
par(mfrow = c(1,1))

plot(cars)

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  labs(title = "Distance vs Speed",
       x = "Speed (mpg)",
       y = "Stopping Distance (ft)") +
  theme_minimal()

# Exercise 5
x <- y <- seq(0, 5, length.out = 50)
z <- outer(x, y, function(x, y) sin(x) + sqrt(y))

contour(x, y, z, 
        xlab = "X-axis", 
        ylab = "Y-axis", 
        main = "Contour Plot of z = sin(x) * sqrt(y)", 
        nlevels = 20, col = rainbow(20))

par(bg = "grey")

persp(x, y, z, 
      theta = 10, # rotate view around z-axis
      phi = 20, # tilt camera from above toward horizon
      expand = 0.5, # vertical scaling of z
      col = "lightblue", # surface color
      shade = 0.75) # shading strength
