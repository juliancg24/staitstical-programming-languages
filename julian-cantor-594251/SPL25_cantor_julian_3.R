################################################################################
#################### Statistical Programming Languages #########################
####################        SPL25 - Take Home Exam         ####################
################################################################################

# Name: Julian Cantor
# Matrikelnummer: 594251

################################################################################
####################           Exercise 3                  ####################
####################      Airquality Graphic               ####################
################################################################################

# Set working directory
setwd("/Users/juliancantor/staitstical-programming-languages")

# Load the built-in airquality dataset
data(airquality)

# Check the structure
str(airquality)

# Open PDF device with 7x7 inches
pdf("airquality_graphic.pdf", width = 7, height = 7)

# Set up the layout:
# Row 1: Histogram (spanning both columns)
# Row 2: Scatter plot (left) and Boxplot (right)
# Matrix: 1 1 0
#         2 2 3
#         2 2 3
layout(matrix(c(1, 1, 0, 2, 2, 3, 2, 2, 3), nrow = 3, byrow = TRUE))

# Show the layout (for debugging, commented out for final version)
#layout.show(3)

################################################################################
# Plot 1: Histogram of Solar Radiation (top, spanning both columns)
################################################################################

# Calculate breaks for histogram with custom y-axis tick marks at 0 and 10 only
hist(airquality$Solar.R, 
     breaks = seq(0, 350, by = 10),
     col = "grey",
     main = NULL,
     xlab = "Solar Radiation",
     ylab = "Frequency",
     las = 1,
     yaxt = "n")
axis(2, at = c(0, 5, 10, 15), labels = c("0", "", "10", ""), las = 1)

################################################################################
# Plot 2: Scatter plot of Mean Ozone vs Solar Radiation (bottom left)
################################################################################     
month_colors <- c("yellow", "orange", "red", "purple", "blue")
month_pch <- c(0, 1, 2, 5, 6)  # Different point symbols
month_names <- c("May", "June", "July", "August", "September")

# Create the scatter plot
plot(airquality$Solar.R, airquality$Ozone,
     col = month_colors[airquality$Month - 4],  # Month 5 -> index 1, etc.
     pch = month_pch[airquality$Month - 4],
     xlab = "Solar Radiation",
     ylab = "Mean Ozone",
     main = NULL,
     las = 0,
     xlim = c(0, 350),
     xaxt = "n")
# Add custom x-axis with tick at 350
axis(1, at = seq(0, 350, by = 50))

# Add legend
legend("topleft", 
       legend = month_names,
       col = month_colors,
       pch = month_pch,
       title = "Month",
       cex = 0.8)

################################################################################
# Plot 3: Boxplot of Mean Ozone by Month (bottom right)
################################################################################

# Create boxplot
boxplot(Ozone ~ Month, 
        data = airquality,
        col = month_colors,
        xlab = "Month",
        ylab = "Mean Ozone",
        main = NULL,
        las = 0)

# Close the PDF device
dev.off()

# Reset layout to default
layout(1)

# Confirm file was created
cat("PDF file 'airquality_graphic.pdf' has been created in the working directory.\n")


