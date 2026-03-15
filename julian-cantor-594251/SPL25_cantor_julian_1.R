################################################################################
#################### Statistical Programming Languages #########################
####################        SPL25 - Take Home Exam         ####################
################################################################################

# Name: Julian Cantor
# Matrikelnummer: 594251

################################################################################
####################           Exercise 1                  ####################
####################         Session Info                   ####################
################################################################################

# Set working directory
setwd("/Users/juliancantor/staitstical-programming-languages")

# The code below creates a file "my_session.txt" containing the output of
# sessionInfo()

# sink() diverts R output to a connection (file)
sink("my_session.txt")

# sessionInfo() returns information about the current R session
sessionInfo()

# Close the sink to stop diverting output to the file
sink()

################################################################################
# Explanation of sessionInfo() output:
#
# 1. R version: Shows the version of R being used and its release date.
#
# 2. Platform: Shows the computer architecture and operating system.
#    In my case, "aarch64-apple-darwin25.0.0" means an ARM  -based Mac.
#
# 3. Running under: The operating system name and version (e.g., macOS Tahoe).
#
# 4. Matrix products: Information about which BLAS/LAPACK libraries are used
#    for matrix computations. These affect numerical precision and speed.
#
# 5. locale: The regional and language settings of the machine, including
#    character encoding (e.g., UTF-8). This affects how text and dates are
#    displayed and processed. 
#
# 6. time zone: The timezone setting of the computer.In my case its Bogota, 
#    Colombia settings.
#
# 7. attached base packages: Lists the base packages that are loaded by default
#    when R starts (stats, graphics, grDevices, utils, datasets, methods, base).
#
# 8. loaded via a namespace (and not attached): Shows additional packages that
#    are loaded but not attached. These packages must be explicitly called
#    using library() to use their functions directly.
################################################################################
