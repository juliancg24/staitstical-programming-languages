################################################################################ 
#################### Statistical Programming Languages #########################
####################           Solutions - Day 2       #########################
################################################################################

########################### Reading and Writing Data ###########################

#########################################
### Exercise 1 (A simple data frame) ####
#########################################

index <- c(2.8, 1.2, 2.1, 1.6, 1.5, 4.6, 3.6, 2.1, 6.5, 4.6, 3.0, 1.3, 4.2)
unemployment <- c(9.4, 10.4, 10.8, 10.5, 18.4, 11.1, 2.6, 8.8, 5.0, 21.5, 6.7, 
                  2.5, 5.6)
countries <- c("Belgium", "Denmark", "France", "GB", "Ireland", "Italy", 
               "Luxembourg", "Holland", "Portugal", "Spain", "USA", "Japan", 
               "Germany")
# Create data frame
my_df <- data.frame(Country = countries, Index = index, 
                    Unemployment = unemployment)

# 1 a)
# Compute the minimum and maximum of each column, 
# print the values and their corresponding country.
# Calculate min/max
min_index <- min(my_df$Index); min_index
max_index <- max(my_df$Index); max_index
min_unemployment <- min(my_df$Unemployment); min_unemployment
max_unemployment <- max(my_df$Unemployment); max_unemployment
# Select rows with min/max values
min_index_country <- my_df$Country[my_df$Index == min_index]
min_index_country

max_index_country <- my_df$Country[my_df$Index == max_index]
max_index_country

min_unemployment_country <- my_df$Country[my_df$Unemployment == min_unemployment]
min_unemployment_country

max_unemployment_country <- my_df$Country[my_df$Unemployment == max_unemployment]
max_unemployment_country
# Alternative (shorter):
my_df$Country[which.min(my_df$Index)]
my_df$Country[which.max(my_df$Index)]
my_df$Country[which.min(my_df$Unemployment)]
my_df$Country[which.max(my_df$Unemployment)]
# However, be careful with which.min and which.max as they only give the location
# of the FIRST minimum and maximum, see ?which.min!

# 1 b)
# Compute the range of each column
range_index <- max_index - min_index; range_index
range_unemployment <- max_unemployment - min_unemployment; range_unemployment
# Alternative:
diff(range(my_df$Index))
diff(range(my_df$Unemployment))

#####################################
### Exercise 2 (Manipulate data) ####
#####################################

data(mtcars)
help(mtcars)

# 2 a)
# Create a data set mtcars mod, which corresponds 
# to the data set mtcars sorted by columns mpg
# and cyl in descending order.
mtcars_mod <- mtcars[order(mtcars$mpg, mtcars$cyl, decreasing = TRUE), ]
# Alternative: mtcars_mod <- mtcars[order(-mtcars$mpg, mtcars$cyl), ]
mtcars_mod
# 2 b)
# Remove the column carb from mtcars mod.
mtcars_mod$carb <- NULL
# Alternative: mtcars_mod <- mtcars_mod[, names(mtcars_mod) != "carb"]
#mtcars_mod <- mtcars_mod[, -11]

# 2 c)
# Switch the columns mpg and hp in mtcars mod.
# Find positions of the respective columns
names(mtcars_mod) %in% c("mpg", "hp")
which(names(mtcars_mod) %in% c("mpg", "hp"))
# Switch these positions (1 and 4)
mtcars_mod <- mtcars_mod[, c(4, 2:3, 1, 5:9)]
# More general alternative:
ind_mpg <- which(names(mtcars_mod) == "mpg")
ind_hp <- which(names(mtcars_mod) == "hp")
col_ind <- 1:ncol(mtcars_mod)
col_ind[ind_mpg] <- ind_hp
col_ind[ind_hp] <- ind_mpg
mtcars_mod <- mtcars_mod[, col_ind]

# 2 d)
#Extract only the cars of brand Mercedes by finding 
# the indices of row names containing the string
#"Merc". (Hint: ?grep)

# Look for "Merc" in rownames
indices <- grep("Merc", rownames(mtcars_mod))
mtcars_mod[indices, ]

#########################################
### Exercise 3 (Read and write data) ####
#########################################

# 3 a)
# Read the file dax prices.csv into a data frame dax.prices.
getwd()  # check working directory and save the file dax_prices.csv there
setwd("./SPL/2025_wise/Course Material/Exercises/day_2")
dax.prices <- read.csv("dax_prices.csv")

# 3 b)
# Inspect the first rows of dax.prices. Are the prices 
# (contained in variable DAX) correctly interpreted as numeric?
head(dax.prices)
class(dax.prices$DAX) 
# Alternative: 
is.numeric(dax.prices$DAX)
# Yes, the prices are interpreted correctly

# 3 c)
# Rename the column DAX to DAX Prices.
names(dax.prices)[names(dax.prices) == "DAX"] <- "DAX_Prices"
# Alternative: 
# index_dax <- which(names(dax.prices) == "DAX")
# names(dax.prices)[index_dax] <- "DAX_Prices"


# 3 d)
# Write dax.prices to a file named dax_prices.txt in your \
# working directory, with “;” as separator and “,” as decimal point.
write.table(dax.prices, file = "dax_prices.txt", sep = ";", dec = ",", 
            row.names = FALSE)
# 3 e)
# Read dax prices.txt into a data frame dax.prices.txt. 
# Make sure the prices are correctly interpreted as numeric.
# Use correct separator and character for decimal point
dax.prices.txt <- read.table("dax_prices.txt", sep = ";", dec = ",", 
                             header = TRUE)
class(dax.prices.txt$DAX)




############################### String Handling ################################
##########################################
### Exercise 4 (Regular expressions I) ###
##########################################
# 4 a)
# use function grep
?grep #for syntax

# Find these words 
# pit |spot |spate |slap two| respite

# Control:
a <- c("pit", "spot", "spate", "slap two", "respite", 
       "pt", "Pot", "peat", "part")

# . any character expect for a new line

regex_1a <- "p.t"
grep(regex_1a, a, value = TRUE)
# value  = TRUE --> return a vector containing the matching elements

# 4 b)

# rap them| tapeth| apth| wrap/try |sap tray| 87ap9th |apothecary 

# Control:
b <- c("rap them", "tapeth", "apth", "wrap/try", "sap tray", "87ap9th",
       "apothecary", "aleht", "happy them", "tarpth", "Apt", "peth", "tarreth",
       "ddapdg", "apples", "shape the")
# * - Matches the preceding element zero or more times
# + - Matches the preceding element one or more times
# ? - Matches the preceding element zero or one time

# Good answer ???
regex_1b <- "p.?t"
grep(regex_1b, b, value = TRUE)
# Nope?

# Another guess
regex_1b <- "ap.?t"

grep(regex_1b, b, value = TRUE)

# 4 c)

# affgfking |rafgkahe |bafghk| baffgkit |baffg kit 

# Control:
c <- c("affgfking", "rafgkahe", "bafghk", "baffgkit", "baffg kit", "fgok",
         "a fgk", "affgm", "afffhk", "afg.K", "aff gm", "afffhgk")

# "f+g" ?

grep("f+g", c, value = TRUE)

#[1] "affgfking" "rafgkahe"  "bafghk"    "baffgkit" 
#[5] "baffg kit" "fgok"      "a fgk"     "affgm"    
#[9] "afg.K"    

# "af+g" ?

grep("af+g", c, value = TRUE)

#[1] "affgfking" "rafgkahe"  "bafghk"    "baffgkit" 
#[5] "baffg kit" "affgm"     "afg.K"  

# [abc] — any one of a, b, or c
# "af+g[hk]" ?

grep("af+g[hk]", c, value = TRUE)

# [1] "rafgkahe" "bafghk"   "baffgkit" 

# "af+g[fhk]" ?
grep("af+g[fhk]", c, value = TRUE)

# "affgfking" "rafgkahe"  "bafghk"    "baffgkit" 

regex_1c <- "af+g[fh ]?k"

grep(regex_1c, c, value = TRUE)

# 4 d)

# Control:
d <- c("assumes word senses. Within", "does the clustering. In the",
       "but when? It was hard to tell", 'he arrive." After she had',
       "mess! He did not let it", "it wasn't hers!' She replied",
       "always thought so.) Then", "in the U.S.A., people often",
       ' John?", he often thought, but', "weighed 17.5 grams",
       "well ... they'd better not", 'A.I. has long been a very',
       'like that", he thought', "but W. G. Grace never had much")

# assumes word senses. Within | does the clustering. In the |
# but when? It was hard to tell | he arrive.” After she had
# mess! He did not let it | it wasn’t hers!’ She replied |
# always thought so.) Then 

# ".*" --> anything at the beginning, zero or more times
# "[.?!] --> any of these
# .*[.?!] enough?
grep(".*[.?!]", d, value = TRUE) # --> Nope

# R has escaped the " " " by adding a backslash  
# not needed for `

# .*[^A-Z.][.?!] enough?
grep(".*[^A-Z.][.?!]", d, value = TRUE) # --> Nope

# add special characters "), \", ' "
# .*[^A-Z.][.?!] [)\"']? enough?
grep(".*[^A-Z.][.?!][)\"']?", d, value = TRUE) # --> Nope

# add a space
regex_1d <- ".*[^A-Z.][.?!][)\"']?[ ]"


grep(regex_1d, d, value = TRUE)

############################################
### Exercise 5 (Regular expressions II) ####
#############################################
# 5 a)
# ^ start of the string
# $ end of the string
# we need the above to make the entire string to match the pattern
# not just a substring
# (...) treat everything inside as one group
# One digit number 
# two digit number
# three digit numbers starting with 1
# three digit number starting with 2
# three digit number starting with 25

regex_2a <- "^([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])$"
# Control:
grep(regex_2a, c("0", "45", "255", "256", "1003", "1.3", "-1"), value = TRUE)

regex_2a0 <- "([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])"
# Control:
grep(regex_2a0, c("00", "-k45", "2550", "256", "1003", "1.3", "-1"), value = TRUE)



# 5 b)
# ^ and $
# [-+]? find me a - or + 0 or one times
# [0-9]* a number one or more times
# in regex, . means any character except new line
# \. -- > I add one backslash to escape regex notation
# the second \ is used for escaping in R
# I add a double \\. to escape R notation
x <- "\\."

regex_2b <- "^[-+]?[0-9]*\\.?[0-9]+$"
# Control:
grep(regex_2b, c("0.95", "-12.405", "3", "H8.8", "3.4.3"), value = TRUE)

##############################################
### Exercise 6 (Regular expressions III) ####
#############################################
# time

# {} -- a quantifier, repeat the number of time sin curly bracket 
# [0-9]
# : 
# [0-9]{2}
# (:[0-9]{2})* why capturing group?
# I would like to capture pairs + colon 0 or more times, not 

regex_time <- "[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?"


# date
regex_date <- "[0-9]{2,4}( |-|/)([0-9]{2}|May)( |-|/)[0-9]{2,4}"

# [0-9] as above repeat 2 or 4 times (could be a day or a year)
# (...) capturing group 
# capture space OR  - OR / 
# Middle group: or two numbers OR word
# another separator group
# and capture day or year

# Control:
dt <- c("05/29/2015", "Friday, 29 May 2015", "Friday, 29 May 2015 05:50",
        "Friday, 29 May 2015 05:50 AM", "Friday, 29 May 2015 5:50", 
        "Friday, 29 May 2015 5:50 AM", "Friday, 29 May 2015 05:50:06", 
        "05/29/2015 05:50", "05!29/2015 05:50 AM", "05/29/2015 5:50", 
        "05/29/2015 5:50 AM", "05/29/2015 05:50:06", 
        "2015-05-16T05:50:06.7199222-04:00", "Fri, 16 May 2015 05:50:06 GMT", 
        "2015-05-16T05:50:06", "05:50", "05:50 AM", "5:50", "5:50 AM", "05:50:06")

# Find all matched positions
# gregexpr(regex_date, dt) 
# 1 found a match, -1  --  no match
# length in characters ("chars")

# This matches an element: you get true or false
regmatches(dt, gregexpr(regex_date, dt))
regmatches(dt, gregexpr(regex_time, dt))

# This shows elements, but not an extrected one
grep(regex_date, dt, value = TRUE)
grep(regex_time, dt, value = TRUE)


###########################################
### Exercise 7 (Regular expressions IV) ###
###########################################

# html tags can also include the "<" and >" signs in quotes 
# which does not close the tag. This was not respected in the 
# regular expression used in the slides,but should be included as 
# an option (see, e.g., 
# https://www.geeksforgeeks.org/how-to-validate-html-tag-using-regular-expression/)

html <- "<(\"[^\"]*\"|'[^']*'|[^'\">])*>" 
#“don’t stop at > if it’s inside quotes”
# note that in R we have to escape " by using \

# "[^\"]*" match a double-quoted string that may contain anything 
# except another double quote 

#  '[^']*' match a singe-quoted string that may contain anything 
# except another singe quote

# match any character that is not ', ", or > 
# (so you don’t accidentally swallow the tag closer or start a quote).


# Control: 
lecture_html <- "<.*?>"
test <- "<input value = '>'>"
regmatches(test, gregexpr(lecture_html, test)) 
# closes incorrectly with first ">"
regmatches(test, gregexpr(html, test)) 
# closes correctly with second ">"

