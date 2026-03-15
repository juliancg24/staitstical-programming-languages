################################################################################
#################### Statistical Programming Languages #########################
####################        SPL25 - Take Home Exam         ####################
################################################################################

# Name: Julian Cantor
# Matrikelnummer: 594251

################################################################################
####################           Exercise 2                  ####################
####################            COVID-19                   ####################
################################################################################

# Set working directory
setwd("/Users/juliancantor/staitstical-programming-languages")

################################################################################
# a) Read data, delete unnecessary variables, rename, transform date to POSIXct
################################################################################

# Read the CSV file into a data frame
corona <- read.csv("corona.csv")

# Check the structure of the data
str(corona)

# Delete variables not in the table: day, month, year, geoId, 
# countryterritoryCode, continentExp
corona$day <- NULL
corona$month <- NULL
corona$year <- NULL
corona$geoId <- NULL
corona$countryterritoryCode <- NULL
corona$continentExp <- NULL

# Rename variables according to New name column
names(corona)[names(corona) == "dateRep"] <- "date"
names(corona)[names(corona) == "countriesAndTerritories"] <- "country"
names(corona)[names(corona) == "popData2020"] <- "population"

# Transform date to POSIXct
# The date format is DD/MM/YYYY
corona$date <- as.POSIXct(corona$date, format = "%d/%m/%Y")

# Check the structure after transformations
str(corona)

# Are the classes of other variables represented adequately?
# - date:         POSIXct - YES, appropriate for date-time data
# - cases:        integer - YES, appropriate for counts of infected people
# - deaths:       integer - YES, appropriate for counts of deaths
# - country:      character - NO, it would be better as a factor variable
# - population:   integer - YES, appropriate for population counts
# - indicator14:  numeric - YES, appropriate for a continuous rate per 100K ppl

################################################################################
# b) How many observations contain missing values?
################################################################################

# Count observations (rows) with at least one missing value.
# complete.cases counts if no NA in the row, so we negate it with !
n_missing <- sum(!complete.cases(corona))
n_missing
# Answer: 719

################################################################################
# c) How many observations counted less than 20 deaths? What's their share?
################################################################################

# Count observations with deaths < 20
n_less_20 <- sum(corona$deaths < 20, na.rm = TRUE)
n_less_20 # 14799

# Total number of observations
n_total <- nrow(corona)
n_total # 23054

# Share (proportion) of observations with less than 20 deaths
share_less_20 <- n_less_20 / n_total
share_less_20
# Answer: Approximately 0.6419 or 64.19%

################################################################################
# d) T-test: indicator14 differs in Czechia vs Slovakia in January 2022?
################################################################################

# Filter data for Czechia in January 2022
czechia_jan2022 <- corona$indicator14[corona$country == "Czechia" & 
                                       format(corona$date, "%Y-%m") == "2022-01"]

# Filter data for Slovakia in January 2022
slovakia_jan2022 <- corona$indicator14[corona$country == "Slovakia" &
                                        format(corona$date, "%Y-%m") == "2022-01"]

# Check the data
length(czechia_jan2022)
length(slovakia_jan2022)

# Perform a two-sample t-test (significance level 5%)
# H0: means are equal
# H1: means are different
t_test_result <- t.test(czechia_jan2022, slovakia_jan2022)
t_test_result # p-value = 0.08096

# Decision: If p-value < 0.05, we reject H0 and conclude means differ.
# If p-value >= 0.05, we cannot reject H0 and conclude no significant difference.
# Based on the p-value from the t-test output, we cannot reject H0, so we cannot
# conclude that the means differ significantly.

################################################################################
# e) Total deaths per country in September 2020, sorted descending
################################################################################

# Filter data for September 2020
sept_2020 <- corona[format(corona$date, "%Y-%m") == "2020-09", ]

# Compute total deaths per country using tapply
deaths_per_country <- tapply(sept_2020$deaths, sept_2020$country, sum, na.rm = TRUE)

# Sort in descending order
deaths_sorted <- sort(deaths_per_country, decreasing = TRUE)
deaths_sorted

# Answer: 
# Spain        France       Romania        Poland         Italy       Germany 
# 3291          1358          1181           485           427           364 
# Czechia      Bulgaria   Netherlands       Hungary      Portugal       Belgium 
# 276           196           194           165           153           147 
# Greece       Croatia       Austria        Sweden       Ireland       Denmark 
# 122            97            67            51            29            26 
# Malta      Slovakia      Slovenia       Finland        Norway     Lithuania 
# 23            21            19            15            10             9 
# Latvia       Estonia        Cyprus    Luxembourg       Iceland Liechtenstein 
# 3             2             1             1             0             0 

################################################################################
# f) Create indicator7 (7-day indicator)
################################################################################

# indicator7 = (sum of cases from day i-6 to day i) / population * 100000
# For the first 6 days of each country, indicator7 = NA

# Sort data by country and date first
corona <- corona[order(corona$country, corona$date), ]

# Function to calculate indicator7 for a country's data frame
calc_indicator7 <- function(df) {
  n <- nrow(df)
  sapply(1:n, function(i) {
    if (i < 7) NA
    else (sum(df$cases[(i-6):i], na.rm = TRUE) / df$population[i]) * 100000
  })
}

# Apply to each country using by() and combine results
corona$indicator7 <- unlist(by(corona, corona$country, calc_indicator7))

# Check the result
head(corona[, c("date", "country", "cases", "population", "indicator7")], 20)
# looks good for austria

################################################################################
# g) Create factor variable alert_level based on indicator7
################################################################################

# alert_level based on indicator7:
# "green" if indicator7 < 35
# "yellow" if indicator7 >= 35 and < 50
# "red" if indicator7 >= 50 and < 100
# "darkred" if indicator7 >= 100

# Initialize alert_level
corona$alert_level <- NA

# Assign alert levels based on indicator7
corona$alert_level[corona$indicator7 < 35] <- "green"
corona$alert_level[corona$indicator7 >= 35 & corona$indicator7 < 50] <- "yellow"
corona$alert_level[corona$indicator7 >= 50 & corona$indicator7 < 100] <- "red"
corona$alert_level[corona$indicator7 >= 100] <- "darkred"

# Convert to factor
corona$alert_level <- factor(corona$alert_level, 
                              levels = c("green", "yellow", "red", "darkred"))

# Check the result
table(corona$alert_level, useNA = "ifany")
# Answer:
# green  yellow     red darkred    <NA> 
#  7677    1267    2810   11120     180 

# Check final structure
str(corona)
head(corona)

