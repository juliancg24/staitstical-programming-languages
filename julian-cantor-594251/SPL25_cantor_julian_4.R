################################################################################
#################### Statistical Programming Languages #########################
####################        SPL25 - Take Home Exam         ####################
################################################################################

# Name: Julian Cantor
# Matrikelnummer: 594251

################################################################################
####################           Exercise 4                  ####################
####################         Data Cleaning                 ####################
################################################################################

# Set working directory
setwd("/Users/juliancantor/staitstical-programming-languages")

################################################################################
# Read the CSV file
################################################################################

# Read the file with ":" as separator, UTF-8 encoding
nick <- read.csv("bavarian_nicknames_fem.csv", 
                 sep = ":", 
                 header = FALSE, 
                 stringsAsFactors = FALSE,
                 fileEncoding = "UTF-8")

# Assign column names
names(nick) <- c("Nickname", "Name")

# Check initial structure
str(nick)
head(nick, 10)

################################################################################
# Step 1: Remove leading and trailing whitespace
################################################################################

nick$Nickname <- trimws(nick$Nickname)
nick$Name <- trimws(nick$Name)

################################################################################
# Step 2: Remove empty lines (rows where both Nickname and Name are empty)
################################################################################

nick <- nick[nick$Nickname != "" | nick$Name != "", ]
nick <- nick[nick$Nickname != "" & nick$Name != "", ]

################################################################################
# Step 3: Remove square brackets and their content from Name
################################################################################

# Pattern: \[.*?\] matches [anything]
nick$Name <- gsub("\\[.*?\\]", "", nick$Name)

# Trim whitespace again after removal
nick$Name <- trimws(nick$Name)

################################################################################
# Step 4: Expand shortening notation in Nickname
################################################################################

# This function expands all shortening patterns in a single nickname string
expand_nickname <- function(nickname) {
  results <- nickname
  
  # Pattern 1: Handle (X) optional groups - creates two versions
  # E.g., "Sig(g)i" -> "Sigi" and "Siggi"
  # E.g., "Äva(r)l" -> "Äval" and "Ävarl"
  # E.g., "Fin(n)-é/i" -> "Fin-é/i" and "Finn-é/i"
  # E.g., "Steff(l)" -> "Steff" and "Steffl"
  # E.g., "Vev(erl)" -> "Vev" and "Veverl"
  
  new_results <- c()
  for (res in results) {
    if (grepl("\\([^)]+\\)", res)) {
      # Find the optional group
      match <- regmatches(res, regexpr("\\([^)]+\\)", res))
      content <- gsub("[()]", "", match)  # Content inside parentheses
      
      # Version without the optional content
      without_opt <- sub("\\([^)]+\\)", "", res)
      # Version with the optional content (no parentheses)
      with_opt <- sub("\\([^)]+\\)", content, res)
      
      # Recursively expand if there are more patterns
      new_results <- c(new_results, expand_nickname(without_opt), 
                       expand_nickname(with_opt))
    } else {
      new_results <- c(new_results, res)
    }
  }
  results <- unique(new_results)
  
  # Pattern 2: Handle -X/Y patterns (dash followed by alternatives)
  # E.g., "Ag-e/i" -> "Age" and "Agi"
  # E.g., "Lin-i/e" -> "Lini" and "Line"
  # E.g., "Urs-l/i" -> "Ursl" and "Ursi"
  
  new_results <- c()
  for (res in results) {
    if (grepl("-[^/,\\s]+/[^/,\\s]+", res)) {
      # Match pattern like "-e/i" or "-l/i"
      match <- regmatches(res, regexpr("-[^/,\\s]+/[^/,\\s]+", res))
      parts <- strsplit(gsub("^-", "", match), "/")[[1]]
      base <- sub("-[^/,\\s]+/[^/,\\s]+", "", res)
      
      for (p in parts) {
        new_results <- c(new_results, paste0(base, p))
      }
    } else {
      new_results <- c(new_results, res)
    }
  }
  results <- unique(new_results)
  
  # Pattern 3: Handle X/Y patterns without dash (simple alternatives)
  # E.g., "Sopherl/Sofferl" -> "Sopherl" and "Sofferl"
  # E.g., "Sophe/Sofe" -> "Sophe" and "Sofe"
  
  new_results <- c()
  for (res in results) {
    if (grepl("/", res)) {
      parts <- strsplit(res, "/")[[1]]
      new_results <- c(new_results, parts)
    } else {
      new_results <- c(new_results, res)
    }
  }
  results <- unique(new_results)
  
  return(results)
}

# Expand nicknames for each row
expanded_rows <- list()
for (i in seq_len(nrow(nick))) {
  # Split by comma first (multiple nicknames in one cell)
  nicknames <- strsplit(nick$Nickname[i], ",")[[1]]
  nicknames <- trimws(nicknames)
  
  # Expand each nickname
  all_expanded <- c()
  for (nn in nicknames) {
    if (nn != "") {
      all_expanded <- c(all_expanded, expand_nickname(nn))
    }
  }
  
  # Store each expanded nickname with its name
  for (exp_nick in all_expanded) {
    expanded_rows[[length(expanded_rows) + 1]] <- data.frame(
      Nickname = trimws(exp_nick),
      Name = nick$Name[i],
      stringsAsFactors = FALSE
    )
  }
}

# Combine all expanded rows
nick <- do.call(rbind, expanded_rows)

################################################################################
# Step 5: Remove duplicates and merge names for duplicate nicknames
################################################################################

# Find unique nicknames
unique_nicknames <- unique(nick$Nickname)

# Create new data frame with merged names
nick_merged <- data.frame(
  Nickname = character(0),
  Name = character(0),
  stringsAsFactors = FALSE
)

for (nn in unique_nicknames) {
  # Get all names associated with this nickname
  all_names <- nick$Name[nick$Nickname == nn]
  
  # Split names by comma and collect all individual names
  all_individual_names <- c()
  for (name_str in all_names) {
    names_split <- strsplit(name_str, ",")[[1]]
    names_split <- trimws(names_split)
    all_individual_names <- c(all_individual_names, names_split)
  }
  
  # Remove duplicates
  unique_names <- unique(all_individual_names)
  unique_names <- unique_names[unique_names != ""]
  
  # Combine back into a single string
  merged_name <- paste(unique_names, collapse = ", ")
  
  # Add to result
  nick_merged <- rbind(nick_merged, data.frame(
    Nickname = nn,
    Name = merged_name,
    stringsAsFactors = FALSE
  ))
}

# Replace nick with the merged version
nick <- nick_merged

################################################################################
# Step 6: Final cleanup
################################################################################

# Remove any remaining empty rows
nick <- nick[nick$Nickname != "" & nick$Name != "", ]

# Remove any rows with empty Nickname
nick <- nick[!is.na(nick$Nickname) & nick$Nickname != "", ]

# Trim whitespace one more time
nick$Nickname <- trimws(nick$Nickname)
nick$Name <- trimws(nick$Name)

# Reset row names
rownames(nick) <- NULL

################################################################################
# Check the result
################################################################################

str(nick)
head(nick, 20)
nrow(nick)

# Check for duplicates in Nickname
any(duplicated(nick$Nickname)) # FALSE

# Sample checks
nick[nick$Nickname == "Mirl", ]  # Should have merged names
nick[nick$Nickname == "Sigi", ]  # Should exist from Sig(g)i expansion
nick[nick$Nickname == "Siggi", ] # Should exist from Sig(g)i expansion
nick[nick$Nickname == "Age", ]   # Should exist from Ag-e/i expansion
nick[nick$Nickname == "Agi", ]   # Should exist from Ag-e/i expansion
nick[nick$Nickname == "Vicki", ] # Should have merged names 

################################################################################