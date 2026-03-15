################################################################################
#################### Statistical Programming Languages #########################
####################        SPL25 - Take Home Exam         ####################
################################################################################

# Name: Julian Cantor
# Matrikelnummer: 594251

################################################################################
####################           Exercise 5                  ####################
####################           Battleship                  ####################
################################################################################

# Set working directory
setwd("/Users/juliancantor/staitstical-programming-languages")

################################################################################
# Helper function: Check if a ship can be placed at a given position
################################################################################

# This function checks if a ship of given length can be placed starting at
# position (row, col) with the given orientation (horizontal/vertical)
# The grid is marked: 0 = empty, 1 = ship, 2 = buffer zone around ships
can_place_ship <- function(grid, row, col, length, horizontal) {
  n_row <- nrow(grid)
  n_col <- ncol(grid)
  
  if (horizontal) {
    # Check if ship fits within grid boundaries
    if (col + length - 1 > n_col) {
      return(FALSE)
    }
    # Check if all cells are empty (no ship or buffer)
    for (c in col:(col + length - 1)) {
      if (grid[row, c] != 0) {
        return(FALSE)
      }
    }
  } else {
    # Vertical placement
    # Check if ship fits within grid boundaries
    if (row + length - 1 > n_row) {
      return(FALSE)
    }
    # Check if all cells are empty (no ship or buffer)
    for (r in row:(row + length - 1)) {
      if (grid[r, col] != 0) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

################################################################################
# Helper function: Place a ship on the grid
################################################################################

# Places a ship and marks the buffer zone around it
# Ship cells are marked with the ship length (2, 3, 4, or 5) for coloring
place_ship <- function(grid, row, col, length, horizontal) {
  n_row <- nrow(grid)
  n_col <- ncol(grid)
  
  # Determine ship cells
  if (horizontal) {
    ship_cells <- cbind(rep(row, length), col:(col + length - 1))
  } else {
    ship_cells <- cbind(row:(row + length - 1), rep(col, length))
  }
  
  # Mark ship cells with ship length (for coloring by size)
  for (i in seq_len(nrow(ship_cells))) {
    grid[ship_cells[i, 1], ship_cells[i, 2]] <- length
  }
  
  # Mark buffer zone (adjacent cells) with -1
  # Buffer is only horizontal and vertical neighbors (not diagonal)
  for (i in seq_len(nrow(ship_cells))) {
    r <- ship_cells[i, 1]
    c <- ship_cells[i, 2]
    
    # Check four directions: up, down, left, right
    neighbors <- list(c(r - 1, c), c(r + 1, c), c(r, c - 1), c(r, c + 1))
    
    for (neighbor in neighbors) {
      nr <- neighbor[1]
      nc <- neighbor[2]
      # Check if within bounds and empty
      if (nr >= 1 && nr <= n_row && nc >= 1 && nc <= n_col) {
        if (grid[nr, nc] == 0) {
          grid[nr, nc] <- -1  # Buffer zone
        }
      }
    }
  }
  
  return(grid)
}

################################################################################
# Helper function: Find all valid positions for a ship
################################################################################

find_valid_positions <- function(grid, length, horizontal) {
  n_row <- nrow(grid)
  n_col <- ncol(grid)
  
  valid_positions <- list()
  
  for (r in 1:n_row) {
    for (c in 1:n_col) {
      if (can_place_ship(grid, r, c, length, horizontal)) {
        valid_positions[[length(valid_positions) + 1]] <- c(r, c)
      }
    }
  }
  
  return(valid_positions)
}

################################################################################
# Helper function: Plot the combat area
################################################################################

plot_combat_area <- function(grid) {
  n_row <- nrow(grid)
  n_col <- ncol(grid)
  
  # Save old margins and set new ones to prevent label overflow
  old_par <- par(no.readonly = TRUE)
  par(mar = c(1, 3, 4, 1))
  
  # Create empty plot with exact limits (xaxs/yaxs = "i" removes extra padding)
  plot(NA, xlim = c(0.5, n_col + 0.5), ylim = c(0.5, n_row + 0.5),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", asp = 1,
       xaxs = "i", yaxs = "i", bty = "n", main = "Combat Area")
  
  # Add dotted grid lines using segments (constrained to grid boundaries)
  # Horizontal lines
  for (y in seq(0.5, n_row + 0.5, by = 1)) {
    segments(0.5, y, n_col + 0.5, y, col = "gray", lty = 3)
  }
  # Vertical lines
  for (x in seq(0.5, n_col + 0.5, by = 1)) {
    segments(x, 0.5, x, n_row + 0.5, col = "gray", lty = 3)
  }
  
  # Add column numbers at the top (using text to ensure all labels appear)
  text(x = 1:n_col, y = n_row + 0.75, labels = 1:n_col, xpd = TRUE, cex = 0.9)
  
  # Add row letters on the left (using text to ensure all labels appear)
  row_labels <- LETTERS[1:n_row]
  text(x = 0, y = n_row:1, labels = row_labels, xpd = TRUE, cex = 0.9)
  
  # Draw ships (cells with value 2, 3, 4, or 5 representing ship size)
  # Color ships using default R palette: size 2 = color 2, size 3 = color 3, etc.
  for (r in 1:n_row) {
    for (c in 1:n_col) {
      ship_size <- grid[r, c]
      if (ship_size %in% c(2, 3, 4, 5)) {
        # Plot row from top to bottom, so invert y coordinate
        y <- n_row - r + 1
        # Use pch = 16 (solid circle) with color = ship_size (from default palette)
        points(c, y, pch = 16, cex = 2.5, col = ship_size)
      }
    }
  }
  
  # Restore old margins
  par(old_par)
}

################################################################################
# Main function: combat_area
################################################################################

combat_area <- function(n_row = 10, n_col = n_row, n5 = 1, n4 = 2, n3 = 3, n2 = 4) {
  
  # Create empty grid (0 = empty)
  grid <- matrix(0, nrow = n_row, ncol = n_col)
  
  # Create list of ships to place: (count, length)
  ships <- c(rep(5, n5), rep(4, n4), rep(3, n3), rep(2, n2))
  
  # Place ships from largest to smallest (already sorted)
  for (ship_length in ships) {
    
    # Randomly choose initial orientation
    # TRUE = horizontal, FALSE = vertical
    orientation <- sample(c(TRUE, FALSE), 1)
    
    # Try to find valid position in chosen orientation
    valid_positions <- find_valid_positions(grid, ship_length, orientation)
    
    if (length(valid_positions) == 0) {
      # Try the other orientation
      orientation <- !orientation
      valid_positions <- find_valid_positions(grid, ship_length, orientation)
      
      if (length(valid_positions) == 0) {
        # No valid position in either orientation
        stop(paste0("Ship of length ", ship_length, " does not have enough space."))
      }
    }
    
    # Randomly choose a valid position
    pos_idx <- sample(seq_along(valid_positions), 1)
    pos <- valid_positions[[pos_idx]]
    
    # Place the ship
    grid <- place_ship(grid, pos[1], pos[2], ship_length, orientation)
  }
  
  # Plot the combat area
  plot_combat_area(grid)
  
  # Return invisibly (the function returns a plot)
  invisible(grid)
}

################################################################################
# Test the function with default parameters
################################################################################

# Test with default parameters
combat_area()

# Test with custom parameters
combat_area(n_row = 12, n_col = 13, n5 = 3)

# Test with smaller grid (might fail due to space constraints)
#combat_area(n_row = 5, n_col = 5, n5 = 1, n4 = 1, n3 = 1, n2 = 1)


