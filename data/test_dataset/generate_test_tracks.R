# Script to generate test track files for Morris Water Maze analysis
# These files simulate different swimming strategies observed in MWM experiments

# Load required libraries
library(dplyr)

# Set random seed for reproducibility
set.seed(123)

# Define arena parameters (in cm)
arena_radius <- 75
platform_radius <- 10
platform_x <- 25  # Platform in NE quadrant
platform_y <- 25

# Function to generate timestamps
generate_time <- function(n_points, max_time = 60) {
  seq(0, max_time, length.out = n_points)
}

# Function to generate random track with direct path to platform (spatial strategy)
generate_direct_path <- function(start_angle, n_points = 50) {
  # Start position on edge of pool
  start_x <- arena_radius * cos(start_angle)
  start_y <- arena_radius * sin(start_angle)
  
  # Generate path with some random variation
  # More direct path to platform with some natural variation
  time <- generate_time(n_points)
  
  # Linear interpolation from start to platform with noise
  t <- seq(0, 1, length.out = n_points)
  x <- start_x + t * (platform_x - start_x) + rnorm(n_points, 0, 5)
  y <- start_y + t * (platform_y - start_y) + rnorm(n_points, 0, 5)
  
  data.frame(Time = time, X = x, Y = y)
}

# Function to generate thigmotaxic swimming pattern (wall-hugging)
generate_thigmotaxic <- function(n_points = 100) {
  # Generate points along the edge of the pool
  time <- generate_time(n_points)
  angles <- seq(0, 2*pi, length.out = n_points) + rnorm(n_points, 0, 0.1)
  
  # Add small variation in radius to simulate natural swimming
  radius <- arena_radius * 0.9 + rnorm(n_points, 0, 2)
  
  x <- radius * cos(angles)
  y <- radius * sin(angles)
  
  data.frame(Time = time, X = x, Y = y)
}

# Function to generate random search pattern
generate_random_search <- function(n_points = 80) {
  time <- generate_time(n_points)
  
  # Random walk within the arena
  x <- vector("numeric", n_points)
  y <- vector("numeric", n_points)
  
  # Start at a random position
  x[1] <- runif(1, -arena_radius * 0.8, arena_radius * 0.8)
  y[1] <- runif(1, -arena_radius * 0.8, arena_radius * 0.8)
  
  # Random walk with momentum
  for (i in 2:n_points) {
    # Calculate new position with momentum from previous step
    dx <- rnorm(1, 0, 10)
    dy <- rnorm(1, 0, 10)
    
    x[i] <- x[i-1] + dx
    y[i] <- y[i-1] + dy
    
    # Ensure staying within arena
    dist <- sqrt(x[i]^2 + y[i]^2)
    if (dist > arena_radius * 0.9) {
      # Bounce back from wall
      angle <- atan2(y[i], x[i])
      x[i] <- arena_radius * 0.85 * cos(angle)
      y[i] <- arena_radius * 0.85 * sin(angle)
    }
  }
  
  data.frame(Time = time, X = x, Y = y)
}

# Function to generate scanning strategy (circling pattern)
generate_scanning <- function(n_points = 70) {
  time <- generate_time(n_points)
  
  # Start at a random position
  center_x <- runif(1, -arena_radius * 0.5, arena_radius * 0.5)
  center_y <- runif(1, -arena_radius * 0.5, arena_radius * 0.5)
  
  # Spiral pattern from center outwards
  t <- seq(0, 4*pi, length.out = n_points)
  radius <- seq(5, 30, length.out = n_points)
  
  x <- center_x + radius * cos(t) + rnorm(n_points, 0, 3)
  y <- center_y + radius * sin(t) + rnorm(n_points, 0, 3)
  
  # Ensure within arena
  for (i in 1:n_points) {
    dist <- sqrt(x[i]^2 + y[i]^2)
    if (dist > arena_radius * 0.9) {
      angle <- atan2(y[i], x[i])
      x[i] <- arena_radius * 0.85 * cos(angle)
      y[i] <- arena_radius * 0.85 * sin(angle)
    }
  }
  
  data.frame(Time = time, X = x, Y = y)
}

# Function to generate chaining strategy (concentric circular swimming)
generate_chaining <- function(n_points = 90) {
  time <- generate_time(n_points)
  
  # Concentric circular pattern at a fixed distance from center
  chain_radius <- arena_radius * 0.6
  t <- seq(0, 2*pi, length.out = n_points)
  
  x <- chain_radius * cos(t) + rnorm(n_points, 0, 5)
  y <- chain_radius * sin(t) + rnorm(n_points, 0, 5)
  
  data.frame(Time = time, X = x, Y = y)
}

# Generate and save track files
for (i in 1:32) {
  # Choose a strategy based on the track number
  # Improve learning over time (day 2 better than day 1)
  # Treatment group learns better than control
  
  track_num <- i
  is_day2 <- (track_num - 1) %/% 4 %% 2 == 1
  is_treatment <- ((track_num - 1) %/% 8) %% 2 == 1
  
  # Calculate probabilities for different strategies based on conditions
  if (is_day2 && is_treatment) {
    # Best performance: Day 2 + Treatment
    probs <- c(0.7, 0.1, 0.1, 0.05, 0.05) # High chance of direct path
  } else if (is_day2) {
    # Day 2 control still better than day 1
    probs <- c(0.4, 0.2, 0.2, 0.1, 0.1)
  } else if (is_treatment) {
    # Day 1 treatment better than day 1 control
    probs <- c(0.3, 0.2, 0.2, 0.15, 0.15)
  } else {
    # Day 1 control - worst performance
    probs <- c(0.1, 0.3, 0.3, 0.15, 0.15)
  }
  
  # Select strategy
  strategy <- sample(1:5, 1, prob = probs)
  
  # Generate track data based on strategy
  track_data <- switch(strategy,
                      generate_direct_path(runif(1, 0, 2*pi)),          # Spatial/direct
                      generate_random_search(),                         # Random search
                      generate_thigmotaxic(),                           # Thigmotaxis
                      generate_scanning(),                              # Scanning
                      generate_chaining()                               # Chaining
  )
  
  # Save track file
  filename <- sprintf("/home/santi/Projects/MWM_estrategias_busqueda/data/test_dataset/Track_%d.csv", i)
  write.csv(track_data, filename, row.names = FALSE)
  
  cat(sprintf("Generated Track_%d.csv\n", i))
}

cat("All test track files generated successfully!\n")
