# ============================================================================
# 01_load_data.R
# Description: Example script for loading and exploring datasets
# Usage: Rscript scripts/01_load_data.R
# ============================================================================

library(tidyverse)

# ============================================================================
# Load Sample Datasets
# ============================================================================

cat("Loading sample datasets...\n\n")

# Load iris dataset
iris_df <- read_csv("data/raw/iris.csv", show_col_types = FALSE)
cat("=== Iris Dataset ===\n")
cat("Dimensions:", nrow(iris_df), "rows x", ncol(iris_df), "columns\n")
cat("Columns:", paste(names(iris_df), collapse = ", "), "\n")
cat("\nSummary:\n")
print(summary(iris_df))

cat("\n")

# Load mtcars dataset
mtcars_df <- read_csv("data/raw/mtcars.csv", show_col_types = FALSE)
cat("=== MTCars Dataset ===\n")
cat("Dimensions:", nrow(mtcars_df), "rows x", ncol(mtcars_df), "columns\n")
cat("Columns:", paste(names(mtcars_df), collapse = ", "), "\n")
cat("\nSummary:\n")
print(summary(mtcars_df))

# ============================================================================
# Quick Data Exploration
# ============================================================================

cat("\n=== Quick Analysis ===\n")

# Iris: Species counts
cat("\nIris species distribution:\n")
iris_df %>%
  count(Species) %>%
  print()

# MTCars: Average MPG by cylinder count
cat("\nMTCars average MPG by cylinders:\n")
mtcars_df %>%
  group_by(cyl) %>%
  summarise(
    avg_mpg = mean(mpg),
    count = n(),
    .groups = "drop"
  ) %>%
  print()

cat("\nData loading complete!\n")
