# ============================================================================
# Analysis Template
# Description: Boilerplate for data analysis workflows
# Usage: Rscript scripts/analysis_template.R
# ============================================================================

# Load libraries
library(tidyverse)
library(plotly)

# Configuration
DATA_DIR <- "data/raw"
OUTPUT_DIR <- "output"

# ============================================================================
# Data Loading
# ============================================================================

load_data <- function(filename) {
  filepath <- file.path(DATA_DIR, filename)
  if (!file.exists(filepath)) {
    stop(paste("File not found:", filepath))
  }
  read_csv(filepath, show_col_types = FALSE)
}

# ============================================================================
# Data Validation
# ============================================================================

validate_data <- function(df, required_cols = NULL) {
  # Check for empty dataframe
  if (nrow(df) == 0) {
    warning("Dataframe is empty")
    return(FALSE)
  }

  # Check for required columns
  if (!is.null(required_cols)) {
    missing <- setdiff(required_cols, names(df))
    if (length(missing) > 0) {
      stop(paste("Missing required columns:", paste(missing, collapse = ", ")))
    }
  }

  # Report summary
  cat("Rows:", nrow(df), "\n")
  cat("Columns:", ncol(df), "\n")
  cat("Column names:", paste(names(df), collapse = ", "), "\n")

  TRUE
}

# ============================================================================
# Data Processing
# ============================================================================

process_data <- function(df) {
  df %>%
    # Add your transformations here
    mutate(across(where(is.character), as.factor))
}

# ============================================================================
# Visualization
# ============================================================================

create_summary_plot <- function(df, x_var, y_var, color_var = NULL) {
  p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    theme_minimal()

  if (!is.null(color_var)) {
    p <- p + geom_point(aes(color = .data[[color_var]]))
  } else {
    p <- p + geom_point()
  }

  ggplotly(p)
}

# ============================================================================
# Export Functions
# ============================================================================

save_plot <- function(plot, filename, width = 10, height = 6) {
  filepath <- file.path(OUTPUT_DIR, filename)
  ggsave(filepath, plot, width = width, height = height)
  cat("Saved:", filepath, "\n")
}

save_data <- function(df, filename) {
  filepath <- file.path("data/processed", filename)
  write_csv(df, filepath)
  cat("Saved:", filepath, "\n")
}

# ============================================================================
# Main Execution
# ============================================================================

main <- function() {
  cat("=== Analysis Template ===\n")

  # Example: Load and process iris data
  df <- load_data("iris.csv")
  validate_data(df)

  df_processed <- process_data(df)

  # Create visualization
  p <- create_summary_plot(df_processed, "Sepal.Length", "Sepal.Width", "Species")
  print(p)

  cat("\nAnalysis complete!\n")
}

# Run if executed directly
if (!interactive()) {
  main()
}
