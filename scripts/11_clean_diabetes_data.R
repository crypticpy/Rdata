# ============================================================================
# Title: Clean Diabetes Health Indicators Dataset
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Clean and prepare BRFSS 2015 diabetes health indicators data
#          with proper factor labels and derived variables
# Input: data/raw/diabetes_012_health_indicators_BRFSS2015.csv
# Output: data/processed/diabetes_clean.rds
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)

# Helper function: convert to snake_case (replaces janitor::clean_names)
to_snake_case <- function(x) {
  x |>
    str_replace_all("([a-z])([A-Z])", "\\1_\\2") |>
    str_replace_all("([A-Z]+)([A-Z][a-z])", "\\1_\\2") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

# Load data -------------------------------------------------------------------
message("Loading raw data...")
raw_data <- read_csv(
  "data/raw/diabetes_012_health_indicators_BRFSS2015.csv",
  show_col_types = FALSE
)

message(sprintf("Raw data dimensions: %d rows x %d columns",
                nrow(raw_data), ncol(raw_data)))

# Clean column names ----------------------------------------------------------
message("Cleaning column names to snake_case...")
new_names <- to_snake_case(names(raw_data))
clean_data <- raw_data
names(clean_data) <- new_names

# Display column name mapping
name_mapping <- tibble(
  original = names(raw_data),
  cleaned = names(clean_data)
)
print(name_mapping, n = Inf)

# Define factor labels --------------------------------------------------------

# Diabetes status (0 = no diabetes, 1 = prediabetes, 2 = diabetes)
diabetes_labels <- c(
  "0" = "No Diabetes",
  "1" = "Prediabetes",
  "2" = "Diabetes"
)

# Age categories (1-13)
age_labels <- c(
  "1"  = "18-24",
  "2"  = "25-29",
  "3"  = "30-34",
  "4"  = "35-39",
  "5"  = "40-44",
  "6"  = "45-49",

  "7"  = "50-54",
  "8"  = "55-59",
  "9"  = "60-64",
  "10" = "65-69",
  "11" = "70-74",
  "12" = "75-79",
  "13" = "80+"
)

# General health (1 = excellent to 5 = poor)
gen_hlth_labels <- c(
  "1" = "Excellent",
  "2" = "Very Good",
  "3" = "Good",
  "4" = "Fair",
  "5" = "Poor"
)

# Education levels (1-6)
education_labels <- c(
  "1" = "Never attended",
  "2" = "Elementary",
  "3" = "Some high school",
  "4" = "High school graduate",
  "5" = "Some college",
  "6" = "College graduate"
)

# Income levels (1-8)
income_labels <- c(
  "1" = "<$10k",
  "2" = "$10k-$15k",
  "3" = "$15k-$20k",
  "4" = "$20k-$25k",
  "5" = "$25k-$35k",
  "6" = "$35k-$50k",
  "7" = "$50k-$75k",
  "8" = ">$75k"
)

# Sex labels (0 = female, 1 = male)
sex_labels <- c(
  "0" = "Female",
  "1" = "Male"
)

# BMI category breaks and labels
bmi_breaks <- c(-Inf, 18.5, 25, 30, 35, 40, Inf)
bmi_labels <- c(
  "Underweight",
  "Normal",
  "Overweight",
  "Obese I",
  "Obese II",
  "Obese III"
)

# Create factor labels and derived variables ----------------------------------
message("Creating factor labels and derived variables...")

diabetes_clean <- clean_data |>
  mutate(
    # Target variable with labels
    diabetes_status = factor(
      diabetes_012,
      levels = c(0, 1, 2),
      labels = c("No Diabetes", "Prediabetes", "Diabetes")
    ),

    # Age group labels
    age_group = factor(
      age,
      levels = 1:13,
      labels = age_labels
    ),

    # General health labels
    gen_hlth_label = factor(
      gen_hlth,
      levels = 1:5,
      labels = gen_hlth_labels
    ),

    # Education labels
    education_label = factor(
      education,
      levels = 1:6,
      labels = education_labels
    ),

    # Income labels
    income_label = factor(
      income,
      levels = 1:8,
      labels = income_labels
    ),

    # Sex labels
    sex_label = factor(
      sex,
      levels = c(0, 1),
      labels = c("Female", "Male")
    ),

    # BMI category (derived variable)
    bmi_category = cut(
      bmi,
      breaks = bmi_breaks,
      labels = bmi_labels,
      right = FALSE
    ),

    # Convert binary variables to logical for clarity
    across(
      c(high_bp, high_chol, chol_check, smoker, stroke,
        heart_diseaseor_attack, phys_activity, fruits, veggies,
        hvy_alcohol_consump, any_healthcare, no_docbc_cost,
        diff_walk),
      ~ as.integer(.)
    )
  )

# Validate data integrity -----------------------------------------------------
message("\n--- Data Integrity Validation ---")

# Check for missing values
missing_counts <- colSums(is.na(diabetes_clean))
total_missing <- sum(missing_counts)

if (total_missing == 0) {
  message("PASS: No missing values detected")
} else {
  message(sprintf("WARNING: %d missing values detected", total_missing))
  print(missing_counts[missing_counts > 0])
}

# Check for duplicates
n_duplicates <- sum(duplicated(diabetes_clean))
message(sprintf("Duplicate rows: %d", n_duplicates))

# Validate factor levels
message("\n--- Factor Level Validation ---")

# Diabetes status
message(sprintf("Diabetes status levels: %s",
                paste(levels(diabetes_clean$diabetes_status), collapse = ", ")))
print(table(diabetes_clean$diabetes_status, useNA = "ifany"))

# Age groups
message(sprintf("\nAge group levels: %d categories",
                length(levels(diabetes_clean$age_group))))

# BMI category
message("\nBMI category distribution:")
print(table(diabetes_clean$bmi_category, useNA = "ifany"))

# Print summary statistics ----------------------------------------------------
message("\n--- Summary Statistics ---")
message(sprintf("Final dataset dimensions: %d rows x %d columns",
                nrow(diabetes_clean), ncol(diabetes_clean)))

# Numeric summary
message("\nNumeric variable summaries:")
diabetes_clean |>
  select(bmi, ment_hlth, phys_hlth) |>
  summary() |>
  print()

# Categorical summaries
message("\nDiabetes status breakdown:")
diabetes_clean |>
  count(diabetes_status) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

message("\nGeneral health breakdown:")
diabetes_clean |>
  count(gen_hlth_label) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

message("\nBMI category breakdown:")
diabetes_clean |>
  count(bmi_category) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

message("\nSex breakdown:")
diabetes_clean |>
  count(sex_label) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

# Document column types
message("\n--- Column Types ---")
col_types <- sapply(diabetes_clean, class)
type_summary <- tibble(
  column = names(col_types),
  type = sapply(col_types, function(x) paste(x, collapse = ", "))
)
print(type_summary, n = Inf)

# Save cleaned data -----------------------------------------------------------
message("\nSaving cleaned data to data/processed/diabetes_clean.rds...")
saveRDS(diabetes_clean, "data/processed/diabetes_clean.rds")

# Verify saved file
saved_data <- readRDS("data/processed/diabetes_clean.rds")
message(sprintf("Saved file verified: %d rows x %d columns",
                nrow(saved_data), ncol(saved_data)))

message("\n=== Data cleaning complete ===")
message("Output file: data/processed/diabetes_clean.rds")
