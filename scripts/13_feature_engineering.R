# ============================================================================
# Title: Feature Engineering for Diabetes Prediction
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Create derived features, composite scores, and train/test split
#          for machine learning modeling on diabetes dataset
# Input: data/processed/diabetes_clean.rds
# Output: data/processed/diabetes_features.rds
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(rsample)

# Load data -------------------------------------------------------------------
cat("Loading cleaned diabetes dataset...\n")
df <- readRDS("data/processed/diabetes_clean.rds")
cat("Original dimensions:", nrow(df), "rows,", ncol(df), "columns\n\n")

# Feature Engineering ---------------------------------------------------------
cat("Creating engineered features...\n\n")

df_features <- df |>
  mutate(
    # 1. Binary target for binary classification
    diabetes_binary = as.integer(diabetes_012 > 0),

    # 2. Composite Risk Scores ------------------------------------------------

    # Cardiovascular risk: sum of CV-related conditions (0-4 scale)
    cardiovascular_risk = high_bp + high_chol + stroke + heart_diseaseor_attack,

    # Lifestyle score: healthy behaviors minus risky ones (-1 to 3 scale)
    lifestyle_score = phys_activity + fruits + veggies - hvy_alcohol_consump,

    # Health burden: normalized mental + physical health issues (0-2 scale)
    health_burden = (ment_hlth + phys_hlth) / 30,

    # Access barrier: lack of healthcare or cost barrier (0/1)
    access_barrier = as.integer(any_healthcare == 0 | no_docbc_cost == 1),

    # SES score: socioeconomic proxy (1-7 scale)
    ses_score = (education + income) / 2,

    # 3. Interaction Features -------------------------------------------------

    # Age-BMI risk: normalized interaction
    age_bmi_risk = (age / 13) * (bmi / 50),

    # Age amplified by high blood pressure
    age_high_bp = age * high_bp,

    # BMI effect when physically inactive (obesity without exercise)
    bmi_phys_activity = bmi * (1 - phys_activity),

    # 4. High-Risk Flags ------------------------------------------------------

    # High risk if: CV risk >= 2 OR BMI >= 35 OR poor general health
    high_risk_flag = as.integer(
      cardiovascular_risk >= 2 | bmi >= 35 | gen_hlth >= 4
    ),

    # Multiple comorbidities: 3+ binary conditions present
    multiple_comorbidities = as.integer(
      (high_bp + high_chol + stroke + heart_diseaseor_attack +
       smoker + diff_walk + hvy_alcohol_consump) >= 3
    ),

    # 5. Polynomial/Non-linear Features ---------------------------------------

    # BMI squared to capture non-linear effect
    bmi_squared = bmi^2,

    # Age squared for non-linear age effects
    age_squared = age^2
  )

# Print summary of new features -----------------------------------------------
cat("--- New Features Summary ---\n\n")

new_features <- c(
  "diabetes_binary", "cardiovascular_risk", "lifestyle_score",
  "health_burden", "access_barrier", "ses_score", "age_bmi_risk",
  "age_high_bp", "bmi_phys_activity", "high_risk_flag",
  "multiple_comorbidities", "bmi_squared", "age_squared"
)

cat("Binary Target:\n")
cat("  diabetes_binary distribution:\n")
print(table(df_features$diabetes_binary, useNA = "ifany"))
cat("\n")

cat("Composite Scores (summary statistics):\n")
df_features |>
  select(cardiovascular_risk, lifestyle_score, health_burden,
         access_barrier, ses_score) |>
  summary() |>
  print()

cat("\nInteraction Features (summary statistics):\n")
df_features |>
  select(age_bmi_risk, age_high_bp, bmi_phys_activity) |>
  summary() |>
  print()

cat("\nHigh-Risk Flags:\n")
cat("  high_risk_flag: ", sum(df_features$high_risk_flag),
    " (", round(100 * mean(df_features$high_risk_flag), 1), "%)\n", sep = "")
cat("  multiple_comorbidities: ", sum(df_features$multiple_comorbidities),
    " (", round(100 * mean(df_features$multiple_comorbidities), 1), "%)\n\n",
    sep = "")

cat("Polynomial Features:\n")
df_features |>
  select(bmi_squared, age_squared) |>
  summary() |>
  print()

# 6. Data Splitting for Modeling ----------------------------------------------
cat("\n--- Creating Train/Test Split ---\n")
set.seed(42)

# Stratified split by diabetes_binary to handle class imbalance
split_obj <- initial_split(df_features, prop = 0.80, strata = diabetes_binary)

train_data <- training(split_obj)
test_data <- testing(split_obj)

cat("Training set: ", nrow(train_data), " rows (",
    round(100 * nrow(train_data) / nrow(df_features), 1), "%)\n", sep = "")
cat("Test set:     ", nrow(test_data), " rows (",
    round(100 * nrow(test_data) / nrow(df_features), 1), "%)\n\n", sep = "")

# Verify stratification
cat("Stratification check (diabetes_binary proportion):\n")
cat("  Full data: ", round(100 * mean(df_features$diabetes_binary), 2), "%\n", sep = "")
cat("  Training:  ", round(100 * mean(train_data$diabetes_binary), 2), "%\n", sep = "")
cat("  Test:      ", round(100 * mean(test_data$diabetes_binary), 2), "%\n\n", sep = "")

# 7. Feature Selection Prep ---------------------------------------------------
cat("--- Preparing Feature Matrix ---\n")

# Define modeling columns (exclude redundant factor labels)
label_columns <- c(
  "diabetes_status", "age_group", "gen_hlth_label",
  "education_label", "income_label", "sex_label", "bmi_category"
)

# Get feature column names (all columns except labels)
feature_names <- names(df_features)[!names(df_features) %in% label_columns]

cat("Total columns in full dataset:", ncol(df_features), "\n")
cat("Label columns removed:", length(label_columns), "\n")
cat("Feature columns for modeling:", length(feature_names), "\n\n")

cat("Feature columns:\n")
print(feature_names)

# Create clean versions for modeling (without factor labels)
train_features <- train_data |> select(all_of(feature_names))
test_features <- test_data |> select(all_of(feature_names))

# Prepare output list ---------------------------------------------------------
cat("\n--- Saving Output ---\n")

output <- list(
  full_data = df_features,
  train = train_features,
  test = test_features,
  feature_names = feature_names,
  split = split_obj,
  metadata = list(
    created = Sys.time(),
    n_rows = nrow(df_features),
    n_features = length(feature_names),
    train_size = nrow(train_features),
    test_size = nrow(test_features),
    seed = 42,
    new_features = new_features
  )
)

# Save outputs ----------------------------------------------------------------
saveRDS(output, "data/processed/diabetes_features.rds")
cat("Saved to: data/processed/diabetes_features.rds\n\n")

# Final summary ---------------------------------------------------------------
cat("============================================================\n")
cat("Feature Engineering Complete\n")
cat("============================================================\n")
cat("New features created:   ", length(new_features), "\n")
cat("Total feature columns:  ", length(feature_names), "\n")
cat("Training samples:       ", nrow(train_features), "\n")
cat("Test samples:           ", nrow(test_features), "\n")
cat("Class balance (binary): ",
    round(100 * mean(df_features$diabetes_binary == 0), 1), "% negative / ",
    round(100 * mean(df_features$diabetes_binary == 1), 1), "% positive\n",
    sep = "")
cat("============================================================\n")
