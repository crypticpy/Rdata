# ============================================================================
# Title: Diabetes Prediction Model Building
# Author: Data Science Team
# Date: 2025-12-05
# Purpose: Build and compare logistic regression and random forest models
#          for diabetes prediction using engineered features
# Input: data/processed/diabetes_features.rds
# Output: output/models/logistic_model.rds
#         output/models/random_forest_model.rds
#         output/models/model_predictions.rds
#         output/models/model_summary.rds
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(broom)
library(ranger)

cat("============================================================\n")
cat("DIABETES PREDICTION MODEL BUILDING\n")
cat("============================================================\n\n")

# Load data -------------------------------------------------------------------
cat("Loading feature-engineered data...\n")
features <- readRDS("data/processed/diabetes_features.rds")

train <- features$train
test <- features$test
feature_names <- features$feature_names

cat(sprintf("Training set: %s observations, %s variables\n",
            format(nrow(train), big.mark = ","), ncol(train)))
cat(sprintf("Test set: %s observations, %s variables\n\n",
            format(nrow(test), big.mark = ","), ncol(test)))

# Check target distribution
cat("Target variable distribution (Training):\n")
train |>
  count(diabetes_binary) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()
cat("\n")

# ============================================================================
# MODEL 1: LOGISTIC REGRESSION (BASELINE)
# ============================================================================

cat("============================================================\n")
cat("MODEL 1: LOGISTIC REGRESSION\n")
cat("============================================================\n\n")

# Fit logistic regression with key predictors
cat("Fitting logistic regression model...\n")
start_time <- Sys.time()

logistic_model <- glm(
  diabetes_binary ~ gen_hlth + bmi + age + high_bp + high_chol +
    cardiovascular_risk + lifestyle_score + health_burden +
    diff_walk + sex + education + income + age_bmi_risk +
    bmi_squared + high_risk_flag,
  data = train,
  family = binomial(link = "logit")
)

logistic_time <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Training completed in %.2f seconds\n\n", logistic_time))

# Model summary
cat("Model Summary:\n")
cat(sprintf("  AIC: %.2f\n", AIC(logistic_model)))
cat(sprintf("  Null deviance: %.2f on %d df\n",
            logistic_model$null.deviance, logistic_model$df.null))
cat(sprintf("  Residual deviance: %.2f on %d df\n",
            logistic_model$deviance, logistic_model$df.residual))

# Calculate McFadden's R-squared
null_ll <- logistic_model$null.deviance / -2
model_ll <- logistic_model$deviance / -2
mcfadden_r2 <- 1 - (model_ll / null_ll)
cat(sprintf("  McFadden's R-squared: %.4f\n\n", mcfadden_r2))

# Extract odds ratios with 95% CI
cat("Odds Ratios with 95% Confidence Intervals:\n")
logistic_coefs <- tidy(logistic_model, exponentiate = TRUE, conf.int = TRUE) |>
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    or_formatted = sprintf("%.3f (%.3f, %.3f)%s",
                           estimate, conf.low, conf.high, significance)
  ) |>
  arrange(desc(abs(log(estimate))))

logistic_coefs |>
  select(term, OR = estimate, ci_low = conf.low, ci_high = conf.high,
         p.value, significance) |>
  print(n = 20)

cat("\nSignificance: *** p<0.001, ** p<0.01, * p<0.05\n\n")

# Check for multicollinearity with VIF
cat("Variance Inflation Factors (VIF):\n")
# Calculate VIF manually since car package may not be available
# VIF = 1/(1-R^2) for each predictor regressed on others

# Alternative: use simple correlation check for key variables
vif_check <- function(model) {
  # Get model matrix without intercept
  X <- model.matrix(model)[, -1]

  # Calculate VIF for each variable
  vif_values <- sapply(1:ncol(X), function(i) {
    y <- X[, i]
    others <- X[, -i]
    r2 <- summary(lm(y ~ others))$r.squared
    1 / (1 - r2)
  })

  names(vif_values) <- colnames(X)
  return(vif_values)
}

# Suppress warnings for VIF calculation (can be computationally intensive)
vif_values <- tryCatch({
  vif_check(logistic_model)
}, error = function(e) {
  cat("  VIF calculation skipped (computational constraints)\n")
  NULL
})

if (!is.null(vif_values)) {
  vif_df <- tibble(
    variable = names(vif_values),
    vif = vif_values
  ) |>
    arrange(desc(vif))

  cat("  Top 10 VIF values:\n")
  print(head(vif_df, 10))

  high_vif <- sum(vif_values > 5)
  cat(sprintf("\n  Variables with VIF > 5 (potential multicollinearity): %d\n", high_vif))
  if (high_vif > 0) {
    cat("  Note: Some multicollinearity expected with engineered features\n")
  }
}

cat("\n")

# ============================================================================
# MODEL 2: RANDOM FOREST
# ============================================================================

cat("============================================================\n")
cat("MODEL 2: RANDOM FOREST\n")
cat("============================================================\n\n")

# Prepare data for random forest
# Remove redundant columns (original diabetes_012 since we use diabetes_binary)
rf_train <- train |>
  select(-diabetes_012) |>
  mutate(diabetes_binary = factor(diabetes_binary, levels = c(0, 1)))

rf_test <- test |>
  select(-diabetes_012) |>
  mutate(diabetes_binary = factor(diabetes_binary, levels = c(0, 1)))

# Set number of threads
n_threads <- max(1, parallel::detectCores() - 1)
cat(sprintf("Using %d threads for parallel processing\n", n_threads))

# Fit random forest
cat("Fitting random forest model (500 trees)...\n")
start_time <- Sys.time()

set.seed(42)
rf_model <- ranger(
  diabetes_binary ~ .,
  data = rf_train,
  num.trees = 500,
  importance = "permutation",
  probability = TRUE,
  seed = 42,
  num.threads = n_threads,
  verbose = FALSE
)

rf_time <- difftime(Sys.time(), start_time, units = "secs")
cat(sprintf("Training completed in %.2f seconds\n\n", rf_time))

# Model summary
cat("Random Forest Summary:\n")
cat(sprintf("  Number of trees: %d\n", rf_model$num.trees))
cat(sprintf("  Number of features: %d\n", rf_model$num.independent.variables))
cat(sprintf("  Mtry (features per split): %d\n", rf_model$mtry))
cat(sprintf("  Min node size: %d\n", rf_model$min.node.size))
cat(sprintf("  OOB prediction error: %.4f\n\n", rf_model$prediction.error))

# Extract and display variable importance
cat("Variable Importance (Permutation):\n")
importance_df <- tibble(
  variable = names(rf_model$variable.importance),
  importance = rf_model$variable.importance
) |>
  arrange(desc(importance)) |>
  mutate(rank = row_number())

print(importance_df, n = 20)

cat("\n")

# ============================================================================
# MODEL PREDICTIONS ON TEST SET
# ============================================================================

cat("============================================================\n")
cat("GENERATING PREDICTIONS ON TEST SET\n")
cat("============================================================\n\n")

# Logistic regression predictions
cat("Generating logistic regression predictions...\n")
logistic_pred_prob <- predict(logistic_model, newdata = test, type = "response")

# Random forest predictions
cat("Generating random forest predictions...\n")
rf_predictions <- predict(rf_model, data = rf_test, type = "response")
rf_pred_prob <- rf_predictions$predictions[, 2]  # Probability of class 1

# Create predictions data frame
predictions_df <- tibble(
  actual = test$diabetes_binary,
  logistic_prob = logistic_pred_prob,
  rf_prob = rf_pred_prob,
  logistic_pred = as.integer(logistic_pred_prob >= 0.5),
  rf_pred = as.integer(rf_pred_prob >= 0.5)
)

cat(sprintf("\nPredictions generated for %s test observations\n",
            format(nrow(predictions_df), big.mark = ",")))

# Quick accuracy check
logistic_acc <- mean(predictions_df$logistic_pred == predictions_df$actual)
rf_acc <- mean(predictions_df$rf_pred == predictions_df$actual)

cat("\nPreliminary Accuracy (threshold = 0.5):\n")
cat(sprintf("  Logistic Regression: %.4f (%.2f%%)\n", logistic_acc, logistic_acc * 100))
cat(sprintf("  Random Forest: %.4f (%.2f%%)\n", rf_acc, rf_acc * 100))

# Prediction distribution summary
cat("\nPredicted Probability Summary:\n")
cat("\n  Logistic Regression:\n")
summary(predictions_df$logistic_prob) |> print()

cat("\n  Random Forest:\n")
summary(predictions_df$rf_prob) |> print()

cat("\n")

# ============================================================================
# SAVE OUTPUTS
# ============================================================================

cat("============================================================\n")
cat("SAVING MODEL OUTPUTS\n")
cat("============================================================\n\n")

output_dir <- "output/models"

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save logistic regression model
logistic_path <- file.path(output_dir, "logistic_model.rds")
saveRDS(logistic_model, logistic_path)
cat(sprintf("Saved: %s\n", logistic_path))

# Save random forest model
rf_path <- file.path(output_dir, "random_forest_model.rds")
saveRDS(rf_model, rf_path)
cat(sprintf("Saved: %s\n", rf_path))

# Save predictions
predictions_list <- list(
  predictions = predictions_df,
  test_actuals = test$diabetes_binary,
  logistic_probabilities = logistic_pred_prob,
  rf_probabilities = rf_pred_prob,
  generated_at = Sys.time()
)
predictions_path <- file.path(output_dir, "model_predictions.rds")
saveRDS(predictions_list, predictions_path)
cat(sprintf("Saved: %s\n", predictions_path))

# Create comprehensive model summary
model_summary <- list(
  # Logistic regression summary
  logistic = list(
    coefficients = logistic_coefs,
    aic = AIC(logistic_model),
    bic = BIC(logistic_model),
    null_deviance = logistic_model$null.deviance,
    residual_deviance = logistic_model$deviance,
    mcfadden_r2 = mcfadden_r2,
    n_obs = nrow(train),
    training_time_secs = as.numeric(logistic_time),
    test_accuracy = logistic_acc,
    vif_values = if (exists("vif_values") && !is.null(vif_values)) vif_values else NULL
  ),

  # Random forest summary
  random_forest = list(
    variable_importance = importance_df,
    num_trees = rf_model$num.trees,
    mtry = rf_model$mtry,
    min_node_size = rf_model$min.node.size,
    oob_error = rf_model$prediction.error,
    n_obs = nrow(train),
    training_time_secs = as.numeric(rf_time),
    test_accuracy = rf_acc
  ),

  # Dataset info
  data_info = list(
    train_n = nrow(train),
    test_n = nrow(test),
    n_features = length(feature_names),
    feature_names = feature_names
  ),

  # Metadata
  created_at = Sys.time(),
  r_version = R.version.string,
  packages = list(
    ranger = packageVersion("ranger"),
    broom = packageVersion("broom")
  )
)

summary_path <- file.path(output_dir, "model_summary.rds")
saveRDS(model_summary, summary_path)
cat(sprintf("Saved: %s\n", summary_path))

cat("\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("============================================================\n")
cat("MODEL BUILDING COMPLETE\n")
cat("============================================================\n\n")

cat("Models Built:\n")
cat("  1. Logistic Regression (Baseline)\n")
cat(sprintf("     - McFadden's R-squared: %.4f\n", mcfadden_r2))
cat(sprintf("     - AIC: %.2f\n", AIC(logistic_model)))
cat(sprintf("     - Test Accuracy: %.2f%%\n", logistic_acc * 100))
cat("\n")
cat("  2. Random Forest\n")
cat(sprintf("     - OOB Error: %.4f\n", rf_model$prediction.error))
cat(sprintf("     - Test Accuracy: %.2f%%\n", rf_acc * 100))
cat("\n")

cat("Top 5 Predictors (by Random Forest Importance):\n")
importance_df |>
  head(5) |>
  mutate(display = sprintf("  %d. %s (%.4f)", rank, variable, importance)) |>
  pull(display) |>
  cat(sep = "\n")

cat("\n\nOutput Files:\n")
cat(sprintf("  - %s\n", logistic_path))
cat(sprintf("  - %s\n", rf_path))
cat(sprintf("  - %s\n", predictions_path))
cat(sprintf("  - %s\n", summary_path))

cat("\n============================================================\n")
cat("Next step: Run model evaluation script (15_model_evaluation.R)\n")
cat("============================================================\n")
