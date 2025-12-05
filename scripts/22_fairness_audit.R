# ============================================================================
# Title: Comprehensive Fairness and Bias Audit for Diabetes Prediction Models
# Author: Data Science Team
# Date: 2025-12-05
# Purpose: Examine algorithmic equity across demographic groups using
#          fairness metrics critical for ML in healthcare
# Input: data/processed/diabetes_features.rds
#        output/models/random_forest_model.rds
# Output: output/figures/diabetes_fairness_*.png
#         output/fairness_audit_results.rds
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(DALEX)
library(fairmodels)
library(ranger)
library(patchwork)

cat("============================================================\n")
cat("COMPREHENSIVE FAIRNESS AND BIAS AUDIT\n")
cat("Diabetes Prediction Model - Algorithmic Equity Analysis\n")
cat("============================================================\n\n")

# Load data and models --------------------------------------------------------
cat("Loading data and trained models...\n")

features <- readRDS("data/processed/diabetes_features.rds")
rf_model <- readRDS("output/models/random_forest_model.rds")

# Use test data for fairness evaluation
test_data <- features$test
full_data <- features$full_data

cat(sprintf("Test data: %s observations\n", format(nrow(test_data), big.mark = ",")))
cat(sprintf("Full data: %s observations\n\n", format(nrow(full_data), big.mark = ",")))

# ============================================================================
# SECTION 1: PREPARE DATA WITH PROTECTED ATTRIBUTES
# ============================================================================

cat("============================================================\n")
cat("SECTION 1: PREPARING DATA WITH PROTECTED ATTRIBUTES\n")
cat("============================================================\n\n")

# Create protected attribute categories for test data
# Note: Test data has numeric codes, we need to create meaningful categories

prepare_fairness_data <- function(data) {
  data |>
    mutate(
      # Sex: 0 = Female (reference), 1 = Male
      sex_group = factor(
        ifelse(sex == 0, "Female", "Male"),
        levels = c("Female", "Male")
      ),

      # Age groups based on CDC age coding (1-13 representing 5-year intervals)
      # 1 = 18-24, 2 = 25-29, ..., 13 = 80+
      age_category = factor(
        case_when(
          age <= 5 ~ "Young (18-44)",      # Ages 1-5 map to 18-44
          age <= 9 ~ "Middle (45-64)",     # Ages 6-9 map to 45-64
          TRUE ~ "Senior (65+)"            # Ages 10-13 map to 65+
        ),
        levels = c("Young (18-44)", "Middle (45-64)", "Senior (65+)")
      ),

      # Income levels: 1-8 scale
      # 1 = <$10k, 2 = $10k-$15k, 3 = $15k-$20k, 4 = $20k-$25k
      # 5 = $25k-$35k, 6 = $35k-$50k, 7 = $50k-$75k, 8 = >$75k
      income_category = factor(
        case_when(
          income <= 4 ~ "Low (<$25K)",
          income <= 6 ~ "Medium ($25K-$50K)",
          TRUE ~ "High (>$50K)"
        ),
        levels = c("Low (<$25K)", "Medium ($25K-$50K)", "High (>$50K)")
      ),

      # Education levels: 1-6 scale
      # 1 = Never attended, 2 = Elementary, 3 = Some high school
      # 4 = HS Graduate, 5 = Some college, 6 = College graduate
      education_category = factor(
        case_when(
          education <= 3 ~ "Less than HS",
          education == 4 ~ "HS Graduate",
          education == 5 ~ "Some College",
          TRUE ~ "College Graduate"
        ),
        levels = c("Less than HS", "HS Graduate", "Some College", "College Graduate")
      )
    )
}

# Prepare test data with protected attributes
test_fairness <- prepare_fairness_data(test_data)

# Distribution of protected groups
cat("Protected Attribute Distributions (Test Data):\n\n")

cat("Sex:\n")
test_fairness |>
  count(sex_group) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

cat("\nAge Category:\n")
test_fairness |>
  count(age_category) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

cat("\nIncome Category:\n")
test_fairness |>
  count(income_category) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

cat("\nEducation Category:\n")
test_fairness |>
  count(education_category) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print()

cat("\n")

# ============================================================================
# SECTION 2: CREATE DALEX EXPLAINER
# ============================================================================

cat("============================================================\n")
cat("SECTION 2: CREATING DALEX EXPLAINER FOR RANDOM FOREST\n")
cat("============================================================\n\n")

# Prepare feature data (X) without target variable
# Keep original test_data structure for model predictions
X_test <- test_data |>
  select(-diabetes_012, -diabetes_binary)

y_test <- as.numeric(test_data$diabetes_binary)

# Define prediction function for ranger probability model
predict_function <- function(model, newdata) {
  pred <- predict(model, data = newdata)
  return(pred$predictions[, 2])  # Return probability of class 1 (diabetes)
}

cat("Creating DALEX explainer for Random Forest model...\n")

explainer_rf <- DALEX::explain(
  model = rf_model,
  data = X_test,
  y = y_test,
  predict_function = predict_function,
  label = "Random Forest",
  verbose = FALSE
)

cat("Explainer created successfully.\n")
cat(sprintf("  Features: %d\n", ncol(explainer_rf$data)))
cat(sprintf("  Observations: %s\n\n", format(nrow(explainer_rf$data), big.mark = ",")))

# ============================================================================
# SECTION 3: CALCULATE FAIRNESS METRICS BY PROTECTED ATTRIBUTES
# ============================================================================

cat("============================================================\n")
cat("SECTION 3: CALCULATING FAIRNESS METRICS\n")
cat("============================================================\n\n")

# Store all fairness results
fairness_results <- list()

# Function to calculate comprehensive fairness metrics
calculate_fairness_metrics <- function(explainer, protected_attr, privileged_group,
                                       data, attr_name) {

  cat(sprintf("\n--- Analyzing Fairness by %s ---\n", attr_name))
  cat(sprintf("Privileged (reference) group: %s\n\n", privileged_group))

  # Create fairness object
  fobject <- fairness_check(
    explainer,
    protected = protected_attr,
    privileged = privileged_group,
    cutoff = 0.5,
    verbose = FALSE
  )

  return(fobject)
}

# Calculate fairness for each protected attribute
# 3.1 Sex Fairness
cat("3.1 Sex-Based Fairness Analysis\n")
cat("================================\n")

fobject_sex <- calculate_fairness_metrics(
  explainer = explainer_rf,
  protected_attr = test_fairness$sex_group,
  privileged_group = "Male",
  data = test_fairness,
  attr_name = "Sex"
)

print(fobject_sex)
fairness_results$sex <- fobject_sex

# 3.2 Age Fairness
cat("\n\n3.2 Age-Based Fairness Analysis\n")
cat("================================\n")

fobject_age <- calculate_fairness_metrics(
  explainer = explainer_rf,
  protected_attr = test_fairness$age_category,
  privileged_group = "Middle (45-64)",
  data = test_fairness,
  attr_name = "Age Category"
)

print(fobject_age)
fairness_results$age <- fobject_age

# 3.3 Income Fairness
cat("\n\n3.3 Income-Based Fairness Analysis\n")
cat("===================================\n")

fobject_income <- calculate_fairness_metrics(
  explainer = explainer_rf,
  protected_attr = test_fairness$income_category,
  privileged_group = "High (>$50K)",
  data = test_fairness,
  attr_name = "Income Category"
)

print(fobject_income)
fairness_results$income <- fobject_income

# 3.4 Education Fairness
cat("\n\n3.4 Education-Based Fairness Analysis\n")
cat("======================================\n")

fobject_education <- calculate_fairness_metrics(
  explainer = explainer_rf,
  protected_attr = test_fairness$education_category,
  privileged_group = "College Graduate",
  data = test_fairness,
  attr_name = "Education Category"
)

print(fobject_education)
fairness_results$education <- fobject_education

# ============================================================================
# SECTION 4: DETAILED DISPARITY ANALYSIS
# ============================================================================

cat("\n============================================================\n")
cat("SECTION 4: DETAILED DISPARITY ANALYSIS\n")
cat("============================================================\n\n")

# Extract and analyze metric disparities
extract_disparities <- function(fobject, attr_name) {

  # Get fairness check data
  fc_data <- fobject$fairness_check_data

  if (is.null(fc_data)) {
    cat(sprintf("No fairness data available for %s\n", attr_name))
    return(NULL)
  }

  # Pivot to wide format for analysis
  disparities <- fc_data |>
    as_tibble() |>
    rename(group = subgroup, metric = metric, score = score) |>
    mutate(
      protected_attr = attr_name,
      # Four-fifths rule: ratio < 0.8 indicates adverse impact
      adverse_impact = abs(1 - score) > 0.2,
      disparity_magnitude = abs(1 - score)
    )

  return(disparities)
}

# Combine all disparities
all_disparities <- bind_rows(
  extract_disparities(fobject_sex, "Sex"),
  extract_disparities(fobject_age, "Age"),
  extract_disparities(fobject_income, "Income"),
  extract_disparities(fobject_education, "Education")
)

# Summary of disparities
cat("Four-Fifths Rule Analysis (Adverse Impact Threshold: ratio outside 0.8-1.25):\n\n")

adverse_summary <- all_disparities |>
  filter(adverse_impact) |>
  arrange(desc(disparity_magnitude))

if (nrow(adverse_summary) > 0) {
  cat(sprintf("ALERT: %d metric-group combinations show adverse impact!\n\n",
              nrow(adverse_summary)))

  cat("Top Disparities Detected:\n")
  adverse_summary |>
    head(20) |>
    mutate(
      display = sprintf("  %s | %s | %s: ratio=%.3f (disparity=%.1f%%)",
                        protected_attr, group, metric, score, disparity_magnitude * 100)
    ) |>
    pull(display) |>
    cat(sep = "\n")
} else {
  cat("No significant disparities detected under four-fifths rule.\n")
}

cat("\n")

# ============================================================================
# SECTION 5: METRIC-BY-METRIC ANALYSIS
# ============================================================================

cat("============================================================\n")
cat("SECTION 5: METRIC-BY-METRIC DETAILED ANALYSIS\n")
cat("============================================================\n\n")

# Analyze each key fairness metric
analyze_metric <- function(disparities, metric_name, description) {
  cat(sprintf("--- %s ---\n", metric_name))
  cat(sprintf("Definition: %s\n\n", description))

  metric_data <- disparities |>
    filter(metric == metric_name) |>
    arrange(score)

  if (nrow(metric_data) == 0) {
    cat("No data available for this metric.\n\n")
    return()
  }

  cat("Disparity Ratios (1.0 = parity with reference group):\n")
  metric_data |>
    mutate(
      status = case_when(
        score < 0.8 ~ "[ADVERSE IMPACT]",
        score > 1.25 ~ "[FAVORABLE BIAS]",
        TRUE ~ "[ACCEPTABLE]"
      ),
      display = sprintf("  %s - %s: %.3f %s",
                        protected_attr, group, score, status)
    ) |>
    pull(display) |>
    cat(sep = "\n")

  cat("\n")
}

# Statistical Parity (Demographic Parity)
analyze_metric(
  all_disparities,
  "Statistical parity ratio   (TP + FP)/(TP + FP + TN + FN)",
  "P(Y_hat=1|A=a) / P(Y_hat=1|A=privileged) - Equal positive prediction rates"
)

# Equal Opportunity (True Positive Rate Parity)
analyze_metric(
  all_disparities,
  "Equal opportunity ratio     TP/(TP + FN)",
  "TPR(A=a) / TPR(A=privileged) - Equal sensitivity across groups"
)

# Predictive Parity (Precision Parity)
analyze_metric(
  all_disparities,
  "Predictive parity ratio     TP/(TP + FP)",
  "PPV(A=a) / PPV(A=privileged) - Equal precision across groups"
)

# Predictive Equality (False Positive Rate Parity)
analyze_metric(
  all_disparities,
  "Predictive equality ratio   FP/(FP + TN)",
  "FPR(A=a) / FPR(A=privileged) - Equal false positive rates"
)

# Accuracy Equality
analyze_metric(
  all_disparities,
  "Accuracy equality ratio    (TP + TN)/(TP + FP + TN + FN)",
  "Accuracy(A=a) / Accuracy(A=privileged) - Equal accuracy across groups"
)

# ============================================================================
# SECTION 6: INTERSECTIONAL ANALYSIS
# ============================================================================

cat("============================================================\n")
cat("SECTION 6: INTERSECTIONAL FAIRNESS ANALYSIS\n")
cat("============================================================\n\n")

cat("Examining fairness at intersections of protected attributes...\n")
cat("(Do disparities compound for multiply-marginalized groups?)\n\n")

# Create intersectional groups
test_intersect <- test_fairness |>
  mutate(
    intersect_sex_income = paste(sex_group, income_category, sep = " + "),
    intersect_sex_age = paste(sex_group, age_category, sep = " + "),
    intersect_income_age = paste(income_category, age_category, sep = " + "),
    intersect_all = paste(sex_group, income_category, age_category, sep = " + ")
  )

# Calculate group-level metrics manually for intersectional analysis
calculate_group_metrics <- function(data, group_var, actual, predicted_prob,
                                    threshold = 0.5) {

  data |>
    mutate(
      actual = {{ actual }},
      pred_prob = {{ predicted_prob }},
      pred_class = as.integer(pred_prob >= threshold)
    ) |>
    group_by({{ group_var }}) |>
    summarize(
      n = n(),
      n_positive = sum(actual == 1),
      n_negative = sum(actual == 0),

      # Confusion matrix components
      tp = sum(pred_class == 1 & actual == 1),
      fp = sum(pred_class == 1 & actual == 0),
      tn = sum(pred_class == 0 & actual == 0),
      fn = sum(pred_class == 0 & actual == 1),

      # Metrics
      positive_rate = mean(pred_class),
      tpr = tp / n_positive,              # True positive rate (sensitivity)
      fpr = fp / n_negative,              # False positive rate
      ppv = tp / (tp + fp),               # Precision
      npv = tn / (tn + fn),               # Negative predictive value
      accuracy = (tp + tn) / n,
      base_rate = n_positive / n,         # Actual prevalence

      .groups = "drop"
    ) |>
    mutate(
      # Handle division by zero
      tpr = replace_na(tpr, 0),
      fpr = replace_na(fpr, 0),
      ppv = replace_na(ppv, 0),
      npv = replace_na(npv, 0)
    )
}

# Get predictions for intersectional analysis
pred_probs <- predict(rf_model, data = X_test, type = "response")$predictions[, 2]

test_intersect$pred_prob <- pred_probs

# Calculate metrics for key intersections
cat("--- Sex x Income Intersection ---\n")
intersect_sex_income <- calculate_group_metrics(
  test_intersect,
  intersect_sex_income,
  diabetes_binary,
  pred_prob
)

intersect_sex_income |>
  arrange(desc(base_rate)) |>
  select(intersect_sex_income, n, base_rate, positive_rate, tpr, fpr, accuracy) |>
  mutate(across(where(is.numeric) & !n, ~round(., 3))) |>
  print(n = 10)

cat("\n--- Sex x Age Intersection ---\n")
intersect_sex_age <- calculate_group_metrics(
  test_intersect,
  intersect_sex_age,
  diabetes_binary,
  pred_prob
)

intersect_sex_age |>
  arrange(desc(base_rate)) |>
  select(intersect_sex_age, n, base_rate, positive_rate, tpr, fpr, accuracy) |>
  mutate(across(where(is.numeric) & !n, ~round(., 3))) |>
  print(n = 10)

cat("\n--- Income x Age Intersection ---\n")
intersect_income_age <- calculate_group_metrics(
  test_intersect,
  intersect_income_age,
  diabetes_binary,
  pred_prob
)

intersect_income_age |>
  arrange(desc(base_rate)) |>
  select(intersect_income_age, n, base_rate, positive_rate, tpr, fpr, accuracy) |>
  mutate(across(where(is.numeric) & !n, ~round(., 3))) |>
  print(n = 15)

# Identify potentially compounding disparities
cat("\n\nIntersectional Disparity Analysis:\n")
cat("==================================\n\n")

# Compare intersectional groups to overall metrics
overall_metrics <- test_intersect |>
  summarize(
    overall_tpr = sum(pred_prob >= 0.5 & diabetes_binary == 1) / sum(diabetes_binary == 1),
    overall_fpr = sum(pred_prob >= 0.5 & diabetes_binary == 0) / sum(diabetes_binary == 0),
    overall_accuracy = mean((pred_prob >= 0.5) == diabetes_binary)
  )

cat(sprintf("Overall Model Performance:\n"))
cat(sprintf("  TPR: %.3f | FPR: %.3f | Accuracy: %.3f\n\n",
            overall_metrics$overall_tpr, overall_metrics$overall_fpr,
            overall_metrics$overall_accuracy))

# Find groups with largest disparities
cat("Groups with Lowest True Positive Rates (potential under-detection):\n")
intersect_income_age |>
  filter(n >= 100) |>  # Minimum sample size for reliability
  arrange(tpr) |>
  head(5) |>
  mutate(
    tpr_disparity = round((tpr / overall_metrics$overall_tpr - 1) * 100, 1),
    display = sprintf("  %s: TPR=%.3f (%.1f%% from overall, n=%d)",
                      intersect_income_age, tpr, tpr_disparity, n)
  ) |>
  pull(display) |>
  cat(sep = "\n")

cat("\n\nGroups with Highest False Positive Rates (potential over-flagging):\n")
intersect_income_age |>
  filter(n >= 100) |>
  arrange(desc(fpr)) |>
  head(5) |>
  mutate(
    fpr_disparity = round((fpr / overall_metrics$overall_fpr - 1) * 100, 1),
    display = sprintf("  %s: FPR=%.3f (+%.1f%% from overall, n=%d)",
                      intersect_income_age, fpr, fpr_disparity, n)
  ) |>
  pull(display) |>
  cat(sep = "\n")

cat("\n")

# ============================================================================
# SECTION 7: CALIBRATION ANALYSIS BY GROUP
# ============================================================================

cat("============================================================\n")
cat("SECTION 7: CALIBRATION ANALYSIS BY GROUP\n")
cat("============================================================\n\n")

cat("Checking if predicted probabilities match actual rates within groups...\n\n")

# Function to assess calibration
assess_calibration <- function(data, group_var, n_bins = 10) {
  data |>
    mutate(
      prob_bin = cut(pred_prob, breaks = seq(0, 1, length.out = n_bins + 1),
                     include.lowest = TRUE)
    ) |>
    group_by({{ group_var }}, prob_bin) |>
    summarize(
      n = n(),
      mean_predicted = mean(pred_prob),
      mean_actual = mean(diabetes_binary),
      calibration_error = mean_predicted - mean_actual,
      .groups = "drop"
    )
}

# Calibration by sex
cat("--- Calibration by Sex ---\n")
calib_sex <- assess_calibration(test_intersect, sex_group)

calib_sex_summary <- calib_sex |>
  group_by(sex_group) |>
  summarize(
    mean_calibration_error = weighted.mean(calibration_error, n, na.rm = TRUE),
    max_calibration_error = max(abs(calibration_error), na.rm = TRUE),
    n_total = sum(n),
    .groups = "drop"
  )

print(calib_sex_summary)

cat("\n--- Calibration by Income ---\n")
calib_income <- assess_calibration(test_intersect, income_category)

calib_income_summary <- calib_income |>
  group_by(income_category) |>
  summarize(
    mean_calibration_error = weighted.mean(calibration_error, n, na.rm = TRUE),
    max_calibration_error = max(abs(calibration_error), na.rm = TRUE),
    n_total = sum(n),
    .groups = "drop"
  )

print(calib_income_summary)

cat("\n--- Calibration by Age ---\n")
calib_age <- assess_calibration(test_intersect, age_category)

calib_age_summary <- calib_age |>
  group_by(age_category) |>
  summarize(
    mean_calibration_error = weighted.mean(calibration_error, n, na.rm = TRUE),
    max_calibration_error = max(abs(calibration_error), na.rm = TRUE),
    n_total = sum(n),
    .groups = "drop"
  )

print(calib_age_summary)

cat("\n")

# ============================================================================
# SECTION 8: VISUALIZATIONS
# ============================================================================

cat("============================================================\n")
cat("SECTION 8: GENERATING FAIRNESS VISUALIZATIONS\n")
cat("============================================================\n\n")

# Ensure output directory exists
output_dir <- "output/figures"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Set common theme
theme_fairness <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# 8.1 Fairness Overview Plot
cat("Creating fairness overview plot...\n")

# Combine all fairness objects for stacked radar
p_overview <- plot(fobject_sex, fobject_age, fobject_income, fobject_education) +
  ggtitle("Fairness Metrics Overview",
          subtitle = "Random Forest Diabetes Prediction Model") +
  theme_fairness

ggsave(
  file.path(output_dir, "diabetes_fairness_overview.png"),
  p_overview,
  width = 14,
  height = 10,
  dpi = 300
)
cat("  Saved: output/figures/diabetes_fairness_overview.png\n")

# 8.2 Sex Fairness Details
cat("Creating sex fairness plot...\n")

p_sex <- plot(fobject_sex) +
  ggtitle("Fairness Metrics by Sex",
          subtitle = "Reference group: Male | Threshold for concern: outside 0.8-1.25") +
  theme_fairness

ggsave(
  file.path(output_dir, "diabetes_fairness_sex.png"),
  p_sex,
  width = 10,
  height = 8,
  dpi = 300
)
cat("  Saved: output/figures/diabetes_fairness_sex.png\n")

# 8.3 Age Fairness Details
cat("Creating age fairness plot...\n")

p_age <- plot(fobject_age) +
  ggtitle("Fairness Metrics by Age Category",
          subtitle = "Reference group: Middle (45-64) | Threshold for concern: outside 0.8-1.25") +
  theme_fairness

ggsave(
  file.path(output_dir, "diabetes_fairness_age.png"),
  p_age,
  width = 10,
  height = 8,
  dpi = 300
)
cat("  Saved: output/figures/diabetes_fairness_age.png\n")

# 8.4 Income Fairness Details
cat("Creating income fairness plot...\n")

p_income <- plot(fobject_income) +
  ggtitle("Fairness Metrics by Income Category",
          subtitle = "Reference group: High (>$50K) | Threshold for concern: outside 0.8-1.25") +
  theme_fairness

ggsave(
  file.path(output_dir, "diabetes_fairness_income.png"),
  p_income,
  width = 10,
  height = 8,
  dpi = 300
)
cat("  Saved: output/figures/diabetes_fairness_income.png\n")

# 8.5 Intersectional Heatmap
cat("Creating intersectional fairness heatmap...\n")

# Prepare intersectional data for heatmap
intersect_heatmap_data <- intersect_income_age |>
  separate(intersect_income_age, into = c("income_cat", "age_cat"), sep = " \\+ ") |>
  mutate(
    income_cat = factor(income_cat,
                        levels = c("Low (<$25K)", "Medium ($25K-$50K)", "High (>$50K)")),
    age_cat = factor(age_cat,
                     levels = c("Young (18-44)", "Middle (45-64)", "Senior (65+)"))
  )

# TPR Heatmap
p_tpr_heatmap <- ggplot(intersect_heatmap_data,
                        aes(x = age_cat, y = income_cat, fill = tpr)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f\n(n=%d)", tpr, n)),
            size = 3.5, color = "white") +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#fee08b",
    high = "#1a9850",
    midpoint = overall_metrics$overall_tpr,
    name = "TPR",
    limits = c(0, 1)
  ) +
  labs(
    title = "True Positive Rate by Income and Age",
    subtitle = "Ability to detect diabetes cases across intersectional groups",
    x = "Age Category",
    y = "Income Category"
  ) +
  theme_fairness +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# FPR Heatmap
p_fpr_heatmap <- ggplot(intersect_heatmap_data,
                        aes(x = age_cat, y = income_cat, fill = fpr)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f\n(n=%d)", fpr, n)),
            size = 3.5, color = "white") +
  scale_fill_gradient2(
    low = "#1a9850",
    mid = "#fee08b",
    high = "#d73027",
    midpoint = overall_metrics$overall_fpr,
    name = "FPR",
    limits = c(0, 0.5)
  ) +
  labs(
    title = "False Positive Rate by Income and Age",
    subtitle = "Rate of incorrectly flagging non-diabetic individuals",
    x = "Age Category",
    y = "Income Category"
  ) +
  theme_fairness +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Accuracy Heatmap
p_acc_heatmap <- ggplot(intersect_heatmap_data,
                        aes(x = age_cat, y = income_cat, fill = accuracy)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f\n(n=%d)", accuracy, n)),
            size = 3.5, color = "white") +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#fee08b",
    high = "#1a9850",
    midpoint = overall_metrics$overall_accuracy,
    name = "Accuracy",
    limits = c(0.5, 1)
  ) +
  labs(
    title = "Overall Accuracy by Income and Age",
    subtitle = "Classification accuracy across intersectional groups",
    x = "Age Category",
    y = "Income Category"
  ) +
  theme_fairness +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine heatmaps
p_intersectional <- p_tpr_heatmap / p_fpr_heatmap / p_acc_heatmap +
  plot_annotation(
    title = "Intersectional Fairness Analysis: Income x Age",
    subtitle = "Color scale centered on overall model performance",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
    )
  )

ggsave(
  file.path(output_dir, "diabetes_fairness_intersectional.png"),
  p_intersectional,
  width = 10,
  height = 16,
  dpi = 300
)
cat("  Saved: output/figures/diabetes_fairness_intersectional.png\n")

cat("\n")

# ============================================================================
# SECTION 9: BIAS MITIGATION RECOMMENDATIONS
# ============================================================================

cat("============================================================\n")
cat("SECTION 9: BIAS MITIGATION RECOMMENDATIONS\n")
cat("============================================================\n\n")

# Analyze findings and generate recommendations
generate_recommendations <- function(disparities, intersect_data, overall) {

  recommendations <- list()

  # Check for adverse impact in any metric
  adverse_impacts <- disparities |>
    filter(adverse_impact) |>
    distinct(protected_attr, group, metric) |>
    group_by(protected_attr) |>
    summarize(n_issues = n(), .groups = "drop")

  cat("SUMMARY OF IDENTIFIED DISPARITIES:\n")
  cat("-----------------------------------\n\n")

  if (nrow(adverse_impacts) > 0) {
    for (i in 1:nrow(adverse_impacts)) {
      cat(sprintf("  - %s: %d metric(s) with adverse impact\n",
                  adverse_impacts$protected_attr[i],
                  adverse_impacts$n_issues[i]))
    }
  } else {
    cat("  No significant adverse impacts detected under four-fifths rule.\n")
  }

  cat("\n\nRECOMMENDATIONS FOR FAIR DEPLOYMENT:\n")
  cat("-------------------------------------\n\n")

  cat("1. PRE-PROCESSING STRATEGIES:\n")
  cat("   - Consider reweighting training samples to balance representation\
")
  cat("   - Apply disparate impact remover to transform features\n")
  cat("   - Use SMOTE or similar techniques stratified by protected attributes\n\n")

  cat("2. IN-PROCESSING STRATEGIES:\n")
  cat("   - Add fairness constraints to the optimization objective\n")
  cat("   - Use adversarial debiasing to remove protected attribute signal\n")
  cat("   - Consider threshold optimization per group (equalized odds post-processing)\n\n")

  cat("3. POST-PROCESSING STRATEGIES:\n")
  cat("   - Calibrate predictions within each protected group separately\n")
  cat("   - Apply reject option classification near decision boundary\n")
  cat("   - Use group-specific thresholds to achieve equalized odds\n\n")

  cat("4. DEPLOYMENT CONSIDERATIONS:\n")
  cat("   - Implement continuous monitoring of fairness metrics in production\n")
  cat("   - Establish feedback loops for identifying emerging disparities\n")
  cat("   - Document known limitations for clinical decision support\n")
  cat("   - Ensure human oversight for high-stakes predictions\n\n")

  cat("5. TRADEOFF CONSIDERATIONS:\n")
  cat("   - Statistical parity may conflict with calibration\n")
  cat("   - Equal opportunity and predictive parity cannot always be achieved simultaneously\n")
  cat("   - Document which fairness definition is prioritized and why\n")
  cat("   - Consider context: false negatives (missed diabetes) may be more costly than false positives\n\n")

  # Specific recommendations based on findings
  cat("6. SPECIFIC FINDINGS AND ACTIONS:\n")

  # Check for age-related disparities
  age_issues <- disparities |>
    filter(protected_attr == "Age", adverse_impact)

  if (nrow(age_issues) > 0) {
    cat("\n   AGE-RELATED ISSUES:\n")
    cat("   - Consider age-stratified models or ensemble approaches\n")
    cat("   - Review feature importance: are age-correlated features dominating?\n")
    cat("   - Validate predictions separately for younger populations if under-represented\n")
  }

  # Check for income-related disparities
  income_issues <- disparities |>
    filter(protected_attr == "Income", adverse_impact)

  if (nrow(income_issues) > 0) {
    cat("\n   INCOME-RELATED ISSUES:\n")
    cat("   - Low-income groups may have different feature distributions\n")
    cat("   - Consider socioeconomic fairness as a deployment criterion\n")
    cat("   - Healthcare access barriers may affect label quality in training data\n")
  }

  cat("\n")

  return(list(
    adverse_impacts = adverse_impacts,
    n_total_issues = sum(adverse_impacts$n_issues)
  ))
}

recommendations <- generate_recommendations(all_disparities, intersect_income_age,
                                            overall_metrics)

# ============================================================================
# SECTION 10: LIMITATIONS AND CAVEATS
# ============================================================================

cat("============================================================\n")
cat("SECTION 10: LIMITATIONS AND METHODOLOGICAL CAVEATS\n")
cat("============================================================\n\n")

cat("IMPORTANT LIMITATIONS TO CONSIDER:\n\n")

cat("1. FAIRNESS DEFINITION TENSIONS:\n")
cat("   - Impossibility theorem: Cannot satisfy all fairness criteria simultaneously\n")
cat("     when base rates differ across groups (which they do here)\n")
cat("   - Choice of fairness metric should align with domain values and costs\n\n")

cat("2. OBSERVATIONAL DATA LIMITATIONS:\n")
cat("   - Historical bias in training data may be perpetuated\n")
cat("   - Label bias: Diabetes diagnosis may itself be biased by healthcare access\n")
cat("   - Measurement bias: Self-reported variables subject to group differences\n\n")

cat("3. INTERSECTIONALITY COMPLEXITY:\n")
cat("   - Small sample sizes for some intersectional groups reduce reliability\n")
cat("   - Groups with n < 100 excluded from some analyses\n")
cat("   - Further granularity (e.g., race/ethnicity) not available in this dataset\n\n")

cat("4. THRESHOLD SENSITIVITY:\n")
cat("   - All metrics computed at 0.5 probability threshold\n")
cat("   - Different thresholds may change fairness conclusions\n")
cat("   - Optimal threshold may differ by use case (screening vs. diagnosis)\n\n")

cat("5. TEMPORAL AND GEOGRAPHIC LIMITATIONS:\n")
cat("   - BRFSS data from specific time period and US population\n")
cat("   - Fairness conclusions may not generalize to other contexts\n\n")

# ============================================================================
# SECTION 11: SAVE COMPREHENSIVE RESULTS
# ============================================================================

cat("============================================================\n")
cat("SECTION 11: SAVING AUDIT RESULTS\n")
cat("============================================================\n\n")

# Compile comprehensive results
audit_results <- list(
  # Summary information
  summary = list(
    model = "Random Forest",
    n_test = nrow(test_fairness),
    analysis_date = Sys.time(),
    threshold = 0.5,
    overall_metrics = overall_metrics
  ),

  # Fairness objects
  fairness_objects = list(
    sex = fobject_sex,
    age = fobject_age,
    income = fobject_income,
    education = fobject_education
  ),

  # All disparity data
  all_disparities = all_disparities,

  # Flagged issues (below four-fifths threshold)
  flagged_disparities = all_disparities |>
    filter(adverse_impact) |>
    arrange(protected_attr, group, metric),

  # Intersectional analysis
  intersectional = list(
    sex_income = intersect_sex_income,
    sex_age = intersect_sex_age,
    income_age = intersect_income_age
  ),

  # Calibration analysis
  calibration = list(
    by_sex = calib_sex_summary,
    by_income = calib_income_summary,
    by_age = calib_age_summary
  ),

  # Recommendations summary
  recommendations = recommendations,

  # Metadata
  metadata = list(
    r_version = R.version.string,
    packages = list(
      fairmodels = packageVersion("fairmodels"),
      DALEX = packageVersion("DALEX"),
      ranger = packageVersion("ranger")
    ),
    created_at = Sys.time()
  )
)

# Save results
output_path <- "output/fairness_audit_results.rds"
saveRDS(audit_results, output_path)
cat(sprintf("Saved comprehensive audit results: %s\n", output_path))

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n============================================================\n")
cat("FAIRNESS AUDIT COMPLETE\n")
cat("============================================================\n\n")

cat("Protected Attributes Analyzed:\n")
cat("  - Sex (Male vs Female)\n")
cat("  - Age (Young vs Middle vs Senior)\n")
cat("  - Income (Low vs Medium vs High)\n")
cat("  - Education (Less than HS to College Graduate)\n\n")

cat("Fairness Metrics Evaluated:\n")
cat("  - Statistical Parity (Demographic Parity)\n")
cat("  - Equal Opportunity (TPR Parity)\n")
cat("  - Predictive Parity (Precision Parity)\n")
cat("  - Predictive Equality (FPR Parity)\n")
cat("  - Accuracy Equality\n\n")

n_flagged <- nrow(audit_results$flagged_disparities)
if (n_flagged > 0) {
  cat(sprintf("ALERT: %d metric-group combinations flagged for adverse impact\n",
              n_flagged))
  cat("Review recommendations section for mitigation strategies.\n\n")
} else {
  cat("STATUS: No major adverse impacts detected under four-fifths rule.\n")
  cat("However, continuous monitoring recommended.\n\n")
}

cat("Output Files:\n")
cat("  - output/figures/diabetes_fairness_overview.png\n")
cat("  - output/figures/diabetes_fairness_sex.png\n")
cat("  - output/figures/diabetes_fairness_age.png\n")
cat("  - output/figures/diabetes_fairness_income.png\n")
cat("  - output/figures/diabetes_fairness_intersectional.png\n")
cat("  - output/fairness_audit_results.rds\n\n")

cat("============================================================\n")
cat("This audit provides a foundation for responsible AI deployment.\n")
cat("Fairness is an ongoing commitment, not a one-time check.\n")
cat("============================================================\n")
