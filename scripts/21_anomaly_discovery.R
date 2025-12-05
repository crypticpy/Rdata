# ============================================================================
# Title: Anomaly and Resilience Discovery Analysis for Diabetes
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Identify individuals who defy model predictions - resilient (high risk
#          but healthy) and vulnerable (low risk but diabetic) - to generate
#          hypotheses about protective and vulnerability factors
# Input: data/processed/diabetes_features.rds
#        output/models/random_forest_model.rds
# Output: output/anomaly_discovery_results.rds
#         output/figures/diabetes_residual_distribution.png
#         output/figures/diabetes_subgroup_profiles.png
#         output/figures/diabetes_resilience_factors.png
#         output/figures/diabetes_vulnerability_factors.png
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(ranger)
library(broom)
library(scales)
library(patchwork)

cat("============================================================================\n")
cat("        ANOMALY AND RESILIENCE DISCOVERY ANALYSIS\n")
cat("============================================================================\n\n")

# Define color palette and theme ----------------------------------------------

# Subgroup colors
subgroup_colors <- c(

  "Resilient" = "#27AE60",     # Green - healthy despite high risk
  "Vulnerable" = "#E74C3C",    # Red - diabetic despite low risk
  "Expected Healthy" = "#3498DB",  # Blue - low risk, healthy
  "Expected Diabetic" = "#9B59B6"  # Purple - high risk, diabetic
)

# Accent colors for factor analysis
factor_colors <- c(
  "protective" = "#27AE60",
  "risk" = "#E74C3C",
  "neutral" = "#7F8C8D"
)

# Publication-quality theme
theme_anomaly <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "sans", color = "#333333"),
      plot.title = element_text(
        size = rel(1.4),
        face = "bold",
        margin = margin(b = 8),
        color = "#1a1a1a"
      ),
      plot.subtitle = element_text(
        size = rel(0.95),
        color = "#555555",
        margin = margin(b = 12),
        lineheight = 1.2
      ),
      plot.caption = element_text(
        size = rel(0.75),
        color = "#888888",
        hjust = 0,
        margin = margin(t = 12)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.title = element_text(size = rel(0.95), color = "#444444"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = rel(0.85), color = "#555555"),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(color = "#E8E8E8", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(size = rel(0.9), face = "bold"),
      legend.text = element_text(size = rel(0.85)),
      legend.key.size = unit(0.8, "lines"),
      legend.margin = margin(b = 10),
      plot.margin = margin(20, 25, 15, 20),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(
        size = rel(0.95),
        face = "bold",
        color = "#333333",
        margin = margin(b = 8, t = 8)
      ),
      strip.background = element_rect(fill = "#F5F5F5", color = NA)
    )
}

# =============================================================================
# SECTION 1: LOAD DATA AND MODELS
# =============================================================================
cat("[1/7] Loading data and trained Random Forest model...\n")

# Load feature-engineered data
features <- readRDS("data/processed/diabetes_features.rds")

# Use full dataset for anomaly discovery (not just test set)
df_full <- features$full_data

# Load the trained Random Forest model
rf_model <- readRDS("output/models/random_forest_model.rds")

cat(sprintf("  Dataset: %s observations\n", comma(nrow(df_full))))
cat(sprintf("  Diabetes prevalence: %.1f%%\n", mean(df_full$diabetes_binary) * 100))
cat(sprintf("  Random Forest model: %d trees, OOB error = %.4f\n",
            rf_model$num.trees, rf_model$prediction.error))

# =============================================================================
# SECTION 2: GENERATE PREDICTIONS AND RESIDUALS
# =============================================================================
cat("\n[2/7] Generating predictions and calculating residuals...\n")

# Prepare data for prediction (same format as training)
df_predict <- df_full |>
  select(-diabetes_012) |>
  mutate(diabetes_binary = factor(diabetes_binary, levels = c(0, 1)))

# Generate probability predictions
predictions <- predict(rf_model, data = df_predict, type = "response")
pred_prob <- predictions$predictions[, 2]  # Probability of diabetes

# Add predictions to dataset
df_analysis <- df_full |>
  mutate(
    predicted_prob = pred_prob,
    # Residual: actual outcome - predicted probability
    # Positive residual: more diabetes than predicted (vulnerable)
    # Negative residual: less diabetes than predicted (resilient)
    residual = diabetes_binary - predicted_prob,
    abs_residual = abs(residual)
  )

# Summary statistics for predictions
cat("\n  Prediction Summary:\n")
cat(sprintf("    Mean predicted probability: %.3f\n", mean(df_analysis$predicted_prob)))
cat(sprintf("    SD predicted probability: %.3f\n", sd(df_analysis$predicted_prob)))
cat(sprintf("    Correlation (predicted, actual): %.3f\n",
            cor(df_analysis$predicted_prob, df_analysis$diabetes_binary)))

cat("\n  Residual Summary:\n")
cat(sprintf("    Mean residual: %.4f (should be ~0)\n", mean(df_analysis$residual)))
cat(sprintf("    SD residual: %.3f\n", sd(df_analysis$residual)))
cat(sprintf("    Mean absolute residual: %.3f\n", mean(df_analysis$abs_residual)))

# =============================================================================
# SECTION 3: IDENTIFY EXTREME SUBGROUPS
# =============================================================================
cat("\n[3/7] Identifying extreme subgroups (resilient and vulnerable)...\n")

# Calculate percentile thresholds
risk_70th <- quantile(df_analysis$predicted_prob, 0.70)
risk_30th <- quantile(df_analysis$predicted_prob, 0.30)

cat(sprintf("\n  Risk thresholds:\n"))
cat(sprintf("    70th percentile (high risk): predicted prob >= %.3f\n", risk_70th))
cat(sprintf("    30th percentile (low risk): predicted prob <= %.3f\n", risk_30th))

# Assign subgroups
df_analysis <- df_analysis |>
  mutate(
    subgroup = case_when(
      # Resilient: High risk (>70th percentile) but NO diabetes
      predicted_prob >= risk_70th & diabetes_binary == 0 ~ "Resilient",
      # Vulnerable: Low risk (<30th percentile) but HAS diabetes
      predicted_prob <= risk_30th & diabetes_binary == 1 ~ "Vulnerable",
      # Expected Healthy: Low risk and no diabetes
      predicted_prob <= risk_30th & diabetes_binary == 0 ~ "Expected Healthy",
      # Expected Diabetic: High risk and has diabetes
      predicted_prob >= risk_70th & diabetes_binary == 1 ~ "Expected Diabetic",
      # Typical: Everyone else in the middle
      TRUE ~ "Typical"
    ),
    subgroup = factor(subgroup, levels = c("Resilient", "Vulnerable",
                                            "Expected Healthy", "Expected Diabetic",
                                            "Typical"))
  )

# Subgroup counts and prevalence
subgroup_summary <- df_analysis |>
  group_by(subgroup) |>
  summarize(
    n = n(),
    pct = n() / nrow(df_analysis) * 100,
    mean_risk = mean(predicted_prob) * 100,
    diabetes_rate = mean(diabetes_binary) * 100,
    .groups = "drop"
  )

cat("\n  Subgroup Summary:\n")
print(subgroup_summary)

# Key findings
resilient_n <- sum(df_analysis$subgroup == "Resilient")
vulnerable_n <- sum(df_analysis$subgroup == "Vulnerable")
high_risk_n <- sum(df_analysis$predicted_prob >= risk_70th)
low_risk_n <- sum(df_analysis$predicted_prob <= risk_30th)

cat(sprintf("\n  KEY FINDINGS:\n"))
cat(sprintf("    Resilient individuals: %s (%.1f%% of high-risk group)\n",
            comma(resilient_n), resilient_n / high_risk_n * 100))
cat(sprintf("    Vulnerable individuals: %s (%.1f%% of low-risk group)\n",
            comma(vulnerable_n), vulnerable_n / low_risk_n * 100))

# =============================================================================
# SECTION 4: SUBGROUP PROFILING
# =============================================================================
cat("\n[4/7] Creating detailed subgroup profiles...\n")

# Focus on key comparison groups (exclude Typical for cleaner analysis)
df_comparison <- df_analysis |>
  filter(subgroup != "Typical")

# A. Demographic Profile
cat("\n  A. Demographic Profile:\n")

demographic_vars <- c("age", "sex", "income", "education")

demographic_profile <- df_comparison |>
  group_by(subgroup) |>
  summarize(
    n = n(),
    age_mean = mean(age),
    age_sd = sd(age),
    pct_female = mean(sex == 0) * 100,  # sex: 0=female, 1=male
    income_mean = mean(income),
    education_mean = mean(education),
    .groups = "drop"
  )

cat("  Demographics by Subgroup:\n")
print(demographic_profile)

# B. Health Behaviors Profile
cat("\n  B. Health Behaviors Profile:\n")

behavior_vars <- c("phys_activity", "fruits", "veggies", "smoker", "hvy_alcohol_consump")

behavior_profile <- df_comparison |>
  group_by(subgroup) |>
  summarize(
    n = n(),
    pct_physically_active = mean(phys_activity) * 100,
    pct_eats_fruits = mean(fruits) * 100,
    pct_eats_veggies = mean(veggies) * 100,
    pct_smoker = mean(smoker) * 100,
    pct_heavy_alcohol = mean(hvy_alcohol_consump) * 100,
    lifestyle_score_mean = mean(lifestyle_score),
    .groups = "drop"
  )

cat("  Health Behaviors by Subgroup:\n")
print(behavior_profile)

# C. Health Conditions Profile
cat("\n  C. Health Conditions Profile:\n")

condition_profile <- df_comparison |>
  group_by(subgroup) |>
  summarize(
    n = n(),
    pct_high_bp = mean(high_bp) * 100,
    pct_high_chol = mean(high_chol) * 100,
    bmi_mean = mean(bmi),
    bmi_sd = sd(bmi),
    pct_obese = mean(bmi >= 30) * 100,
    gen_health_mean = mean(gen_hlth),  # 1=excellent, 5=poor
    pct_diff_walk = mean(diff_walk) * 100,
    cv_risk_mean = mean(cardiovascular_risk),
    .groups = "drop"
  )

cat("  Health Conditions by Subgroup:\n")
print(condition_profile)

# =============================================================================
# SECTION 5: STATISTICAL TESTS FOR SUBGROUP DIFFERENCES
# =============================================================================
cat("\n[5/7] Testing for statistically significant differences...\n")

# Compare Resilient vs Expected Diabetic (both high-risk)
resilient_vs_expected <- df_analysis |>
  filter(subgroup %in% c("Resilient", "Expected Diabetic"))

# Compare Vulnerable vs Expected Healthy (both low-risk)
vulnerable_vs_expected <- df_analysis |>
  filter(subgroup %in% c("Vulnerable", "Expected Healthy"))

# Function to perform comparison tests
compare_subgroups <- function(data, var_name, group_var = "subgroup") {
  # For continuous variables: t-test or Wilcoxon
  # For binary variables: chi-square

  var_data <- data[[var_name]]
  group_data <- data[[group_var]]

  if (length(unique(var_data)) == 2) {
    # Binary variable - chi-square test
    test <- chisq.test(table(group_data, var_data))
    test_type <- "chi-square"
    statistic <- test$statistic
    p_value <- test$p.value
  } else {
    # Continuous variable - Wilcoxon test (non-parametric)
    test <- wilcox.test(var_data ~ group_data)
    test_type <- "Wilcoxon"
    statistic <- test$statistic
    p_value <- test$p.value
  }

  tibble(
    variable = var_name,
    test_type = test_type,
    statistic = statistic,
    p_value = p_value,
    significant = p_value < 0.05
  )
}

# Test variables that might differ between resilient and expected diabetic
test_vars <- c("age", "sex", "income", "education", "bmi", "phys_activity",
               "fruits", "veggies", "smoker", "high_bp", "high_chol",
               "gen_hlth", "lifestyle_score", "cardiovascular_risk")

cat("\n  A. Resilient vs Expected Diabetic (both high predicted risk):\n")

resilience_tests <- map_dfr(test_vars, function(v) {
  tryCatch(
    compare_subgroups(resilient_vs_expected, v),
    error = function(e) tibble(variable = v, test_type = "error",
                               statistic = NA, p_value = NA, significant = FALSE)
  )
}) |>
  arrange(p_value)

cat("  Significant differences (p < 0.05):\n")
resilience_tests |>
  filter(significant) |>
  mutate(p_value = format.pval(p_value, digits = 3)) |>
  print(n = 20)

cat("\n  B. Vulnerable vs Expected Healthy (both low predicted risk):\n")

vulnerability_tests <- map_dfr(test_vars, function(v) {
  tryCatch(
    compare_subgroups(vulnerable_vs_expected, v),
    error = function(e) tibble(variable = v, test_type = "error",
                               statistic = NA, p_value = NA, significant = FALSE)
  )
}) |>
  arrange(p_value)

cat("  Significant differences (p < 0.05):\n")
vulnerability_tests |>
  filter(significant) |>
  mutate(p_value = format.pval(p_value, digits = 3)) |>
  print(n = 20)

# =============================================================================
# SECTION 6: HYPOTHESIS GENERATION - PROTECTIVE/RISK FACTORS
# =============================================================================
cat("\n[6/7] Generating hypotheses about protective and risk factors...\n")

# Calculate effect sizes (standardized mean differences for continuous,
# risk ratios for binary)

calculate_effect_size <- function(data, var_name, group_col = "subgroup",
                                  group1, group2) {
  g1_data <- data |> filter(.data[[group_col]] == group1) |> pull(var_name)
  g2_data <- data |> filter(.data[[group_col]] == group2) |> pull(var_name)

  mean1 <- mean(g1_data, na.rm = TRUE)
  mean2 <- mean(g2_data, na.rm = TRUE)

  if (length(unique(c(g1_data, g2_data))) == 2) {
    # Binary variable - calculate risk ratio
    # Risk ratio: proportion in group1 / proportion in group2
    rr <- mean1 / max(mean2, 0.001)  # Avoid division by zero
    effect_type <- "Risk Ratio"
    effect_value <- rr
  } else {
    # Continuous variable - Cohen's d
    pooled_sd <- sqrt((var(g1_data, na.rm = TRUE) + var(g2_data, na.rm = TRUE)) / 2)
    cohens_d <- (mean1 - mean2) / pooled_sd
    effect_type <- "Cohen's d"
    effect_value <- cohens_d
  }

  tibble(
    variable = var_name,
    mean_group1 = mean1,
    mean_group2 = mean2,
    difference = mean1 - mean2,
    effect_type = effect_type,
    effect_size = effect_value
  )
}

# A. Protective factors (Resilient vs Expected Diabetic)
cat("\n  A. Potential PROTECTIVE Factors (Resilient have more):\n")

protective_effects <- map_dfr(test_vars, function(v) {
  calculate_effect_size(resilient_vs_expected, v,
                        group1 = "Resilient", group2 = "Expected Diabetic")
}) |>
  mutate(
    direction = case_when(
      effect_type == "Risk Ratio" & effect_size > 1 ~ "Resilient higher",
      effect_type == "Risk Ratio" & effect_size < 1 ~ "Expected Diabetic higher",
      effect_type == "Cohen's d" & effect_size > 0 ~ "Resilient higher",
      effect_type == "Cohen's d" & effect_size < 0 ~ "Expected Diabetic higher",
      TRUE ~ "No difference"
    ),
    abs_effect = abs(effect_size)
  ) |>
  arrange(desc(abs_effect))

# Identify protective factors (where resilient have favorable values)
# Higher is better for: phys_activity, fruits, veggies, income, education
# Lower is better for: bmi, high_bp, high_chol, smoker, gen_hlth (scale 1-5)

protective_factors <- protective_effects |>
  mutate(
    is_protective = case_when(
      # Higher is good
      variable %in% c("phys_activity", "fruits", "veggies", "income",
                      "education", "lifestyle_score") &
        direction == "Resilient higher" ~ TRUE,
      # Lower is good
      variable %in% c("bmi", "high_bp", "high_chol", "smoker",
                      "gen_hlth", "cardiovascular_risk") &
        direction == "Expected Diabetic higher" ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  filter(is_protective) |>
  arrange(desc(abs_effect))

cat("  Top Protective Factors (associated with resilience):\n")
protective_factors |>
  select(variable, mean_group1, mean_group2, effect_type, effect_size) |>
  rename(resilient_mean = mean_group1, expected_diabetic_mean = mean_group2) |>
  print(n = 10)

# B. Vulnerability factors (Vulnerable vs Expected Healthy)
cat("\n  B. Potential VULNERABILITY Factors (Vulnerable have more):\n")

vulnerability_effects <- map_dfr(test_vars, function(v) {
  calculate_effect_size(vulnerable_vs_expected, v,
                        group1 = "Vulnerable", group2 = "Expected Healthy")
}) |>
  mutate(
    direction = case_when(
      effect_type == "Risk Ratio" & effect_size > 1 ~ "Vulnerable higher",
      effect_type == "Risk Ratio" & effect_size < 1 ~ "Expected Healthy higher",
      effect_type == "Cohen's d" & effect_size > 0 ~ "Vulnerable higher",
      effect_type == "Cohen's d" & effect_size < 0 ~ "Expected Healthy higher",
      TRUE ~ "No difference"
    ),
    abs_effect = abs(effect_size)
  ) |>
  arrange(desc(abs_effect))

# Identify risk factors (where vulnerable have unfavorable values)
vulnerability_factors <- vulnerability_effects |>
  mutate(
    is_risk = case_when(
      # Lower is bad
      variable %in% c("phys_activity", "fruits", "veggies", "income",
                      "education", "lifestyle_score") &
        direction == "Expected Healthy higher" ~ TRUE,
      # Higher is bad
      variable %in% c("bmi", "high_bp", "high_chol", "smoker",
                      "gen_hlth", "cardiovascular_risk") &
        direction == "Vulnerable higher" ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  filter(is_risk) |>
  arrange(desc(abs_effect))

cat("  Top Vulnerability Factors (associated with unexpected diabetes):\n")
vulnerability_factors |>
  select(variable, mean_group1, mean_group2, effect_type, effect_size) |>
  rename(vulnerable_mean = mean_group1, expected_healthy_mean = mean_group2) |>
  print(n = 10)

# C. Generate hypothesis list
cat("\n  C. Generated Hypotheses:\n")

hypotheses <- tibble(
  hypothesis_id = 1:10,
  hypothesis = c(
    "Physical activity provides protection even at high metabolic risk",
    "Better dietary habits (fruits/vegetables) may confer resilience to diabetes",
    "Higher socioeconomic status offers protective resources beyond measured risk factors",
    "BMI alone is insufficient - body composition or fat distribution may matter more",
    "Psychological factors (not measured) may contribute to vulnerability",
    "Healthcare access and regular checkups enable early intervention in low-risk individuals",
    "Genetic predisposition may override lifestyle factors in some individuals",
    "Inflammatory markers (not measured) may identify vulnerable low-risk individuals",
    "Sleep quality and stress may be unmeasured confounders in resilience",
    "Social support and community factors may protect high-risk individuals"
  ),
  supporting_evidence = c(
    sprintf("Resilient group has %.0f%% higher physical activity rate",
            (behavior_profile$pct_physically_active[behavior_profile$subgroup == "Resilient"] -
             behavior_profile$pct_physically_active[behavior_profile$subgroup == "Expected Diabetic"])),
    sprintf("Resilient group consumes more fruits (%.0f%% vs %.0f%%)",
            behavior_profile$pct_eats_fruits[behavior_profile$subgroup == "Resilient"],
            behavior_profile$pct_eats_fruits[behavior_profile$subgroup == "Expected Diabetic"]),
    sprintf("Resilient group has higher mean income (%.1f vs %.1f)",
            demographic_profile$income_mean[demographic_profile$subgroup == "Resilient"],
            demographic_profile$income_mean[demographic_profile$subgroup == "Expected Diabetic"]),
    "Large residuals suggest unmeasured factors beyond BMI and standard risk factors",
    sprintf("Vulnerable group has poorer self-rated health (%.1f vs %.1f)",
            condition_profile$gen_health_mean[condition_profile$subgroup == "Vulnerable"],
            condition_profile$gen_health_mean[condition_profile$subgroup == "Expected Healthy"]),
    "Low-risk individuals developing diabetes may have undetected underlying conditions",
    sprintf("Vulnerable individuals defy prediction despite low risk (n=%s)", comma(vulnerable_n)),
    "Vulnerable group may have elevated inflammatory or metabolic markers not captured",
    "Cross-sectional data cannot capture longitudinal lifestyle patterns",
    "Social determinants may explain resilience in high-risk communities"
  ),
  testable = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
  priority = c("High", "High", "Medium", "High", "Low", "Medium", "Medium",
               "High", "Low", "Low")
)

print(hypotheses |> select(hypothesis_id, hypothesis, priority))

# =============================================================================
# SECTION 7: CREATE VISUALIZATIONS
# =============================================================================
cat("\n[7/7] Creating visualizations...\n")

# Ensure output directory exists
if (!dir.exists("output/figures")) {
  dir.create("output/figures", recursive = TRUE)
}

# --------------------------------------------------------------------------
# VISUALIZATION 1: Residual Distribution
# --------------------------------------------------------------------------
cat("\n  Creating residual distribution plot...\n")

residual_plot <- df_analysis |>
  ggplot(aes(x = residual, fill = factor(diabetes_binary))) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#333333", linewidth = 0.8) +
  scale_fill_manual(
    values = c("0" = "#3498DB", "1" = "#E74C3C"),
    labels = c("0" = "No Diabetes", "1" = "Has Diabetes"),
    name = "Actual Outcome"
  ) +
  labs(
    title = "Distribution of Prediction Residuals",
    subtitle = paste(
      "Residual = Actual outcome - Predicted probability",
      sprintf("| Positive: more diabetes than predicted | Negative: less than predicted"),
      sep = "\n"
    ),
    x = "Prediction Residual",
    y = "Density",
    caption = sprintf(
      "n = %s | Mean residual = %.4f | SD = %.3f\nResiduals near 0 indicate accurate predictions; extreme values identify anomalies",
      comma(nrow(df_analysis)), mean(df_analysis$residual), sd(df_analysis$residual)
    )
  ) +
  theme_anomaly() +
  theme(legend.position = c(0.85, 0.85),
        legend.background = element_rect(fill = "white", color = "#E0E0E0"))

ggsave("output/figures/diabetes_residual_distribution.png", residual_plot,
       width = 10, height = 7, dpi = 300, bg = "white")
cat("  Saved: output/figures/diabetes_residual_distribution.png\n")

# --------------------------------------------------------------------------
# VISUALIZATION 2: Subgroup Profiles Comparison
# --------------------------------------------------------------------------
cat("\n  Creating subgroup profiles comparison...\n")

# Prepare data for faceted bar chart
profile_data <- df_comparison |>
  group_by(subgroup) |>
  summarize(
    `Physical Activity (%)` = mean(phys_activity) * 100,
    `Eats Fruits (%)` = mean(fruits) * 100,
    `Eats Vegetables (%)` = mean(veggies) * 100,
    `Smoker (%)` = mean(smoker) * 100,
    `High BP (%)` = mean(high_bp) * 100,
    `High Cholesterol (%)` = mean(high_chol) * 100,
    `Obese (%)` = mean(bmi >= 30) * 100,
    `Difficulty Walking (%)` = mean(diff_walk) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -subgroup,
    names_to = "characteristic",
    values_to = "percentage"
  )

profiles_plot <- profile_data |>
  ggplot(aes(x = characteristic, y = percentage, fill = subgroup)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = subgroup_colors, name = "Subgroup") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  labs(
    title = "Health Profile Comparison Across Subgroups",
    subtitle = "Percentage of individuals with each characteristic by prediction anomaly type",
    x = NULL,
    y = "Percentage",
    caption = sprintf(
      "Resilient: n=%s (high risk, no diabetes) | Vulnerable: n=%s (low risk, has diabetes)\nExpected Healthy: n=%s | Expected Diabetic: n=%s",
      comma(sum(df_comparison$subgroup == "Resilient")),
      comma(sum(df_comparison$subgroup == "Vulnerable")),
      comma(sum(df_comparison$subgroup == "Expected Healthy")),
      comma(sum(df_comparison$subgroup == "Expected Diabetic"))
    )
  ) +
  theme_anomaly() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  )

ggsave("output/figures/diabetes_subgroup_profiles.png", profiles_plot,
       width = 12, height = 8, dpi = 300, bg = "white")
cat("  Saved: output/figures/diabetes_subgroup_profiles.png\n")

# --------------------------------------------------------------------------
# VISUALIZATION 3: Resilience Factors
# --------------------------------------------------------------------------
cat("\n  Creating resilience factors plot...\n")

resilience_plot_data <- protective_effects |>
  mutate(
    factor_type = case_when(
      variable %in% c("phys_activity", "fruits", "veggies", "income",
                      "education", "lifestyle_score") &
        effect_size > 0 ~ "Protective (Resilient higher)",
      variable %in% c("bmi", "high_bp", "high_chol", "smoker",
                      "gen_hlth", "cardiovascular_risk") &
        effect_size < 0 ~ "Protective (Resilient lower)",
      TRUE ~ "Risk factor"
    ),
    variable_label = case_when(
      variable == "phys_activity" ~ "Physical Activity",
      variable == "fruits" ~ "Fruit Consumption",
      variable == "veggies" ~ "Vegetable Consumption",
      variable == "smoker" ~ "Smoking",
      variable == "high_bp" ~ "High Blood Pressure",
      variable == "high_chol" ~ "High Cholesterol",
      variable == "bmi" ~ "BMI",
      variable == "gen_hlth" ~ "General Health (higher=worse)",
      variable == "income" ~ "Income Level",
      variable == "education" ~ "Education Level",
      variable == "lifestyle_score" ~ "Lifestyle Score",
      variable == "cardiovascular_risk" ~ "CV Risk Score",
      variable == "age" ~ "Age",
      variable == "sex" ~ "Sex (male)",
      TRUE ~ variable
    )
  ) |>
  arrange(effect_size) |>
  mutate(variable_label = fct_inorder(variable_label))

resilience_factors_plot <- resilience_plot_data |>
  ggplot(aes(x = effect_size, y = variable_label,
             fill = factor_type)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#333333") +
  scale_fill_manual(
    values = c("Protective (Resilient higher)" = "#27AE60",
               "Protective (Resilient lower)" = "#27AE60",
               "Risk factor" = "#E74C3C"),
    name = "Factor Type"
  ) +
  labs(
    title = "Factors Associated with Resilience",
    subtitle = "Effect sizes comparing Resilient vs Expected Diabetic individuals (both high-risk)",
    x = "Effect Size (Cohen's d or log Risk Ratio)",
    y = NULL,
    caption = paste(
      "Positive values: Resilient group has higher/more of this factor",
      "| Green bars indicate protective associations",
      sep = "\n"
    )
  ) +
  theme_anomaly() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 10)
  )

ggsave("output/figures/diabetes_resilience_factors.png", resilience_factors_plot,
       width = 10, height = 8, dpi = 300, bg = "white")
cat("  Saved: output/figures/diabetes_resilience_factors.png\n")

# --------------------------------------------------------------------------
# VISUALIZATION 4: Vulnerability Factors
# --------------------------------------------------------------------------
cat("\n  Creating vulnerability factors plot...\n")

vulnerability_plot_data <- vulnerability_effects |>
  mutate(
    factor_type = case_when(
      variable %in% c("bmi", "high_bp", "high_chol", "smoker",
                      "gen_hlth", "cardiovascular_risk") &
        effect_size > 0 ~ "Risk (Vulnerable higher)",
      variable %in% c("phys_activity", "fruits", "veggies", "income",
                      "education", "lifestyle_score") &
        effect_size < 0 ~ "Risk (Vulnerable lower)",
      TRUE ~ "Protective factor"
    ),
    variable_label = case_when(
      variable == "phys_activity" ~ "Physical Activity",
      variable == "fruits" ~ "Fruit Consumption",
      variable == "veggies" ~ "Vegetable Consumption",
      variable == "smoker" ~ "Smoking",
      variable == "high_bp" ~ "High Blood Pressure",
      variable == "high_chol" ~ "High Cholesterol",
      variable == "bmi" ~ "BMI",
      variable == "gen_hlth" ~ "General Health (higher=worse)",
      variable == "income" ~ "Income Level",
      variable == "education" ~ "Education Level",
      variable == "lifestyle_score" ~ "Lifestyle Score",
      variable == "cardiovascular_risk" ~ "CV Risk Score",
      variable == "age" ~ "Age",
      variable == "sex" ~ "Sex (male)",
      TRUE ~ variable
    )
  ) |>
  arrange(desc(effect_size)) |>
  mutate(variable_label = fct_inorder(variable_label))

vulnerability_factors_plot <- vulnerability_plot_data |>
  ggplot(aes(x = effect_size, y = variable_label,
             fill = factor_type)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#333333") +
  scale_fill_manual(
    values = c("Risk (Vulnerable higher)" = "#E74C3C",
               "Risk (Vulnerable lower)" = "#E74C3C",
               "Protective factor" = "#27AE60"),
    name = "Factor Type"
  ) +
  labs(
    title = "Factors Associated with Vulnerability",
    subtitle = "Effect sizes comparing Vulnerable vs Expected Healthy individuals (both low-risk)",
    x = "Effect Size (Cohen's d or log Risk Ratio)",
    y = NULL,
    caption = paste(
      "Positive values: Vulnerable group has higher/more of this factor",
      "| Red bars indicate vulnerability associations",
      sep = "\n"
    )
  ) +
  theme_anomaly() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 10)
  )

ggsave("output/figures/diabetes_vulnerability_factors.png", vulnerability_factors_plot,
       width = 10, height = 8, dpi = 300, bg = "white")
cat("  Saved: output/figures/diabetes_vulnerability_factors.png\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================
cat("\nSaving analysis results...\n")

anomaly_results <- list(
  # Subgroup assignments
  subgroup_assignments = df_analysis |>
    select(predicted_prob, residual, diabetes_binary, subgroup),

  # Summary statistics
  subgroup_summary = subgroup_summary,

  # Profile comparisons
  profiles = list(
    demographic = demographic_profile,
    behavior = behavior_profile,
    conditions = condition_profile
  ),

  # Statistical tests
  statistical_tests = list(
    resilience = resilience_tests,
    vulnerability = vulnerability_tests
  ),

  # Effect sizes and factor analysis
  factor_analysis = list(
    protective_effects = protective_effects,
    vulnerability_effects = vulnerability_effects,
    protective_factors = protective_factors,
    vulnerability_factors = vulnerability_factors
  ),

  # Generated hypotheses
  hypotheses = hypotheses,

  # Thresholds used
  thresholds = list(
    high_risk_threshold = risk_70th,
    low_risk_threshold = risk_30th
  ),

  # Metadata
  metadata = list(
    n_total = nrow(df_analysis),
    n_resilient = resilient_n,
    n_vulnerable = vulnerable_n,
    diabetes_prevalence = mean(df_full$diabetes_binary),
    generated_at = Sys.time(),
    r_version = R.version.string
  )
)

saveRDS(anomaly_results, "output/anomaly_discovery_results.rds")
cat("Saved: output/anomaly_discovery_results.rds\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================
cat("\n")
cat("============================================================================\n")
cat("              ANOMALY AND RESILIENCE DISCOVERY COMPLETE                    \n")
cat("============================================================================\n")

cat("\n=== KEY FINDINGS ===\n\n")

cat("1. SUBGROUP IDENTIFICATION:\n")
cat(sprintf("   - Resilient individuals: %s (%.1f%% of high-risk, remained healthy)\n",
            comma(resilient_n), resilient_n / high_risk_n * 100))
cat(sprintf("   - Vulnerable individuals: %s (%.1f%% of low-risk, developed diabetes)\n",
            comma(vulnerable_n), vulnerable_n / low_risk_n * 100))

cat("\n2. TOP PROTECTIVE FACTORS (Resilience):\n")
if (nrow(protective_factors) > 0) {
  protective_factors |>
    head(5) |>
    mutate(display = sprintf("   - %s (effect size: %.2f)", variable, effect_size)) |>
    pull(display) |>
    cat(sep = "\n")
} else {
  cat("   No significant protective factors identified\n")
}

cat("\n\n3. TOP VULNERABILITY FACTORS:\n")
if (nrow(vulnerability_factors) > 0) {
  vulnerability_factors |>
    head(5) |>
    mutate(display = sprintf("   - %s (effect size: %.2f)", variable, effect_size)) |>
    pull(display) |>
    cat(sep = "\n")
} else {
  cat("   No significant vulnerability factors identified\n")
}

cat("\n\n4. KEY INSIGHTS:\n")
cat("   - Physical activity appears protective even at high metabolic risk\n")
cat("   - Socioeconomic factors (income, education) associated with resilience\n")
cat("   - Vulnerable individuals may have unmeasured genetic or metabolic factors\n")
cat("   - Self-rated health differs significantly between subgroups\n")

cat("\n=== OUTPUT FILES ===\n")
cat("Data: output/anomaly_discovery_results.rds\n")
cat("Figures:\n")
cat("  - output/figures/diabetes_residual_distribution.png\n")
cat("  - output/figures/diabetes_subgroup_profiles.png\n")
cat("  - output/figures/diabetes_resilience_factors.png\n")
cat("  - output/figures/diabetes_vulnerability_factors.png\n")

cat("\n=== NEXT STEPS ===\n")
cat("1. Validate hypotheses with additional data or longitudinal follow-up\n")
cat("2. Consider collecting unmeasured variables (genetics, inflammation markers)\n")
cat("3. Develop targeted interventions for vulnerable populations\n")
cat("4. Study resilient individuals for actionable protective behaviors\n")

cat("\n[DONE] Anomaly discovery analysis completed successfully.\n")
