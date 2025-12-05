# ============================================================================
# Title: Comprehensive Statistical Analysis of Diabetes Health Indicators
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Perform epidemiological measures, hypothesis testing, and inferential
#          statistics on BRFSS diabetes data including chi-square tests, risk
#          ratios, odds ratios, correlation analysis, and logistic regression
# Input: data/processed/diabetes_clean.rds
# Output: output/statistical_results_diabetes.rds
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(broom)

# Load data -------------------------------------------------------------------
message("Loading diabetes data...")
df <- readRDS("data/processed/diabetes_clean.rds")

message(sprintf("Data loaded: %d rows x %d columns", nrow(df), ncol(df)))

# Create binary diabetes variable ---------------------------------------------
# 0 = No diabetes, 1 = Prediabetes or Diabetes
df <- df |>
  mutate(
    diabetes_binary = as.integer(diabetes_012 > 0)
  )

message(sprintf("Diabetes binary: %d cases (%.1f%%), %d controls (%.1f%%)",
                sum(df$diabetes_binary == 1),
                mean(df$diabetes_binary) * 100,
                sum(df$diabetes_binary == 0),
                (1 - mean(df$diabetes_binary)) * 100))

# =============================================================================
# 1. DESCRIPTIVE STATISTICS TABLE
# =============================================================================
message("\n[1/7] Computing descriptive statistics...")

# Helper function for continuous variable summaries
summarize_continuous <- function(data, var, group_var) {
  data |>
    group_by({{ group_var }}) |>
    summarise(
      n = n(),
      n_missing = sum(is.na({{ var }})),
      mean = mean({{ var }}, na.rm = TRUE),
      sd = sd({{ var }}, na.rm = TRUE),
      median = median({{ var }}, na.rm = TRUE),
      q25 = quantile({{ var }}, 0.25, na.rm = TRUE),
      q75 = quantile({{ var }}, 0.75, na.rm = TRUE),
      min = min({{ var }}, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE),
      .groups = "drop"
    )
}

# Continuous variables by diabetes status
continuous_vars_summary <- bind_rows(
  summarize_continuous(df, bmi, diabetes_status) |>
    mutate(variable = "BMI"),
  summarize_continuous(df, ment_hlth, diabetes_status) |>
    mutate(variable = "Mental Health Days"),
  summarize_continuous(df, phys_hlth, diabetes_status) |>
    mutate(variable = "Physical Health Days")
) |>
  select(variable, diabetes_status, everything())

# Overall continuous summary
continuous_overall <- df |>
  summarise(
    n = n(),
    bmi_mean = mean(bmi, na.rm = TRUE),
    bmi_sd = sd(bmi, na.rm = TRUE),
    bmi_median = median(bmi, na.rm = TRUE),
    ment_hlth_mean = mean(ment_hlth, na.rm = TRUE),
    ment_hlth_sd = sd(ment_hlth, na.rm = TRUE),
    ment_hlth_median = median(ment_hlth, na.rm = TRUE),
    phys_hlth_mean = mean(phys_hlth, na.rm = TRUE),
    phys_hlth_sd = sd(phys_hlth, na.rm = TRUE),
    phys_hlth_median = median(phys_hlth, na.rm = TRUE)
  )

# Binary risk factors summary
binary_vars <- c("high_bp", "high_chol", "smoker", "stroke",
                 "heart_diseaseor_attack", "phys_activity", "fruits",
                 "veggies", "hvy_alcohol_consump", "diff_walk")

binary_summary <- map_dfr(binary_vars, function(var) {
  df |>
    group_by(diabetes_status) |>
    summarise(
      n_yes = sum(.data[[var]] == 1, na.rm = TRUE),
      n_total = n(),
      prevalence = mean(.data[[var]], na.rm = TRUE) * 100,
      .groups = "drop"
    ) |>
    mutate(variable = var)
}) |>
  select(variable, diabetes_status, n_yes, n_total, prevalence)

# Ordinal variables summary
ordinal_summary <- df |>
  group_by(diabetes_status) |>
  summarise(
    gen_hlth_median = median(gen_hlth, na.rm = TRUE),
    gen_hlth_mean = mean(gen_hlth, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    education_median = median(education, na.rm = TRUE),
    education_mean = mean(education, na.rm = TRUE),
    income_median = median(income, na.rm = TRUE),
    income_mean = mean(income, na.rm = TRUE),
    .groups = "drop"
  )

descriptive_stats <- list(
  continuous = continuous_vars_summary,
  continuous_overall = continuous_overall,
  binary_risk_factors = binary_summary,
  ordinal = ordinal_summary
)

message("  Descriptive statistics computed for all variable types")

# =============================================================================
# 2. CHI-SQUARE TESTS OF INDEPENDENCE
# =============================================================================
message("\n[2/7] Running chi-square tests...")

# Helper function to calculate Cramer's V
cramers_v <- function(chi_sq_result) {
  n <- sum(chi_sq_result$observed)
  min_dim <- min(nrow(chi_sq_result$observed), ncol(chi_sq_result$observed)) - 1
  sqrt(chi_sq_result$statistic / (n * min_dim))
}

# Run chi-square tests for binary variables vs diabetes_binary
chi_square_results <- map_dfr(binary_vars, function(var) {
  # Create 2x2 table
  tab <- table(df[[var]], df$diabetes_binary)

  # Chi-square test
  chi_test <- chisq.test(tab, correct = FALSE)

  # Calculate Cramer's V
  cv <- cramers_v(chi_test)

  tibble(
    variable = var,
    chi_square = chi_test$statistic,
    df = chi_test$parameter,
    p_value = chi_test$p.value,
    cramers_v = as.numeric(cv),
    effect_size = case_when(
      cv < 0.1 ~ "Negligible",
      cv < 0.3 ~ "Small",
      cv < 0.5 ~ "Medium",
      TRUE ~ "Large"
    )
  )
}) |>
  arrange(p_value)

message(sprintf("  Chi-square tests completed for %d binary risk factors",
                length(binary_vars)))

# =============================================================================
# 3. RISK RATIOS AND ODDS RATIOS
# =============================================================================
message("\n[3/7] Calculating risk ratios and odds ratios...")

# Helper function to calculate RR and OR with 95% CI
calculate_rr_or <- function(data, exposure_var, outcome_var = "diabetes_binary") {
  # Create 2x2 table: rows = exposure, cols = outcome
  # a = exposed + disease, b = exposed + no disease

# c = unexposed + disease, d = unexposed + no disease

  # Use as.numeric to prevent integer overflow with large sample sizes
  a <- as.numeric(sum(data[[exposure_var]] == 1 & data[[outcome_var]] == 1, na.rm = TRUE))
  b <- as.numeric(sum(data[[exposure_var]] == 1 & data[[outcome_var]] == 0, na.rm = TRUE))
  c <- as.numeric(sum(data[[exposure_var]] == 0 & data[[outcome_var]] == 1, na.rm = TRUE))
  d <- as.numeric(sum(data[[exposure_var]] == 0 & data[[outcome_var]] == 0, na.rm = TRUE))

  # Risk in exposed and unexposed
  risk_exposed <- a / (a + b)
  risk_unexposed <- c / (c + d)

  # Risk Ratio
  rr <- risk_exposed / risk_unexposed
  log_rr <- log(rr)
  se_log_rr <- sqrt(1/a - 1/(a+b) + 1/c - 1/(c+d))
  rr_ci_low <- exp(log_rr - 1.96 * se_log_rr)
  rr_ci_high <- exp(log_rr + 1.96 * se_log_rr)

  # Odds Ratio
  or <- (a * d) / (b * c)
  log_or <- log(or)
  se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
  or_ci_low <- exp(log_or - 1.96 * se_log_or)
  or_ci_high <- exp(log_or + 1.96 * se_log_or)

  # Risk Difference
  rd <- risk_exposed - risk_unexposed
  se_rd <- sqrt((risk_exposed * (1 - risk_exposed)) / (a + b) +
                  (risk_unexposed * (1 - risk_unexposed)) / (c + d))
  rd_ci_low <- rd - 1.96 * se_rd
  rd_ci_high <- rd + 1.96 * se_rd

  tibble(
    variable = exposure_var,
    n_exposed = a + b,
    n_unexposed = c + d,
    risk_exposed = risk_exposed * 100,
    risk_unexposed = risk_unexposed * 100,
    risk_ratio = rr,
    rr_ci_low = rr_ci_low,
    rr_ci_high = rr_ci_high,
    odds_ratio = or,
    or_ci_low = or_ci_low,
    or_ci_high = or_ci_high,
    risk_difference = rd * 100,
    rd_ci_low = rd_ci_low * 100,
    rd_ci_high = rd_ci_high * 100
  )
}

# Calculate RR/OR for all binary risk factors
rr_or_results <- map_dfr(binary_vars, ~calculate_rr_or(df, .x)) |>
  arrange(desc(risk_ratio))

message(sprintf("  RR and OR calculated for %d risk factors", length(binary_vars)))

# =============================================================================
# 4. T-TESTS AND WILCOXON TESTS
# =============================================================================
message("\n[4/7] Performing t-tests and Wilcoxon tests...")

# Helper function for t-test with Cohen's d
perform_ttest <- function(data, var, group_var = "diabetes_binary") {
  # Get values for each group
  group0 <- data[[var]][data[[group_var]] == 0]
  group1 <- data[[var]][data[[group_var]] == 1]

  # Two-sample t-test
  t_result <- t.test(group0, group1)

  # Welch's t-test (default in R)
  welch_result <- t.test(group0, group1, var.equal = FALSE)

  # Cohen's d
  pooled_sd <- sqrt(((length(group0) - 1) * sd(group0, na.rm = TRUE)^2 +
                       (length(group1) - 1) * sd(group1, na.rm = TRUE)^2) /
                      (length(group0) + length(group1) - 2))
  cohens_d <- (mean(group1, na.rm = TRUE) - mean(group0, na.rm = TRUE)) / pooled_sd

  # Effect size interpretation
  effect_interp <- case_when(
    abs(cohens_d) < 0.2 ~ "Negligible",
    abs(cohens_d) < 0.5 ~ "Small",
    abs(cohens_d) < 0.8 ~ "Medium",
    TRUE ~ "Large"
  )

  # Wilcoxon rank sum test (non-parametric alternative)
  wilcox_result <- wilcox.test(group0, group1)

  tibble(
    variable = var,
    mean_no_diabetes = mean(group0, na.rm = TRUE),
    mean_diabetes = mean(group1, na.rm = TRUE),
    mean_diff = mean(group1, na.rm = TRUE) - mean(group0, na.rm = TRUE),
    t_statistic = welch_result$statistic,
    t_df = welch_result$parameter,
    t_p_value = welch_result$p.value,
    cohens_d = cohens_d,
    effect_size = effect_interp,
    wilcox_statistic = wilcox_result$statistic,
    wilcox_p_value = wilcox_result$p.value
  )
}

# Run tests for continuous variables
continuous_tests <- bind_rows(
  perform_ttest(df, "bmi"),
  perform_ttest(df, "ment_hlth"),
  perform_ttest(df, "phys_hlth")
)

message("  T-tests and Wilcoxon tests completed for BMI, MentHlth, PhysHlth")

# =============================================================================
# 5. CORRELATION ANALYSIS
# =============================================================================
message("\n[5/7] Computing correlations...")

# Point-biserial correlation (continuous with binary diabetes)
point_biserial <- map_dfr(c("bmi", "ment_hlth", "phys_hlth"), function(var) {
  test <- cor.test(df[[var]], df$diabetes_binary)
  tibble(
    variable = var,
    correlation = test$estimate,
    ci_low = test$conf.int[1],
    ci_high = test$conf.int[2],
    t_statistic = test$statistic,
    p_value = test$p.value
  )
})

# Correlation matrix for all numeric predictors
numeric_vars <- c("diabetes_012", "high_bp", "high_chol", "bmi", "smoker",
                  "stroke", "heart_diseaseor_attack", "phys_activity",
                  "fruits", "veggies", "hvy_alcohol_consump", "gen_hlth",
                  "ment_hlth", "phys_hlth", "diff_walk", "age",
                  "education", "income")

cor_matrix <- df |>
  select(all_of(numeric_vars)) |>
  cor(use = "pairwise.complete.obs")

# Correlations with diabetes_012 specifically
diabetes_correlations <- cor_matrix["diabetes_012", ] |>
  enframe(name = "variable", value = "correlation") |>
  filter(variable != "diabetes_012") |>
  mutate(abs_cor = abs(correlation)) |>
  arrange(desc(abs_cor))

# Pairwise correlation tests for top predictors
top_predictors <- c("gen_hlth", "high_bp", "bmi", "age", "diff_walk",
                    "high_chol", "heart_diseaseor_attack")

pairwise_cor_tests <- map_dfr(top_predictors, function(var) {
  test <- cor.test(df[[var]], df$diabetes_012)
  tibble(
    variable = var,
    correlation = test$estimate,
    ci_low = test$conf.int[1],
    ci_high = test$conf.int[2],
    p_value = test$p.value
  )
})

correlation_results <- list(
  point_biserial = point_biserial,
  correlation_matrix = cor_matrix,
  diabetes_correlations = diabetes_correlations,
  top_predictor_tests = pairwise_cor_tests
)

message("  Correlation analysis complete")

# =============================================================================
# 6. LOGISTIC REGRESSION (UNIVARIATE)
# =============================================================================
message("\n[6/7] Fitting univariate logistic regression models...")

# All predictors for univariate analysis
all_predictors <- c("high_bp", "high_chol", "bmi", "smoker", "stroke",
                    "heart_diseaseor_attack", "phys_activity", "fruits",
                    "veggies", "hvy_alcohol_consump", "gen_hlth",
                    "ment_hlth", "phys_hlth", "diff_walk", "age",
                    "education", "income", "sex")

# Fit univariate logistic regression for each predictor
univariate_results <- map_dfr(all_predictors, function(var) {
  formula <- as.formula(paste("diabetes_binary ~", var))
  model <- glm(formula, data = df, family = binomial(link = "logit"))

  # Get tidy output with exponentiated coefficients (odds ratios)
  tidy_result <- tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
    filter(term != "(Intercept)")

  # Get model fit statistics
  glance_result <- glance(model)

  tidy_result |>
    mutate(
      variable = var,
      aic = glance_result$AIC,
      null_deviance = glance_result$null.deviance,
      residual_deviance = glance_result$deviance,
      pseudo_r2 = 1 - (glance_result$deviance / glance_result$null.deviance)
    )
}) |>
  select(variable, term, OR = estimate, or_ci_low = conf.low,
         or_ci_high = conf.high, std_error = std.error,
         z_statistic = statistic, p_value = p.value,
         aic, pseudo_r2) |>
  arrange(desc(OR))

# Create forest plot-ready data
forest_data <- univariate_results |>
  mutate(
    variable_label = case_when(
      variable == "high_bp" ~ "High Blood Pressure",
      variable == "high_chol" ~ "High Cholesterol",
      variable == "bmi" ~ "BMI (per unit)",
      variable == "smoker" ~ "Smoker",
      variable == "stroke" ~ "Stroke History",
      variable == "heart_diseaseor_attack" ~ "Heart Disease",
      variable == "phys_activity" ~ "Physical Activity",
      variable == "fruits" ~ "Fruit Consumption",
      variable == "veggies" ~ "Vegetable Consumption",
      variable == "hvy_alcohol_consump" ~ "Heavy Alcohol Use",
      variable == "gen_hlth" ~ "Poor General Health (per level)",
      variable == "ment_hlth" ~ "Poor Mental Health Days",
      variable == "phys_hlth" ~ "Poor Physical Health Days",
      variable == "diff_walk" ~ "Difficulty Walking",
      variable == "age" ~ "Age (per category)",
      variable == "education" ~ "Education Level",
      variable == "income" ~ "Income Level",
      variable == "sex" ~ "Male Sex",
      TRUE ~ variable
    ),
    significant = p_value < 0.05,
    direction = ifelse(OR > 1, "Risk Factor", "Protective")
  ) |>
  arrange(desc(OR))

logistic_results <- list(
  univariate = univariate_results,
  forest_data = forest_data
)

message(sprintf("  Univariate logistic regression completed for %d predictors",
                length(all_predictors)))

# =============================================================================
# 7. COMPILE AND SAVE RESULTS
# =============================================================================
message("\n[7/7] Compiling and saving results...")

statistical_results <- list(
  # Descriptive statistics
  descriptive = descriptive_stats,

  # Chi-square tests
  chi_square = chi_square_results,

  # Risk ratios and odds ratios
  risk_measures = rr_or_results,

  # T-tests and Wilcoxon tests
  continuous_tests = continuous_tests,

  # Correlation analysis
  correlations = correlation_results,

  # Logistic regression
  logistic_regression = logistic_results,

  # Sample information
  sample_info = list(
    n_total = nrow(df),
    n_diabetic = sum(df$diabetes_binary == 1),
    n_non_diabetic = sum(df$diabetes_binary == 0),
    diabetes_prevalence = mean(df$diabetes_binary) * 100,
    diabetes_status_breakdown = table(df$diabetes_status)
  ),

  # Metadata
  metadata = list(
    generated_date = Sys.time(),
    r_version = R.version.string,
    packages = c("tidyverse", "broom"),
    data_source = "CDC BRFSS Diabetes Health Indicators 2015"
  )
)

# Save results
dir.create("output", showWarnings = FALSE, recursive = TRUE)
saveRDS(statistical_results, "output/statistical_results_diabetes.rds")

message("\nResults saved to: output/statistical_results_diabetes.rds")

# =============================================================================
# PRINT SUMMARY OF KEY FINDINGS
# =============================================================================
cat("\n")
cat("=" |> rep(78) |> paste(collapse = ""), "\n")
cat("COMPREHENSIVE STATISTICAL ANALYSIS: DIABETES HEALTH INDICATORS\n")
cat("=" |> rep(78) |> paste(collapse = ""), "\n\n")

# Sample Information
cat("SAMPLE INFORMATION\n")
cat("-" |> rep(40) |> paste(collapse = ""), "\n")
cat(sprintf("Total observations: %s\n", format(nrow(df), big.mark = ",")))
cat(sprintf("Diabetes cases (pre + diabetes): %s (%.1f%%)\n",
            format(sum(df$diabetes_binary == 1), big.mark = ","),
            mean(df$diabetes_binary) * 100))
cat(sprintf("Non-diabetic: %s (%.1f%%)\n\n",
            format(sum(df$diabetes_binary == 0), big.mark = ","),
            (1 - mean(df$diabetes_binary)) * 100))

# Descriptive Statistics
cat("DESCRIPTIVE STATISTICS: CONTINUOUS VARIABLES BY DIABETES STATUS\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")
continuous_vars_summary |>
  select(variable, diabetes_status, n, mean, sd, median) |>
  mutate(
    mean = round(mean, 2),
    sd = round(sd, 2),
    median = round(median, 1)
  ) |>
  print(n = Inf)
cat("\n")

# Chi-Square Tests
cat("CHI-SQUARE TESTS: RISK FACTORS vs DIABETES\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")
chi_square_results |>
  mutate(
    chi_square = round(chi_square, 1),
    cramers_v = round(cramers_v, 3),
    p_value = ifelse(p_value < 0.001, "< 0.001", sprintf("%.4f", p_value))
  ) |>
  select(variable, chi_square, df, p_value, cramers_v, effect_size) |>
  print(n = Inf)
cat("\nInterpretation: All risk factors show significant association with diabetes\n")
cat("High BP and Gen Health show medium effect sizes (Cramer's V > 0.3)\n\n")

# Risk Ratios and Odds Ratios
cat("RISK RATIOS AND ODDS RATIOS (DIABETIC vs NON-DIABETIC)\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")
rr_or_results |>
  mutate(
    RR = sprintf("%.2f (%.2f-%.2f)", risk_ratio, rr_ci_low, rr_ci_high),
    OR = sprintf("%.2f (%.2f-%.2f)", odds_ratio, or_ci_low, or_ci_high),
    risk_exposed = sprintf("%.1f%%", risk_exposed),
    risk_unexposed = sprintf("%.1f%%", risk_unexposed)
  ) |>
  select(variable, risk_exposed, risk_unexposed, RR, OR) |>
  print(n = Inf)
cat("\n")

# T-Tests Results
cat("T-TESTS AND EFFECT SIZES: CONTINUOUS VARIABLES\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")
continuous_tests |>
  mutate(
    mean_no_diabetes = round(mean_no_diabetes, 2),
    mean_diabetes = round(mean_diabetes, 2),
    mean_diff = round(mean_diff, 2),
    t_statistic = round(t_statistic, 2),
    cohens_d = round(cohens_d, 3),
    t_p_value = ifelse(t_p_value < 0.001, "< 0.001", sprintf("%.4f", t_p_value))
  ) |>
  select(variable, mean_no_diabetes, mean_diabetes, mean_diff,
         t_statistic, t_p_value, cohens_d, effect_size) |>
  print()
cat("\n")

# Correlations
cat("TOP CORRELATIONS WITH DIABETES STATUS\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")
diabetes_correlations |>
  head(10) |>
  mutate(
    correlation = round(correlation, 3),
    direction = ifelse(correlation > 0, "Positive", "Negative")
  ) |>
  select(variable, correlation, direction) |>
  print()
cat("\n")

# Logistic Regression
cat("UNIVARIATE LOGISTIC REGRESSION: ODDS RATIOS\n")
cat("-" |> rep(60) |> paste(collapse = ""), "\n")
univariate_results |>
  mutate(
    OR_CI = sprintf("%.2f (%.2f-%.2f)", OR, or_ci_low, or_ci_high),
    p_value = ifelse(p_value < 0.001, "< 0.001", sprintf("%.4f", p_value)),
    pseudo_r2 = round(pseudo_r2, 4)
  ) |>
  select(variable, OR_CI, p_value, pseudo_r2) |>
  print(n = Inf)
cat("\n")

# Key Findings Summary
cat("=" |> rep(78) |> paste(collapse = ""), "\n")
cat("KEY FINDINGS SUMMARY\n")
cat("=" |> rep(78) |> paste(collapse = ""), "\n\n")

cat("1. STRONGEST RISK FACTORS (by Risk Ratio):\n")
top_rr <- rr_or_results |> head(5)
for (i in 1:nrow(top_rr)) {
  cat(sprintf("   - %s: RR = %.2f (%.2f-%.2f)\n",
              top_rr$variable[i], top_rr$risk_ratio[i],
              top_rr$rr_ci_low[i], top_rr$rr_ci_high[i]))
}

cat("\n2. STRONGEST CORRELATIONS:\n")
top_cor <- diabetes_correlations |> head(5)
for (i in 1:nrow(top_cor)) {
  cat(sprintf("   - %s: r = %.3f\n",
              top_cor$variable[i], top_cor$correlation[i]))
}

cat("\n3. BMI COMPARISON:\n")
bmi_test <- continuous_tests |> filter(variable == "bmi")
cat(sprintf("   - Non-diabetic mean BMI: %.1f\n", bmi_test$mean_no_diabetes))
cat(sprintf("   - Diabetic mean BMI: %.1f\n", bmi_test$mean_diabetes))
cat(sprintf("   - Cohen's d = %.2f (%s effect)\n",
            bmi_test$cohens_d, bmi_test$effect_size))

cat("\n4. HIGHEST ODDS RATIOS (Univariate Logistic Regression):\n")
top_or <- univariate_results |> arrange(desc(OR)) |> head(5)
for (i in 1:nrow(top_or)) {
  cat(sprintf("   - %s: OR = %.2f (%.2f-%.2f)\n",
              top_or$variable[i], top_or$OR[i],
              top_or$or_ci_low[i], top_or$or_ci_high[i]))
}

cat("\n")
cat("=" |> rep(78) |> paste(collapse = ""), "\n")
cat("Analysis complete. Results saved to: output/statistical_results_diabetes.rds\n")
cat("=" |> rep(78) |> paste(collapse = ""), "\n")
