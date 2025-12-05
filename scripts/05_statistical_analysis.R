# ============================================================================
# Title: Statistical Analysis of Iris and mtcars Datasets
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Perform statistical analysis and generate summary tables
# Input: data/raw/iris.csv, data/raw/mtcars.csv
# Output: output/statistical_results.rds
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(broom)

# Load data -------------------------------------------------------------------
iris_data <- read_csv("data/raw/iris.csv")
mtcars_data <- read_csv("data/raw/mtcars.csv")

# IRIS ANALYSIS ===============================================================

# Descriptive statistics by species ------------------------------------------
iris_descriptive <- iris_data |>
  group_by(Species) |>
  summarize(
    n = n(),
    sepal_length_mean = mean(Sepal.Length, na.rm = TRUE),
    sepal_length_sd = sd(Sepal.Length, na.rm = TRUE),
    sepal_width_mean = mean(Sepal.Width, na.rm = TRUE),
    sepal_width_sd = sd(Sepal.Width, na.rm = TRUE),
    petal_length_mean = mean(Petal.Length, na.rm = TRUE),
    petal_length_sd = sd(Petal.Length, na.rm = TRUE),
    petal_width_mean = mean(Petal.Width, na.rm = TRUE),
    petal_width_sd = sd(Petal.Width, na.rm = TRUE),
    .groups = "drop"
  )

# Overall descriptive statistics
iris_overall <- iris_data |>
  summarize(
    n = n(),
    n_species = n_distinct(Species),
    across(
      c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
      list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

# ANOVA on Petal Length by Species -------------------------------------------
petal_length_anova <- aov(Petal.Length ~ Species, data = iris_data)
petal_length_anova_tidy <- tidy(petal_length_anova)
petal_length_anova_summary <- glance(petal_length_anova)

# Post-hoc Tukey HSD test
petal_length_tukey <- TukeyHSD(petal_length_anova)
petal_length_tukey_tidy <- tidy(petal_length_tukey)

# Check ANOVA assumptions
iris_anova_diagnostics <- list(
  shapiro_test = shapiro.test(residuals(petal_length_anova)),
  levene_test = car::leveneTest(Petal.Length ~ Species, data = iris_data)
)

# MTCARS ANALYSIS =============================================================

# Descriptive statistics for key variables ------------------------------------
mtcars_descriptive <- mtcars_data |>
  summarize(
    n = n(),
    across(
      c(mpg, hp, wt, disp),
      list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        q25 = ~quantile(., 0.25, na.rm = TRUE),
        q75 = ~quantile(., 0.75, na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

# Correlation matrix for key variables ---------------------------------------
mtcars_cor_vars <- mtcars_data |>
  select(mpg, hp, wt, disp)

mtcars_correlation <- cor(mtcars_cor_vars, use = "complete.obs")

# Correlation tests with p-values
mtcars_cor_tests <- list()
var_names <- c("mpg", "hp", "wt", "disp")
for (i in 1:(length(var_names) - 1)) {
  for (j in (i + 1):length(var_names)) {
    var1 <- var_names[i]
    var2 <- var_names[j]
    test <- cor.test(mtcars_cor_vars[[var1]], mtcars_cor_vars[[var2]])
    mtcars_cor_tests[[paste(var1, var2, sep = "_vs_")]] <- tidy(test)
  }
}

# Bind correlation tests into a single data frame
mtcars_cor_tests_df <- bind_rows(mtcars_cor_tests, .id = "comparison")

# Linear regression: mpg ~ hp + wt -------------------------------------------
mpg_model <- lm(mpg ~ hp + wt, data = mtcars_data)

# Model summary
mpg_model_tidy <- tidy(mpg_model, conf.int = TRUE)
mpg_model_glance <- glance(mpg_model)

# Model diagnostics
mpg_model_diagnostics <- list(
  residuals = residuals(mpg_model),
  fitted = fitted(mpg_model),
  shapiro_test = shapiro.test(residuals(mpg_model)),
  vif = car::vif(mpg_model)
)

# Augmented data with residuals and diagnostics
mpg_model_augment <- augment(mpg_model)

# COMPILE RESULTS =============================================================

statistical_results <- list(
  # Iris results
  iris = list(
    descriptive_by_species = iris_descriptive,
    descriptive_overall = iris_overall,
    anova = list(
      model = petal_length_anova_tidy,
      summary = petal_length_anova_summary,
      tukey = petal_length_tukey_tidy,
      diagnostics = iris_anova_diagnostics
    )
  ),

  # mtcars results
  mtcars = list(
    descriptive = mtcars_descriptive,
    correlation = list(
      matrix = mtcars_correlation,
      tests = mtcars_cor_tests_df
    ),
    regression = list(
      model = mpg_model_tidy,
      summary = mpg_model_glance,
      diagnostics = mpg_model_diagnostics,
      augmented_data = mpg_model_augment
    )
  ),

  # Metadata
  metadata = list(
    generated_date = Sys.time(),
    r_version = R.version.string,
    packages = c("tidyverse", "broom", "car")
  )
)

# Save results ----------------------------------------------------------------
dir.create("output", showWarnings = FALSE, recursive = TRUE)
saveRDS(statistical_results, "output/statistical_results.rds")

# Print summary of key findings -----------------------------------------------
cat("\n")
cat("=============================================================================\n")
cat("STATISTICAL ANALYSIS SUMMARY\n")
cat("=============================================================================\n\n")

cat("IRIS DATASET ANALYSIS\n")
cat("---------------------\n")
cat("Sample size:", nrow(iris_data), "observations across", n_distinct(iris_data$Species), "species\n\n")

cat("Descriptive Statistics by Species (Petal Length):\n")
print(iris_descriptive |> select(Species, n, petal_length_mean, petal_length_sd))
cat("\n")

cat("ANOVA: Petal Length by Species\n")
cat(sprintf("F(%d, %d) = %.2f, p %s\n",
            petal_length_anova_tidy$df[1],
            petal_length_anova_tidy$df[2],
            petal_length_anova_tidy$statistic[1],
            ifelse(petal_length_anova_tidy$p.value[1] < 0.001,
                   "< 0.001",
                   sprintf("= %.3f", petal_length_anova_tidy$p.value[1]))))
cat("Result: Significant differences in petal length across species\n\n")

cat("Post-hoc Tukey HSD Comparisons:\n")
print(petal_length_tukey_tidy |>
        select(contrast, estimate, conf.low, conf.high, adj.p.value))
cat("\n")

cat("=============================================================================\n")
cat("MTCARS DATASET ANALYSIS\n")
cat("-----------------------\n")
cat("Sample size:", nrow(mtcars_data), "vehicles\n\n")

cat("Descriptive Statistics for Key Variables:\n")
key_stats <- mtcars_descriptive |>
  select(n, ends_with("_mean"), ends_with("_sd"))
print(key_stats)
cat("\n")

cat("Correlation Matrix:\n")
print(round(mtcars_correlation, 3))
cat("\n")

cat("Strongest Correlations:\n")
mtcars_cor_tests_df |>
  arrange(desc(abs(estimate))) |>
  head(3) |>
  select(comparison, estimate, p.value) |>
  print()
cat("\n")

cat("Linear Regression: mpg ~ hp + wt\n")
cat(sprintf("R-squared = %.3f, Adjusted R-squared = %.3f\n",
            mpg_model_glance$r.squared,
            mpg_model_glance$adj.r.squared))
cat(sprintf("F(%d, %d) = %.2f, p %s\n\n",
            mpg_model_glance$df,
            mpg_model_glance$df.residual,
            mpg_model_glance$statistic,
            ifelse(mpg_model_glance$p.value < 0.001,
                   "< 0.001",
                   sprintf("= %.3f", mpg_model_glance$p.value))))

cat("Coefficients:\n")
print(mpg_model_tidy |>
        select(term, estimate, std.error, conf.low, conf.high, p.value))
cat("\n")

cat("Interpretation:\n")
cat("- Holding weight constant, each additional hp decreases mpg by",
    round(abs(mpg_model_tidy$estimate[mpg_model_tidy$term == "hp"]), 3), "\n")
cat("- Holding horsepower constant, each additional 1000 lbs decreases mpg by",
    round(abs(mpg_model_tidy$estimate[mpg_model_tidy$term == "wt"]), 3), "\n")
cat("\n")

cat("Model Diagnostics:\n")
cat("- Shapiro-Wilk test for normality of residuals: W =",
    round(mpg_model_diagnostics$shapiro_test$statistic, 4),
    ", p =", round(mpg_model_diagnostics$shapiro_test$p.value, 3), "\n")
cat("- VIF (Variance Inflation Factor):\n")
print(mpg_model_diagnostics$vif)
cat("  (VIF < 5 indicates no problematic multicollinearity)\n\n")

cat("=============================================================================\n")
cat("Results saved to: output/statistical_results.rds\n")
cat("=============================================================================\n")
