# ============================================================================
# Title: Comprehensive Model Evaluation - Logistic Regression vs Random Forest
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Evaluate and compare model performance for diabetes prediction with
#          focus on metrics appropriate for imbalanced classification
# Input: output/models/logistic_model.rds, random_forest_model.rds,
#        model_predictions.rds, model_summary.rds
# Output: output/model_evaluation_results.rds
#         output/figures/model_*.png (5 visualizations)
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(yardstick)
library(pROC)
library(scales)
library(broom)

# Define color palette and theme ----------------------------------------------

# Colorblind-safe model colors
model_colors <- c(
  "Logistic Regression" = "#2A9D8F",
  "Random Forest" = "#E76F51"
)

# Accent colors for visualizations
accent_colors <- c(
  "teal" = "#2A9D8F",
  "coral" = "#E76F51",
  "gold" = "#E9C46A",
  "navy" = "#264653",
  "sand" = "#F4A261"
)

# Modern theme for model evaluation plots
theme_evaluation <- function(base_size = 12) {
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

# Load data -------------------------------------------------------------------
cat("Loading model predictions and summary...\n")

model_preds <- readRDS("output/models/model_predictions.rds")
model_summary <- readRDS("output/models/model_summary.rds")

# Extract predictions dataframe
preds <- model_preds$predictions

cat("Test set size:", nrow(preds), "observations\n")
cat("Class distribution:\n")
table(preds$actual) |> print()
cat("Prevalence:", mean(preds$actual), "\n")

# Store prevalence for later use
prevalence <- mean(preds$actual)

# =============================================================================
# 1. CONFUSION MATRICES AND CLASSIFICATION METRICS
# =============================================================================
cat("\n[1/8] Computing confusion matrices and classification metrics...\n")

# Function to compute all metrics from confusion matrix
compute_classification_metrics <- function(actual, predicted, prob, model_name) {

  # Create confusion matrix components
  tp <- sum(actual == 1 & predicted == 1)
  tn <- sum(actual == 0 & predicted == 0)
  fp <- sum(actual == 0 & predicted == 1)
  fn <- sum(actual == 1 & predicted == 0)

  n <- length(actual)

  # Core metrics
  accuracy <- (tp + tn) / n
  sensitivity <- tp / (tp + fn)  # Recall / True Positive Rate
  specificity <- tn / (tn + fp)  # True Negative Rate
  precision <- tp / (tp + fp)     # Positive Predictive Value
  npv <- tn / (tn + fn)           # Negative Predictive Value

  # F1 Score
  f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)

  # Balanced Accuracy (average of sensitivity and specificity)
  balanced_accuracy <- (sensitivity + specificity) / 2

  # Matthews Correlation Coefficient (use as.numeric to avoid integer overflow)
  mcc_num <- (as.numeric(tp) * as.numeric(tn)) - (as.numeric(fp) * as.numeric(fn))
  mcc_denom <- sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) *
                    as.numeric(tn + fp) * as.numeric(tn + fn))
  mcc <- if (is.na(mcc_denom) || mcc_denom == 0) 0 else mcc_num / mcc_denom

  # Return tibble with all metrics

  tibble(
    model = model_name,
    n = n,
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    npv = npv,
    f1_score = f1,
    balanced_accuracy = balanced_accuracy,
    mcc = mcc
  )
}

# Compute metrics for both models at default threshold (0.5)
logistic_metrics <- compute_classification_metrics(
  actual = preds$actual,
  predicted = preds$logistic_pred,
  prob = preds$logistic_prob,
  model_name = "Logistic Regression"
)

rf_metrics <- compute_classification_metrics(
  actual = preds$actual,
  predicted = preds$rf_pred,
  prob = preds$rf_prob,
  model_name = "Random Forest"
)

classification_metrics <- bind_rows(logistic_metrics, rf_metrics)

cat("Classification metrics at threshold = 0.5:\n")
classification_metrics |>
  select(model, accuracy, sensitivity, specificity, precision, f1_score, balanced_accuracy) |>
  print()

# =============================================================================
# 2. THRESHOLD-INDEPENDENT METRICS (AUC-ROC, AUC-PR, BRIER)
# =============================================================================
cat("\n[2/8] Computing threshold-independent metrics...\n")

# Compute ROC curves and AUC using pROC
roc_logistic <- roc(preds$actual, preds$logistic_prob, quiet = TRUE)
roc_rf <- roc(preds$actual, preds$rf_prob, quiet = TRUE)

# AUC with confidence intervals
auc_logistic <- ci.auc(roc_logistic)
auc_rf <- ci.auc(roc_rf)

cat("AUC-ROC:\n")
cat(sprintf("  Logistic: %.4f (95%% CI: %.4f - %.4f)\n",
            auc_logistic[2], auc_logistic[1], auc_logistic[3]))
cat(sprintf("  Random Forest: %.4f (95%% CI: %.4f - %.4f)\n",
            auc_rf[2], auc_rf[1], auc_rf[3]))

# Compute Precision-Recall curves
compute_pr_curve <- function(actual, prob) {
  # Sort by probability descending
  ord <- order(prob, decreasing = TRUE)
  actual_sorted <- actual[ord]
  prob_sorted <- prob[ord]

  # Calculate cumulative TP and FP
  cum_tp <- cumsum(actual_sorted)
  total_positives <- sum(actual)

  # Recall = TP / Total Positives
  recall <- cum_tp / total_positives

  # Precision = TP / (TP + FP) = TP / rank
  precision <- cum_tp / seq_along(cum_tp)

  # Add point at recall = 0
  recall <- c(0, recall)
  precision <- c(1, precision)
  threshold <- c(1, prob_sorted)

  tibble(
    threshold = threshold,
    precision = precision,
    recall = recall
  )
}

pr_logistic <- compute_pr_curve(preds$actual, preds$logistic_prob)
pr_rf <- compute_pr_curve(preds$actual, preds$rf_prob)

# Compute AUC-PR (area under precision-recall curve)
compute_auc_pr <- function(pr_data) {
  # Use trapezoidal rule
  n <- nrow(pr_data)
  auc <- 0
  for (i in 2:n) {
    auc <- auc + (pr_data$recall[i] - pr_data$recall[i-1]) *
      (pr_data$precision[i] + pr_data$precision[i-1]) / 2
  }
  auc
}

auc_pr_logistic <- compute_auc_pr(pr_logistic)
auc_pr_rf <- compute_auc_pr(pr_rf)

cat(sprintf("AUC-PR:\n  Logistic: %.4f\n  Random Forest: %.4f\n",
            auc_pr_logistic, auc_pr_rf))

# Compute Brier Score (calibration measure)
brier_logistic <- mean((preds$logistic_prob - preds$actual)^2)
brier_rf <- mean((preds$rf_prob - preds$actual)^2)

cat(sprintf("Brier Score (lower is better):\n  Logistic: %.4f\n  Random Forest: %.4f\n",
            brier_logistic, brier_rf))

# Create threshold-independent metrics tibble
threshold_independent_metrics <- tibble(
  model = c("Logistic Regression", "Random Forest"),
  auc_roc = c(auc_logistic[2], auc_rf[2]),
  auc_roc_lower = c(auc_logistic[1], auc_rf[1]),
  auc_roc_upper = c(auc_logistic[3], auc_rf[3]),
  auc_pr = c(auc_pr_logistic, auc_pr_rf),
  brier_score = c(brier_logistic, brier_rf)
)

# =============================================================================
# 3. ROC CURVE DATA FOR PLOTTING
# =============================================================================
cat("\n[3/8] Generating ROC curve data...\n")

# Extract ROC curve data
roc_data <- bind_rows(
  tibble(
    model = "Logistic Regression",
    sensitivity = roc_logistic$sensitivities,
    specificity = roc_logistic$specificities,
    threshold = roc_logistic$thresholds
  ),
  tibble(
    model = "Random Forest",
    sensitivity = roc_rf$sensitivities,
    specificity = roc_rf$specificities,
    threshold = roc_rf$thresholds
  )
)

# =============================================================================
# 4. PRECISION-RECALL CURVE DATA
# =============================================================================
cat("\n[4/8] Generating Precision-Recall curve data...\n")

pr_data <- bind_rows(
  pr_logistic |> mutate(model = "Logistic Regression"),
  pr_rf |> mutate(model = "Random Forest")
)

# =============================================================================
# 5. CALIBRATION ANALYSIS
# =============================================================================
cat("\n[5/8] Performing calibration analysis...\n")

compute_calibration <- function(actual, prob, model_name, n_bins = 10) {
  # Create decile bins
  bin_breaks <- seq(0, 1, length.out = n_bins + 1)

  tibble(
    actual = actual,
    prob = prob
  ) |>
    mutate(
      bin = cut(prob, breaks = bin_breaks, include.lowest = TRUE, labels = FALSE)
    ) |>
    group_by(bin) |>
    summarize(
      n = n(),
      mean_predicted = mean(prob),
      mean_observed = mean(actual),
      se = sqrt(mean_observed * (1 - mean_observed) / n),
      .groups = "drop"
    ) |>
    mutate(
      model = model_name,
      lower = pmax(0, mean_observed - 1.96 * se),
      upper = pmin(1, mean_observed + 1.96 * se)
    )
}

calibration_logistic <- compute_calibration(preds$actual, preds$logistic_prob,
                                             "Logistic Regression")
calibration_rf <- compute_calibration(preds$actual, preds$rf_prob,
                                       "Random Forest")

calibration_data <- bind_rows(calibration_logistic, calibration_rf)

# Compute calibration slope and intercept using linear regression
compute_calibration_coeffs <- function(actual, prob, model_name) {
  # Logit of predicted probabilities
  logit_prob <- qlogis(pmin(pmax(prob, 0.001), 0.999))

  # Fit logistic regression
  fit <- glm(actual ~ logit_prob, family = binomial())

  tibble(
    model = model_name,
    calibration_intercept = coef(fit)[1],
    calibration_slope = coef(fit)[2]
  )
}

calib_coeffs_logistic <- compute_calibration_coeffs(preds$actual, preds$logistic_prob,
                                                     "Logistic Regression")
calib_coeffs_rf <- compute_calibration_coeffs(preds$actual, preds$rf_prob,
                                               "Random Forest")

calibration_coefficients <- bind_rows(calib_coeffs_logistic, calib_coeffs_rf)

cat("Calibration coefficients (ideal: intercept=0, slope=1):\n")
print(calibration_coefficients)

# =============================================================================
# 6. OPTIMAL THRESHOLD ANALYSIS
# =============================================================================
cat("\n[6/8] Finding optimal thresholds...\n")

find_optimal_thresholds <- function(actual, prob, model_name) {

  thresholds <- seq(0.05, 0.95, by = 0.01)

  results <- map_dfr(thresholds, function(thresh) {
    pred <- as.integer(prob >= thresh)

    tp <- sum(actual == 1 & pred == 1)
    tn <- sum(actual == 0 & pred == 0)
    fp <- sum(actual == 0 & pred == 1)
    fn <- sum(actual == 1 & pred == 0)

    sens <- tp / (tp + fn)
    spec <- tn / (tn + fp)
    prec <- if ((tp + fp) > 0) tp / (tp + fp) else 0
    f1 <- if ((prec + sens) > 0) 2 * prec * sens / (prec + sens) else 0

    tibble(
      threshold = thresh,
      sensitivity = sens,
      specificity = spec,
      precision = prec,
      f1_score = f1,
      youden_j = sens + spec - 1,
      balanced = abs(sens - spec)  # Lower is more balanced
    )
  })

  # Find optimal thresholds
  youden_optimal <- results |> slice_max(youden_j, n = 1)
  balanced_optimal <- results |> slice_min(balanced, n = 1)
  f1_optimal <- results |> slice_max(f1_score, n = 1)

  tibble(
    model = model_name,
    metric = c("Youden's J (max sens + spec - 1)",
               "Balanced (sens = spec)",
               "F1 (max F1 score)"),
    optimal_threshold = c(youden_optimal$threshold[1],
                          balanced_optimal$threshold[1],
                          f1_optimal$threshold[1]),
    sensitivity = c(youden_optimal$sensitivity[1],
                    balanced_optimal$sensitivity[1],
                    f1_optimal$sensitivity[1]),
    specificity = c(youden_optimal$specificity[1],
                    balanced_optimal$specificity[1],
                    f1_optimal$specificity[1]),
    f1_score = c(youden_optimal$f1_score[1],
                 balanced_optimal$f1_score[1],
                 f1_optimal$f1_score[1])
  )
}

optimal_logistic <- find_optimal_thresholds(preds$actual, preds$logistic_prob,
                                             "Logistic Regression")
optimal_rf <- find_optimal_thresholds(preds$actual, preds$rf_prob,
                                       "Random Forest")

optimal_thresholds <- bind_rows(optimal_logistic, optimal_rf)

cat("Optimal thresholds:\n")
print(optimal_thresholds, n = 10)

# =============================================================================
# 7. MODEL COMPARISON SUMMARY TABLE
# =============================================================================
cat("\n[7/8] Creating model comparison summary...\n")

model_comparison <- classification_metrics |>
  select(model, accuracy, sensitivity, specificity, precision, npv,
         f1_score, balanced_accuracy, mcc) |>
  left_join(threshold_independent_metrics |>
              select(model, auc_roc, auc_pr, brier_score),
            by = "model")

cat("\n=== MODEL COMPARISON SUMMARY ===\n")
cat("\nMetrics at default threshold (0.5):\n")
model_comparison |>
  pivot_longer(-model, names_to = "metric", values_to = "value") |>
  pivot_wider(names_from = model, values_from = value) |>
  print(n = 15)

# =============================================================================
# 8. SAVE EVALUATION RESULTS
# =============================================================================
cat("\n[8/8] Saving evaluation results...\n")

evaluation_results <- list(
  # Classification metrics at default threshold
  classification_metrics = classification_metrics,

  # Threshold-independent metrics
  threshold_independent_metrics = threshold_independent_metrics,

  # Confusion matrix data
  confusion_matrices = list(
    logistic = classification_metrics |>
      filter(model == "Logistic Regression") |>
      select(tp, tn, fp, fn),
    random_forest = classification_metrics |>
      filter(model == "Random Forest") |>
      select(tp, tn, fp, fn)
  ),

  # Curve data for plotting
  roc_curve_data = roc_data,
  pr_curve_data = pr_data,
  calibration_data = calibration_data,

  # Calibration coefficients
  calibration_coefficients = calibration_coefficients,

  # Optimal threshold analysis
  optimal_thresholds = optimal_thresholds,

  # Model comparison summary
  model_comparison = model_comparison,

  # Metadata
  metadata = list(
    test_n = nrow(preds),
    prevalence = prevalence,
    generated_at = Sys.time(),
    r_version = R.version.string
  )
)

saveRDS(evaluation_results, "output/model_evaluation_results.rds")
cat("Saved evaluation results to: output/model_evaluation_results.rds\n")

# =============================================================================
# VISUALIZATION 1: ROC CURVES COMPARISON
# =============================================================================
cat("\n[VIZ 1/5] Creating ROC curves comparison...\n")

roc_plot <- roc_data |>
  mutate(fpr = 1 - specificity) |>
  ggplot(aes(x = fpr, y = sensitivity, color = model)) +
  # Reference diagonal
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "#888888", linewidth = 0.8) +
  # ROC curves
  geom_path(linewidth = 1.2, alpha = 0.9) +
  # Styling
  scale_color_manual(
    values = model_colors,
    labels = c(
      "Logistic Regression" = sprintf("Logistic Regression (AUC = %.3f)",
                                       auc_logistic[2]),
      "Random Forest" = sprintf("Random Forest (AUC = %.3f)", auc_rf[2])
    )
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), expand = c(0.01, 0.01)) +
  labs(
    title = "ROC Curves: Model Comparison for Diabetes Prediction",
    subtitle = sprintf("Test set: n = %s | Both models show good discrimination ability",
                       comma(nrow(preds))),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Model",
    caption = "AUC values shown with 95% confidence intervals from DeLong method"
  ) +
  theme_evaluation() +
  theme(
    legend.position = c(0.6, 0.25),
    legend.background = element_rect(fill = "white", color = "#E0E0E0"),
    legend.margin = margin(8, 12, 8, 12),
    panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.4)
  ) +
  coord_equal()

ggsave("output/figures/model_roc_curves.png", roc_plot,
       width = 9, height = 8, dpi = 300, bg = "white")
cat("Saved: output/figures/model_roc_curves.png\n")

# =============================================================================
# VISUALIZATION 2: PRECISION-RECALL CURVES
# =============================================================================
cat("\n[VIZ 2/5] Creating Precision-Recall curves...\n")

pr_plot <- pr_data |>
  ggplot(aes(x = recall, y = precision, color = model)) +
  # Baseline (prevalence) reference line
  geom_hline(yintercept = prevalence, linetype = "dashed",
             color = "#888888", linewidth = 0.8) +
  annotate("text", x = 0.95, y = prevalence + 0.03,
           label = sprintf("Baseline (Prevalence = %.1f%%)", prevalence * 100),
           hjust = 1, size = 3.5, color = "#666666") +
  # PR curves
  geom_path(linewidth = 1.2, alpha = 0.9) +
  # Styling
  scale_color_manual(
    values = model_colors,
    labels = c(
      "Logistic Regression" = sprintf("Logistic Regression (AUC-PR = %.3f)",
                                       auc_pr_logistic),
      "Random Forest" = sprintf("Random Forest (AUC-PR = %.3f)", auc_pr_rf)
    )
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), expand = c(0.01, 0.01)) +
  labs(
    title = "Precision-Recall Curves: Model Comparison",
    subtitle = sprintf("Test set with %.1f%% prevalence | PR curves more informative for imbalanced data",
                       prevalence * 100),
    x = "Recall (Sensitivity)",
    y = "Precision (Positive Predictive Value)",
    color = "Model",
    caption = "Dashed line shows the baseline precision equal to class prevalence"
  ) +
  theme_evaluation() +
  theme(
    legend.position = c(0.35, 0.2),
    legend.background = element_rect(fill = "white", color = "#E0E0E0"),
    legend.margin = margin(8, 12, 8, 12),
    panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.4)
  )

ggsave("output/figures/model_pr_curves.png", pr_plot,
       width = 10, height = 7, dpi = 300, bg = "white")
cat("Saved: output/figures/model_pr_curves.png\n")

# =============================================================================
# VISUALIZATION 3: CONFUSION MATRIX HEATMAPS
# =============================================================================
cat("\n[VIZ 3/5] Creating confusion matrix heatmaps...\n")

# Prepare confusion matrix data for visualization
cm_data <- classification_metrics |>
  select(model, tp, tn, fp, fn) |>
  pivot_longer(cols = c(tp, tn, fp, fn), names_to = "cell", values_to = "count") |>
  mutate(
    actual = case_when(
      cell %in% c("tp", "fn") ~ "Positive (Diabetes)",
      TRUE ~ "Negative (No Diabetes)"
    ),
    predicted = case_when(
      cell %in% c("tp", "fp") ~ "Positive",
      TRUE ~ "Negative"
    ),
    actual = factor(actual, levels = c("Positive (Diabetes)", "Negative (No Diabetes)")),
    predicted = factor(predicted, levels = c("Negative", "Positive"))
  ) |>
  group_by(model) |>
  mutate(
    total = sum(count),
    pct = count / total * 100,
    label = sprintf("%s\n(%.1f%%)", comma(count), pct)
  ) |>
  ungroup()

# Color mapping for cells
cm_data <- cm_data |>
  mutate(
    cell_type = case_when(
      cell == "tp" ~ "True Positive",
      cell == "tn" ~ "True Negative",
      cell == "fp" ~ "False Positive",
      cell == "fn" ~ "False Negative"
    ),
    fill_color = case_when(
      cell %in% c("tp", "tn") ~ count,  # Correct predictions
      TRUE ~ -count  # Incorrect predictions (negative for coloring)
    )
  )

cm_plot <- cm_data |>
  ggplot(aes(x = predicted, y = actual)) +
  geom_tile(aes(fill = cell_type), color = "white", linewidth = 2) +
  geom_text(aes(label = label), size = 4.5, fontface = "bold", color = "white") +
  scale_fill_manual(
    values = c(
      "True Positive" = "#2A9D8F",
      "True Negative" = "#2A9D8F",
      "False Positive" = "#E76F51",
      "False Negative" = "#E76F51"
    ),
    guide = "none"
  ) +
  facet_wrap(~model) +
  labs(
    title = "Confusion Matrices: Logistic Regression vs Random Forest",
    subtitle = sprintf("Test set: n = %s | Green = correct predictions, Red = errors",
                       comma(nrow(preds))),
    x = "Predicted Class",
    y = "Actual Class",
    caption = "Cell values show count and percentage of total test set"
  ) +
  theme_evaluation() +
  theme(
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 13),
    panel.grid = element_blank(),
    aspect.ratio = 1
  )

ggsave("output/figures/model_confusion_matrices.png", cm_plot,
       width = 12, height = 6, dpi = 300, bg = "white")
cat("Saved: output/figures/model_confusion_matrices.png\n")

# =============================================================================
# VISUALIZATION 4: FEATURE IMPORTANCE COMPARISON
# =============================================================================
cat("\n[VIZ 4/5] Creating feature importance comparison...\n")

# Get logistic regression coefficients (odds ratios)
logistic_importance <- model_summary$logistic$coefficients |>
  filter(term != "(Intercept)") |>
  mutate(
    # Use log(OR) for comparison, centered at 0
    importance = log(estimate),
    direction = ifelse(importance > 0, "Increases Risk", "Decreases Risk"),
    abs_importance = abs(importance)
  ) |>
  arrange(desc(abs_importance)) |>
  slice_head(n = 15) |>
  mutate(
    model = "Logistic Regression",
    feature = term
  ) |>
  select(model, feature, importance, direction, abs_importance)

# Get random forest variable importance
rf_importance <- model_summary$random_forest$variable_importance |>
  arrange(desc(importance)) |>
  slice_head(n = 15) |>
  mutate(
    model = "Random Forest",
    feature = variable,
    direction = "Importance",
    abs_importance = importance
  ) |>
  select(model, feature, importance, direction, abs_importance)

# Create side-by-side comparison
# For logistic: show log(OR) with direction
logistic_plot <- logistic_importance |>
  mutate(feature = fct_reorder(feature, abs_importance)) |>
  ggplot(aes(x = importance, y = feature, fill = direction)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_vline(xintercept = 0, color = "#333333", linewidth = 0.5) +
  scale_fill_manual(values = c("Increases Risk" = "#E76F51",
                                "Decreases Risk" = "#2A9D8F")) +
  labs(
    title = "Logistic Regression",
    subtitle = "Log(Odds Ratio) - distance from 0",
    x = "Log(Odds Ratio)",
    y = NULL,
    fill = "Effect"
  ) +
  theme_evaluation() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10)
  )

rf_plot <- rf_importance |>
  mutate(feature = fct_reorder(feature, importance)) |>
  ggplot(aes(x = importance, y = feature)) +
  geom_col(fill = "#E76F51", width = 0.7, alpha = 0.9) +
  labs(
    title = "Random Forest",
    subtitle = "Mean Decrease in Impurity",
    x = "Variable Importance",
    y = NULL
  ) +
  theme_evaluation() +
  theme(axis.text.y = element_text(size = 10))

# Combine plots
library(patchwork)

importance_plot <- (logistic_plot | rf_plot) +
  plot_annotation(
    title = "Feature Importance Comparison: Logistic Regression vs Random Forest",
    subtitle = "Top 15 features by importance | Different importance measures for each model type",
    caption = "Logistic: Log(OR) shows effect direction; RF: Mean decrease in node impurity (Gini)",
    theme = theme_evaluation() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
  )

ggsave("output/figures/model_feature_importance.png", importance_plot,
       width = 14, height = 8, dpi = 300, bg = "white")
cat("Saved: output/figures/model_feature_importance.png\n")

# =============================================================================
# VISUALIZATION 5: CALIBRATION PLOT
# =============================================================================
cat("\n[VIZ 5/5] Creating calibration plot...\n")

calibration_plot <- calibration_data |>
  ggplot(aes(x = mean_predicted, y = mean_observed, color = model)) +
  # Perfect calibration line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "#888888", linewidth = 0.8) +
  # Error bars
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.02,
                alpha = 0.5, linewidth = 0.6) +
  # Points
  geom_point(size = 4, alpha = 0.9) +
  # Line connecting points
  geom_line(linewidth = 1, alpha = 0.7) +
  # Styling
  scale_color_manual(values = model_colors) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Calibration Plot: Predicted vs Observed Probabilities",
    subtitle = "Points on the diagonal indicate perfect calibration",
    x = "Mean Predicted Probability (by decile)",
    y = "Observed Proportion (actual diabetes rate)",
    color = "Model",
    caption = sprintf(
      "Logistic: Intercept = %.3f, Slope = %.3f | RF: Intercept = %.3f, Slope = %.3f\nIdeal calibration: Intercept = 0, Slope = 1",
      calibration_coefficients$calibration_intercept[1],
      calibration_coefficients$calibration_slope[1],
      calibration_coefficients$calibration_intercept[2],
      calibration_coefficients$calibration_slope[2]
    )
  ) +
  theme_evaluation() +
  theme(
    legend.position = c(0.2, 0.85),
    legend.background = element_rect(fill = "white", color = "#E0E0E0"),
    legend.margin = margin(8, 12, 8, 12),
    panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.4)
  ) +
  coord_equal()

ggsave("output/figures/model_calibration.png", calibration_plot,
       width = 9, height = 8, dpi = 300, bg = "white")
cat("Saved: output/figures/model_calibration.png\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================
cat("\n")
cat("==============================================================================\n")
cat("                    MODEL EVALUATION COMPLETE                                 \n")
cat("==============================================================================\n")

cat("\n=== CLASSIFICATION METRICS (Threshold = 0.5) ===\n")
model_comparison |>
  mutate(across(where(is.numeric), ~round(., 4))) |>
  print()

cat("\n=== KEY FINDINGS ===\n")
cat(sprintf("1. AUC-ROC: Logistic = %.4f, RF = %.4f\n",
            auc_logistic[2], auc_rf[2]))
cat(sprintf("2. AUC-PR: Logistic = %.4f, RF = %.4f\n",
            auc_pr_logistic, auc_pr_rf))
cat(sprintf("3. Sensitivity: Logistic = %.1f%%, RF = %.1f%%\n",
            classification_metrics$sensitivity[1] * 100,
            classification_metrics$sensitivity[2] * 100))
cat(sprintf("4. Specificity: Logistic = %.1f%%, RF = %.1f%%\n",
            classification_metrics$specificity[1] * 100,
            classification_metrics$specificity[2] * 100))

cat("\n=== OUTPUT FILES ===\n")
cat("Data: output/model_evaluation_results.rds\n")
cat("Figures:\n")
cat("  - output/figures/model_roc_curves.png\n")
cat("  - output/figures/model_pr_curves.png\n")
cat("  - output/figures/model_confusion_matrices.png\n")
cat("  - output/figures/model_feature_importance.png\n")
cat("  - output/figures/model_calibration.png\n")

cat("\n[DONE] Model evaluation completed successfully.\n")
