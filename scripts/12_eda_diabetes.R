# ============================================================================
# Title: EDA Visualizations for Diabetes Health Indicators
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Create publication-quality, modern visualizations exploring diabetes
#          risk factors, demographics, and health behaviors
# Input: data/processed/diabetes_clean.rds
# Output: output/figures/diabetes_*.png (7 visualizations)
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(scales)

# Set working directory context -----------------------------------------------
# (Using relative paths from project root)

# Define color palette and theme ----------------------------------------------

# Colorblind-safe palette for diabetes status
diabetes_colors <- c(

"No Diabetes" = "#2A9D8F",
"Prediabetes" = "#E9C46A",
"Diabetes" = "#E76F51"
)

# Modern theme for all visualizations
theme_diabetes <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      # Text styling
      text = element_text(family = "sans", color = "#333333"),
      plot.title = element_text(
        size = rel(1.5),
        face = "bold",
        margin = margin(b = 8),
        color = "#1a1a1a"
      ),
      plot.subtitle = element_text(
        size = rel(1.0),
        color = "#555555",
        margin = margin(b = 15),
        lineheight = 1.2
      ),
      plot.caption = element_text(
        size = rel(0.75),
        color = "#888888",
        hjust = 0,
        margin = margin(t = 15)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",

      # Axis styling
      axis.title = element_text(size = rel(0.95), color = "#444444"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = rel(0.85), color = "#555555"),
      axis.ticks = element_blank(),

      # Grid styling
      panel.grid.major = element_line(color = "#E8E8E8", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),

      # Legend styling
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(size = rel(0.9), face = "bold"),
      legend.text = element_text(size = rel(0.85)),
      legend.key.size = unit(0.8, "lines"),
      legend.margin = margin(b = 10),

      # Plot margins
      plot.margin = margin(20, 25, 15, 20),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),

      # Strip text for facets
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
df <- readRDS("data/processed/diabetes_clean.rds")

cat("Data loaded:", nrow(df), "rows,", ncol(df), "columns\n")

# =============================================================================
# 1. CLASS DISTRIBUTION BAR CHART
# =============================================================================
cat("\n[1/7] Creating class distribution chart...\n")

class_summary <- df |>
  count(diabetes_status) |>
  mutate(
    pct = n / sum(n) * 100,
    label = paste0(comma(n), "\n(", round(pct, 1), "%)")
  )

p1 <- ggplot(class_summary, aes(x = diabetes_status, y = n, fill = diabetes_status)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    vjust = -0.3,
    size = 4,
    fontface = "bold",
    color = "#333333",
    lineheight = 0.9
  ) +
  scale_fill_manual(values = diabetes_colors) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Severe Class Imbalance in Diabetes Dataset",
    subtitle = "No Diabetes cases dominate at 84%, while Prediabetes is vastly underrepresented at just 1.8%",
    x = NULL,
    y = "Number of Respondents",
    caption = "Source: CDC BRFSS Diabetes Health Indicators | N = 253,680"
  ) +
  theme_diabetes() +
  theme(
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 0.4)
  )

ggsave(
  "output/figures/diabetes_class_distribution.png",
  plot = p1,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

# =============================================================================
# 2. RISK FACTOR PREVALENCE BY DIABETES STATUS
# =============================================================================
cat("[2/7] Creating risk factor prevalence chart...\n")

# Calculate prevalence of each risk factor by diabetes status
risk_factors <- df |>
  group_by(diabetes_status) |>
  summarise(
    `High Blood Pressure` = mean(high_bp, na.rm = TRUE) * 100,
    `High Cholesterol` = mean(high_chol, na.rm = TRUE) * 100,
    `Stroke History` = mean(stroke, na.rm = TRUE) * 100,
    `Heart Disease` = mean(heart_diseaseor_attack, na.rm = TRUE) * 100,
    `Difficulty Walking` = mean(diff_walk, na.rm = TRUE) * 100,
    `Current/Former Smoker` = mean(smoker, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -diabetes_status,
    names_to = "risk_factor",
    values_to = "prevalence"
  ) |>
  mutate(
    risk_factor = factor(
      risk_factor,
      levels = c(
        "Stroke History", "Heart Disease", "Difficulty Walking",
        "High Blood Pressure", "High Cholesterol", "Current/Former Smoker"
      )
    )
  )

p2 <- ggplot(
  risk_factors,
  aes(x = prevalence, y = fct_rev(risk_factor), fill = diabetes_status)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = paste0(round(prevalence, 0), "%")),
    position = position_dodge(width = 0.75),
    hjust = -0.15,
    size = 3,
    fontface = "bold"
  ) +
  scale_fill_manual(values = diabetes_colors, name = "Diabetes Status") +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.2)),
    breaks = seq(0, 80, 20)
  ) +
  labs(
    title = "Diabetes Linked to Dramatically Higher Risk Factor Prevalence",
    subtitle = "People with diabetes show 3-6x higher rates of stroke and heart disease compared to non-diabetics",
    x = "Prevalence (%)",
    y = NULL,
    caption = "Source: CDC BRFSS Diabetes Health Indicators | N = 253,680"
  ) +
  theme_diabetes() +
  theme(
    panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal"
  )

ggsave(
  "output/figures/diabetes_risk_factors.png",
  plot = p2,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)

# =============================================================================
# 3. BMI DISTRIBUTION BY OUTCOME
# =============================================================================
cat("[3/7] Creating BMI density chart...\n")

# Calculate median BMI for each group
bmi_medians <- df |>
  group_by(diabetes_status) |>
  summarise(median_bmi = median(bmi, na.rm = TRUE), .groups = "drop")

p3 <- ggplot(df, aes(x = bmi, fill = diabetes_status, color = diabetes_status)) +
  # Density curves
  geom_density(alpha = 0.35, linewidth = 1) +
  # WHO cutoff lines
  geom_vline(
    xintercept = c(18.5, 25, 30),
    linetype = "dashed",
    color = "#666666",
    linewidth = 0.6
  ) +
  # WHO category labels
  annotate(
    "text",
    x = c(16, 21.75, 27.5, 35),
    y = 0.085,
    label = c("Under-\nweight", "Normal", "Over-\nweight", "Obese"),
    size = 3,
    color = "#666666",
    fontface = "italic",
    lineheight = 0.85
  ) +
  # Rug plot (sampled for performance)
  geom_rug(
    data = df |> slice_sample(n = 5000),
    aes(color = diabetes_status),
    alpha = 0.15,
    length = unit(0.02, "npc"),
    sides = "b"
  ) +
  scale_fill_manual(values = diabetes_colors, name = "Diabetes Status") +
  scale_color_manual(values = diabetes_colors, name = "Diabetes Status") +
  scale_x_continuous(
    limits = c(10, 60),
    breaks = seq(15, 55, 10)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Diabetes Risk Shifts Dramatically with Higher BMI",
    subtitle = "People with diabetes cluster in obese ranges (BMI >30), while non-diabetics peak near normal weight",
    x = "Body Mass Index (BMI)",
    y = "Density",
    caption = "Source: CDC BRFSS Diabetes Health Indicators | Dashed lines indicate WHO weight category cutoffs"
  ) +
  theme_diabetes() +
  theme(
    panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.4),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "#DDDDDD", linewidth = 0.3)
  )

ggsave(
  "output/figures/diabetes_bmi_density.png",
  plot = p3,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

# =============================================================================
# 4. AGE VS DIABETES RISK
# =============================================================================
cat("[4/7] Creating age risk chart...\n")

age_diabetes <- df |>
  count(age_group, diabetes_status) |>
  group_by(age_group) |>
  mutate(
    pct = n / sum(n) * 100,
    total = sum(n)
  ) |>
  ungroup()

# Get diabetes percentages for annotation
diabetes_by_age <- age_diabetes |>
  filter(diabetes_status == "Diabetes") |>
  select(age_group, diabetes_pct = pct)

p4 <- ggplot(age_diabetes, aes(x = age_group, y = pct, fill = diabetes_status)) +
  geom_col(position = "stack", width = 0.75) +
  # Add diabetes % labels on top
  geom_text(
    data = diabetes_by_age,
    aes(x = age_group, y = 102, label = paste0(round(diabetes_pct, 0), "%"), fill = NULL),
    size = 3,
    fontface = "bold",
    color = "#E76F51"
  ) +
  scale_fill_manual(values = diabetes_colors, name = "Diabetes Status") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Diabetes Prevalence Climbs Steeply with Age",
    subtitle = "Risk increases from 2% in young adults (18-24) to over 23% in those 65+. Numbers show diabetes %.",
    x = "Age Group",
    y = "Percentage of Respondents",
    caption = "Source: CDC BRFSS Diabetes Health Indicators | N = 253,680"
  ) +
  theme_diabetes() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 0.4)
  )

ggsave(
  "output/figures/diabetes_age_risk.png",
  plot = p4,
  width = 11,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

# =============================================================================
# 5. CORRELATION HEATMAP
# =============================================================================
cat("[5/7] Creating correlation heatmap...\n")

# Select numeric/ordinal variables for correlation
cor_vars <- df |>
  select(
    diabetes_012,
    high_bp, high_chol, bmi, smoker, stroke,
    heart_diseaseor_attack, phys_activity, fruits, veggies,
    hvy_alcohol_consump, gen_hlth, ment_hlth, phys_hlth,
    diff_walk, age, education, income
  )

# Calculate correlation matrix
cor_matrix <- cor(cor_vars, use = "pairwise.complete.obs")

# Extract correlations with diabetes_012 and prepare for plotting
diabetes_cors <- cor_matrix["diabetes_012", ] |>
  enframe(name = "variable", value = "correlation") |>
  filter(variable != "diabetes_012") |>
  mutate(
    variable = case_when(
      variable == "high_bp" ~ "High Blood Pressure",
      variable == "high_chol" ~ "High Cholesterol",
      variable == "bmi" ~ "BMI",
      variable == "smoker" ~ "Smoker",
      variable == "stroke" ~ "Stroke History",
      variable == "heart_diseaseor_attack" ~ "Heart Disease",
      variable == "phys_activity" ~ "Physical Activity",
      variable == "fruits" ~ "Fruit Consumption",
      variable == "veggies" ~ "Vegetable Consumption",
      variable == "hvy_alcohol_consump" ~ "Heavy Alcohol Use",
      variable == "gen_hlth" ~ "Poor General Health",
      variable == "ment_hlth" ~ "Poor Mental Health Days",
      variable == "phys_hlth" ~ "Poor Physical Health Days",
      variable == "diff_walk" ~ "Difficulty Walking",
      variable == "age" ~ "Age",
      variable == "education" ~ "Education Level",
      variable == "income" ~ "Income Level",
      TRUE ~ variable
    ),
    abs_cor = abs(correlation),
    direction = ifelse(correlation > 0, "Positive", "Negative")
  ) |>
  arrange(desc(abs_cor))

p5 <- ggplot(
  diabetes_cors,
  aes(x = correlation, y = fct_reorder(variable, abs_cor), fill = correlation)
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(
      label = sprintf("%.2f", correlation),
      hjust = ifelse(correlation > 0, -0.2, 1.2)
    ),
    size = 3.2,
    fontface = "bold"
  ) +
  geom_vline(xintercept = 0, linewidth = 0.8, color = "#333333") +
  scale_fill_gradient2(
    low = "#2A9D8F",
    mid = "#F5F5F5",
    high = "#E76F51",
    midpoint = 0,
    limits = c(-0.3, 0.3),
    name = "Correlation"
  ) +
  scale_x_continuous(
    limits = c(-0.15, 0.35),
    breaks = seq(-0.1, 0.3, 0.1)
  ) +
  labs(
    title = "Key Factors Correlated with Diabetes Status",
    subtitle = "Poor general health and high blood pressure show strongest associations; lifestyle factors show weaker links",
    x = "Pearson Correlation Coefficient",
    y = NULL,
    caption = "Source: CDC BRFSS Diabetes Health Indicators | Correlations with diabetes status (0/1/2 scale)"
  ) +
  theme_diabetes() +
  theme(
    panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    legend.position = "right"
  )

ggsave(
  "output/figures/diabetes_correlation.png",
  plot = p5,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

# =============================================================================
# 6. GENERAL HEALTH IMPACT
# =============================================================================
cat("[6/7] Creating general health chart...\n")

health_diabetes <- df |>
  count(gen_hlth_label, diabetes_status) |>
  group_by(gen_hlth_label) |>
  mutate(
    pct = n / sum(n) * 100,
    total = sum(n)
  ) |>
  ungroup()

# Get diabetes rates for annotation
diabetes_rates <- health_diabetes |>
  filter(diabetes_status == "Diabetes") |>
  mutate(label = paste0(round(pct, 0), "%"))

p6 <- ggplot(
  health_diabetes,
  aes(x = gen_hlth_label, y = pct, fill = diabetes_status)
) +
  geom_col(position = "stack", width = 0.7) +
  # Annotate diabetes percentages
  geom_segment(
    data = diabetes_rates,
    aes(
      x = as.numeric(gen_hlth_label) + 0.45,
      xend = as.numeric(gen_hlth_label) + 0.55,
      y = pct / 2 + (100 - pct),
      yend = pct / 2 + (100 - pct)
    ),
    color = "#E76F51",
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = diabetes_rates,
    aes(
      x = as.numeric(gen_hlth_label) + 0.65,
      y = pct / 2 + (100 - pct),
      label = label
    ),
    size = 3.5,
    fontface = "bold",
    color = "#E76F51",
    hjust = 0,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = diabetes_colors, name = "Diabetes Status") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Diabetes Prevalence Explodes Among Those with Poor Health",
    subtitle = "From 5% among 'Excellent' health to 35% among 'Poor' health - a 7-fold increase",
    x = "Self-Reported General Health Status",
    y = "Percentage of Respondents",
    caption = "Source: CDC BRFSS Diabetes Health Indicators | N = 253,680"
  ) +
  theme_diabetes() +
  theme(
    panel.grid.major.y = element_line(color = "#E8E8E8", linewidth = 0.4)
  )

ggsave(
  "output/figures/diabetes_gen_health.png",
  plot = p6,
  width = 10,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

# =============================================================================
# 7. LIFESTYLE FACTORS COMPARISON
# =============================================================================
cat("[7/7] Creating lifestyle factors chart...\n")

lifestyle <- df |>
  group_by(diabetes_status) |>
  summarise(
    `Physical Activity` = mean(phys_activity, na.rm = TRUE) * 100,
    `Eats Fruit Daily` = mean(fruits, na.rm = TRUE) * 100,
    `Eats Vegetables Daily` = mean(veggies, na.rm = TRUE) * 100,
    `Heavy Alcohol Use` = mean(hvy_alcohol_consump, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -diabetes_status,
    names_to = "lifestyle_factor",
    values_to = "prevalence"
  ) |>
  mutate(
    lifestyle_factor = factor(
      lifestyle_factor,
      levels = c(
        "Physical Activity", "Eats Vegetables Daily",
        "Eats Fruit Daily", "Heavy Alcohol Use"
      )
    )
  )

p7 <- ggplot(
  lifestyle,
  aes(x = prevalence, y = fct_rev(lifestyle_factor), color = diabetes_status)
) +
  # Lollipop segments
  geom_linerange(
    data = lifestyle |>
      select(lifestyle_factor, diabetes_status, prevalence) |>
      pivot_wider(names_from = diabetes_status, values_from = prevalence),
    aes(
      y = fct_rev(lifestyle_factor),
      xmin = `No Diabetes`,
      xmax = Diabetes,
      color = NULL
    ),
    color = "#CCCCCC",
    linewidth = 2,
    inherit.aes = FALSE
  ) +
  # Points
  geom_point(size = 5) +
  # Labels
  geom_text(
    aes(label = paste0(round(prevalence, 0), "%")),
    vjust = -1,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_manual(values = diabetes_colors, name = "Diabetes Status") +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    breaks = seq(0, 100, 25)
  ) +
  labs(
    title = "Lifestyle Gaps Between Diabetics and Non-Diabetics",
    subtitle = "Diabetics show 18 percentage points lower physical activity rates; diet differences are more modest",
    x = "Prevalence (%)",
    y = NULL,
    caption = "Source: CDC BRFSS Diabetes Health Indicators | N = 253,680"
  ) +
  theme_diabetes() +
  theme(
    panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

ggsave(
  "output/figures/diabetes_lifestyle.png",
  plot = p7,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# =============================================================================
# COMPLETION SUMMARY
# =============================================================================
cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("EDA VISUALIZATION COMPLETE\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("\n7 publication-quality visualizations saved to:\n")
cat("  output/figures/\n\n")

files <- list.files("output/figures/", pattern = "diabetes_.*\\.png$", full.names = TRUE)
for (f in files) {
  info <- file.info(f)
  cat(sprintf("  - %s (%.1f KB)\n", basename(f), info$size / 1024))
}

cat("\nAll visualizations use:\n")
cat("  - Colorblind-safe palette\n")
cat("  - 300 DPI resolution\n")
cat("  - White background\n")
cat("  - Narrative titles\n")
cat("  - Modern theme_diabetes()\n")
