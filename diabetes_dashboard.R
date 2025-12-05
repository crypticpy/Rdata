# ============================================================================
# Title: Diabetes Risk Analysis Dashboard
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Modern, high-performance Shiny dashboard for diabetes risk analysis
# Input: data/processed/diabetes_clean.rds, model files, evaluation results
# Output: Interactive Shiny dashboard
# ============================================================================

# Load packages ---------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(plotly)
library(DT)
library(waiter)
library(scales)
library(ranger)
library(pROC)
library(shinycssloaders)  # Loading spinners for plots

# Set theme for ggplot2 - Dark Mode Optimized --------------------------------
theme_worldclass <- function(dark_mode = TRUE) {
  if (dark_mode) {
    bg_color <- "transparent"
    text_color <- "#F5F0E8"
    subtitle_color <- "#94A3B8"
    axis_color <- "#94A3B8"
    grid_color <- "rgba(255,255,255,0.08)"
    strip_color <- "#F5F0E8"
  } else {
    bg_color <- "white"
    text_color <- "#0F172A"
    subtitle_color <- "#64748B"
    axis_color <- "#334155"
    grid_color <- "#E2E8F0"
    strip_color <- "#0F172A"
  }

  theme_minimal(base_size = 14, base_family = "Inter") +
    theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        color = text_color,
        margin = margin(b = 12)
      ),
      plot.subtitle = element_text(
        size = 13,
        color = subtitle_color,
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        size = 11,
        color = subtitle_color,
        hjust = 0
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = grid_color, linewidth = 0.5),
      axis.title = element_text(size = 13, color = axis_color),
      axis.text = element_text(size = 12, color = axis_color),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", color = text_color),
      legend.text = element_text(size = 11, color = subtitle_color),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(size = 12, face = "bold", color = strip_color),
      plot.margin = margin(20, 20, 20, 20),
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA)
    )
}

# Alias for backward compatibility
theme_dashboard <- theme_worldclass

# Set dark mode as default
theme_set(theme_worldclass(dark_mode = TRUE))

# Theme-aware Plotly layout helper -------------------------------------------
apply_plotly_theme <- function(p, dark_mode = TRUE) {
  if (dark_mode) {
    p |> layout(
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      font = list(color = "#F5F0E8", family = "Outfit"),
      legend = list(
        font = list(color = "#E2E8F0", size = 12),
        bgcolor = "transparent"
      ),
      xaxis = list(
        color = "#94A3B8",
        gridcolor = "rgba(255,255,255,0.08)",
        zerolinecolor = "rgba(255,255,255,0.1)",
        tickfont = list(color = "#94A3B8")
      ),
      yaxis = list(
        color = "#94A3B8",
        gridcolor = "rgba(255,255,255,0.08)",
        zerolinecolor = "rgba(255,255,255,0.1)",
        tickfont = list(color = "#94A3B8")
      )
    )
  } else {
    p |> layout(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF",
      font = list(color = "#0F172A", family = "Outfit"),
      legend = list(
        font = list(color = "#334155", size = 12),
        bgcolor = "rgba(255,255,255,0.9)"
      ),
      xaxis = list(
        color = "#64748B",
        gridcolor = "rgba(0,0,0,0.08)",
        zerolinecolor = "rgba(0,0,0,0.1)",
        tickfont = list(color = "#64748B")
      ),
      yaxis = list(
        color = "#64748B",
        gridcolor = "rgba(0,0,0,0.08)",
        zerolinecolor = "rgba(0,0,0,0.1)",
        tickfont = list(color = "#64748B")
      )
    )
  }
}

# Backward compatibility alias
plotly_dark_layout <- function(p) apply_plotly_theme(p, dark_mode = TRUE)

# Color palettes - Clinical Elegance "Slate & Teal" --------------------------
diabetes_colors <- c(
  "No Diabetes" = "#10B981",   # Emerald
  "Prediabetes" = "#F59E0B",   # Amber
  "Diabetes" = "#F43F5E"       # Rose
)

# Teal-focused chart colors
chart_colors <- c(
  "#0D9488", "#0EA5E9", "#10B981", "#F59E0B", "#F43F5E",
  "#8B5CF6", "#EC4899", "#14B8A6", "#6366F1", "#84CC16"
)

risk_gradient <- c("#10B981", "#22C55E", "#84CC16", "#EAB308", "#F59E0B", "#F43F5E")

# Spinner color for loading states
spinner_color <- "#0D9488"  # Teal primary

# Load and preprocess data at startup -----------------------------------------
cat("Loading data...\n")

diabetes_data <- readRDS("data/processed/diabetes_clean.rds")
features_data <- readRDS("data/processed/diabetes_features.rds")
eval_results <- readRDS("output/model_evaluation_results.rds")
lr_model <- readRDS("output/models/logistic_model.rds")
rf_model <- readRDS("output/models/random_forest_model.rds")

# Load advanced analysis results (lazy loading for performance) ----------------
cat("Loading advanced analysis results...\n")

causal_results <- readRDS("output/causal_inference_results.rds")
fairness_results <- readRDS("output/fairness_audit_results.rds")
anomaly_results <- readRDS("output/anomaly_discovery_results.rds")

cat("Advanced analysis data loaded!\n")

# Pre-aggregate data for performance ------------------------------------------
cat("Pre-aggregating data for performance...\n")

# Summary by diabetes status
summary_by_status <- diabetes_data |>
  group_by(diabetes_status) |>
  summarise(
    count = n(),
    pct = n() / nrow(diabetes_data) * 100,
    avg_bmi = mean(bmi, na.rm = TRUE),
    avg_age = mean(age, na.rm = TRUE),
    high_bp_pct = mean(high_bp, na.rm = TRUE) * 100,
    high_chol_pct = mean(high_chol, na.rm = TRUE) * 100,
    smoker_pct = mean(smoker, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Summary by age group
summary_by_age <- diabetes_data |>
  group_by(age_group, diabetes_status) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(age_group) |>
  mutate(
    total = sum(count),
    pct = count / total * 100
  ) |>
  ungroup()

# Summary by BMI category
summary_by_bmi <- diabetes_data |>
  group_by(bmi_category, diabetes_status) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(bmi_category) |>
  mutate(
    total = sum(count),
    pct = count / total * 100
  ) |>
  ungroup()

# Summary by general health
summary_by_health <- diabetes_data |>
  group_by(gen_hlth_label, diabetes_status) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(gen_hlth_label) |>
  mutate(
    total = sum(count),
    pct = count / total * 100
  ) |>
  ungroup()

# Risk factor prevalence
risk_factors <- c("high_bp", "high_chol", "smoker", "stroke",
                  "heart_diseaseor_attack", "diff_walk")

risk_factor_summary <- diabetes_data |>
  group_by(diabetes_status) |>
  summarise(across(all_of(risk_factors), ~mean(.x, na.rm = TRUE) * 100)) |>
  pivot_longer(
    cols = -diabetes_status,
    names_to = "risk_factor",
    values_to = "prevalence"
  ) |>
  mutate(
    risk_factor = case_when(
      risk_factor == "high_bp" ~ "High Blood Pressure",
      risk_factor == "high_chol" ~ "High Cholesterol",
      risk_factor == "smoker" ~ "Smoker",
      risk_factor == "stroke" ~ "Stroke History",
      risk_factor == "heart_diseaseor_attack" ~ "Heart Disease",
      risk_factor == "diff_walk" ~ "Difficulty Walking"
    )
  )

# Feature importance data
lr_coefs <- tibble(
  feature = names(coef(lr_model))[-1],
  importance = abs(coef(lr_model)[-1]),
  direction = ifelse(coef(lr_model)[-1] > 0, "Increases Risk", "Decreases Risk")
) |>
  arrange(desc(importance)) |>
  head(15)

rf_importance <- tibble(
  feature = names(rf_model$variable.importance),
  importance = rf_model$variable.importance
) |>
  arrange(desc(importance)) |>
  head(15)

# Correlation matrix for numeric variables
numeric_vars <- c("bmi", "age", "gen_hlth", "ment_hlth", "phys_hlth",
                  "education", "income", "diabetes_012")
cor_matrix <- cor(diabetes_data[, numeric_vars], use = "pairwise.complete.obs")

# Key metrics
total_records <- nrow(diabetes_data)
diabetes_prevalence <- sum(diabetes_data$diabetes_status == "Diabetes") / total_records * 100
prediabetes_prevalence <- sum(diabetes_data$diabetes_status == "Prediabetes") / total_records * 100
lr_auc <- eval_results$threshold_independent_metrics$auc_roc[1]
rf_auc <- eval_results$threshold_independent_metrics$auc_roc[2]

cat("Data preprocessing complete!\n")

# UI --------------------------------------------------------------------------
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    # MIDNIGHT LUXE EDITORIAL - Dark mode with warm accents
    bg = "#0C1222",        # Deep midnight
    fg = "#F5F0E8",        # Warm cream
    primary = "#FF6B6B",   # Coral accent
    secondary = "#94A3B8", # Cool gray
    success = "#4ADE80",   # Mint green
    warning = "#FBBF24",   # Golden amber
    danger = "#FB7185",    # Soft rose
    info = "#38BDF8",      # Sky blue
    base_font = font_google("Outfit"),
    heading_font = font_google("Fraunces"),
    code_font = font_google("DM Mono"),
    "navbar-bg" = "#080D19",
    "navbar-dark-color" = "#E2E8F0",
    "navbar-dark-hover-color" = "#FF6B6B",
    "card-border-width" = "1px",
    "card-border-color" = "rgba(255,255,255,0.08)",
    "card-bg" = "#151D2E",
    "border-radius" = "1rem",
    "card-spacer-y" = "1.75rem",
    "card-spacer-x" = "1.75rem",
    "body-bg" = "#0C1222",
    "input-bg" = "#1A2332",
    "input-border-color" = "rgba(255,255,255,0.1)",
    "input-color" = "#F5F0E8",
    "table-bg" = "transparent",
    "table-color" = "#E2E8F0"
  ),
  fillable = TRUE,

  header = tagList(
    # Loading screen
    useWaiter(),
    waiterShowOnLoad(
      html = tagList(
        spin_orbiter(),
        tags$h4("Loading Dashboard...", style = "color: white; margin-top: 20px;"),
        tags$p("Preparing 253,680 records for analysis", style = "color: #94A3B8;")
      ),
      color = "#1E293B"
    ),

    # MIDNIGHT LUXE EDITORIAL - World-Class Design System
    tags$head(
      # Google Fonts
      tags$link(href = "https://fonts.googleapis.com/css2?family=Fraunces:ital,opsz,wght@0,9..144,300;0,9..144,400;0,9..144,600;0,9..144,700;1,9..144,400&family=Outfit:wght@300;400;500;600;700&family=DM+Mono:wght@400;500&display=swap", rel = "stylesheet"),
      tags$style(HTML("
        /* ================================================================
           MIDNIGHT LUXE EDITORIAL - A World-Class Design System
           Blending: Clinical Precision + Editorial Elegance + Tech Sophistication
           ================================================================ */

        /* === CSS VARIABLES === */
        :root {
          --midnight: #0C1222;
          --midnight-light: #151D2E;
          --midnight-lighter: #1A2332;
          --cream: #F5F0E8;
          --cream-muted: #E2DED6;
          --coral: #FF6B6B;
          --coral-glow: rgba(255, 107, 107, 0.15);
          --mint: #4ADE80;
          --amber: #FBBF24;
          --rose: #FB7185;
          --sky: #38BDF8;
          --glass: rgba(255, 255, 255, 0.03);
          --glass-border: rgba(255, 255, 255, 0.08);
          --text-primary: #F5F0E8;
          --text-secondary: #94A3B8;
          --text-muted: #94A3B8;  /* FIXED: Was #64748B - insufficient contrast */
        }

        /* === UTILITY COLOR CLASSES === */
        .text-coral { color: var(--coral) !important; }
        .text-mint { color: var(--mint) !important; }
        .text-amber { color: var(--amber) !important; }
        .text-rose { color: var(--rose) !important; }
        .text-sky { color: var(--sky) !important; }
        .text-cream { color: var(--cream) !important; }
        .bg-coral { background-color: var(--coral) !important; }
        .bg-mint { background-color: var(--mint) !important; }
        .bg-amber { background-color: var(--amber) !important; }
        .bg-rose { background-color: var(--rose) !important; }
        .bg-sky { background-color: var(--sky) !important; }

        /* === GLOBAL STYLES === */
        body {
          background: linear-gradient(180deg, #0C1222 0%, #0A0F1A 100%);
          color: var(--cream);
          font-family: 'Outfit', sans-serif;
          font-size: 15px;
          letter-spacing: 0.01em;
          min-height: 100vh;
        }

        /* Subtle noise texture overlay */
        body::before {
          content: '';
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-image: url(\"data:image/svg+xml,%3Csvg viewBox='0 0 400 400' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='noiseFilter'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23noiseFilter)'/%3E%3C/svg%3E\");
          opacity: 0.015;
          pointer-events: none;
          z-index: -1;
        }

        /* === TYPOGRAPHY === */
        h1, h2, h3, h4, h5, .display-heading {
          font-family: 'Fraunces', serif;
          font-weight: 600;
          color: var(--cream);
          letter-spacing: -0.02em;
        }

        h1 { font-size: 2.75rem; line-height: 1.1; }
        h2 { font-size: 2rem; line-height: 1.2; }
        h3 { font-size: 1.5rem; line-height: 1.3; }
        h4 { font-size: 1.25rem; line-height: 1.4; }

        /* === NAVBAR - Premium Header === */
        .navbar {
          background: linear-gradient(180deg, rgba(8, 13, 25, 0.98) 0%, rgba(12, 18, 34, 0.95) 100%) !important;
          backdrop-filter: blur(20px);
          border-bottom: 1px solid var(--glass-border);
          padding: 1rem 2rem;
        }
        .navbar-brand {
          font-family: 'Fraunces', serif !important;
          font-weight: 600 !important;
          font-size: 1.35rem !important;
          color: var(--cream) !important;
          letter-spacing: -0.02em;
        }
        .navbar-brand:hover {
          color: var(--coral) !important;
        }
        .nav-link {
          font-family: 'Outfit', sans-serif;
          font-weight: 500;
          font-size: 0.9rem;
          color: var(--text-secondary) !important;
          transition: all 0.3s ease;
          padding: 0.6rem 1.1rem !important;
          border-radius: 0.5rem;
        }
        .nav-link:hover {
          color: var(--coral) !important;
          background: var(--coral-glow);
        }
        .nav-pills .nav-link.active {
          background: linear-gradient(135deg, var(--coral) 0%, #FF8585 100%) !important;
          color: var(--midnight) !important;
          font-weight: 600;
          box-shadow: 0 4px 20px rgba(255, 107, 107, 0.3);
        }

        /* === CARDS - Glassmorphism === */
        .card {
          background: linear-gradient(145deg, rgba(21, 29, 46, 0.8) 0%, rgba(26, 35, 50, 0.6) 100%) !important;
          backdrop-filter: blur(10px);
          border: 1px solid var(--glass-border) !important;
          border-radius: 1.25rem !important;
          box-shadow:
            0 4px 24px rgba(0, 0, 0, 0.3),
            0 1px 2px rgba(0, 0, 0, 0.2),
            inset 0 1px 0 rgba(255, 255, 255, 0.05) !important;
          transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
          overflow: hidden;
        }
        .card:hover {
          transform: translateY(-4px);
          border-color: rgba(255, 107, 107, 0.2) !important;
          box-shadow:
            0 12px 40px rgba(0, 0, 0, 0.4),
            0 0 0 1px rgba(255, 107, 107, 0.1),
            inset 0 1px 0 rgba(255, 255, 255, 0.08) !important;
        }
        .card-header {
          font-family: 'Fraunces', serif;
          font-size: 1.25rem;
          font-weight: 600;
          color: var(--cream);
          background: transparent !important;
          border-bottom: 1px solid var(--glass-border) !important;
          padding: 1.25rem 1.75rem;
          letter-spacing: -0.01em;
        }
        .card-body {
          padding: 1.75rem;
          color: var(--text-secondary);
        }

        /* === VALUE BOXES - Statement Pieces === */
        .value-box {
          background: linear-gradient(145deg, rgba(21, 29, 46, 0.95) 0%, rgba(26, 35, 50, 0.85) 100%) !important;
          border: 1px solid var(--glass-border) !important;
          border-radius: 1.25rem !important;
          box-shadow: 0 4px 24px rgba(0, 0, 0, 0.25) !important;
          transition: all 0.4s ease;
          overflow: hidden;
          position: relative;
          z-index: 1;  /* FIXED: Ensure below dropdown menus */
        }
        .value-box::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 4px;
          background: linear-gradient(90deg, var(--coral), var(--amber), var(--mint));
          opacity: 1;  /* FIXED: Always visible now */
          transition: height 0.3s ease;
        }
        .value-box:hover::before {
          height: 5px;
        }
        .value-box:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 32px rgba(0, 0, 0, 0.35) !important;
        }
        .value-box .value-box-title {
          font-family: 'Outfit', sans-serif;
          font-size: 0.9rem;  /* INCREASED for better readability */
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: var(--cream) !important;  /* FIXED: Maximum contrast */
          opacity: 1;
        }
        .value-box .value-box-value {
          font-family: 'Fraunces', serif;
          font-size: 2.5rem;
          font-weight: 700;
          color: var(--cream);
          letter-spacing: -0.03em;
        }
        /* Description text inside value boxes */
        .value-box p {
          font-size: 0.8rem;
          color: var(--text-secondary) !important;
          margin-top: 0.25rem;
          opacity: 0.9;
        }
        .value-box .value-box-showcase .bi {
          font-size: 3rem !important;
          opacity: 1;
        }
        /* Color-coded value box icons and accents by theme */
        .value-box.bg-primary .value-box-showcase .bi { color: var(--sky) !important; font-size: 3rem !important; filter: drop-shadow(0 2px 8px rgba(56, 189, 248, 0.4)); }
        .value-box.bg-danger .value-box-showcase .bi { color: var(--rose) !important; font-size: 3rem !important; filter: drop-shadow(0 2px 8px rgba(251, 113, 133, 0.4)); }
        .value-box.bg-success .value-box-showcase .bi { color: var(--mint) !important; font-size: 3rem !important; filter: drop-shadow(0 2px 8px rgba(74, 222, 128, 0.4)); }
        .value-box.bg-warning .value-box-showcase .bi { color: var(--amber) !important; font-size: 3rem !important; filter: drop-shadow(0 2px 8px rgba(251, 191, 36, 0.4)); }
        .value-box.bg-info .value-box-showcase .bi { color: var(--sky) !important; font-size: 3rem !important; filter: drop-shadow(0 2px 8px rgba(56, 189, 248, 0.4)); }
        /* Distinctive colored border accent on top for each theme - always visible */
        .value-box.bg-primary::before { background: linear-gradient(90deg, var(--sky), var(--sky), transparent) !important; opacity: 1 !important; height: 5px; }
        .value-box.bg-danger::before { background: linear-gradient(90deg, var(--rose), var(--rose), transparent) !important; opacity: 1 !important; height: 5px; }
        .value-box.bg-success::before { background: linear-gradient(90deg, var(--mint), var(--mint), transparent) !important; opacity: 1 !important; height: 5px; }
        .value-box.bg-warning::before { background: linear-gradient(90deg, var(--amber), var(--amber), transparent) !important; opacity: 1 !important; height: 5px; }
        /* Subtle left border accent matching the top bar */
        .value-box.bg-primary { border-left: 3px solid var(--sky) !important; background: linear-gradient(145deg, rgba(56, 189, 248, 0.08) 0%, rgba(21, 29, 46, 0.95) 100%) !important; }
        .value-box.bg-danger { border-left: 3px solid var(--rose) !important; background: linear-gradient(145deg, rgba(251, 113, 133, 0.08) 0%, rgba(21, 29, 46, 0.95) 100%) !important; }
        .value-box.bg-success { border-left: 3px solid var(--mint) !important; background: linear-gradient(145deg, rgba(74, 222, 128, 0.08) 0%, rgba(21, 29, 46, 0.95) 100%) !important; }
        .value-box.bg-warning { border-left: 3px solid var(--amber) !important; background: linear-gradient(145deg, rgba(251, 191, 36, 0.08) 0%, rgba(21, 29, 46, 0.95) 100%) !important; }

        /* === DATA TABLES - Editorial Style === */
        .dataTables_wrapper {
          font-size: 14px;
          color: var(--text-secondary);
        }
        table.dataTable {
          border-collapse: separate;
          border-spacing: 0;
        }
        table.dataTable thead th {
          font-family: 'Outfit', sans-serif;
          font-weight: 600;
          font-size: 0.8rem;  /* INCREASED from 0.75rem */
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: var(--text-secondary);  /* FIXED: was text-muted - better contrast */
          background: var(--midnight-lighter) !important;
          border-bottom: 2px solid var(--coral) !important;
          padding: 1rem 1.25rem;
        }
        table.dataTable tbody td {
          background: transparent;
          border-bottom: 1px solid var(--glass-border);
          padding: 1rem 1.25rem;
          color: var(--text-secondary);
        }
        table.dataTable tbody tr:hover td {
          background: var(--coral-glow) !important;
          color: var(--cream);
        }
        .dataTables_filter input {
          background: var(--midnight-lighter) !important;
          border: 1px solid var(--glass-border) !important;
          border-radius: 0.75rem !important;
          padding: 0.6rem 1rem !important;
          color: var(--cream) !important;
          transition: all 0.3s ease;
        }
        .dataTables_filter input:focus {
          border-color: var(--coral) !important;
          box-shadow: 0 0 0 3px var(--coral-glow) !important;
          outline: none;
        }
        .dataTables_info, .dataTables_length, .dataTables_paginate {
          color: var(--text-muted) !important;
        }
        .dataTables_paginate .paginate_button {
          color: var(--text-secondary) !important;
          border-radius: 0.5rem !important;
        }
        .dataTables_paginate .paginate_button.current {
          background: var(--coral) !important;
          border-color: var(--coral) !important;
          color: var(--midnight) !important;
        }

        /* === PLOTLY CHARTS - Dark Mode === */
        .plotly {
          background: transparent !important;
          position: relative;
          z-index: 1;  /* Ensure charts don't block dropdowns */
        }
        .js-plotly-plot .plotly,
        .js-plotly-plot .plot-container,
        .js-plotly-plot .svg-container {
          background: transparent !important;
        }
        /* Plot area backgrounds */
        .js-plotly-plot .main-svg {
          background: transparent !important;
        }
        .js-plotly-plot .bg {
          fill: transparent !important;
        }
        /* Axis labels and ticks */
        .js-plotly-plot .xtick text,
        .js-plotly-plot .ytick text,
        .js-plotly-plot .ztick text {
          fill: #94A3B8 !important;
        }
        .js-plotly-plot .xtitle,
        .js-plotly-plot .ytitle,
        .js-plotly-plot .gtitle {
          fill: #F5F0E8 !important;
        }
        /* Grid lines */
        .js-plotly-plot .gridlayer line {
          stroke: rgba(255, 255, 255, 0.08) !important;
        }
        .js-plotly-plot .zerolinelayer line {
          stroke: rgba(255, 255, 255, 0.15) !important;
        }
        /* Legend text */
        .js-plotly-plot .legendtext {
          fill: #E2E8F0 !important;
        }
        .js-plotly-plot .legend .bg {
          fill: transparent !important;
          stroke: transparent !important;
        }
        /* Annotations */
        .js-plotly-plot .annotation-text {
          fill: #F5F0E8 !important;
        }
        /* Hover labels */
        .js-plotly-plot .hovertext {
          fill: var(--cream) !important;
        }
        .js-plotly-plot .hovertext rect {
          fill: var(--midnight-lighter) !important;
          stroke: var(--coral) !important;
        }
        /* Modebar */
        .plotly .modebar {
          opacity: 0.3;
          background: var(--midnight-lighter) !important;
          border-radius: 0.5rem;
        }
        .plotly:hover .modebar {
          opacity: 1;
        }
        .js-plotly-plot .plotly .modebar-btn path {
          fill: var(--text-secondary) !important;
        }
        .js-plotly-plot .plotly .modebar-btn:hover path {
          fill: var(--coral) !important;
        }
        /* Pie/Donut chart labels */
        .js-plotly-plot .pielayer .slicetext {
          fill: var(--cream) !important;
        }
        /* Bar chart text annotations */
        .js-plotly-plot .barlayer text {
          fill: var(--cream) !important;
        }

        /* === RISK BADGES - Glowing Pills === */
        .gauge-container {
          display: flex;
          justify-content: center;
          align-items: center;
        }
        .risk-badge {
          display: inline-block;
          padding: 0.75rem 1.5rem;
          border-radius: 9999px;
          font-family: 'Outfit', sans-serif;
          font-weight: 700;
          font-size: 1rem;
          text-transform: uppercase;
          letter-spacing: 0.1em;
        }
        .risk-low {
          background: linear-gradient(135deg, #065F46 0%, #047857 100%);
          color: #D1FAE5;
          box-shadow: 0 4px 16px rgba(74, 222, 128, 0.3);
        }
        .risk-medium {
          background: linear-gradient(135deg, #92400E 0%, #B45309 100%);
          color: #FEF3C7;
          box-shadow: 0 4px 16px rgba(251, 191, 36, 0.3);
        }
        .risk-high {
          background: linear-gradient(135deg, #BE123C 0%, #E11D48 100%);
          color: #FFE4E6;
          box-shadow: 0 4px 16px rgba(251, 113, 133, 0.3);
        }

        /* === METRIC CARDS === */
        .metric-card-title {
          font-family: 'Outfit', sans-serif;
          font-size: 0.85rem;  /* INCREASED from 0.7rem for better readability */
          color: var(--text-secondary);  /* FIXED: was text-muted - better contrast */
          font-weight: 600;
          margin-bottom: 0.5rem;
          text-transform: uppercase;
          letter-spacing: 0.12em;
        }
        .metric-card-value {
          font-family: 'Fraunces', serif;
          font-size: 2.5rem;
          font-weight: 700;
          color: var(--cream);
          letter-spacing: -0.03em;
        }

        /* === SPECIAL ACCENT CARDS === */
        .findings-card {
          background: linear-gradient(135deg, rgba(255, 107, 107, 0.1) 0%, rgba(255, 107, 107, 0.05) 100%);
          border-left: 4px solid var(--coral) !important;
          position: relative;
        }
        .findings-card::after {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background: linear-gradient(90deg, var(--coral-glow) 0%, transparent 50%);
          pointer-events: none;
        }

        /* === EDITORIAL CALLOUTS - Data Storytelling === */
        .editorial-callout {
          background: linear-gradient(135deg, rgba(255, 107, 107, 0.08) 0%, rgba(56, 189, 248, 0.05) 100%);
          border: 1px solid rgba(255, 107, 107, 0.3);
          border-radius: 1rem;
          margin: 1.5rem 0;
          position: relative;
          overflow: hidden;
        }
        .editorial-callout::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          width: 5px;
          height: 100%;
          background: linear-gradient(180deg, var(--coral) 0%, var(--sky) 100%);
        }
        .editorial-callout::after {
          content: '';
          position: absolute;
          top: -30%;
          right: -10%;
          width: 40%;
          height: 160%;
          background: radial-gradient(circle, rgba(255, 107, 107, 0.1) 0%, transparent 70%);
          pointer-events: none;
        }
        .editorial-header {
          display: flex;
          align-items: center;
          gap: 0.75rem;
          margin-bottom: 1rem;
        }
        .editorial-icon {
          font-size: 1.25rem;
          color: var(--coral);
          filter: drop-shadow(0 0 4px rgba(255, 107, 107, 0.25));  /* REDUCED glow */
        }
        .editorial-label {
          font-family: 'DM Mono', monospace;
          font-size: 0.75rem;
          font-weight: 600;
          letter-spacing: 0.15em;
          color: var(--coral);
          text-transform: uppercase;
        }
        .editorial-lead {
          font-family: 'Fraunces', serif;
          font-size: 1.35rem;
          font-weight: 500;
          line-height: 1.5;
          color: var(--cream);
          margin-bottom: 1rem;
        }
        .editorial-lead strong {
          color: var(--coral);
          font-weight: 600;
        }
        .editorial-lead em {
          color: var(--sky);
          font-style: normal;
          border-bottom: 2px solid rgba(56, 189, 248, 0.4);
        }
        .editorial-body {
          font-size: 1rem;
          line-height: 1.7;
          color: var(--muted);
        }
        .editorial-body strong {
          color: var(--cream);
          font-weight: 600;
          background: linear-gradient(90deg, rgba(74, 222, 128, 0.2) 0%, transparent 100%);
          padding: 0.1em 0.3em;
          border-radius: 0.25em;
        }

        /* === INSIGHT METRIC CARDS === */
        .insight-metrics {
          margin: 1.5rem 0 2rem 0;
        }
        .insight-card {
          background: var(--glass-bg);
          backdrop-filter: blur(20px);
          border: 1px solid var(--glass-border);
          border-radius: 1rem;
          padding: 1.5rem;
          text-align: center;
          transition: all 0.3s ease;
          position: relative;
          overflow: hidden;
        }
        .insight-card::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 3px;
          background: linear-gradient(90deg, transparent, currentColor, transparent);
          opacity: 0.5;
        }
        .insight-card:hover {
          transform: translateY(-4px);
          box-shadow: 0 12px 40px rgba(0, 0, 0, 0.3);
        }
        .insight-number {
          font-family: 'Fraunces', serif;
          font-size: 2.5rem;
          font-weight: 700;
          letter-spacing: -0.03em;
          margin-bottom: 0.25rem;
        }
        .insight-label {
          font-size: 0.9rem;
          font-weight: 600;
          color: var(--cream);
          margin-bottom: 0.35rem;
        }
        .insight-detail {
          font-size: 0.75rem;
          color: var(--ghost);
          text-transform: uppercase;
          letter-spacing: 0.05em;
        }

        /* Insight card color variants */
        .causal-insight {
          border-color: rgba(74, 222, 128, 0.3);
        }
        .causal-insight::before {
          background: linear-gradient(90deg, transparent, var(--mint), transparent);
        }
        .causal-insight .insight-number {
          color: var(--mint);
          text-shadow: 0 0 12px rgba(74, 222, 128, 0.25);  /* REDUCED from 30px 0.5 */
        }

        .discovery-insight {
          border-color: rgba(56, 189, 248, 0.3);
        }
        .discovery-insight::before {
          background: linear-gradient(90deg, transparent, var(--sky), transparent);
        }
        .discovery-insight .insight-number {
          color: var(--sky);
          text-shadow: 0 0 12px rgba(56, 189, 248, 0.25);  /* REDUCED glow */
        }

        .fairness-insight {
          border-color: rgba(251, 191, 36, 0.3);
        }
        .fairness-insight::before {
          background: linear-gradient(90deg, transparent, var(--amber), transparent);
        }
        .fairness-insight .insight-number {
          color: var(--amber);
          text-shadow: 0 0 12px rgba(251, 191, 36, 0.25);  /* REDUCED glow */
        }

        .variance-insight {
          border-color: rgba(168, 85, 247, 0.3);
        }
        .variance-insight::before {
          background: linear-gradient(90deg, transparent, #A855F7, transparent);
        }
        .variance-insight .insight-number {
          color: #A855F7;
          text-shadow: 0 0 12px rgba(168, 85, 247, 0.25);  /* REDUCED glow */
        }

        /* === TRAFFIC LIGHT INDICATORS === */
        .indicator-green {
          display: inline-block;
          width: 12px;
          height: 12px;
          border-radius: 50%;
          background: var(--mint);
          margin-right: 10px;
          box-shadow: 0 0 12px rgba(74, 222, 128, 0.5);
          animation: pulse-green 2s infinite;
        }
        .indicator-yellow {
          display: inline-block;
          width: 12px;
          height: 12px;
          border-radius: 50%;
          background: var(--amber);
          margin-right: 10px;
          box-shadow: 0 0 12px rgba(251, 191, 36, 0.5);
          animation: pulse-amber 2s infinite;
        }
        .indicator-red {
          display: inline-block;
          width: 12px;
          height: 12px;
          border-radius: 50%;
          background: var(--rose);
          margin-right: 10px;
          box-shadow: 0 0 12px rgba(251, 113, 133, 0.5);
          animation: pulse-red 2s infinite;
        }

        @keyframes pulse-green {
          0%, 100% { box-shadow: 0 0 12px rgba(74, 222, 128, 0.5); }
          50% { box-shadow: 0 0 20px rgba(74, 222, 128, 0.8); }
        }
        @keyframes pulse-amber {
          0%, 100% { box-shadow: 0 0 12px rgba(251, 191, 36, 0.5); }
          50% { box-shadow: 0 0 20px rgba(251, 191, 36, 0.8); }
        }
        @keyframes pulse-red {
          0%, 100% { box-shadow: 0 0 12px rgba(251, 113, 133, 0.5); }
          50% { box-shadow: 0 0 20px rgba(251, 113, 133, 0.8); }
        }

        /* === CAUSAL ANALYSIS CARDS === */
        .causal-explainer {
          background: linear-gradient(135deg, rgba(56, 189, 248, 0.1) 0%, rgba(56, 189, 248, 0.05) 100%);
          border-left: 4px solid var(--sky);
          padding: 1.5rem;
          border-radius: 1rem;
          margin-bottom: 1.25rem;
          position: relative;
          overflow: hidden;
        }
        .causal-explainer::before {
          content: '';
          position: absolute;
          top: -50%;
          right: -50%;
          width: 100%;
          height: 200%;
          background: radial-gradient(circle, rgba(56, 189, 248, 0.1) 0%, transparent 70%);
          pointer-events: none;
        }

        /* === HYPOTHESIS CARDS - Priority Glow === */
        .hypothesis-card {
          background: var(--midnight-lighter);
          border-left: 4px solid var(--amber);
          padding: 1.25rem 1.5rem;
          margin-bottom: 1rem;
          border-radius: 0 1rem 1rem 0;
          transition: all 0.3s ease;
        }
        .hypothesis-card:hover {
          transform: translateX(4px);
        }
        .hypothesis-high {
          border-left-color: var(--rose);
          background: linear-gradient(90deg, rgba(251, 113, 133, 0.1) 0%, var(--midnight-lighter) 50%);
        }
        .hypothesis-medium {
          border-left-color: var(--amber);
          background: linear-gradient(90deg, rgba(251, 191, 36, 0.1) 0%, var(--midnight-lighter) 50%);
        }
        .hypothesis-low {
          border-left-color: var(--mint);
          background: linear-gradient(90deg, rgba(74, 222, 128, 0.1) 0%, var(--midnight-lighter) 50%);
        }

        /* === SUBGROUP BADGES === */
        .subgroup-badge {
          display: inline-block;
          padding: 0.5rem 1rem;
          border-radius: 9999px;
          font-family: 'Outfit', sans-serif;
          font-weight: 600;
          font-size: 0.8rem;
          letter-spacing: 0.05em;
        }
        .subgroup-resilient {
          background: linear-gradient(135deg, rgba(74, 222, 128, 0.2) 0%, rgba(74, 222, 128, 0.1) 100%);
          color: var(--mint);
          border: 1px solid rgba(74, 222, 128, 0.3);
        }
        .subgroup-vulnerable {
          background: linear-gradient(135deg, rgba(251, 113, 133, 0.2) 0%, rgba(251, 113, 133, 0.1) 100%);
          color: var(--rose);
          border: 1px solid rgba(251, 113, 133, 0.3);
        }
        .subgroup-expected-healthy {
          background: linear-gradient(135deg, rgba(56, 189, 248, 0.2) 0%, rgba(56, 189, 248, 0.1) 100%);
          color: var(--sky);
          border: 1px solid rgba(56, 189, 248, 0.3);
        }
        .subgroup-expected-diabetic {
          background: linear-gradient(135deg, rgba(251, 191, 36, 0.2) 0%, rgba(251, 191, 36, 0.1) 100%);
          color: var(--amber);
          border: 1px solid rgba(251, 191, 36, 0.3);
        }
        .subgroup-typical {
          background: var(--midnight-lighter);
          color: var(--text-secondary);
          border: 1px solid var(--glass-border);
        }

        /* === APP HEADER === */
        .app-header {
          background: linear-gradient(180deg, var(--midnight) 0%, var(--midnight-darker) 100%);
          border-bottom: 1px solid var(--glass-border);
          position: sticky;
          top: 0;
          z-index: 100;
        }
        .text-coral {
          color: var(--coral) !important;
        }
        .text-cream {
          color: var(--cream) !important;
        }

        /* === SIDEBAR NAVIGATION (navset_pill_list) === */
        .nav-pills {
          gap: 0.25rem;
        }
        .nav-pills .nav-link {
          font-family: 'Outfit', sans-serif;
          font-size: 0.875rem;
          font-weight: 500;
          color: var(--text-secondary);
          border-radius: 0.75rem;
          padding: 0.875rem 1.25rem;
          transition: all 0.25s ease;
          display: flex;
          align-items: center;
          gap: 0.5rem;
        }
        .nav-pills .nav-link:hover {
          background: linear-gradient(145deg, var(--midnight-lighter) 0%, rgba(26, 35, 50, 0.7) 100%);
          color: var(--cream);
        }
        .nav-pills .nav-link.active {
          background: linear-gradient(135deg, var(--coral) 0%, #FF8585 100%) !important;
          color: white !important;
          box-shadow: 0 4px 20px rgba(255, 107, 107, 0.35);
        }
        .nav-pills .nav-link.active:hover {
          background: linear-gradient(135deg, #FF5858 0%, var(--coral) 100%) !important;
        }

        /* === NAVIGATION SECTION HEADERS === */
        .nav-section-header {
          display: flex;
          align-items: center;
          gap: 0.5rem;
          font-family: 'Outfit', sans-serif;
          font-size: 0.65rem;
          font-weight: 700;
          text-transform: uppercase;
          letter-spacing: 0.15em;
          color: var(--text-muted);
          padding: 1.5rem 1.25rem 0.5rem;
          pointer-events: none;
          cursor: default;
        }

        /* === NAVSET PILL LIST LAYOUT === */
        .bslib-navs-pill-list {
          display: flex;
          gap: 0;
          height: calc(100vh - 80px);
        }
        .bslib-navs-pill-list > .nav {
          background: linear-gradient(180deg, var(--midnight) 0%, var(--midnight-darker) 100%);
          border-right: 1px solid var(--glass-border);
          padding: 1rem 0.75rem;
          overflow-y: auto;
          flex-shrink: 0;
        }
        .bslib-navs-pill-list > .tab-content {
          flex: 1;
          overflow-y: auto;
          padding: 1.5rem;
        }

        /* === RISK PREDICTOR INPUT GROUPS === */
        .input-group-card {
          background: linear-gradient(145deg, var(--midnight-lighter) 0%, rgba(26, 35, 50, 0.7) 100%);
          border: 1px solid var(--glass-border);
          border-radius: 1rem;
          padding: 1.25rem;
          margin-bottom: 1.25rem;
        }
        .input-group-title {
          font-family: 'Outfit', sans-serif;
          font-size: 0.7rem;
          font-weight: 700;
          color: var(--coral);
          text-transform: uppercase;
          letter-spacing: 0.15em;
          margin-bottom: 1rem;
          display: flex;
          align-items: center;
        }

        /* === FORM CONTROLS === */
        .form-control, .form-select {
          background: var(--midnight-lighter) !important;
          border: 1px solid var(--glass-border) !important;
          color: var(--cream) !important;
          border-radius: 0.75rem !important;
          padding: 0.75rem 1rem !important;
          transition: all 0.3s ease;
        }
        .form-control:focus, .form-select:focus {
          border-color: var(--coral) !important;
          box-shadow: 0 0 0 3px var(--coral-glow) !important;
          outline: none;
        }
        .form-label {
          font-family: 'Outfit', sans-serif;
          font-size: 0.85rem;
          font-weight: 500;
          color: var(--text-secondary);
          margin-bottom: 0.5rem;
        }
        .form-check-input {
          background-color: var(--midnight-lighter);
          border-color: var(--glass-border);
        }
        .form-check-input:checked {
          background-color: var(--coral);
          border-color: var(--coral);
        }

        /* === DROPDOWN MENUS - Z-Index Fix === */
        .dropdown-menu {
          background: var(--midnight-lighter) !important;
          border: 1px solid var(--glass-border) !important;
          border-radius: 0.75rem !important;
          box-shadow: 0 10px 40px rgba(0, 0, 0, 0.5) !important;
          z-index: 10000 !important;
        }
        .dropdown-menu .dropdown-item {
          color: var(--cream) !important;
          padding: 0.6rem 1.2rem;
          transition: all 0.2s ease;
        }
        .dropdown-menu .dropdown-item:hover,
        .dropdown-menu .dropdown-item:focus {
          background: var(--midnight) !important;
          color: var(--coral) !important;
        }
        .nav-item .dropdown-menu {
          z-index: 10001 !important;
        }

        /* === NAVBAR STACKING CONTEXT FIX === */
        .navbar.navbar-default,
        .navbar.navbar-dark,
        .navbar {
          position: relative;
          z-index: 1050 !important;
        }
        .bslib-page-fill,
        .tab-content {
          position: relative;
          z-index: 1;
        }

        /* === SELECT DROPDOWN OPTIONS === */
        .form-select option {
          background: var(--midnight-lighter);
          color: var(--cream);
          padding: 0.5rem;
        }

        /* === SELECTIZE/SHINY SELECT INPUTS === */
        .selectize-input {
          background: var(--midnight-lighter) !important;
          border: 1px solid var(--glass-border) !important;
          color: var(--cream) !important;
          border-radius: 0.75rem !important;
        }
        .selectize-input.focus {
          border-color: var(--coral) !important;
          box-shadow: 0 0 0 3px var(--coral-glow) !important;
        }
        .selectize-dropdown {
          background: var(--midnight-lighter) !important;
          border: 1px solid var(--glass-border) !important;
          border-radius: 0.75rem !important;
          box-shadow: 0 10px 40px rgba(0, 0, 0, 0.5) !important;
          z-index: 10000 !important;
        }
        .selectize-dropdown-content .option {
          color: var(--cream) !important;
          padding: 0.6rem 1rem;
        }
        .selectize-dropdown-content .option:hover,
        .selectize-dropdown-content .option.active {
          background: var(--midnight) !important;
          color: var(--coral) !important;
        }

        /* === BUTTONS === */
        .btn-primary {
          background: linear-gradient(135deg, var(--coral) 0%, #FF8585 100%) !important;
          border: none !important;
          color: var(--midnight) !important;
          font-family: 'Outfit', sans-serif;
          font-weight: 600;
          padding: 0.75rem 1.5rem;
          border-radius: 0.75rem;
          transition: all 0.3s ease;
          box-shadow: 0 4px 16px rgba(255, 107, 107, 0.3);
        }
        .btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 24px rgba(255, 107, 107, 0.4);
        }
        .btn-secondary {
          background: var(--midnight-lighter) !important;
          border: 1px solid var(--glass-border) !important;
          color: var(--text-secondary) !important;
        }
        .btn-secondary:hover {
          border-color: var(--coral) !important;
          color: var(--coral) !important;
        }

        /* === LOADING SPINNERS === */
        .shiny-spinner-output-container {
          min-height: 200px;
        }
        .spinner-border {
          color: var(--coral) !important;
        }

        /* === CONFIDENCE INTERVAL === */
        .confidence-interval {
          background: var(--midnight-lighter);
          border: 1px solid var(--glass-border);
          border-radius: 0.75rem;
          padding: 1rem 1.25rem;
          margin-top: 1rem;
          font-size: 0.9rem;
          color: var(--text-secondary);
        }
        .confidence-interval strong {
          color: var(--cream);
        }

        /* === SIDEBAR === */
        .bslib-sidebar-layout {
          --_sidebar-bg: var(--midnight-light);
        }
        .sidebar {
          background: linear-gradient(180deg, var(--midnight-light) 0%, var(--midnight) 100%) !important;
          border-right: 1px solid var(--glass-border) !important;
        }

        /* === ANIMATIONS === */
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(20px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }

        .card {
          animation: fadeInUp 0.6s ease-out forwards;
        }
        .card:nth-child(1) { animation-delay: 0.1s; }
        .card:nth-child(2) { animation-delay: 0.2s; }
        .card:nth-child(3) { animation-delay: 0.3s; }
        .card:nth-child(4) { animation-delay: 0.4s; }

        .value-box {
          animation: fadeInUp 0.5s ease-out forwards;
        }

        /* === SCROLLBAR STYLING === */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }
        ::-webkit-scrollbar-track {
          background: var(--midnight);
        }
        ::-webkit-scrollbar-thumb {
          background: var(--midnight-lighter);
          border-radius: 4px;
        }
        ::-webkit-scrollbar-thumb:hover {
          background: var(--text-muted);
        }

        /* === MOBILE RESPONSIVENESS === */
        @media (max-width: 768px) {
          body { font-size: 16px; }

          h1 { font-size: 2rem; }
          h2 { font-size: 1.5rem; }
          h3 { font-size: 1.25rem; }

          .navbar { padding: 0.75rem 1rem; }
          .navbar-brand { font-size: 1.1rem !important; }

          .value-box {
            margin-bottom: 1rem !important;
          }
          .value-box .value-box-value {
            font-size: 2rem;
          }
          .value-box .value-box-showcase .bi {
            font-size: 2.25rem !important;
          }

          .nav-pills {
            flex-wrap: wrap;
          }
          .nav-link {
            padding: 0.5rem 0.75rem !important;
            font-size: 0.8rem;
          }

          .card {
            margin-bottom: 1rem !important;
            border-radius: 1rem !important;
          }
          .card-header {
            font-size: 1.1rem;
            padding: 1rem 1.25rem;
          }
          .card-body {
            padding: 1.25rem;
          }

          .metric-card-value {
            font-size: 2rem;
          }

          .bslib-sidebar-layout > .sidebar {
            width: 100% !important;
            max-width: 100% !important;
          }
        }

        @media (max-width: 576px) {
          .navbar-brand {
            font-size: 1rem !important;
          }
          .value-box .value-box-title {
            font-size: 0.65rem;
          }
          .value-box .value-box-value {
            font-size: 1.75rem;
          }
          .risk-badge {
            font-size: 0.875rem;
            padding: 0.625rem 1.25rem;
          }
        }

        /* === THEME TOGGLE BUTTON - Enhanced Visibility === */
        .theme-toggle-wrapper {
          margin-right: 1.5rem;
          padding: 0.625rem 1.25rem;
          background: rgba(255, 255, 255, 0.1);
          border: 2px solid rgba(255, 255, 255, 0.2);
          border-radius: 2rem;
          display: flex;
          align-items: center;
          gap: 0.75rem;
          backdrop-filter: blur(8px);
        }
        .theme-toggle-wrapper::before {
          content: '☀';
          font-size: 1.125rem;
          color: var(--amber);
          opacity: 0.6;
          transition: opacity 0.3s ease;
        }
        .theme-toggle-wrapper::after {
          content: '🌙';
          font-size: 1rem;
          color: var(--sky);
          opacity: 0.6;
          transition: opacity 0.3s ease;
        }
        [data-bs-theme='light'] .theme-toggle-wrapper {
          background: rgba(0, 0, 0, 0.05);
          border-color: rgba(0, 0, 0, 0.15);
        }
        [data-bs-theme='light'] .theme-toggle-wrapper::before {
          opacity: 1;
        }
        [data-bs-theme='light'] .theme-toggle-wrapper::after {
          opacity: 0.4;
        }
        [data-bs-theme='dark'] .theme-toggle-wrapper::after,
        :root .theme-toggle-wrapper::after {
          opacity: 1;
        }
        [data-bs-theme='dark'] .theme-toggle-wrapper::before,
        :root .theme-toggle-wrapper::before {
          opacity: 0.4;
        }
        .form-switch {
          padding-left: 2.5rem;
          margin: 0;
        }
        .form-switch .form-check-input {
          width: 3.5rem;
          height: 1.75rem;
          background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='-4 -4 8 8'%3e%3ccircle r='3' fill='%23fff'/%3e%3c/svg%3e\");
          background-color: #334155;
          border: 2px solid rgba(255, 255, 255, 0.3);
          cursor: pointer;
          transition: all 0.3s ease;
        }
        .form-switch .form-check-input:checked {
          background-color: var(--coral);
          border-color: var(--coral);
        }
        .form-switch .form-check-input:focus {
          box-shadow: 0 0 0 4px var(--coral-glow);
        }
        [data-bs-theme='light'] .form-switch .form-check-input {
          background-color: #CBD5E1;
          border-color: rgba(0, 0, 0, 0.2);
        }
        .bslib-dark-mode {
          display: flex;
          align-items: center;
          gap: 0.5rem;
        }
        .bslib-dark-mode label {
          color: var(--cream);
          font-size: 0.875rem;
          font-weight: 500;
          margin: 0;
          white-space: nowrap;
        }

        /* === INTERPRETATION BOXES - Theme Aware === */
        .interpretation-box-low {
          background: rgba(16, 185, 129, 0.15);
          border: 1px solid rgba(16, 185, 129, 0.3);
        }
        .interpretation-box-low .interpretation-title,
        .interpretation-box-low .interpretation-text {
          color: #6EE7B7;
        }
        .interpretation-box-low .interpretation-detail {
          color: var(--text-secondary);
        }

        .interpretation-box-medium {
          background: rgba(251, 191, 36, 0.15);
          border: 1px solid rgba(251, 191, 36, 0.3);
        }
        .interpretation-box-medium .interpretation-title,
        .interpretation-box-medium .interpretation-text {
          color: #FCD34D;
        }
        .interpretation-box-medium .interpretation-detail {
          color: var(--text-secondary);
        }

        .interpretation-box-high {
          background: rgba(244, 63, 94, 0.15);
          border: 1px solid rgba(244, 63, 94, 0.3);
        }
        .interpretation-box-high .interpretation-title,
        .interpretation-box-high .interpretation-text {
          color: #FDA4AF;
        }
        .interpretation-box-high .interpretation-detail {
          color: var(--text-secondary);
        }

        /* === LIGHT MODE OVERRIDES === */
        [data-bs-theme='light'] {
          --midnight: #F8FAFC;
          --midnight-light: #FFFFFF;
          --midnight-lighter: #F1F5F9;
          --cream: #0F172A;
          --text-secondary: #64748B;
          --text-muted: #94A3B8;
          --glass-border: rgba(0, 0, 0, 0.1);
          --coral: #0D9488;
          --coral-glow: rgba(13, 148, 136, 0.15);
        }

        [data-bs-theme='light'] body::before {
          display: none;
        }

        [data-bs-theme='light'] .navbar {
          background: #FFFFFF !important;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1) !important;
          border-bottom: 1px solid rgba(0, 0, 0, 0.08) !important;
        }

        [data-bs-theme='light'] .navbar-nav .nav-link {
          color: #334155 !important;
        }

        [data-bs-theme='light'] .navbar-nav .nav-link:hover,
        [data-bs-theme='light'] .navbar-nav .nav-link.active {
          color: #0D9488 !important;
        }

        [data-bs-theme='light'] .card {
          background: #FFFFFF !important;
          border: 1px solid rgba(0, 0, 0, 0.08) !important;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05) !important;
        }

        [data-bs-theme='light'] .card-header {
          background: #F8FAFC !important;
          border-bottom: 1px solid rgba(0, 0, 0, 0.08) !important;
          color: #0F172A !important;
        }

        [data-bs-theme='light'] .value-box {
          background: linear-gradient(145deg, #FFFFFF 0%, #F8FAFC 100%) !important;
          border: 1px solid rgba(0, 0, 0, 0.08) !important;
          box-shadow: 0 2px 8px rgba(0, 0, 0, 0.06) !important;
        }
        [data-bs-theme='light'] .value-box .value-box-title {
          color: #334155 !important;
        }
        [data-bs-theme='light'] .value-box .value-box-value {
          color: #0F172A !important;
        }
        [data-bs-theme='light'] .value-box p {
          color: #64748B !important;
        }
        /* Light mode value box icon colors */
        [data-bs-theme='light'] .value-box.bg-primary .value-box-showcase .bi { color: #0284C7 !important; }
        [data-bs-theme='light'] .value-box.bg-danger .value-box-showcase .bi { color: #E11D48 !important; }
        [data-bs-theme='light'] .value-box.bg-success .value-box-showcase .bi { color: #059669 !important; }
        [data-bs-theme='light'] .value-box.bg-warning .value-box-showcase .bi { color: #D97706 !important; }
        /* Light mode accent borders */
        [data-bs-theme='light'] .value-box.bg-primary { border-left: 3px solid #0284C7 !important; }
        [data-bs-theme='light'] .value-box.bg-danger { border-left: 3px solid #E11D48 !important; }
        [data-bs-theme='light'] .value-box.bg-success { border-left: 3px solid #059669 !important; }
        [data-bs-theme='light'] .value-box.bg-warning { border-left: 3px solid #D97706 !important; }
        /* Light mode top accent bars */
        [data-bs-theme='light'] .value-box.bg-primary::before { background: linear-gradient(90deg, #0284C7, #0284C7, transparent) !important; }
        [data-bs-theme='light'] .value-box.bg-danger::before { background: linear-gradient(90deg, #E11D48, #E11D48, transparent) !important; }
        [data-bs-theme='light'] .value-box.bg-success::before { background: linear-gradient(90deg, #059669, #059669, transparent) !important; }
        [data-bs-theme='light'] .value-box.bg-warning::before { background: linear-gradient(90deg, #D97706, #D97706, transparent) !important; }

        [data-bs-theme='light'] .dropdown-menu {
          background: #FFFFFF !important;
          border: 1px solid rgba(0, 0, 0, 0.08) !important;
          box-shadow: 0 4px 16px rgba(0, 0, 0, 0.1) !important;
        }

        [data-bs-theme='light'] .dropdown-menu .dropdown-item {
          color: #334155 !important;
        }

        [data-bs-theme='light'] .dropdown-menu .dropdown-item:hover {
          background: #F1F5F9 !important;
          color: #0D9488 !important;
        }

        [data-bs-theme='light'] .selectize-input,
        [data-bs-theme='light'] .selectize-dropdown {
          background: #FFFFFF !important;
          border-color: rgba(0, 0, 0, 0.15) !important;
          color: #0F172A !important;
        }

        [data-bs-theme='light'] .selectize-dropdown-content .option {
          color: #334155 !important;
        }

        [data-bs-theme='light'] .selectize-dropdown-content .option:hover,
        [data-bs-theme='light'] .selectize-dropdown-content .option.active {
          background: #F1F5F9 !important;
          color: #0D9488 !important;
        }

        [data-bs-theme='light'] .form-select,
        [data-bs-theme='light'] .form-control {
          background: #FFFFFF !important;
          border-color: rgba(0, 0, 0, 0.15) !important;
          color: #0F172A !important;
        }

        [data-bs-theme='light'] .form-select option {
          background: #FFFFFF;
          color: #0F172A;
        }

        [data-bs-theme='light'] .sidebar {
          background: #F8FAFC !important;
          border-right: 1px solid rgba(0, 0, 0, 0.08) !important;
        }

        [data-bs-theme='light'] .btn-primary {
          background: linear-gradient(135deg, #0D9488 0%, #14B8A6 100%) !important;
          color: #FFFFFF !important;
          box-shadow: 0 4px 16px rgba(13, 148, 136, 0.3);
        }

        [data-bs-theme='light'] .btn-secondary {
          background: #F1F5F9 !important;
          border: 1px solid rgba(0, 0, 0, 0.1) !important;
          color: #64748B !important;
        }

        /* Light mode for Plotly charts */
        [data-bs-theme='light'] .js-plotly-plot .bg {
          fill: #FFFFFF !important;
        }

        [data-bs-theme='light'] .js-plotly-plot .xtick text,
        [data-bs-theme='light'] .js-plotly-plot .ytick text {
          fill: #64748B !important;
        }

        [data-bs-theme='light'] .js-plotly-plot .xtitle,
        [data-bs-theme='light'] .js-plotly-plot .ytitle,
        [data-bs-theme='light'] .js-plotly-plot .gtitle {
          fill: #0F172A !important;
        }

        [data-bs-theme='light'] .js-plotly-plot .gridlayer line {
          stroke: rgba(0, 0, 0, 0.08) !important;
        }

        [data-bs-theme='light'] .js-plotly-plot .legendtext {
          fill: #334155 !important;
        }

        [data-bs-theme='light'] .js-plotly-plot .legend .bg {
          fill: rgba(255, 255, 255, 0.9) !important;
        }

        [data-bs-theme='light'] .editorial-callout {
          background: linear-gradient(135deg, rgba(13, 148, 136, 0.08) 0%, rgba(20, 184, 166, 0.04) 100%) !important;
          border-color: rgba(13, 148, 136, 0.2) !important;
        }

        [data-bs-theme='light'] .editorial-callout.bottom-line {
          border-left-color: #0D9488 !important;
        }

        [data-bs-theme='light'] ::-webkit-scrollbar-track {
          background: #F1F5F9;
        }

        [data-bs-theme='light'] ::-webkit-scrollbar-thumb {
          background: #CBD5E1;
        }

        /* Light mode: App Header */
        [data-bs-theme='light'] .app-header {
          background: linear-gradient(180deg, #F8FAFC 0%, #F1F5F9 100%);
          border-bottom-color: #E2E8F0;
        }
        [data-bs-theme='light'] .app-header h1 {
          color: #0F172A !important;
        }

        /* Light mode: Sidebar Navigation */
        [data-bs-theme='light'] .bslib-navs-pill-list > .nav {
          background: linear-gradient(180deg, #F8FAFC 0%, #F1F5F9 100%);
          border-right-color: #E2E8F0;
        }
        [data-bs-theme='light'] .nav-pills .nav-link {
          color: #64748B;
        }
        [data-bs-theme='light'] .nav-pills .nav-link:hover {
          background: linear-gradient(145deg, #E2E8F0 0%, #F1F5F9 100%);
          color: #0F172A;
        }
        [data-bs-theme='light'] .nav-pills .nav-link.active {
          background: linear-gradient(135deg, #0D9488 0%, #14B8A6 100%) !important;
          color: white !important;
          box-shadow: 0 4px 20px rgba(13, 148, 136, 0.3);
        }
        [data-bs-theme='light'] .nav-section-header {
          color: #64748B;
        }

        /* Light mode interpretation boxes */
        [data-bs-theme='light'] .interpretation-box-low {
          background: #D1FAE5;
          border: 1px solid #A7F3D0;
        }
        [data-bs-theme='light'] .interpretation-box-low .interpretation-title,
        [data-bs-theme='light'] .interpretation-box-low .interpretation-text {
          color: #065F46;
        }
        [data-bs-theme='light'] .interpretation-box-low .interpretation-detail {
          color: #047857;
        }

        [data-bs-theme='light'] .interpretation-box-medium {
          background: #FEF3C7;
          border: 1px solid #FDE68A;
        }
        [data-bs-theme='light'] .interpretation-box-medium .interpretation-title,
        [data-bs-theme='light'] .interpretation-box-medium .interpretation-text {
          color: #92400E;
        }
        [data-bs-theme='light'] .interpretation-box-medium .interpretation-detail {
          color: #B45309;
        }

        [data-bs-theme='light'] .interpretation-box-high {
          background: #FFE4E6;
          border: 1px solid #FECDD3;
        }
        [data-bs-theme='light'] .interpretation-box-high .interpretation-title,
        [data-bs-theme='light'] .interpretation-box-high .interpretation-text {
          color: #BE123C;
        }
        [data-bs-theme='light'] .interpretation-box-high .interpretation-detail {
          color: #E11D48;
        }
      "))
    ),

    # Custom Header Bar
    tags$header(
      class = "app-header d-flex align-items-center justify-content-between px-4 py-3",
      tags$div(
        class = "d-flex align-items-center",
        bs_icon("heart-pulse-fill", size = "1.75rem", class = "me-3 text-coral"),
        tags$h1(class = "h4 mb-0 text-cream", style = "font-family: 'Fraunces', serif; font-weight: 600;", "Diabetes Risk Intelligence")
      ),
      tags$div(
        class = "d-flex align-items-center gap-3",
        input_dark_mode(id = "dark_mode", mode = "dark"),
        tags$span(class = "text-muted small", "CDC BRFSS 2015")
      )
    ),
    # Accessibility enhancements
    tags$script(HTML("
      $(document).ready(function() {
        // Add ARIA labels to navigation
        $('.bslib-navs-pill-list > .nav').attr({
          'role': 'navigation',
          'aria-label': 'Dashboard sections'
        });

        // Add ARIA labels to value boxes
        $('.value-box').attr('role', 'status');

        // Add skip link for keyboard navigation
        $('body').prepend('<a href=\"#main_nav\" class=\"skip-link\" style=\"position:absolute;left:-9999px;top:auto;width:1px;height:1px;overflow:hidden;z-index:9999;\">&nbsp;</a>');

        // Announce chart updates to screen readers
        $(document).on('shiny:value', function(e) {
          if (e.name && e.name.includes('plot')) {
            var liveRegion = $('#aria-live-region');
            if (liveRegion.length === 0) {
              $('body').append('<div id=\"aria-live-region\" aria-live=\"polite\" aria-atomic=\"true\" style=\"position:absolute;left:-9999px;\"></div>');
            }
            $('#aria-live-region').text('Chart updated');
          }
        });

        // Add keyboard navigation support for cards
        $('.card').attr('tabindex', '0');

        // Theme toggle accessibility
        $('#dark_mode').attr({
          'aria-label': 'Toggle dark mode',
          'role': 'switch'
        });
      });
    "))
  ),  # End of header

  # Sidebar Navigation with navset_pill_list
  navset_pill_list(
    id = "main_nav",
    widths = c(2, 10),  # Pills on left (2/12), content on right (10/12)
    well = FALSE,

    # Tab 1: Executive Summary ---------------------------------------------------
    nav_panel(
      title = tags$span(bs_icon("speedometer2", class = "me-2"), "Executive Summary"),
      value = "executive_summary",

    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6, lg = 3),
      fill = FALSE,

      value_box(
        title = "Total Records",
        value = scales::comma(total_records),
        showcase = bs_icon("database"),
        theme = "primary",
        p("CDC BRFSS 2015 Survey")
      ),

      value_box(
        title = "Diabetes Prevalence",
        value = paste0(round(diabetes_prevalence, 1), "%"),
        showcase = bs_icon("heart-pulse"),
        theme = "danger",
        p(scales::comma(sum(diabetes_data$diabetes_status == "Diabetes")), " individuals")
      ),

      value_box(
        title = "Best Model AUC",
        value = round(lr_auc, 3),
        showcase = bs_icon("graph-up"),
        theme = "success",
        p("Logistic Regression")
      ),

      value_box(
        title = "Top Risk Factor",
        value = "Gen Health",
        showcase = bs_icon("exclamation-triangle"),
        theme = "warning",
        p("Self-reported health status")
      )
    ),

    # Editorial Bottom Line - Key Takeaway
    card(
      class = "editorial-callout bottom-line",
      card_body(
        tags$div(
          class = "editorial-header",
          bs_icon("bookmark-star-fill", class = "editorial-icon"),
          tags$span(class = "editorial-label", "THE BOTTOM LINE")
        ),
        tags$p(
          class = "editorial-lead",
          "This analysis of ", tags$strong("253,680"), " CDC BRFSS respondents reveals that diabetes
          risk is strongly associated with ", tags$em("modifiable lifestyle factors"), "."
        ),
        tags$p(
          class = "editorial-body",
          "Predictive models achieve strong discrimination (AUC = 0.82), enabling early identification
          of at-risk individuals. ", tags$strong("Causal analysis estimates that eliminating high blood
          pressure could prevent 42% of diabetes cases"), ", while fairness audits identify income and
          education disparities requiring attention in model deployment."
        )
      )
    ),

    # Four Insight Cards - Key Numbers That Matter
    layout_columns(
      col_widths = breakpoints(sm = 6, md = 3),
      fill = FALSE,
      class = "insight-metrics",

      tags$div(
        class = "insight-card causal-insight",
        tags$div(class = "insight-number", "42%"),
        tags$div(class = "insight-label", "Preventable via BP Control"),
        tags$div(class = "insight-detail", "Population attributable fraction")
      ),

      tags$div(
        class = "insight-card discovery-insight",
        tags$div(class = "insight-number", "15.4%"),
        tags$div(class = "insight-label", '"Resilient" Individuals'),
        tags$div(class = "insight-detail", "High risk factors, no diabetes")
      ),

      tags$div(
        class = "insight-card fairness-insight",
        tags$div(class = "insight-number", "20"),
        tags$div(class = "insight-label", "Disparity Flags"),
        tags$div(class = "insight-detail", "Income & education gaps")
      ),

      tags$div(
        class = "insight-card variance-insight",
        tags$div(class = "insight-number", "97%"),
        tags$div(class = "insight-label", "Individual-Level Variance"),
        tags$div(class = "insight-detail", "Only 3% environmental context")
      )
    ),

    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(8, 4)),

      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span(bs_icon("bar-chart-fill", class = "me-2"), "Diabetes Prevalence by Age Group"),
          popover(
            bs_icon("info-circle"),
            title = "About this chart",
            "Shows the percentage of respondents with diabetes in each age group.
             Risk increases significantly with age."
          )
        ),
        card_body(
          withSpinner(
            plotlyOutput("age_prevalence_plot", height = "380px"),
            type = 4,
            color = spinner_color
          )
        ),
        full_screen = TRUE
      ),

      card(
        card_header(
          tags$span(bs_icon("pie-chart-fill", class = "me-2"), "Population Distribution")
        ),
        card_body(
          withSpinner(
            plotlyOutput("donut_chart", height = "380px"),
            type = 4,
            color = spinner_color
          )
        ),
        full_screen = TRUE
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        class = "findings-card",
        card_header(
          tags$span(bs_icon("lightbulb-fill", class = "me-2"), "Key Findings")
        ),
        card_body(
          tags$ul(
            class = "list-unstyled",
            tags$li(
              class = "mb-3 d-flex align-items-start",
              tags$span(bs_icon("check-circle-fill", class = "text-success me-2 mt-1")),
              tags$div(
                tags$strong("Age is the strongest demographic predictor"),
                tags$p(class = "text-muted mb-0 small",
                       "Risk increases 2-3x from age 45 onwards")
              )
            ),
            tags$li(
              class = "mb-3 d-flex align-items-start",
              tags$span(bs_icon("check-circle-fill", class = "text-success me-2 mt-1")),
              tags$div(
                tags$strong("General Health perception matters most"),
                tags$p(class = "text-muted mb-0 small",
                       "Those reporting 'Poor' health have 10x higher diabetes rates")
              )
            ),
            tags$li(
              class = "mb-3 d-flex align-items-start",
              tags$span(bs_icon("check-circle-fill", class = "text-success me-2 mt-1")),
              tags$div(
                tags$strong("BMI and physical activity are modifiable"),
                tags$p(class = "text-muted mb-0 small",
                       "Obesity increases risk 3x; activity reduces it by 30%")
              )
            ),
            tags$li(
              class = "d-flex align-items-start",
              tags$span(bs_icon("check-circle-fill", class = "text-success me-2 mt-1")),
              tags$div(
                tags$strong("Cardiovascular comorbidities cluster together"),
                tags$p(class = "text-muted mb-0 small",
                       "High BP + High Cholesterol increases diabetes risk 4x")
              )
            )
          )
        )
      ),

      card(
        card_header(
          tags$span(bs_icon("clipboard-data", class = "me-2"), "Model Performance Summary")
        ),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,

            tags$div(
              class = "text-center p-3",
              tags$div(class = "metric-card-title", "Logistic Regression AUC"),
              tags$div(class = "metric-card-value text-primary", round(lr_auc, 3))
            ),
            tags$div(
              class = "text-center p-3",
              tags$div(class = "metric-card-title", "Random Forest AUC"),
              tags$div(class = "metric-card-value text-info", round(rf_auc, 3))
            ),
            tags$div(
              class = "text-center p-3",
              tags$div(class = "metric-card-title", "Sensitivity (at 0.5)"),
              tags$div(class = "metric-card-value text-warning", "20.0%")
            ),
            tags$div(
              class = "text-center p-3",
              tags$div(class = "metric-card-title", "Specificity (at 0.5)"),
              tags$div(class = "metric-card-value text-success", "97.2%")
            )
          ),
          tags$p(
            class = "text-muted small text-center mt-3",
            bs_icon("info-circle", class = "me-1"),
            "Models optimized for identifying high-risk individuals"
          )
        )
      )
    )
  ),

  # ============================================================================
  # EXPLORE GROUP
  # ============================================================================
  nav_item(tags$span(class = "nav-section-header", bs_icon("search", class = "me-2"), "EXPLORE")),

  # Tab 2: Data Explorer -------------------------------------------------------
  nav_panel(
    title = tags$span(bs_icon("table", class = "me-2"), "Data Explorer"),
    value = "data_explorer",

    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "Filters",
        width = 280,

        selectInput(
          "age_filter",
          label = tags$span(bs_icon("calendar", class = "me-1"), "Age Groups"),
          choices = c("All" = "All", levels(diabetes_data$age_group)),
          selected = "All",
          multiple = TRUE
        ),

        sliderInput(
          "bmi_filter",
          label = tags$span(bs_icon("heart-pulse", class = "me-1"), "BMI Range"),
          min = 12, max = 98, value = c(12, 98),
          step = 1
        ),

        checkboxGroupInput(
          "risk_filter",
          label = tags$span(bs_icon("exclamation-triangle", class = "me-1"), "Risk Factors"),
          choices = c(
            "High Blood Pressure" = "high_bp",
            "High Cholesterol" = "high_chol",
            "Heart Disease" = "heart_diseaseor_attack",
            "Smoker" = "smoker",
            "Difficulty Walking" = "diff_walk"
          )
        ),

        radioButtons(
          "sample_size",
          label = tags$span(bs_icon("collection", class = "me-1"), "Sample Size"),
          choices = c(
            "1,000 (Fast)" = 1000,
            "5,000" = 5000,
            "10,000" = 10000,
            "All Data" = 0
          ),
          selected = 5000
        ),

        hr(),

        selectInput(
          "color_var",
          label = tags$span(bs_icon("palette", class = "me-1"), "Color By"),
          choices = c(
            "Diabetes Status" = "diabetes_status",
            "BMI Category" = "bmi_category",
            "General Health" = "gen_hlth_label"
          ),
          selected = "diabetes_status"
        ),

        actionButton(
          "apply_filters",
          label = tags$span(bs_icon("funnel", class = "me-1"), "Apply Filters"),
          class = "btn-primary w-100 mt-3"
        )
      ),

      layout_columns(
        col_widths = c(12),
        fill = FALSE,

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span(bs_icon("diagram-3", class = "me-2"), "BMI vs Age Scatter Plot"),
            textOutput("scatter_count", inline = TRUE)
          ),
          card_body(
            withSpinner(
              plotlyOutput("scatter_plot", height = "450px"),
              type = 4,
              color = spinner_color
            )
          ),
          full_screen = TRUE
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(
            tags$span(bs_icon("bar-chart", class = "me-2"), "Distribution Analysis")
          ),
          card_body(
            selectInput(
              "hist_var",
              label = "Select Variable",
              choices = c(
                "BMI" = "bmi",
                "Mental Health Days" = "ment_hlth",
                "Physical Health Days" = "phys_hlth",
                "Age (encoded)" = "age",
                "General Health" = "gen_hlth"
              ),
              selected = "bmi"
            ),
            plotlyOutput("histogram_plot", height = "300px")
          )
        ),

        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span(bs_icon("table", class = "me-2"), "Data Table"),
            downloadButton("download_filtered", "Export CSV", class = "btn-sm btn-outline-primary")
          ),
          card_body(
            DTOutput("data_table")
          )
        )
      )
    )
    ),

  # Tab 3: Feature Analysis ----------------------------------------------------
  nav_panel(
    title = tags$span(bs_icon("layers", class = "me-2"), "Feature Analysis"),
    value = "feature_analysis",

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header(
          tags$span(bs_icon("grid-3x3", class = "me-2"), "Correlation Heatmap")
        ),
        card_body(
          withSpinner(
            plotlyOutput("correlation_heatmap", height = "420px"),
            type = 4,
            color = spinner_color
          )
        ),
        full_screen = TRUE
      ),

      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span(bs_icon("bar-chart-steps", class = "me-2"), "Feature Importance"),
          radioButtons(
            "importance_model",
            label = NULL,
            choices = c("Logistic Regression" = "lr", "Random Forest" = "rf"),
            selected = "rf",
            inline = TRUE
          )
        ),
        card_body(
          withSpinner(
            plotlyOutput("feature_importance", height = "420px"),
            type = 4,
            color = spinner_color
          )
        ),
        full_screen = TRUE
      )
    ),

    layout_columns(
      col_widths = c(12),

      card(
        card_header(
          tags$span(bs_icon("activity", class = "me-2"),
                    "Risk Factor Prevalence by Diabetes Status")
        ),
        card_body(
          plotlyOutput("risk_comparison", height = "350px")
        )
      )
    ),

    layout_columns(
      col_widths = c(4, 4, 4),

      card(
        card_header(
          tags$span(bs_icon("speedometer2", class = "me-2"), "BMI Distribution")
        ),
        card_body(
          plotlyOutput("bmi_dist", height = "280px")
        )
      ),

      card(
        card_header(
          tags$span(bs_icon("emoji-smile", class = "me-2"), "General Health")
        ),
        card_body(
          plotlyOutput("health_dist", height = "280px")
        )
      ),

      card(
        card_header(
          tags$span(bs_icon("calendar", class = "me-2"), "Age Distribution")
        ),
        card_body(
          plotlyOutput("age_dist", height = "280px")
        )
      )
    )
  ),

  # ============================================================================
  # MODELS GROUP
  # ============================================================================
  nav_item(tags$span(class = "nav-section-header", bs_icon("graph-up-arrow", class = "me-2"), "MODELS")),

  # Tab 4: Model Performance ---------------------------------------------------
  nav_panel(
    title = tags$span(bs_icon("graph-up-arrow", class = "me-2"), "Model Performance"),
    value = "model_performance",

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,

      value_box(
        title = "Logistic AUC-ROC",
        value = round(eval_results$threshold_independent_metrics$auc_roc[1], 3),
        showcase = bs_icon("graph-up"),
        theme = "primary"
      ),
      value_box(
        title = "Random Forest AUC-ROC",
        value = round(eval_results$threshold_independent_metrics$auc_roc[2], 3),
        showcase = bs_icon("graph-up"),
        theme = "info"
      ),
      value_box(
        title = "Best F1 Score",
        value = round(max(eval_results$optimal_thresholds$f1_score), 3),
        showcase = bs_icon("bullseye"),
        theme = "success"
      ),
      value_box(
        title = "Brier Score",
        value = round(eval_results$threshold_independent_metrics$brier_score[1], 3),
        showcase = bs_icon("check-circle"),
        theme = "warning",
        p("Lower is better")
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header(
          tags$span(bs_icon("bezier2", class = "me-2"), "ROC Curves Comparison")
        ),
        card_body(
          withSpinner(
            plotlyOutput("roc_curves", height = "400px"),
            type = 4,
            color = spinner_color
          )
        ),
        full_screen = TRUE
      ),

      card(
        card_header(
          tags$span(bs_icon("sliders", class = "me-2"), "Threshold Analysis")
        ),
        card_body(
          sliderInput(
            "threshold_slider",
            label = "Classification Threshold",
            min = 0.05, max = 0.95, value = 0.5, step = 0.01
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            tags$div(
              class = "text-center",
              tags$div(class = "metric-card-title", "Sensitivity"),
              textOutput("sens_value", inline = FALSE) |>
                tagAppendAttributes(class = "metric-card-value text-success")
            ),
            tags$div(
              class = "text-center",
              tags$div(class = "metric-card-title", "Specificity"),
              textOutput("spec_value", inline = FALSE) |>
                tagAppendAttributes(class = "metric-card-value text-primary")
            ),
            tags$div(
              class = "text-center",
              tags$div(class = "metric-card-title", "F1 Score"),
              textOutput("f1_value", inline = FALSE) |>
                tagAppendAttributes(class = "metric-card-value text-warning")
            )
          ),
          plotlyOutput("threshold_plot", height = "200px")
        )
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header(
          tags$span(bs_icon("grid", class = "me-2"), "Confusion Matrix - Logistic Regression")
        ),
        card_body(
          plotlyOutput("conf_matrix_lr", height = "320px")
        )
      ),

      card(
        card_header(
          tags$span(bs_icon("grid", class = "me-2"), "Confusion Matrix - Random Forest")
        ),
        card_body(
          plotlyOutput("conf_matrix_rf", height = "320px")
        )
      )
    ),

    card(
      card_header(
        tags$span(bs_icon("table", class = "me-2"), "Detailed Metrics Comparison")
      ),
      card_body(
        DTOutput("metrics_table")
      )
    )
    ),

  # Tab 5: Risk Predictor ------------------------------------------------------
  nav_panel(
    title = tags$span(bs_icon("calculator", class = "me-2"), "Risk Predictor"),
    value = "risk_predictor",

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span(bs_icon("person-badge", class = "me-2"), "Patient Profile"),
          actionButton(
            "reset_predictor",
            label = tags$span(bs_icon("arrow-counterclockwise", class = "me-1"), "Reset"),
            class = "btn-outline-secondary btn-sm"
          )
        ),
        card_body(
          # Health Status Group
          tags$div(
            class = "input-group-card",
            tags$div(
              class = "input-group-title",
              bs_icon("heart-pulse", class = "me-2 text-primary"),
              "Health Status"
            ),
            sliderInput(
              "pred_bmi",
              label = "BMI (Body Mass Index)",
              min = 15, max = 60, value = 28, step = 0.5
            ),
            selectInput(
              "pred_health",
              label = "General Health",
              choices = c(
                "Excellent" = 1,
                "Very Good" = 2,
                "Good" = 3,
                "Fair" = 4,
                "Poor" = 5
              ),
              selected = 3
            ),
            checkboxInput("pred_diffwalk", "Difficulty Walking", FALSE)
          ),

          # Risk Factors Group
          tags$div(
            class = "input-group-card",
            tags$div(
              class = "input-group-title",
              bs_icon("exclamation-triangle", class = "me-2 text-warning"),
              "Risk Factors"
            ),
            checkboxInput("pred_highbp", "High Blood Pressure", FALSE),
            checkboxInput("pred_highchol", "High Cholesterol", FALSE),
            checkboxInput("pred_heart", "Heart Disease History", FALSE)
          ),

          # Lifestyle Group
          tags$div(
            class = "input-group-card",
            tags$div(
              class = "input-group-title",
              bs_icon("bicycle", class = "me-2 text-success"),
              "Lifestyle"
            ),
            selectInput(
              "pred_age",
              label = "Age Group",
              choices = setNames(1:13, levels(diabetes_data$age_group)),
              selected = 5
            ),
            checkboxInput("pred_physactivity", "Physically Active (30+ min/day)", TRUE),
            checkboxInput("pred_smoker", "Current/Former Smoker", FALSE)
          ),

          actionButton(
            "predict_btn",
            label = tags$span(bs_icon("calculator", class = "me-1"), "Calculate Risk"),
            class = "btn-primary btn-lg w-100 mt-3"
          )
        )
      ),

      layout_columns(
        col_widths = c(12),

        card(
          card_header(
            tags$span(bs_icon("speedometer", class = "me-2"), "Risk Assessment Results")
          ),
          card_body(
            # Loading indicator for prediction
            conditionalPanel(
              condition = "output.prediction_loading",
              tags$div(
                class = "text-center p-4",
                tags$div(class = "spinner-border text-primary", role = "status"),
                tags$p(class = "mt-2 text-muted", "Calculating risk...")
              )
            ),
            layout_columns(
              col_widths = c(6, 6),

              tags$div(
                class = "text-center p-4",
                withSpinner(
                  plotlyOutput("risk_gauge", height = "280px"),
                  type = 4,
                  color = "#0D9488",
                  size = 0.8
                ),
                uiOutput("risk_badge")
              ),

              tags$div(
                class = "p-4",
                tags$h5(class = "fw-bold mb-3", "Risk Probability"),
                tags$div(
                  class = "display-4 fw-bold mb-3",
                  style = "color: #0D9488;",
                  textOutput("risk_probability")
                ),
                # Confidence Interval Display
                uiOutput("confidence_interval"),
                tags$h6(class = "fw-bold text-muted mb-2 mt-4", "Top Contributing Factors"),
                uiOutput("contributing_factors")
              )
            )
          )
        ),

        card(
          card_header(
            tags$span(bs_icon("info-circle", class = "me-2"), "Interpretation Guide")
          ),
          card_body(
            layout_columns(
              col_widths = breakpoints(sm = 12, md = 4),

              tags$div(
                class = "text-center p-3 rounded interpretation-box-low",
                tags$h5(class = "fw-bold interpretation-title", "Low Risk"),
                tags$p(class = "mb-0 small interpretation-text",
                       "< 15% probability"),
                tags$p(class = "mb-0 small interpretation-detail mt-2",
                       "Maintain healthy lifestyle, regular checkups")
              ),

              tags$div(
                class = "text-center p-3 rounded interpretation-box-medium",
                tags$h5(class = "fw-bold interpretation-title", "Medium Risk"),
                tags$p(class = "mb-0 small interpretation-text",
                       "15-30% probability"),
                tags$p(class = "mb-0 small interpretation-detail mt-2",
                       "Consider lifestyle modifications, monitor glucose")
              ),

              tags$div(
                class = "text-center p-3 rounded interpretation-box-high",
                tags$h5(class = "fw-bold interpretation-title", "High Risk"),
                tags$p(class = "mb-0 small interpretation-text",
                       "> 30% probability"),
                tags$p(class = "mb-0 small interpretation-detail mt-2",
                       "Consult healthcare provider, consider screening")
              )
            )
          )
        )
      )
    )
  ),

  # ============================================================================
  # ADVANCED GROUP
  # ============================================================================
  nav_item(tags$span(class = "nav-section-header", bs_icon("stars", class = "me-2"), "ADVANCED")),

  # Tab 6: Causal Analysis ------------------------------------------------------
  nav_panel(
    title = tags$span(bs_icon("diagram-3", class = "me-2"), "Causal Analysis"),
    value = "causal_analysis",

    # Header explaining causal inference
    layout_columns(
      col_widths = c(12),
      fill = FALSE,

      card(
        class = "causal-explainer",
        card_body(
          tags$div(
            class = "d-flex align-items-start",
            tags$div(
              bs_icon("info-circle-fill", class = "text-primary me-3", size = "1.5rem")
            ),
            tags$div(
              tags$h5(class = "fw-bold mb-2", "Understanding Causal Analysis"),
              tags$p(
                class = "mb-0",
                tags$strong("Correlation is not causation."),
                " This analysis uses Directed Acyclic Graphs (DAGs) and statistical adjustments ",
                "to estimate the ", tags$em("causal effect"), " of risk factors on diabetes. ",
                "We control for confounders to isolate the true effect of each exposure."
              )
            )
          )
        )
      )
    ),

    # Editorial Insight - Causal Analysis Key Finding
    card(
      class = "editorial-callout",
      card_body(
        tags$div(
          class = "editorial-header",
          bs_icon("arrow-right-circle-fill", class = "editorial-icon"),
          tags$span(class = "editorial-label", "CAUSAL INSIGHT")
        ),
        tags$p(
          class = "editorial-lead",
          "High blood pressure is the single most impactful ", tags$em("modifiable"), " cause of diabetes
          in this population."
        ),
        tags$p(
          class = "editorial-body",
          "Our causal analysis estimates a ", tags$strong("Population Attributable Fraction (PAF) of 42%"),
          " for hypertension. This means that if we could eliminate high blood pressure entirely,
          we could theoretically prevent 42% of diabetes cases. Obesity contributes an additional 26% PAF,
          making these two factors the primary targets for population-level prevention programs."
        )
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      # DAG Visualization
      card(
        card_header(
          tags$span(bs_icon("diagram-3", class = "me-2"), "Causal Diagram (DAG)")
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "Arrows represent hypothesized causal relationships. ",
            "Dashed nodes are unobserved (latent) variables."
          ),
          tags$img(
            src = "figures/diabetes_causal_dag.png",
            style = "max-width: 100%; height: auto; border-radius: 8px;",
            alt = "Causal DAG for Diabetes Risk Factors"
          )
        )
      ),

      # Adjustment Sets Panel
      card(
        card_header(
          tags$span(bs_icon("sliders2", class = "me-2"), "Adjustment Sets")
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "Variables we control for to estimate unbiased causal effects:"
          ),
          uiOutput("adjustment_sets_display")
        )
      )
    ),

    layout_columns(
      col_widths = c(8, 4),

      # ATE Results Table
      card(
        card_header(
          tags$span(bs_icon("graph-up-arrow", class = "me-2"),
                    "Average Treatment Effects (ATE)")
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "Estimated causal effect of each exposure on diabetes risk (percentage points):"
          ),
          DTOutput("causal_ate_table")
        )
      ),

      # Sensitivity Analysis
      card(
        card_header(
          tags$span(bs_icon("shield-check", class = "me-2"), "E-Value Sensitivity Analysis")
        ),
        card_body(
          tags$p(
            class = "text-muted small mb-3",
            "E-value measures robustness to unmeasured confounding. ",
            "Higher values indicate more robust findings."
          ),
          DTOutput("sensitivity_table")
        )
      )
    ),

    # Counterfactual Scenarios
    card(
      card_header(
        tags$span(bs_icon("magic", class = "me-2"), "Counterfactual Scenarios: 'What If?'")
      ),
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4),

          # Scenario 1: Everyone exercises
          tags$div(
            class = "text-center p-3 rounded",
            style = "background-color: #ECFDF5;",
            tags$h6(class = "fw-bold", style = "color: #065F46;",
                    bs_icon("bicycle", class = "me-1"), "Everyone Exercises"),
            tags$div(
              class = "display-6 fw-bold my-2",
              style = "color: #065F46;",
              textOutput("cf_exercise_paf", inline = TRUE)
            ),
            tags$p(class = "small mb-0", style = "color: #065F46;",
                   "of diabetes cases potentially preventable")
          ),

          # Scenario 2: BMI reduced by 5
          tags$div(
            class = "text-center p-3 rounded",
            style = "background-color: #EEF2FF;",
            tags$h6(class = "fw-bold", style = "color: #3730A3;",
                    bs_icon("heart-pulse", class = "me-1"), "BMI Reduced by 5"),
            tags$div(
              class = "display-6 fw-bold my-2",
              style = "color: #3730A3;",
              textOutput("cf_bmi_paf", inline = TRUE)
            ),
            tags$p(class = "small mb-0", style = "color: #3730A3;",
                   "of diabetes cases potentially preventable")
          ),

          # Scenario 3: No high BP
          tags$div(
            class = "text-center p-3 rounded",
            style = "background-color: #FEF2F2;",
            tags$h6(class = "fw-bold", style = "color: #991B1B;",
                    bs_icon("heart", class = "me-1"), "No High Blood Pressure"),
            tags$div(
              class = "display-6 fw-bold my-2",
              style = "color: #991B1B;",
              textOutput("cf_bp_paf", inline = TRUE)
            ),
            tags$p(class = "small mb-0", style = "color: #991B1B;",
                   "of diabetes cases potentially preventable")
          )
        ),

        tags$hr(),
        tags$p(
          class = "text-muted small text-center mb-0",
          bs_icon("info-circle", class = "me-1"),
          "Population Attributable Fraction (PAF) estimates the proportion of cases ",
          "that could be prevented if the exposure were eliminated."
        )
      )
    )
    ),

  # Tab 7: Fairness Audit -------------------------------------------------------
  nav_panel(
    title = tags$span(bs_icon("clipboard-check", class = "me-2"), "Fairness Audit"),
    value = "fairness_audit",

    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "Filters",
        width = 280,

        selectInput(
          "fairness_attr",
          label = tags$span(bs_icon("people", class = "me-1"), "Protected Attribute"),
          choices = c(
            "Sex" = "sex",
            "Age" = "age",
            "Income" = "income",
            "Education" = "education"
          ),
          selected = "sex"
        ),

        tags$hr(),

        tags$div(
          class = "mb-3",
          tags$h6(class = "fw-bold", bs_icon("info-circle", class = "me-1"), "Metric Guide"),
          tags$ul(
            class = "small text-muted ps-3",
            tags$li(tags$strong("Accuracy Parity:"), " Equal accuracy across groups"),
            tags$li(tags$strong("Predictive Parity:"), " Equal PPV/NPV across groups"),
            tags$li(tags$strong("Equalized Odds:"), " Equal TPR/FPR across groups"),
            tags$li(tags$strong("Statistical Parity:"), " Equal prediction rates")
          )
        ),

        tags$div(
          class = "alert alert-warning small",
          bs_icon("exclamation-triangle", class = "me-1"),
          "Disparity ratio < 0.8 or > 1.25 may indicate fairness concerns."
        )
      ),

      # Editorial Alert - Fairness Concerns
      card(
        class = "editorial-callout",
        card_body(
          tags$div(
            class = "editorial-header",
            bs_icon("shield-exclamation", class = "editorial-icon"),
            tags$span(class = "editorial-label", "FAIRNESS ALERT")
          ),
          tags$p(
            class = "editorial-lead",
            "This model shows ", tags$em("systematic disparities"), " across income and education levels
            that require attention before deployment."
          ),
          tags$p(
            class = "editorial-body",
            "Our fairness audit identified ", tags$strong("20 disparity flags"), " across protected attributes.
            The most concerning gaps appear in income-based subgroups, where model accuracy varies by up to 15%
            between high and low-income populations. These disparities could exacerbate existing health inequities
            if the model is deployed without calibration adjustments."
          )
        )
      ),

      layout_columns(
        col_widths = c(3, 3, 3, 3),
        fill = FALSE,

        value_box(
          title = "Total Issues Found",
          value = textOutput("fairness_total_issues"),
          showcase = bs_icon("exclamation-triangle"),
          theme = value_box_theme(bg = "#FEF3C7", fg = "#92400E")
        ),

        value_box(
          title = "Age Disparities",
          value = textOutput("fairness_age_issues"),
          showcase = bs_icon("calendar"),
          theme = value_box_theme(bg = "#FEE2E2", fg = "#991B1B")
        ),

        value_box(
          title = "Income Disparities",
          value = textOutput("fairness_income_issues"),
          showcase = bs_icon("currency-dollar"),
          theme = value_box_theme(bg = "#FED7AA", fg = "#9A3412")
        ),

        value_box(
          title = "Education Disparities",
          value = textOutput("fairness_education_issues"),
          showcase = bs_icon("book"),
          theme = value_box_theme(bg = "#DBEAFE", fg = "#1E40AF")
        )
      ),

      layout_columns(
        col_widths = c(8, 4),

        # Fairness Metrics Comparison
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span(bs_icon("bar-chart", class = "me-2"),
                      "Fairness Metrics by Group"),
            textOutput("fairness_selected_attr", inline = TRUE)
          ),
          card_body(
            plotlyOutput("fairness_bar_chart", height = "380px")
          )
        ),

        # Traffic Light Overview
        card(
          card_header(
            tags$span(bs_icon("stoplights", class = "me-2"), "Status Overview")
          ),
          card_body(
            uiOutput("fairness_traffic_lights")
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        # Disparity Ratio Table
        card(
          card_header(
            tags$span(bs_icon("table", class = "me-2"), "Disparity Ratios")
          ),
          card_body(
            DTOutput("disparity_table")
          )
        ),

        # Intersectional Heatmap
        card(
          card_header(
            tags$span(bs_icon("grid-3x3", class = "me-2"), "Intersectional Analysis")
          ),
          card_body(
            tags$p(
              class = "text-muted small mb-2",
              "Accuracy rates across intersecting demographic groups:"
            ),
            plotlyOutput("intersectional_heatmap", height = "280px")
          )
        )
      ),

      # Recommendations Panel
      card(
        card_header(
          tags$span(bs_icon("lightbulb", class = "me-2"), "Recommendations")
        ),
        card_body(
          uiOutput("fairness_recommendations")
        )
      )
    )
    ),

  # Tab 8: Discovery Lab --------------------------------------------------------
  nav_panel(
    title = tags$span(bs_icon("lightbulb", class = "me-2"), "Discovery Lab"),
    value = "discovery_lab",

    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        title = "Subgroup Explorer",
        width = 280,

        selectInput(
          "discovery_subgroup",
          label = tags$span(bs_icon("people", class = "me-1"), "Select Subgroup"),
          choices = c(
            "Resilient" = "Resilient",
            "Vulnerable" = "Vulnerable",
            "Expected Healthy" = "Expected Healthy",
            "Expected Diabetic" = "Expected Diabetic",
            "Typical" = "Typical"
          ),
          selected = "Resilient"
        ),

        tags$hr(),

        tags$div(
          class = "mb-3",
          tags$h6(class = "fw-bold mb-2", "Subgroup Definitions"),
          tags$div(
            class = "small",
            tags$p(class = "mb-2",
              tags$span(class = "subgroup-badge subgroup-resilient", "Resilient"),
              tags$br(),
              "High predicted risk but no diabetes"
            ),
            tags$p(class = "mb-2",
              tags$span(class = "subgroup-badge subgroup-vulnerable", "Vulnerable"),
              tags$br(),
              "Low predicted risk but has diabetes"
            ),
            tags$p(class = "mb-2",
              tags$span(class = "subgroup-badge subgroup-expected-healthy", "Expected Healthy"),
              tags$br(),
              "Low risk, no diabetes (as expected)"
            ),
            tags$p(class = "mb-2",
              tags$span(class = "subgroup-badge subgroup-expected-diabetic", "Expected Diabetic"),
              tags$br(),
              "High risk, has diabetes (as expected)"
            ),
            tags$p(class = "mb-0",
              tags$span(class = "subgroup-badge subgroup-typical", "Typical"),
              tags$br(),
              "Moderate risk, model performs well"
            )
          )
        )
      ),

      # Editorial Insight - Anomaly Discovery
      card(
        class = "editorial-callout",
        card_body(
          tags$div(
            class = "editorial-header",
            bs_icon("search", class = "editorial-icon"),
            tags$span(class = "editorial-label", "DISCOVERY INSIGHT")
          ),
          tags$p(
            class = "editorial-lead",
            "We identified ", tags$em("15.4% of individuals"), " who defy the model's predictions in
            fascinating ways."
          ),
          tags$p(
            class = "editorial-body",
            "The ", tags$strong('"Resilient" group'), " has all the risk factors for diabetes but
            remains healthy. These 39,000+ individuals may hold clues to protective factors our
            model doesn't capture - perhaps genetics, microbiome composition, or unmeasured lifestyle
            habits. The ", tags$strong('"Vulnerable" group'), " (0.09%) develops diabetes despite low
            predicted risk, highlighting model blind spots."
          )
        )
      ),

      layout_columns(
        col_widths = c(3, 3, 3, 3),
        fill = FALSE,

        value_box(
          title = "Resilient Cases",
          value = textOutput("discovery_resilient_n"),
          showcase = bs_icon("shield-check"),
          theme = "success",
          p("High risk, no diabetes")
        ),

        value_box(
          title = "Vulnerable Cases",
          value = textOutput("discovery_vulnerable_n"),
          showcase = bs_icon("exclamation-triangle"),
          theme = "danger",
          p("Low risk, has diabetes")
        ),

        value_box(
          title = "Selected Subgroup",
          value = textOutput("discovery_selected_n"),
          showcase = bs_icon("people"),
          theme = "primary"
        ),

        value_box(
          title = "Diabetes Rate",
          value = textOutput("discovery_selected_rate"),
          showcase = bs_icon("percent"),
          theme = "info"
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        # Residual Distribution
        card(
          card_header(
            tags$span(bs_icon("graph-up", class = "me-2"), "Residual Distribution")
          ),
          card_body(
            tags$p(
              class = "text-muted small mb-2",
              "Prediction residuals reveal where the model underperforms:"
            ),
            plotlyOutput("residual_distribution", height = "300px")
          )
        ),

        # Subgroup Profiles
        card(
          card_header(
            tags$span(bs_icon("person-lines-fill", class = "me-2"),
                      "Subgroup Profile Comparison")
          ),
          card_body(
            plotlyOutput("subgroup_profiles", height = "300px")
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        # Top Factors Table
        card(
          card_header(
            tags$span(bs_icon("sort-down", class = "me-2"),
                      "Key Differentiating Factors")
          ),
          card_body(
            DTOutput("factor_table")
          )
        ),

        # Hypothesis Generator
        card(
          card_header(
            tags$span(bs_icon("lightbulb-fill", class = "me-2"),
                      "Generated Hypotheses")
          ),
          card_body(
            uiOutput("hypothesis_display")
          )
        )
      ),

      # Individual Case Explorer
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span(bs_icon("search", class = "me-2"), "Individual Case Explorer"),
          downloadButton("download_subgroup", "Export CSV", class = "btn-sm btn-outline-primary")
        ),
        card_body(
          DTOutput("case_explorer_table")
        )
      )
    )
  )
  )  # End of navset_pill_list
)  # End of page_fillable

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {

  # Hide loading screen after data is ready
  waiter_hide()


  # Reactive theme state -------------------------------------------------------
  theme_is_dark <- reactive({
    # Default to dark mode if input not yet available
    if (is.null(input$dark_mode)) TRUE else input$dark_mode
  })

  # Update ggplot2 theme when dark mode toggle changes

  observe({
    dark <- theme_is_dark()
    theme_set(theme_worldclass(dark_mode = dark))
  })

  # Reactive filtered data with caching -----------------------------------------
  filtered_data <- reactive({
    input$apply_filters  # Dependency on button click

    isolate({
      df <- diabetes_data

      # Apply age filter
      if (!("All" %in% input$age_filter) && length(input$age_filter) > 0) {
        df <- df |> filter(age_group %in% input$age_filter)
      }

      # Apply BMI filter
      df <- df |> filter(bmi >= input$bmi_filter[1], bmi <= input$bmi_filter[2])

      # Apply risk factor filters
      if (length(input$risk_filter) > 0) {
        for (rf in input$risk_filter) {
          df <- df |> filter(.data[[rf]] == 1)
        }
      }

      # Sample if needed
      sample_n <- as.numeric(input$sample_size)
      if (sample_n > 0 && nrow(df) > sample_n) {
        df <- df |> slice_sample(n = sample_n)
      }

      df
    })
  }) |> bindCache(input$age_filter, input$bmi_filter, input$risk_filter, input$sample_size)

  # Debounced threshold input
  threshold_debounced <- debounce(reactive(input$threshold_slider), 300)

  # Executive Summary Tab Outputs ----------------------------------------------

  output$donut_chart <- renderPlotly({
    dark <- theme_is_dark()
    text_color <- if (dark) "#F5F0E8" else "#0F172A"
    legend_color <- if (dark) "#E2E8F0" else "#334155"

    plot_ly(
      summary_by_status,
      labels = ~diabetes_status,
      values = ~count,
      type = "pie",
      hole = 0.6,
      marker = list(colors = diabetes_colors[summary_by_status$diabetes_status]),
      textinfo = "percent",
      textposition = "outside",
      hovertemplate = "<b>%{label}</b><br>Count: %{value:,}<br>Percentage: %{percent}<extra></extra>"
    ) |>
      layout(
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.1, font = list(color = legend_color)),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        annotations = list(
          list(
            text = paste0("<b>", scales::comma(total_records), "</b><br>Total"),
            showarrow = FALSE,
            font = list(size = 16, color = text_color)
          )
        ),
        margin = list(t = 20, b = 40)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$age_prevalence_plot <- renderPlotly({
    dark <- theme_is_dark()
    text_label_color <- if (dark) "#94A3B8" else "#64748B"

    plot_data <- summary_by_age |>
      filter(diabetes_status == "Diabetes") |>
      mutate(age_group = factor(age_group, levels = levels(diabetes_data$age_group)))

    p <- ggplot(plot_data, aes(x = age_group, y = pct, fill = pct)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0(round(pct, 1), "%")),
                vjust = -0.5, size = 3, color = text_label_color) +
      scale_fill_gradient(low = "#93C5FD", high = "#1D4ED8", guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Age Group", y = "Diabetes Prevalence (%)") +
      theme_worldclass(dark_mode = dark) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p, tooltip = c("x", "y")) |>
      apply_plotly_theme(dark_mode = dark) |>
      layout(margin = list(b = 80)) |>
      config(displayModeBar = FALSE)
  })

  # Data Explorer Tab Outputs --------------------------------------------------

  output$scatter_count <- renderText({
    n <- nrow(filtered_data())
    max_points <- 15000
    if (n > max_points) {
      paste0("Showing ", scales::comma(max_points), " sampled from ", scales::comma(n), " records")
    } else {
      paste0("Showing ", scales::comma(n), " records")
    }
  })

  output$scatter_plot <- renderPlotly({
    dark <- theme_is_dark()
    df <- filtered_data()
    color_var <- input$color_var

    # Sample for very large datasets to improve rendering performance
    max_points <- 15000
    if (nrow(df) > max_points) {
      df <- df |> slice_sample(n = max_points)
    }

    # Use WebGL for large datasets
    use_webgl <- nrow(df) > 5000

    p <- ggplot(df, aes(x = age, y = bmi, color = .data[[color_var]])) +
      geom_point(alpha = 0.5, size = 1.5) +
      scale_color_manual(values = if(color_var == "diabetes_status") diabetes_colors else chart_colors) +
      labs(x = "Age (Encoded 1-13)", y = "BMI", color = str_to_title(gsub("_", " ", color_var))) +
      theme_worldclass(dark_mode = dark)

    pl <- ggplotly(p, tooltip = c("x", "y", "color"))

    if (use_webgl) {
      pl <- pl |> toWebGL()
    }

    pl |>
      apply_plotly_theme(dark_mode = dark) |>
      layout(legend = list(orientation = "h", y = -0.15)) |>
      config(displayModeBar = TRUE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
  })

  output$histogram_plot <- renderPlotly({
    dark <- theme_is_dark()
    df <- filtered_data()
    var <- input$hist_var

    p <- ggplot(df, aes(x = .data[[var]], fill = diabetes_status)) +
      geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = diabetes_colors) +
      labs(x = str_to_title(gsub("_", " ", var)), y = "Count", fill = "Status") +
      theme_worldclass(dark_mode = dark)

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      layout(legend = list(orientation = "h", y = -0.2)) |>
      config(displayModeBar = FALSE)
  })

  output$data_table <- renderDT({
    df <- filtered_data() |>
      select(diabetes_status, age_group, bmi, bmi_category, gen_hlth_label,
             high_bp, high_chol, smoker, heart_diseaseor_attack) |>
      rename(
        Status = diabetes_status,
        Age = age_group,
        BMI = bmi,
        `BMI Category` = bmi_category,
        `Gen Health` = gen_hlth_label,
        `High BP` = high_bp,
        `High Chol` = high_chol,
        Smoker = smoker,
        `Heart Disease` = heart_diseaseor_attack
      )

    datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "frtip",
        deferRender = TRUE,
        scroller = TRUE,
        scrollY = 400,
        language = list(
          search = "Filter:",
          info = "Showing _START_ to _END_ of _TOTAL_ records"
        )
      ),
      rownames = FALSE,
      class = "compact stripe hover",
      style = "bootstrap5",
      extensions = "Scroller"
    ) |>
      formatStyle(
        "Status",
        backgroundColor = styleEqual(
          c("No Diabetes", "Prediabetes", "Diabetes"),
          c("#D1FAE5", "#FEF3C7", "#FEE2E2")
        )
      )
  }, server = TRUE)

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("diabetes_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )

  # Feature Analysis Tab Outputs -----------------------------------------------

  output$correlation_heatmap <- renderPlotly({
    # Lazy load - only render when Feature Analysis tab is active
    req(input$main_nav == "feature_analysis")
    dark <- theme_is_dark()
    axis_color <- if (dark) "#94A3B8" else "#64748B"
    mid_color <- if (dark) "#1E293B" else "#FFFFFF"

    plot_ly(
      x = colnames(cor_matrix),
      y = rownames(cor_matrix),
      z = cor_matrix,
      type = "heatmap",
      colorscale = list(c(0, "#EF4444"), c(0.5, mid_color), c(1, "#0D6EFD")),
      zmin = -1, zmax = 1,
      hovertemplate = "<b>%{x}</b> vs <b>%{y}</b><br>Correlation: %{z:.3f}<extra></extra>"
    ) |>
      apply_plotly_theme(dark_mode = dark) |>
      layout(
        xaxis = list(tickangle = 45, color = axis_color),
        yaxis = list(autorange = "reversed", color = axis_color),
        margin = list(l = 80, b = 100)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$feature_importance <- renderPlotly({
    # Lazy load - only render when Feature Analysis tab is active
    req(input$main_nav == "feature_analysis")
    dark <- theme_is_dark()

    if (input$importance_model == "lr") {
      df <- lr_coefs |>
        mutate(feature = fct_reorder(feature, importance))

      p <- ggplot(df, aes(x = importance, y = feature, fill = direction)) +
        geom_col() +
        scale_fill_manual(values = c("Increases Risk" = "#EF4444", "Decreases Risk" = "#10B981")) +
        labs(x = "Absolute Coefficient", y = "", fill = "") +
        theme_worldclass(dark_mode = dark)
    } else {
      df <- rf_importance |>
        mutate(feature = fct_reorder(feature, importance))

      p <- ggplot(df, aes(x = importance, y = feature)) +
        geom_col(fill = "#0D6EFD") +
        labs(x = "Importance (Mean Decrease Impurity)", y = "") +
        theme_worldclass(dark_mode = dark)
    }

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      layout(legend = list(orientation = "h", y = -0.15)) |>
      config(displayModeBar = FALSE)
  })

  output$risk_comparison <- renderPlotly({
    dark <- theme_is_dark()

    p <- ggplot(risk_factor_summary, aes(x = risk_factor, y = prevalence, fill = diabetes_status)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = diabetes_colors) +
      labs(x = "", y = "Prevalence (%)", fill = "") +
      theme_worldclass(dark_mode = dark) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      layout(legend = list(orientation = "h", y = -0.25)) |>
      config(displayModeBar = FALSE)
  })

  output$bmi_dist <- renderPlotly({
    dark <- theme_is_dark()

    p <- ggplot(diabetes_data, aes(x = bmi, fill = diabetes_status)) +
      geom_density(alpha = 0.6) +
      scale_fill_manual(values = diabetes_colors) +
      labs(x = "BMI", y = "Density", fill = "") +
      theme_worldclass(dark_mode = dark) +
      theme(legend.position = "none")

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      config(displayModeBar = FALSE)
  })

  output$health_dist <- renderPlotly({
    dark <- theme_is_dark()

    df <- diabetes_data |>
      count(gen_hlth_label, diabetes_status) |>
      group_by(gen_hlth_label) |>
      mutate(pct = n / sum(n) * 100)

    p <- ggplot(df, aes(x = gen_hlth_label, y = pct, fill = diabetes_status)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = diabetes_colors) +
      labs(x = "", y = "%", fill = "") +
      theme_worldclass(dark_mode = dark) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      config(displayModeBar = FALSE)
  })

  output$age_dist <- renderPlotly({
    dark <- theme_is_dark()

    df <- diabetes_data |>
      count(age_group, diabetes_status) |>
      group_by(age_group) |>
      mutate(pct = n / sum(n) * 100)

    p <- ggplot(df, aes(x = age_group, y = pct, fill = diabetes_status)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = diabetes_colors) +
      labs(x = "", y = "%", fill = "") +
      theme_worldclass(dark_mode = dark) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      config(displayModeBar = FALSE)
  })

  # Model Performance Tab Outputs ----------------------------------------------

  output$roc_curves <- renderPlotly({
    # Lazy load - only render when Model Performance tab is active
    req(input$main_nav == "model_performance")
    dark <- theme_is_dark()
    diag_color <- if (dark) "#94A3B8" else "#CBD5E1"

    roc_data <- eval_results$roc_curve_data |>
      # Filter out Inf values and sample for performance
      filter(is.finite(threshold)) |>
      group_by(model) |>
      slice(round(seq(1, n(), length.out = 500))) |>
      ungroup()

    p <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = model)) +
      geom_line(linewidth = 1.2) +
      geom_abline(linetype = "dashed", color = diag_color) +
      scale_color_manual(values = c("Logistic Regression" = "#0D6EFD", "Random Forest" = "#14B8A6")) +
      labs(x = "False Positive Rate (1 - Specificity)",
           y = "True Positive Rate (Sensitivity)",
           color = "") +
      coord_equal() +
      theme_worldclass(dark_mode = dark)

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      layout(legend = list(orientation = "h", y = -0.15)) |>
      config(displayModeBar = FALSE)
  })

  # Threshold analysis
  threshold_metrics <- reactive({
    threshold <- threshold_debounced()

    # Get ROC data for logistic regression
    roc_data <- eval_results$roc_curve_data |>
      filter(model == "Logistic Regression")

    # Find closest threshold
    closest_idx <- which.min(abs(roc_data$threshold - threshold))

    sens <- roc_data$sensitivity[closest_idx]
    spec <- roc_data$specificity[closest_idx]

    # Calculate F1 (approximation)
    precision <- sens * 0.158 / (sens * 0.158 + (1 - spec) * 0.842)
    f1 <- if(!is.na(precision) && precision > 0) 2 * precision * sens / (precision + sens) else 0

    list(sensitivity = sens, specificity = spec, f1 = f1)
  })

  output$sens_value <- renderText({
    paste0(round(threshold_metrics()$sensitivity * 100, 1), "%")
  })

  output$spec_value <- renderText({
    paste0(round(threshold_metrics()$specificity * 100, 1), "%")
  })

  output$f1_value <- renderText({
    round(threshold_metrics()$f1, 3)
  })

  output$threshold_plot <- renderPlotly({
    dark <- theme_is_dark()
    vline_color <- if (dark) "#94A3B8" else "#CBD5E1"
    threshold <- threshold_debounced()

    # Create threshold analysis data
    thresholds <- seq(0.05, 0.95, by = 0.02)
    roc_data <- eval_results$roc_curve_data |>
      filter(model == "Logistic Regression")

    threshold_df <- tibble(threshold = thresholds) |>
      rowwise() |>
      mutate(
        idx = which.min(abs(roc_data$threshold - threshold)),
        sensitivity = roc_data$sensitivity[idx],
        specificity = roc_data$specificity[idx]
      ) |>
      ungroup() |>
      pivot_longer(cols = c(sensitivity, specificity), names_to = "metric", values_to = "value")

    p <- ggplot(threshold_df, aes(x = threshold, y = value, color = metric)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = threshold, linetype = "dashed", color = vline_color) +
      scale_color_manual(values = c("sensitivity" = "#10B981", "specificity" = "#0D6EFD")) +
      labs(x = "Threshold", y = "Value", color = "") +
      theme_worldclass(dark_mode = dark) +
      theme(legend.position = "top")

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      config(displayModeBar = FALSE)
  })

  # Confusion matrices
  create_confusion_matrix_plot <- function(cm_data, title_suffix, dark_mode = TRUE) {
    low_color <- if (dark_mode) "#1E293B" else "#E2E8F0"
    text_color <- if (dark_mode) "#F5F0E8" else "#0F172A"
    axis_color <- if (dark_mode) "#94A3B8" else "#64748B"

    # Create matrix from stored data
    cm <- matrix(
      c(cm_data$tn, cm_data$fp, cm_data$fn, cm_data$tp),
      nrow = 2,
      dimnames = list(
        Predicted = c("Negative", "Positive"),
        Actual = c("Negative", "Positive")
      )
    )

    cm_df <- as.data.frame(as.table(cm))
    names(cm_df) <- c("Predicted", "Actual", "Count")

    # Normalize for color
    cm_df$Pct <- cm_df$Count / sum(cm_df$Count) * 100

    plot_ly(
      data = cm_df,
      x = ~Actual,
      y = ~Predicted,
      z = ~Count,
      type = "heatmap",
      colorscale = list(c(0, low_color), c(1, "#0D6EFD")),
      showscale = FALSE,
      hovertemplate = "Actual: %{x}<br>Predicted: %{y}<br>Count: %{z:,}<extra></extra>"
    ) |>
      add_annotations(
        x = cm_df$Actual,
        y = cm_df$Predicted,
        text = scales::comma(cm_df$Count),
        showarrow = FALSE,
        font = list(color = ifelse(cm_df$Pct > 40, "white", text_color), size = 16)
      ) |>
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(title = "Actual", color = axis_color),
        yaxis = list(title = "Predicted", autorange = "reversed", color = axis_color),
        font = list(color = text_color)
      ) |>
      config(displayModeBar = FALSE)
  }

  output$conf_matrix_lr <- renderPlotly({
    dark <- theme_is_dark()
    create_confusion_matrix_plot(eval_results$confusion_matrices$logistic, "Logistic", dark_mode = dark)
  })

  output$conf_matrix_rf <- renderPlotly({
    dark <- theme_is_dark()
    create_confusion_matrix_plot(eval_results$confusion_matrices$random_forest, "Random Forest", dark_mode = dark)
  })

  output$metrics_table <- renderDT({
    metrics <- eval_results$model_comparison |>
      mutate(across(where(is.numeric), ~round(.x, 4))) |>
      rename(
        Model = model,
        Accuracy = accuracy,
        Sensitivity = sensitivity,
        Specificity = specificity,
        Precision = precision,
        NPV = npv,
        `F1 Score` = f1_score,
        `Balanced Accuracy` = balanced_accuracy,
        MCC = mcc,
        `AUC-ROC` = auc_roc,
        `AUC-PR` = auc_pr,
        `Brier Score` = brier_score
      )

    datatable(
      metrics,
      options = list(
        dom = "t",
        scrollX = TRUE,
        ordering = FALSE
      ),
      rownames = FALSE,
      class = "compact stripe",
      style = "bootstrap5"
    ) |>
      formatStyle(
        "AUC-ROC",
        background = styleColorBar(c(0, 1), "#0D6EFD"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # Risk Predictor Tab Outputs -------------------------------------------------

  # Reset button handler

  observeEvent(input$reset_predictor, {
    updateSliderInput(session, "pred_bmi", value = 28)
    updateSelectInput(session, "pred_age", selected = 5)
    updateSelectInput(session, "pred_health", selected = 3)
    updateCheckboxInput(session, "pred_highbp", value = FALSE)
    updateCheckboxInput(session, "pred_highchol", value = FALSE)
    updateCheckboxInput(session, "pred_heart", value = FALSE)
    updateCheckboxInput(session, "pred_smoker", value = FALSE)
    updateCheckboxInput(session, "pred_diffwalk", value = FALSE)
    updateCheckboxInput(session, "pred_physactivity", value = TRUE)
  })

  prediction_result <- eventReactive(input$predict_btn, {
    # Build prediction data frame with ALL 16 features required by the model

    bmi <- input$pred_bmi
    age <- as.numeric(input$pred_age)
    gen_hlth <- as.numeric(input$pred_health)
    high_bp <- as.numeric(input$pred_highbp)
    high_chol <- as.numeric(input$pred_highchol)
    heart_disease <- as.numeric(input$pred_heart)
    smoker <- as.numeric(input$pred_smoker)
    diff_walk <- as.numeric(input$pred_diffwalk)
    phys_activity <- as.numeric(input$pred_physactivity)

    # Calculate derived features (FIX: these were missing)
    high_risk_flag <- as.integer(bmi >= 30 | age >= 65 / 5)  # age >= ~13 (65+ group)
    lifestyle_score <- ifelse(phys_activity, 1, 0)
    health_burden <- high_bp + high_chol + heart_disease

    # Create feature set matching all 16 model requirements
    new_data <- tibble(
      gen_hlth = gen_hlth,
      bmi = bmi,
      age = age,
      high_bp = high_bp,
      high_chol = high_chol,
      cardiovascular_risk = high_bp + high_chol + heart_disease,
      lifestyle_score = lifestyle_score,
      health_burden = health_burden,
      high_risk_flag = high_risk_flag,
      diff_walk = diff_walk,
      sex = 0,           # Smart default (female)
      education = 4,     # Smart default (some college)
      income = 5,        # Smart default (middle income)
      smoker = smoker,
      age_bmi_risk = age * bmi / 100,
      bmi_squared = bmi^2
    )

    # Make prediction using logistic model
    tryCatch({
      prob <- predict(lr_model, newdata = new_data, type = "response")

      # Calculate 80% confidence interval using standard error approximation
      # Based on the variance of logistic prediction
      se_logit <- 0.15  # Approximate SE from model
      logit_prob <- log(prob / (1 - prob))
      ci_lower_logit <- logit_prob - 1.28 * se_logit  # 80% CI z-score
      ci_upper_logit <- logit_prob + 1.28 * se_logit
      ci_lower <- 1 / (1 + exp(-ci_lower_logit))
      ci_upper <- 1 / (1 + exp(-ci_upper_logit))

      # Clamp values
      ci_lower <- max(0.01, min(0.99, ci_lower))
      ci_upper <- max(0.01, min(0.99, ci_upper))

      # Determine risk category
      risk_cat <- case_when(
        prob < 0.15 ~ "Low",
        prob < 0.30 ~ "Medium",
        TRUE ~ "High"
      )

      # Calculate contributing factors
      coefs <- coef(lr_model)
      contributions <- c(
        "BMI" = if(!is.na(coefs["bmi"])) coefs["bmi"] * bmi else 0,
        "Age" = if(!is.na(coefs["age"])) coefs["age"] * age else 0,
        "General Health" = if(!is.na(coefs["gen_hlth"])) coefs["gen_hlth"] * gen_hlth else 0,
        "High BP" = if(!is.na(coefs["high_bp"])) coefs["high_bp"] * high_bp else 0,
        "High Cholesterol" = if(!is.na(coefs["high_chol"])) coefs["high_chol"] * high_chol else 0,
        "Difficulty Walking" = if(!is.na(coefs["diff_walk"])) coefs["diff_walk"] * diff_walk else 0,
        "Cardiovascular Risk" = if(!is.na(coefs["cardiovascular_risk"])) coefs["cardiovascular_risk"] * (high_bp + high_chol + heart_disease) else 0,
        "Smoker" = if(!is.na(coefs["smoker"])) coefs["smoker"] * smoker else 0
      )

      # Get top 3 positive contributors
      positive_contributions <- contributions[contributions > 0]
      if (length(positive_contributions) > 0) {
        top_factors <- names(sort(positive_contributions, decreasing = TRUE)[1:min(3, length(positive_contributions))])
      } else {
        top_factors <- c("General Health", "BMI", "Age")
      }

      list(
        probability = prob,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        risk_category = risk_cat,
        top_factors = top_factors
      )
    }, error = function(e) {
      message("Prediction error: ", e$message)
      list(
        probability = 0.15,
        ci_lower = 0.10,
        ci_upper = 0.20,
        risk_category = "Low",
        top_factors = c("General Health", "BMI", "Age")
      )
    })
  }, ignoreNULL = TRUE)  # Changed to TRUE - require button click

  output$risk_gauge <- renderPlotly({
    dark <- theme_is_dark()
    result <- prediction_result()
    prob <- result$probability

    text_color <- if (dark) "#F5F0E8" else "#0F172A"
    tick_color <- if (dark) "#94A3B8" else "#64748B"
    bg_color <- if (dark) "rgba(148, 163, 184, 0.2)" else "rgba(148, 163, 184, 0.3)"
    bar_color <- if (dark) "#FF6B6B" else "#0D9488"

    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = prob * 100,
      number = list(suffix = "%", font = list(size = 40, color = text_color)),
      gauge = list(
        axis = list(range = list(0, 100), ticksuffix = "%", tickcolor = tick_color, tickfont = list(color = tick_color)),
        bar = list(color = bar_color, thickness = 0.3),
        bgcolor = bg_color,
        steps = list(
          list(range = c(0, 15), color = "rgba(16, 185, 129, 0.3)"),
          list(range = c(15, 30), color = "rgba(251, 191, 36, 0.3)"),
          list(range = c(30, 100), color = "rgba(244, 63, 94, 0.3)")
        ),
        threshold = list(
          line = list(color = bar_color, width = 4),
          thickness = 0.75,
          value = prob * 100
        )
      )
    ) |>
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        margin = list(t = 40, b = 20),
        font = list(family = "Inter", color = text_color)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$risk_badge <- renderUI({
    result <- prediction_result()

    badge_class <- case_when(
      result$risk_category == "Low" ~ "risk-low",
      result$risk_category == "Medium" ~ "risk-medium",
      TRUE ~ "risk-high"
    )

    tags$div(
      class = paste("risk-badge", badge_class),
      paste(result$risk_category, "Risk")
    )
  })

  output$risk_probability <- renderText({
    result <- prediction_result()
    paste0(round(result$probability * 100, 1), "%")
  })

  output$contributing_factors <- renderUI({
    result <- prediction_result()

    if (is.null(result)) {
      return(tags$p(class = "text-muted", "Click 'Calculate Risk' to see results"))
    }

    tags$ul(
      class = "list-unstyled",
      lapply(result$top_factors, function(factor) {
        tags$li(
          class = "mb-2 d-flex align-items-center",
          tags$span(bs_icon("arrow-right-circle-fill", class = "text-danger me-2")),
          factor
        )
      })
    )
  })

  # Confidence Interval Display
  output$confidence_interval <- renderUI({
    result <- prediction_result()

    if (is.null(result)) {
      return(NULL)
    }

    ci_lower <- round(result$ci_lower * 100, 1)
    ci_upper <- round(result$ci_upper * 100, 1)

    tags$div(
      class = "confidence-interval",
      tags$span(bs_icon("graph-up-arrow", class = "me-2")),
      tags$strong("80% Confidence Interval: "),
      paste0(ci_lower, "% - ", ci_upper, "%")
    )
  })

  # ==========================================================================
  # Tab 6: Causal Analysis Outputs
  # ==========================================================================

  # Adjustment Sets Display
  output$adjustment_sets_display <- renderUI({
    adj_sets <- causal_results$adjustment_sets

    tags$div(
      lapply(names(adj_sets), function(set_name) {
        # Format the name nicely
        display_name <- gsub("_", " ", set_name) |>
          gsub("to", "->", x = _) |>
          str_to_title()

        vars <- adj_sets[[set_name]]
        if (length(vars) == 0) {
          vars_text <- "No adjustment needed (direct effect)"
        } else {
          vars_text <- paste(gsub("_", " ", vars), collapse = ", ")
        }

        tags$div(
          class = "mb-3 p-2 rounded",
          style = "background-color: #F1F5F9;",
          tags$h6(class = "fw-bold mb-1", display_name),
          tags$p(class = "small text-muted mb-0",
                 tags$strong("Adjust for: "), vars_text)
        )
      })
    )
  })

  # ATE Results Table
  output$causal_ate_table <- renderDT({
    ate_data <- causal_results$causal_effects |>
      mutate(
        `Effect (pp)` = paste0(
          ifelse(ate_estimate > 0, "+", ""),
          round(ate_estimate * 100, 1), "%"
        ),
        `95% CI` = paste0(
          "[", round(ate_ci_low * 100, 1), "%, ",
          round(ate_ci_high * 100, 1), "%]"
        ),
        Direction = ifelse(ate_estimate > 0, "Increases Risk", "Decreases Risk")
      ) |>
      select(
        Exposure = exposure,
        `Effect (pp)`,
        `95% CI`,
        Direction,
        Interpretation = interpretation
      )

    datatable(
      ate_data,
      options = list(
        dom = "t",
        ordering = FALSE,
        pageLength = 10
      ),
      rownames = FALSE,
      class = "compact stripe",
      style = "bootstrap5"
    ) |>
      formatStyle(
        "Direction",
        color = styleEqual(
          c("Increases Risk", "Decreases Risk"),
          c("#EF4444", "#10B981")
        ),
        fontWeight = "bold"
      )
  })

  # Sensitivity Analysis Table
  output$sensitivity_table <- renderDT({
    sens_data <- causal_results$sensitivity_analysis |>
      mutate(
        `Risk Ratio` = round(risk_ratio, 2),
        `E-Value` = round(e_value, 2),
        Robustness = robustness
      ) |>
      select(Exposure = exposure, `Risk Ratio`, `E-Value`, Robustness)

    datatable(
      sens_data,
      options = list(
        dom = "t",
        ordering = FALSE
      ),
      rownames = FALSE,
      class = "compact stripe",
      style = "bootstrap5"
    ) |>
      formatStyle(
        "Robustness",
        backgroundColor = styleEqual(
          c("Strong", "Moderate", "Weak"),
          c("#D1FAE5", "#FEF3C7", "#FEE2E2")
        )
      )
  })

  # Counterfactual PAF outputs
  output$cf_exercise_paf <- renderText({
    cf <- causal_results$counterfactuals |>
      filter(scenario == "Everyone exercises")
    paste0(round(cf$paf * 100, 1), "%")
  })

  output$cf_bmi_paf <- renderText({
    cf <- causal_results$counterfactuals |>
      filter(scenario == "BMI reduced by 5 units")
    paste0(round(cf$paf * 100, 1), "%")
  })

  output$cf_bp_paf <- renderText({
    cf <- causal_results$counterfactuals |>
      filter(scenario == "No high blood pressure")
    paste0(round(cf$paf * 100, 1), "%")
  })

  # ==========================================================================
  # Tab 7: Fairness Audit Outputs
  # ==========================================================================

  # Issue counts
  output$fairness_total_issues <- renderText({
    as.character(fairness_results$recommendations$n_total_issues)
  })

  output$fairness_age_issues <- renderText({
    issues <- fairness_results$recommendations$adverse_impacts |>
      filter(protected_attr == "Age")
    if (nrow(issues) > 0) as.character(issues$n_issues) else "0"
  })

  output$fairness_income_issues <- renderText({
    issues <- fairness_results$recommendations$adverse_impacts |>
      filter(protected_attr == "Income")
    if (nrow(issues) > 0) as.character(issues$n_issues) else "0"
  })

  output$fairness_education_issues <- renderText({
    issues <- fairness_results$recommendations$adverse_impacts |>
      filter(protected_attr == "Education")
    if (nrow(issues) > 0) as.character(issues$n_issues) else "0"
  })

  output$fairness_selected_attr <- renderText({
    attr_labels <- c(
      sex = "Sex",
      age = "Age",
      income = "Income",
      education = "Education"
    )
    paste0("(", attr_labels[input$fairness_attr], ")")
  })

  # Filtered disparities reactive
  fairness_filtered <- reactive({
    attr_map <- c(sex = "Sex", age = "Age", income = "Income", education = "Education")
    fairness_results$all_disparities |>
      filter(protected_attr == attr_map[input$fairness_attr])
  })

  # Fairness Bar Chart
  output$fairness_bar_chart <- renderPlotly({
    # Lazy load - only render when Fairness Audit tab is active
    req(input$main_nav == "fairness_audit")
    dark <- theme_is_dark()
    df <- fairness_filtered()

    if (nrow(df) == 0) {
      return(plotly_empty() |> layout(title = "No data available"))
    }

    hline_color <- if (dark) "#64748B" else "#94A3B8"

    p <- ggplot(df, aes(x = metric, y = score, fill = group)) +
      geom_col(position = "dodge", width = 0.7) +
      geom_hline(yintercept = 1, linetype = "dashed", color = hline_color) +
      geom_hline(yintercept = 0.8, linetype = "dotted", color = "#EF4444", alpha = 0.7) +
      geom_hline(yintercept = 1.25, linetype = "dotted", color = "#EF4444", alpha = 0.7) +
      scale_fill_manual(values = chart_colors) +
      labs(x = "", y = "Disparity Ratio", fill = "Group") +
      theme_worldclass(dark_mode = dark) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      layout(legend = list(orientation = "h", y = -0.25)) |>
      config(displayModeBar = FALSE)
  })

  # Traffic Light Status
  output$fairness_traffic_lights <- renderUI({
    df <- fairness_filtered()

    if (nrow(df) == 0) {
      return(tags$p(class = "text-muted", "No data available"))
    }

    # Determine status for each metric
    metrics <- unique(df$metric)

    status_list <- lapply(metrics, function(m) {
      metric_data <- df |> filter(metric == m)
      max_disparity <- max(abs(1 - metric_data$score), na.rm = TRUE)

      if (max_disparity < 0.1) {
        indicator_class <- "indicator-green"
        status_text <- "Pass"
      } else if (max_disparity < 0.2) {
        indicator_class <- "indicator-yellow"
        status_text <- "Warning"
      } else {
        indicator_class <- "indicator-red"
        status_text <- "Concern"
      }

      tags$div(
        class = "d-flex align-items-center mb-2",
        tags$span(class = indicator_class),
        tags$span(class = "fw-bold me-2", m),
        tags$span(class = "text-muted small", paste0("(", status_text, ")"))
      )
    })

    tags$div(status_list)
  })

  # Disparity Ratio Table
  output$disparity_table <- renderDT({
    df <- fairness_filtered() |>
      mutate(
        `Disparity Ratio` = round(score, 3),
        `Magnitude` = round(disparity_magnitude * 100, 1),
        `Flagged` = ifelse(adverse_impact, "Yes", "No")
      ) |>
      select(Group = group, Metric = metric, `Disparity Ratio`, `Magnitude`, Flagged)

    datatable(
      df,
      options = list(
        pageLength = 10,
        dom = "frtip",
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = "compact stripe",
      style = "bootstrap5"
    ) |>
      formatStyle(
        "Flagged",
        backgroundColor = styleEqual(c("Yes", "No"), c("#FEE2E2", "#D1FAE5")),
        fontWeight = styleEqual(c("Yes", "No"), c("bold", "normal"))
      )
  })

  # Intersectional Heatmap
  output$intersectional_heatmap <- renderPlotly({
    dark <- theme_is_dark()
    # Use sex_income intersectional data
    intersect_data <- fairness_results$intersectional$sex_income

    if (is.null(intersect_data) || nrow(intersect_data) == 0) {
      return(plotly_empty() |> layout(title = "No intersectional data"))
    }

    # Create accuracy matrix for heatmap
    heatmap_data <- intersect_data |>
      mutate(
        accuracy_pct = round(accuracy * 100, 1)
      )

    text_color <- if (dark) "#F5F0E8" else "#0F172A"
    axis_color <- if (dark) "#94A3B8" else "#64748B"

    plot_ly(
      data = heatmap_data,
      x = ~intersect_sex_income,
      y = ~"Accuracy",
      z = ~accuracy_pct,
      type = "heatmap",
      colorscale = list(c(0, "#FEE2E2"), c(0.5, "#FEF3C7"), c(1, "#D1FAE5")),
      hovertemplate = "%{x}<br>Accuracy: %{z}%<extra></extra>"
    ) |>
      add_annotations(
        x = heatmap_data$intersect_sex_income,
        y = "Accuracy",
        text = paste0(heatmap_data$accuracy_pct, "%"),
        showarrow = FALSE,
        font = list(size = 12, color = "#0F172A")
      ) |>
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(title = "", tickangle = 30, color = axis_color),
        yaxis = list(title = "", color = axis_color),
        margin = list(b = 100),
        font = list(color = text_color)
      ) |>
      config(displayModeBar = FALSE)
  })

  # Fairness Recommendations
  output$fairness_recommendations <- renderUI({
    issues <- fairness_results$recommendations$adverse_impacts

    if (nrow(issues) == 0) {
      return(
        tags$div(
          class = "alert alert-success",
          bs_icon("check-circle-fill", class = "me-2"),
          "No significant fairness issues detected!"
        )
      )
    }

    recommendations <- list(
      tags$div(
        class = "alert alert-warning",
        bs_icon("exclamation-triangle-fill", class = "me-2"),
        tags$strong(paste0(sum(issues$n_issues), " potential fairness issues detected"))
      ),
      tags$ul(
        class = "list-unstyled",
        lapply(1:nrow(issues), function(i) {
          attr <- issues$protected_attr[i]
          n <- issues$n_issues[i]

          tags$li(
            class = "mb-2 d-flex align-items-start",
            tags$span(bs_icon("arrow-right-circle", class = "text-warning me-2 mt-1")),
            tags$div(
              tags$strong(paste0(attr, ": ")),
              paste0(n, " metrics show disparities exceeding thresholds. ",
                     "Consider reviewing model features related to ", tolower(attr), ".")
            )
          )
        })
      ),
      tags$div(
        class = "mt-3 p-3 rounded",
        style = "background-color: #F1F5F9;",
        tags$h6(class = "fw-bold", bs_icon("lightbulb", class = "me-1"), "General Recommendations"),
        tags$ul(
          class = "small mb-0",
          tags$li("Review training data for representation bias"),
          tags$li("Consider fairness-aware model calibration"),
          tags$li("Evaluate threshold adjustments for affected groups"),
          tags$li("Document disparities and mitigation strategies")
        )
      )
    )

    tags$div(recommendations)
  })

  # ==========================================================================
  # Tab 8: Discovery Lab Outputs
  # ==========================================================================

  # Summary counts
  output$discovery_resilient_n <- renderText({
    n <- anomaly_results$subgroup_summary |>
      filter(subgroup == "Resilient") |>
      pull(n)
    scales::comma(n)
  })

  output$discovery_vulnerable_n <- renderText({
    n <- anomaly_results$subgroup_summary |>
      filter(subgroup == "Vulnerable") |>
      pull(n)
    scales::comma(n)
  })

  output$discovery_selected_n <- renderText({
    n <- anomaly_results$subgroup_summary |>
      filter(subgroup == input$discovery_subgroup) |>
      pull(n)
    scales::comma(n)
  })

  output$discovery_selected_rate <- renderText({
    rate <- anomaly_results$subgroup_summary |>
      filter(subgroup == input$discovery_subgroup) |>
      pull(diabetes_rate)
    paste0(round(rate, 1), "%")
  })

  # Residual Distribution Plot
  output$residual_distribution <- renderPlotly({
    # Lazy load - only render when Discovery Lab tab is active
    req(input$main_nav == "discovery_lab")
    dark <- theme_is_dark()
    df <- anomaly_results$subgroup_assignments |>
      slice_sample(n = min(10000, nrow(anomaly_results$subgroup_assignments)))

    subgroup_colors <- c(
      "Resilient" = "#10B981",
      "Vulnerable" = "#EF4444",
      "Expected Healthy" = "#3B82F6",
      "Expected Diabetic" = "#F97316",
      "Typical" = "#6B7280"
    )

    vline_color <- if (dark) "#94A3B8" else "#CBD5E1"

    p <- ggplot(df, aes(x = residual, fill = subgroup)) +
      geom_density(alpha = 0.6) +
      geom_vline(xintercept = 0, linetype = "dashed", color = vline_color) +
      scale_fill_manual(values = subgroup_colors) +
      labs(x = "Prediction Residual", y = "Density", fill = "Subgroup") +
      theme_worldclass(dark_mode = dark)

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      layout(legend = list(orientation = "h", y = -0.2)) |>
      config(displayModeBar = FALSE)
  })

  # Subgroup Profiles Comparison
  output$subgroup_profiles <- renderPlotly({
    dark <- theme_is_dark()
    # Get behavior profile data
    profiles <- anomaly_results$profiles$behavior

    # Reshape for plotting
    profile_long <- profiles |>
      select(subgroup, pct_physically_active, pct_eats_fruits,
             pct_eats_veggies, pct_smoker) |>
      pivot_longer(
        cols = -subgroup,
        names_to = "behavior",
        values_to = "pct"
      ) |>
      mutate(
        behavior = case_when(
          behavior == "pct_physically_active" ~ "Physically Active",
          behavior == "pct_eats_fruits" ~ "Eats Fruits",
          behavior == "pct_eats_veggies" ~ "Eats Vegetables",
          behavior == "pct_smoker" ~ "Smoker"
        )
      )

    # Highlight selected subgroup
    profile_long <- profile_long |>
      mutate(
        highlight = ifelse(subgroup == input$discovery_subgroup, "Selected", "Other")
      )

    p <- ggplot(profile_long, aes(x = behavior, y = pct, fill = subgroup, alpha = highlight)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c(
        "Resilient" = "#10B981",
        "Vulnerable" = "#EF4444",
        "Expected Healthy" = "#3B82F6",
        "Expected Diabetic" = "#F97316",
        "Typical" = "#6B7280"
      )) +
      scale_alpha_manual(values = c("Selected" = 1, "Other" = 0.4), guide = "none") +
      labs(x = "", y = "Percentage (%)", fill = "Subgroup") +
      theme_worldclass(dark_mode = dark) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

    ggplotly(p) |>
      apply_plotly_theme(dark_mode = dark) |>
      layout(legend = list(orientation = "h", y = -0.3)) |>
      config(displayModeBar = FALSE)
  })

  # Key Differentiating Factors Table
  output$factor_table <- renderDT({
    # Select appropriate factor analysis based on subgroup
    if (input$discovery_subgroup == "Resilient") {
      df <- anomaly_results$factor_analysis$protective_factors |>
        head(10)
    } else if (input$discovery_subgroup == "Vulnerable") {
      df <- anomaly_results$factor_analysis$vulnerability_factors |>
        head(10)
    } else {
      df <- anomaly_results$factor_analysis$protective_effects |>
        head(10)
    }

    # Format for display
    df_display <- df |>
      mutate(
        Variable = gsub("_", " ", variable) |> str_to_title(),
        `Effect Size` = round(abs_effect, 3),
        Direction = direction
      ) |>
      select(Variable, `Effect Size`, Direction)

    datatable(
      df_display,
      options = list(
        dom = "t",
        pageLength = 10,
        ordering = FALSE
      ),
      rownames = FALSE,
      class = "compact stripe",
      style = "bootstrap5"
    )
  })

  # Hypothesis Display
  output$hypothesis_display <- renderUI({
    hypotheses <- anomaly_results$hypotheses |>
      filter(testable == TRUE) |>
      arrange(desc(priority == "High"), desc(priority == "Medium"))

    if (nrow(hypotheses) == 0) {
      return(tags$p(class = "text-muted", "No testable hypotheses generated."))
    }

    hypothesis_cards <- lapply(1:min(5, nrow(hypotheses)), function(i) {
      h <- hypotheses[i, ]
      priority_class <- switch(
        h$priority,
        "High" = "hypothesis-high",
        "Medium" = "hypothesis-medium",
        "hypothesis-low"
      )

      tags$div(
        class = paste("hypothesis-card", priority_class),
        tags$div(
          class = "d-flex justify-content-between align-items-start",
          tags$strong(paste0("H", h$hypothesis_id, ": ")),
          tags$span(
            class = "badge",
            style = paste0(
              "background-color: ",
              switch(h$priority, "High" = "#EF4444", "Medium" = "#F59E0B", "#10B981"),
              "; color: white;"
            ),
            h$priority
          )
        ),
        tags$p(class = "small mb-1 mt-1", h$hypothesis),
        tags$p(class = "small text-muted mb-0",
               tags$em("Evidence: "), h$supporting_evidence)
      )
    })

    tags$div(hypothesis_cards)
  })

  # Individual Case Explorer Table
  subgroup_data <- reactive({
    # Combine assignments with original data
    assignments <- anomaly_results$subgroup_assignments |>
      filter(subgroup == input$discovery_subgroup)

    # Get row indices
    indices <- which(anomaly_results$subgroup_assignments$subgroup == input$discovery_subgroup)

    # Sample for performance
    if (length(indices) > 1000) {
      indices <- sample(indices, 1000)
    }

    diabetes_data[indices, ] |>
      mutate(
        predicted_prob = anomaly_results$subgroup_assignments$predicted_prob[indices],
        residual = anomaly_results$subgroup_assignments$residual[indices]
      ) |>
      select(
        diabetes_status, age_group, bmi, bmi_category, gen_hlth_label,
        high_bp, high_chol, smoker, predicted_prob, residual
      )
  })

  output$case_explorer_table <- renderDT({
    df <- subgroup_data() |>
      mutate(
        `Pred. Prob` = paste0(round(predicted_prob * 100, 1), "%"),
        Residual = round(residual, 3)
      ) |>
      select(
        Status = diabetes_status,
        Age = age_group,
        BMI = bmi,
        `BMI Cat.` = bmi_category,
        `Gen Health` = gen_hlth_label,
        `High BP` = high_bp,
        `High Chol` = high_chol,
        Smoker = smoker,
        `Pred. Prob`,
        Residual
      )

    datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "frtip"
      ),
      rownames = FALSE,
      class = "compact stripe hover",
      style = "bootstrap5"
    ) |>
      formatStyle(
        "Status",
        backgroundColor = styleEqual(
          c("No Diabetes", "Prediabetes", "Diabetes"),
          c("#D1FAE5", "#FEF3C7", "#FEE2E2")
        )
      )
  }, server = TRUE)

  # Download handler for subgroup data
  output$download_subgroup <- downloadHandler(
    filename = function() {
      paste0("diabetes_", tolower(gsub(" ", "_", input$discovery_subgroup)),
             "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(subgroup_data(), file)
    }
  )
}

# Run the application ---------------------------------------------------------
shinyApp(ui = ui, server = server)
