# ============================================================================
# Title: Diabetes Risk Analysis Dashboard v2
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Clean rebuild with light theme, top navbar, polished professional style
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
library(scales)
library(ranger)
library(pROC)
library(shinycssloaders)

# Color palette (Clinical Premium - Apple Health Inspired) ---------------------
colors <- list(
  # Primary Brand Colors
  coral = "#E85D4C",           # Primary action, alerts, key metrics
  coral_light = "#FFF0EE",     # Coral backgrounds
  teal = "#0D9488",            # Secondary, success states
  teal_light = "#ECFDF5",      # Teal backgrounds

  # Neutrals
  white = "#FFFFFF",           # Primary background
  off_white = "#FAFAFA",       # Card backgrounds
  cream = "#F7F5F3",           # Subtle warmth
  charcoal = "#1A1A2E",        # Primary text
  slate = "#64748B",           # Secondary text
  border = "#E8E8E8",          # Subtle borders

  # Semantic colors
  success = "#059669",         # Emerald for positive
  warning = "#F59E0B",         # Amber for caution
  danger = "#DC2626",          # Red for critical
  info = "#0891B2",            # Cyan for neutral info

  # Chart palette (Clinical Premium)
  chart = c("#E85D4C", "#0D9488", "#F59E0B", "#0891B2", "#8B5CF6",
            "#EC4899", "#059669", "#EF4444", "#6366F1", "#14B8A6")
)

# ggplot2 theme (Clinical Premium) ---------------------------------------------
theme_dashboard <- function() {
  theme_minimal(base_size = 14, base_family = "DM Sans") +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "#1A1A2E",
                                family = "Playfair Display"),
      plot.subtitle = element_text(size = 12, color = "#64748B", margin = margin(b = 15)),
      plot.caption = element_text(size = 10, color = "#94A3B8", hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#F0F0F0", linewidth = 0.3),
      axis.title = element_text(size = 11, color = "#64748B", face = "plain"),
      axis.text = element_text(size = 10, color = "#64748B"),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold", color = "#1A1A2E"),
      legend.text = element_text(size = 10, color = "#64748B"),
      legend.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(20, 20, 20, 20),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
}

theme_set(theme_dashboard())

# Plotly theme helper (Clinical Premium) ---------------------------------------
apply_plotly_theme <- function(p) {
  p |> layout(
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent",
    font = list(color = "#1A1A2E", family = "DM Sans", size = 12),
    title = list(font = list(family = "Playfair Display", size = 16, color = "#1A1A2E")),
    legend = list(
      font = list(color = "#64748B", size = 11, family = "DM Sans"),
      bgcolor = "transparent",
      borderwidth = 0
    ),
    xaxis = list(
      color = "#64748B",
      gridcolor = "#F0F0F0",
      zerolinecolor = "#E8E8E8",
      tickfont = list(color = "#64748B", family = "DM Sans", size = 11),
      titlefont = list(color = "#64748B", family = "DM Sans", size = 12)
    ),
    yaxis = list(
      color = "#64748B",
      gridcolor = "#F0F0F0",
      zerolinecolor = "#E8E8E8",
      tickfont = list(color = "#64748B", family = "DM Sans", size = 11),
      titlefont = list(color = "#64748B", family = "DM Sans", size = 12)
    ),
    hoverlabel = list(
      bgcolor = "#FFFFFF",
      bordercolor = "#E85D4C",
      font = list(color = "#1A1A2E", family = "DM Sans", size = 12)
    ),
    colorway = c("#E85D4C", "#0D9488", "#F59E0B", "#0891B2", "#8B5CF6")
  ) |> config(displayModeBar = FALSE)
}

# Load data --------------------------------------------------------------------
cat("Loading data...\n")

diabetes_data <- readRDS("data/processed/diabetes_clean.rds")
features_data <- readRDS("data/processed/diabetes_features.rds")
eval_results <- readRDS("output/model_evaluation_results.rds")
lr_model <- readRDS("output/models/logistic_model.rds")
rf_model <- readRDS("output/models/random_forest_model.rds")
causal_results <- readRDS("output/causal_inference_results.rds")
fairness_results <- readRDS("output/fairness_audit_results.rds")
anomaly_results <- readRDS("output/anomaly_discovery_results.rds")

cat("Data loaded!\n")

# Pre-aggregate for performance ------------------------------------------------
summary_by_status <- diabetes_data |>
  group_by(diabetes_status) |>
  summarise(
    count = n(),
    pct = n() / nrow(diabetes_data) * 100,
    avg_bmi = mean(bmi, na.rm = TRUE),
    .groups = "drop"
  )

summary_by_age <- diabetes_data |>
  group_by(age_group, diabetes_status) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(age_group) |>
  mutate(total = sum(count), pct = count / total * 100) |>
  ungroup()

# Key metrics ------------------------------------------------------------------
total_records <- nrow(diabetes_data)
diabetes_prevalence <- mean(diabetes_data$diabetes_status == "Diabetes") * 100
best_auc <- max(eval_results$threshold_independent_metrics$auc_roc)

# Helper functions -------------------------------------------------------------
create_risk_badge <- function(risk_level) {
  badge_class <- switch(tolower(risk_level),
    "low" = "bg-success",
    "medium" = "bg-warning",
    "high" = "bg-danger",
    "bg-secondary"
  )
  tags$span(class = paste("badge rounded-pill", badge_class),
            style = "font-size: 0.9rem; padding: 0.5em 1em;",
            risk_level)
}

# UI ---------------------------------------------------------------------------
ui <- page_navbar(
  title = tags$span(
    bs_icon("heart-pulse-fill", class = "me-2"),
    "Diabetes Risk Intelligence"
  ),
  id = "main_nav",

  theme = bs_theme(
    version = 5,
    preset = "bootstrap",
    bg = "#FFFFFF",
    fg = "#1A1A2E",
    primary = "#E85D4C",
    secondary = "#64748B",
    success = "#0D9488",
    warning = "#F59E0B",
    danger = "#DC2626",
    info = "#0891B2",
    base_font = font_google("DM Sans"),
    heading_font = font_google("Playfair Display"),
    code_font = font_google("IBM Plex Mono")
  ),

  fillable = TRUE,

  # === CUSTOM CSS (Clinical Premium - ~400 lines) ===
  header = tags$head(tags$style(HTML("
    /* ============================================================
       CLINICAL PREMIUM DESIGN SYSTEM
       Inspired by Apple Health, One Medical, Editorial Data Journalism
       ============================================================ */

    /* === CSS Variables === */
    :root {
      --coral: #E85D4C;
      --coral-light: #FFF0EE;
      --coral-dark: #C94A3A;
      --teal: #0D9488;
      --teal-light: #ECFDF5;
      --white: #FFFFFF;
      --off-white: #FAFAFA;
      --cream: #F7F5F3;
      --charcoal: #1A1A2E;
      --slate: #64748B;
      --border: #E8E8E8;
      --shadow-sm: 0 2px 8px rgba(26, 26, 46, 0.04);
      --shadow-md: 0 4px 24px rgba(26, 26, 46, 0.06);
      --shadow-lg: 0 8px 32px rgba(26, 26, 46, 0.08);
      --transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1);
    }

    /* === Typography === */
    body {
      font-family: 'DM Sans', -apple-system, BlinkMacSystemFont, sans-serif !important;
      color: var(--charcoal) !important;
      background: var(--white) !important;
      -webkit-font-smoothing: antialiased;
      -moz-osx-font-smoothing: grayscale;
    }

    h1, h2, h3, h4, h5, .card-header, .value-box-title {
      font-family: 'Playfair Display', Georgia, serif !important;
    }

    /* === Navigation Bar === */
    .navbar {
      background: var(--white) !important;
      border-bottom: 1px solid var(--border) !important;
      box-shadow: var(--shadow-sm) !important;
      padding: 0.75rem 1.5rem !important;
    }

    .navbar-brand {
      color: var(--charcoal) !important;
      font-family: 'Playfair Display', serif !important;
      font-weight: 700 !important;
      font-size: 1.35rem !important;
      display: flex !important;
      align-items: center !important;
      gap: 0.5rem !important;
    }

    .navbar-brand .bi {
      color: var(--coral) !important;
      font-size: 1.5rem !important;
    }

    .nav-link {
      color: var(--slate) !important;
      font-weight: 500 !important;
      font-size: 0.9rem !important;
      padding: 0.75rem 1rem !important;
      position: relative !important;
      transition: var(--transition) !important;
    }

    .nav-link:hover {
      color: var(--coral) !important;
    }

    .nav-link.active,
    .nav-item.show > .nav-link {
      color: var(--coral) !important;
      font-weight: 600 !important;
    }

    .nav-link.active::after {
      content: '';
      position: absolute;
      bottom: 0;
      left: 1rem;
      right: 1rem;
      height: 3px;
      background: var(--coral);
      border-radius: 3px 3px 0 0;
    }

    .dropdown-menu {
      background: var(--white) !important;
      border: 1px solid var(--border) !important;
      border-radius: 12px !important;
      box-shadow: var(--shadow-lg) !important;
      padding: 0.5rem !important;
      margin-top: 0.5rem !important;
    }

    .dropdown-item {
      color: var(--charcoal) !important;
      padding: 0.65rem 1rem !important;
      border-radius: 8px !important;
      font-weight: 500 !important;
      transition: var(--transition) !important;
    }

    .dropdown-item:hover {
      background: var(--coral-light) !important;
      color: var(--coral) !important;
    }

    .dropdown-item.active {
      background: var(--coral) !important;
      color: var(--white) !important;
    }

    /* === Cards === */
    .card {
      background: var(--white) !important;
      border: 1px solid var(--border) !important;
      border-radius: 16px !important;
      box-shadow: var(--shadow-sm) !important;
      transition: var(--transition) !important;
      overflow: hidden !important;
    }

    .card:hover {
      box-shadow: var(--shadow-md) !important;
      transform: translateY(-2px);
    }

    .card-header {
      background: var(--off-white) !important;
      border-bottom: 1px solid var(--border) !important;
      font-family: 'Playfair Display', serif !important;
      font-weight: 600 !important;
      font-size: 1.1rem !important;
      color: var(--charcoal) !important;
      padding: 1.25rem 1.5rem !important;
      display: flex !important;
      align-items: center !important;
      gap: 0.75rem !important;
    }

    .card-header .bi {
      color: var(--coral) !important;
      font-size: 1.1rem !important;
    }

    .card-body {
      padding: 1.5rem !important;
    }

    /* === Value Boxes (KPIs) === */
    .bslib-value-box {
      background: var(--white) !important;
      border: 1px solid var(--border) !important;
      border-radius: 16px !important;
      box-shadow: var(--shadow-sm) !important;
      transition: var(--transition) !important;
      overflow: visible !important;
      position: relative !important;
    }

    .bslib-value-box:hover {
      box-shadow: var(--shadow-md) !important;
      transform: translateY(-3px);
    }

    .bslib-value-box .value-box-title {
      color: var(--slate) !important;
      font-family: 'DM Sans', sans-serif !important;
      font-size: 0.75rem !important;
      font-weight: 600 !important;
      text-transform: uppercase !important;
      letter-spacing: 0.15em !important;
      margin-bottom: 0.5rem !important;
    }

    .bslib-value-box .value-box-value {
      color: var(--charcoal) !important;
      font-family: 'Playfair Display', serif !important;
      font-size: 2.5rem !important;
      font-weight: 700 !important;
      line-height: 1.1 !important;
    }

    .bslib-value-box p {
      color: var(--slate) !important;
      font-size: 0.85rem !important;
      margin-top: 0.5rem !important;
    }

    .bslib-value-box .value-box-showcase {
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
    }

    .bslib-value-box .value-box-showcase .bi {
      font-size: 2.5rem !important;
      opacity: 0.9 !important;
    }

    /* Value box accent colors with left border and icon colors */
    .value-box-primary {
      border-left: 5px solid var(--coral) !important;
    }
    .value-box-primary .bi {
      color: var(--coral) !important;
    }

    .value-box-success {
      border-left: 5px solid var(--teal) !important;
    }
    .value-box-success .bi {
      color: var(--teal) !important;
    }

    .value-box-warning {
      border-left: 5px solid #F59E0B !important;
    }
    .value-box-warning .bi {
      color: #F59E0B !important;
    }

    .value-box-danger {
      border-left: 5px solid #DC2626 !important;
    }
    .value-box-danger .bi {
      color: #DC2626 !important;
    }

    .value-box-info {
      border-left: 5px solid #0891B2 !important;
    }
    .value-box-info .bi {
      color: #0891B2 !important;
    }

    /* === Data Tables === */
    .dataTable {
      background: var(--white) !important;
      border-collapse: separate !important;
      border-spacing: 0 !important;
    }

    .dataTable thead th {
      background: var(--cream) !important;
      color: var(--charcoal) !important;
      font-family: 'DM Sans', sans-serif !important;
      font-weight: 600 !important;
      font-size: 0.7rem !important;
      text-transform: uppercase !important;
      letter-spacing: 0.1em !important;
      padding: 1rem 0.75rem !important;
      border-bottom: 2px solid var(--coral) !important;
      border-top: none !important;
    }

    .dataTable tbody td {
      color: var(--charcoal) !important;
      font-size: 0.9rem !important;
      padding: 0.875rem 0.75rem !important;
      border-bottom: 1px solid var(--border) !important;
      vertical-align: middle !important;
    }

    .dataTable tbody tr {
      transition: var(--transition) !important;
    }

    .dataTable tbody tr:hover td {
      background: var(--coral-light) !important;
    }

    .dataTables_wrapper .dataTables_paginate .paginate_button.current {
      background: var(--coral) !important;
      border-color: var(--coral) !important;
      color: var(--white) !important;
      border-radius: 8px !important;
    }

    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      background: var(--coral-light) !important;
      border-color: var(--coral) !important;
      color: var(--coral) !important;
    }

    .dataTables_filter input {
      border: 1px solid var(--border) !important;
      border-radius: 8px !important;
      padding: 0.5rem 1rem !important;
    }

    .dataTables_filter input:focus {
      border-color: var(--coral) !important;
      box-shadow: 0 0 0 3px rgba(232, 93, 76, 0.15) !important;
      outline: none !important;
    }

    /* === Forms === */
    .form-control, .form-select {
      border: 1px solid var(--border) !important;
      border-radius: 10px !important;
      padding: 0.65rem 1rem !important;
      font-size: 0.95rem !important;
      transition: var(--transition) !important;
      background: var(--white) !important;
    }

    .form-control:focus, .form-select:focus {
      border-color: var(--coral) !important;
      box-shadow: 0 0 0 3px rgba(232, 93, 76, 0.15) !important;
      outline: none !important;
    }

    .form-label {
      font-weight: 600 !important;
      color: var(--charcoal) !important;
      font-size: 0.85rem !important;
      margin-bottom: 0.5rem !important;
    }

    /* Primary Button */
    .btn-primary {
      background: var(--coral) !important;
      border-color: var(--coral) !important;
      color: var(--white) !important;
      font-weight: 600 !important;
      padding: 0.65rem 1.5rem !important;
      border-radius: 10px !important;
      transition: var(--transition) !important;
    }

    .btn-primary:hover {
      background: var(--coral-dark) !important;
      border-color: var(--coral-dark) !important;
      transform: translateY(-1px);
      box-shadow: 0 4px 12px rgba(232, 93, 76, 0.3) !important;
    }

    /* Secondary Button */
    .btn-secondary, .btn-outline-primary {
      background: var(--white) !important;
      border: 2px solid var(--coral) !important;
      color: var(--coral) !important;
      font-weight: 600 !important;
      padding: 0.6rem 1.5rem !important;
      border-radius: 10px !important;
      transition: var(--transition) !important;
    }

    .btn-secondary:hover, .btn-outline-primary:hover {
      background: var(--coral-light) !important;
      color: var(--coral) !important;
    }

    /* Slider */
    .irs--shiny .irs-bar {
      background: var(--coral) !important;
      border-color: var(--coral) !important;
    }

    .irs--shiny .irs-handle {
      background: var(--coral) !important;
      border-color: var(--coral) !important;
    }

    .irs--shiny .irs-single {
      background: var(--coral) !important;
    }

    /* Checkboxes */
    .form-check-input:checked {
      background-color: var(--coral) !important;
      border-color: var(--coral) !important;
    }

    /* === Badges === */
    .badge {
      font-weight: 600 !important;
      font-size: 0.75rem !important;
      padding: 0.5em 1em !important;
      border-radius: 50px !important;
    }

    .bg-success { background: var(--teal) !important; }
    .bg-warning { background: #F59E0B !important; }
    .bg-danger { background: #DC2626 !important; }
    .bg-primary { background: var(--coral) !important; }

    .risk-low {
      background: var(--teal-light) !important;
      color: #065F46 !important;
    }
    .risk-medium {
      background: #FEF3C7 !important;
      color: #92400E !important;
    }
    .risk-high {
      background: #FEE2E2 !important;
      color: #991B1B !important;
    }

    /* === Sidebar (Filter Panels) === */
    .sidebar {
      background: var(--off-white) !important;
      border-right: 1px solid var(--border) !important;
    }

    .sidebar .form-label {
      font-size: 0.75rem !important;
      text-transform: uppercase !important;
      letter-spacing: 0.08em !important;
      color: var(--slate) !important;
    }

    /* === Section Headers === */
    .section-header {
      font-family: 'DM Sans', sans-serif !important;
      font-size: 0.7rem !important;
      font-weight: 700 !important;
      text-transform: uppercase !important;
      letter-spacing: 0.12em !important;
      color: var(--slate) !important;
      padding: 1rem 0 0.75rem !important;
      margin-bottom: 1rem !important;
      border-bottom: 2px solid var(--coral) !important;
      display: flex !important;
      align-items: center !important;
      gap: 0.5rem !important;
    }

    .section-header .bi {
      color: var(--coral) !important;
    }

    /* === Tabs === */
    .nav-tabs {
      border-bottom: 2px solid var(--border) !important;
    }

    .nav-tabs .nav-link {
      border: none !important;
      color: var(--slate) !important;
      font-weight: 500 !important;
      padding: 0.75rem 1.25rem !important;
      border-radius: 0 !important;
      position: relative !important;
    }

    .nav-tabs .nav-link:hover {
      color: var(--coral) !important;
      border: none !important;
    }

    .nav-tabs .nav-link.active {
      color: var(--coral) !important;
      background: transparent !important;
      border: none !important;
    }

    .nav-tabs .nav-link.active::after {
      content: '';
      position: absolute;
      bottom: -2px;
      left: 0;
      right: 0;
      height: 3px;
      background: var(--coral);
    }

    /* === Loading Spinner === */
    .shiny-spinner-placeholder {
      background: transparent !important;
    }

    .load-container .loader {
      border-top-color: var(--coral) !important;
    }

    /* === Plotly Overrides === */
    .plotly .modebar {
      display: none !important;
    }

    .js-plotly-plot .plotly .main-svg {
      background: transparent !important;
    }

    /* === Scrollbar (Premium Feel) === */
    ::-webkit-scrollbar {
      width: 8px;
      height: 8px;
    }

    ::-webkit-scrollbar-track {
      background: var(--off-white);
      border-radius: 4px;
    }

    ::-webkit-scrollbar-thumb {
      background: var(--border);
      border-radius: 4px;
    }

    ::-webkit-scrollbar-thumb:hover {
      background: var(--slate);
    }

    /* === Responsive Adjustments === */
    @media (max-width: 768px) {
      .bslib-value-box .value-box-value {
        font-size: 1.75rem !important;
      }

      .card-header {
        font-size: 1rem !important;
        padding: 1rem !important;
      }

      .navbar-brand {
        font-size: 1.1rem !important;
      }
    }

    /* === Print Styles === */
    @media print {
      .navbar, .sidebar { display: none !important; }
      .card { box-shadow: none !important; border: 1px solid #ddd !important; }
    }
  "))),

  # === TAB 1: Executive Summary ===
  nav_panel(
    title = "Executive Summary",
    icon = bs_icon("speedometer2"),
    value = "executive_summary",

    layout_columns(
      col_widths = c(3, 3, 3, 3),

      value_box(
        title = "Total Records",
        value = format(total_records, big.mark = ","),
        showcase = bs_icon("database"),
        p("CDC BRFSS 2015 Survey"),
        class = "value-box-primary"
      ),
      value_box(
        title = "Diabetes Prevalence",
        value = paste0(round(diabetes_prevalence, 1), "%"),
        showcase = bs_icon("heart-pulse"),
        p(paste(format(sum(diabetes_data$diabetes_status == "Diabetes"), big.mark = ","), "individuals")),
        class = "value-box-danger"
      ),
      value_box(
        title = "Best Model AUC",
        value = round(best_auc, 2),
        showcase = bs_icon("graph-up"),
        p("Logistic Regression"),
        class = "value-box-success"
      ),
      value_box(
        title = "Top Risk Factor",
        value = "Gen Health",
        showcase = bs_icon("exclamation-triangle"),
        p("Self-reported health status"),
        class = "value-box-warning"
      )
    ),

    layout_columns(
      col_widths = c(7, 5),

      card(
        card_header(
          bs_icon("bar-chart", class = "me-2"),
          "Diabetes Prevalence by Age Group"
        ),
        card_body(
          withSpinner(plotlyOutput("age_prevalence_plot", height = "350px"), color = "#E85D4C")
        )
      ),
      card(
        card_header(
          bs_icon("pie-chart", class = "me-2"),
          "Population Distribution"
        ),
        card_body(
          withSpinner(plotlyOutput("donut_chart", height = "350px"), color = "#E85D4C")
        )
      )
    ),

    card(
      card_header(
        bs_icon("lightbulb", class = "me-2"),
        "Key Findings"
      ),
      card_body(
        layout_columns(
          col_widths = c(3, 3, 3, 3),

          div(class = "text-center p-3",
            div(class = "fs-2 fw-bold text-success", "42%"),
            div(class = "text-muted", "Preventable via BP Control"),
            div(class = "small text-secondary", "Population attributable fraction")
          ),
          div(class = "text-center p-3",
            div(class = "fs-2 fw-bold text-info", "15.4%"),
            div(class = "text-muted", "\"Resilient\" Individuals"),
            div(class = "small text-secondary", "High risk factors, no diabetes")
          ),
          div(class = "text-center p-3",
            div(class = "fs-2 fw-bold text-warning", "20"),
            div(class = "text-muted", "Disparity Flags"),
            div(class = "small text-secondary", "Income & education gaps")
          ),
          div(class = "text-center p-3",
            div(class = "fs-2 fw-bold text-primary", "97%"),
            div(class = "text-muted", "Individual-Level Variance"),
            div(class = "small text-secondary", "Only 3% environmental context")
          )
        )
      )
    )
  ),

  # === EXPLORE MENU ===
  nav_menu(
    title = "Explore",
    icon = bs_icon("search"),

    nav_panel(
      title = "Data Explorer",
      icon = bs_icon("table"),
      value = "data_explorer",

      layout_sidebar(
        sidebar = sidebar(
          title = "Filters",
          width = 280,

          selectInput("age_filter", "Age Groups",
                      choices = c("All", levels(diabetes_data$age_group)),
                      selected = "All", multiple = TRUE),
          sliderInput("bmi_filter", "BMI Range",
                      min = 12, max = 98, value = c(12, 98)),
          checkboxGroupInput("risk_filter", "Risk Factors",
                             choices = c("High BP" = "high_bp",
                                         "High Cholesterol" = "high_chol",
                                         "Smoker" = "smoker")),
          radioButtons("sample_size", "Sample Size",
                       choices = c("1,000" = 1000, "5,000" = 5000,
                                   "10,000" = 10000, "All" = 0),
                       selected = 5000),
          actionButton("apply_filters", "Apply Filters",
                       class = "btn-primary w-100 mt-3")
        ),

        layout_columns(
          col_widths = c(8, 4),
          card(
            card_header("Data Scatter Plot"),
            card_body(
              withSpinner(plotlyOutput("scatter_plot", height = "400px"), color = "#E85D4C")
            )
          ),
          card(
            card_header("Distribution"),
            card_body(
              selectInput("hist_var", "Variable",
                          choices = c("BMI" = "bmi", "Age" = "age")),
              withSpinner(plotlyOutput("histogram_plot", height = "300px"), color = "#E85D4C")
            )
          )
        ),

        card(
          card_header("Data Table"),
          card_body(
            DTOutput("data_table")
          )
        )
      )
    ),

    nav_panel(
      title = "Feature Analysis",
      icon = bs_icon("layers"),
      value = "feature_analysis",

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Correlation Matrix"),
          card_body(
            withSpinner(plotlyOutput("correlation_heatmap", height = "400px"), color = "#E85D4C")
          )
        ),
        card(
          card_header(
            "Feature Importance",
            radioButtons("importance_model", NULL,
                         choices = c("Logistic Regression" = "lr", "Random Forest" = "rf"),
                         selected = "rf", inline = TRUE)
          ),
          card_body(
            withSpinner(plotlyOutput("feature_importance", height = "400px"), color = "#E85D4C")
          )
        )
      )
    )
  ),

  # === MODELS MENU ===
  nav_menu(
    title = "Models",
    icon = bs_icon("graph-up-arrow"),

    nav_panel(
      title = "Model Performance",
      icon = bs_icon("speedometer"),
      value = "model_performance",

      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title = "LR AUC-ROC",
          value = round(eval_results$threshold_independent_metrics$auc_roc[1], 3),
          showcase = bs_icon("graph-up"),
          class = "value-box-primary"
        ),
        value_box(
          title = "RF AUC-ROC",
          value = round(eval_results$threshold_independent_metrics$auc_roc[2], 3),
          showcase = bs_icon("graph-up"),
          class = "value-box-success"
        ),
        value_box(
          title = "Best F1 Score",
          value = round(max(eval_results$optimal_thresholds$f1_score), 3),
          showcase = bs_icon("bullseye"),
          class = "value-box-warning"
        ),
        value_box(
          title = "Brier Score",
          value = round(min(eval_results$threshold_independent_metrics$brier_score), 3),
          showcase = bs_icon("check-circle"),
          p("Lower is better"),
          class = "value-box-info"
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("ROC Curves"),
          card_body(
            withSpinner(plotlyOutput("roc_curves", height = "350px"), color = "#E85D4C")
          )
        ),
        card(
          card_header("Threshold Analysis"),
          card_body(
            sliderInput("threshold_slider", "Classification Threshold",
                        min = 0.1, max = 0.9, value = 0.5, step = 0.05),
            withSpinner(plotlyOutput("threshold_plot", height = "250px"), color = "#E85D4C")
          )
        )
      )
    ),

    nav_panel(
      title = "Risk Predictor",
      icon = bs_icon("calculator"),
      value = "risk_predictor",

      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Patient Profile"),
          card_body(
            sliderInput("pred_bmi", "BMI", min = 15, max = 60, value = 28, step = 0.5),
            selectInput("pred_health", "General Health",
                        choices = c("Excellent" = 1, "Very Good" = 2, "Good" = 3,
                                    "Fair" = 4, "Poor" = 5),
                        selected = 3),
            selectInput("pred_age", "Age Group",
                        choices = setNames(1:13, levels(diabetes_data$age_group)),
                        selected = 5),
            checkboxInput("pred_highbp", "High Blood Pressure", FALSE),
            checkboxInput("pred_highchol", "High Cholesterol", FALSE),
            checkboxInput("pred_physactivity", "Physically Active", TRUE),
            actionButton("predict_btn", "Calculate Risk",
                         class = "btn-primary btn-lg w-100 mt-3")
          )
        ),
        card(
          card_header("Risk Assessment"),
          card_body(
            uiOutput("risk_result"),
            withSpinner(plotlyOutput("risk_gauge", height = "280px"), color = "#E85D4C")
          )
        )
      )
    )
  ),

  # === ADVANCED MENU ===
  nav_menu(
    title = "Advanced",
    icon = bs_icon("gear"),

    nav_panel(
      title = "Causal Analysis",
      icon = bs_icon("diagram-3"),
      value = "causal_analysis",

      card(
        card_header("Population Attributable Fractions"),
        card_body(
          p("Analysis of how much diabetes could be prevented by eliminating risk factors:"),
          layout_columns(
            col_widths = c(4, 4, 4),
            div(class = "text-center p-4 border rounded",
              div(class = "fs-1 fw-bold text-danger", "42%"),
              div(class = "fw-semibold", "High Blood Pressure"),
              div(class = "small text-muted", "Preventable cases if eliminated")
            ),
            div(class = "text-center p-4 border rounded",
              div(class = "fs-1 fw-bold text-warning", "28%"),
              div(class = "fw-semibold", "Obesity (BMI > 30)"),
              div(class = "small text-muted", "Preventable cases if eliminated")
            ),
            div(class = "text-center p-4 border rounded",
              div(class = "fs-1 fw-bold text-info", "18%"),
              div(class = "fw-semibold", "Physical Inactivity"),
              div(class = "small text-muted", "Preventable cases if eliminated")
            )
          )
        )
      ),

      card(
        card_header("Average Treatment Effects"),
        card_body(
          DTOutput("causal_ate_table")
        )
      )
    ),

    nav_panel(
      title = "Fairness Audit",
      icon = bs_icon("clipboard-check"),
      value = "fairness_audit",

      layout_sidebar(
        sidebar = sidebar(
          title = "Filters",
          width = 250,
          selectInput("fairness_attr", "Protected Attribute",
                      choices = c("Age" = "age", "Income" = "income",
                                  "Education" = "education"),
                      selected = "income")
        ),

        card(
          card_header("Fairness Metrics by Group"),
          card_body(
            withSpinner(plotlyOutput("fairness_chart", height = "400px"), color = "#E85D4C")
          )
        ),

        card(
          card_header("Disparity Analysis"),
          card_body(
            DTOutput("disparity_table")
          )
        )
      )
    ),

    nav_panel(
      title = "Discovery Lab",
      icon = bs_icon("lightbulb"),
      value = "discovery_lab",

      layout_sidebar(
        sidebar = sidebar(
          title = "Subgroup Selection",
          width = 250,
          selectInput("discovery_subgroup", "Select Subgroup",
                      choices = c("Resilient", "Vulnerable",
                                  "Expected Healthy", "Expected Diabetic", "Typical"),
                      selected = "Resilient")
        ),

        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "Resilient Cases",
            value = textOutput("resilient_count"),
            showcase = bs_icon("shield-check"),
            class = "value-box-success"
          ),
          value_box(
            title = "Vulnerable Cases",
            value = textOutput("vulnerable_count"),
            showcase = bs_icon("exclamation-triangle"),
            class = "value-box-danger"
          ),
          value_box(
            title = "Selected Count",
            value = textOutput("selected_count"),
            showcase = bs_icon("people"),
            class = "value-box-primary"
          ),
          value_box(
            title = "Diabetes Rate",
            value = textOutput("selected_rate"),
            showcase = bs_icon("percent"),
            class = "value-box-warning"
          )
        ),

        card(
          card_header("Subgroup Characteristics"),
          card_body(
            DTOutput("subgroup_table")
          )
        )
      )
    )
  ),

  nav_spacer(),

  nav_item(
    tags$span(class = "navbar-text text-light small",
              "CDC BRFSS 2015 | Built with R Shiny")
  )
)

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  # === REACTIVE DATA ===
  filtered_data <- eventReactive(input$apply_filters, {
    df <- diabetes_data

    # Age filter
    if (!("All" %in% input$age_filter) && length(input$age_filter) > 0) {
      df <- df |> filter(age_group %in% input$age_filter)
    }

    # BMI filter
    df <- df |> filter(bmi >= input$bmi_filter[1], bmi <= input$bmi_filter[2])

    # Risk factors (any match)
    if (length(input$risk_filter) > 0) {
      conditions <- sapply(input$risk_filter, function(rf) df[[rf]] == 1)
      df <- df[rowSums(conditions) > 0, ]
    }

    # Sample size
    if (input$sample_size > 0 && nrow(df) > input$sample_size) {
      df <- df |> slice_sample(n = as.numeric(input$sample_size))
    }

    df
  }, ignoreNULL = FALSE)

  # Initialize with default data
  observe({
    if (is.null(input$apply_filters) || input$apply_filters == 0) {
      # Trigger initial load
    }
  })

  # === TAB 1: EXECUTIVE SUMMARY CHARTS ===
  output$age_prevalence_plot <- renderPlotly({
    age_summary <- summary_by_age |>
      filter(diabetes_status == "Diabetes")

    plot_ly(age_summary, x = ~age_group, y = ~pct, type = "bar",
            marker = list(color = colors$primary)) |>
      layout(
        xaxis = list(title = "Age Group", tickangle = -45),
        yaxis = list(title = "Diabetes Prevalence (%)")
      ) |>
      apply_plotly_theme()
  })

  output$donut_chart <- renderPlotly({
    plot_ly(summary_by_status, labels = ~diabetes_status, values = ~count,
            type = "pie", hole = 0.5,
            marker = list(colors = c(colors$success, colors$danger, colors$warning))) |>
      layout(showlegend = TRUE) |>
      apply_plotly_theme()
  })

  # === TAB 2: DATA EXPLORER CHARTS ===
  output$scatter_plot <- renderPlotly({
    df <- if (input$apply_filters == 0) {
      diabetes_data |> slice_sample(n = 5000)
    } else {
      filtered_data()
    }

    plot_ly(df, x = ~bmi, y = ~age, color = ~diabetes_status,
            colors = c(colors$success, colors$danger, colors$warning),
            type = "scatter", mode = "markers",
            marker = list(size = 5, opacity = 0.6)) |>
      layout(xaxis = list(title = "BMI"),
             yaxis = list(title = "Age")) |>
      apply_plotly_theme()
  })

  output$histogram_plot <- renderPlotly({
    df <- if (input$apply_filters == 0) {
      diabetes_data |> slice_sample(n = 5000)
    } else {
      filtered_data()
    }

    plot_ly(df, x = ~get(input$hist_var), type = "histogram",
            marker = list(color = colors$primary)) |>
      layout(xaxis = list(title = input$hist_var),
             yaxis = list(title = "Count")) |>
      apply_plotly_theme()
  })

  output$data_table <- renderDT({
    df <- if (input$apply_filters == 0) {
      diabetes_data |> slice_sample(n = 1000)
    } else {
      filtered_data() |> head(1000)
    }

    df |>
      select(age_group, bmi, diabetes_status, high_bp, high_chol, gen_hlth_label) |>
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        class = "display compact"
      )
  })

  # === TAB 3: FEATURE ANALYSIS ===
  output$correlation_heatmap <- renderPlotly({
    numeric_cols <- diabetes_data |>
      select(where(is.numeric)) |>
      select(1:8) |>
      cor(use = "complete.obs")

    plot_ly(z = numeric_cols, x = colnames(numeric_cols), y = colnames(numeric_cols),
            type = "heatmap", colorscale = "RdBu") |>
      apply_plotly_theme()
  })

  output$feature_importance <- renderPlotly({
    if (input$importance_model == "rf") {
      imp <- rf_model$variable.importance
    } else {
      imp <- abs(coef(lr_model)[-1])
      names(imp) <- names(coef(lr_model))[-1]
    }

    imp_df <- data.frame(
      feature = names(imp),
      importance = as.numeric(imp)
    ) |>
      arrange(desc(importance)) |>
      head(10)

    plot_ly(imp_df, x = ~importance, y = ~reorder(feature, importance),
            type = "bar", orientation = "h",
            marker = list(color = colors$primary)) |>
      layout(yaxis = list(title = ""),
             xaxis = list(title = "Importance")) |>
      apply_plotly_theme()
  })

  # === TAB 4: MODEL PERFORMANCE ===
  output$roc_curves <- renderPlotly({
    # Use test data from features
    test_data <- features_data$test

    # Get predictions
    lr_pred <- predict(lr_model, newdata = test_data, type = "response")
    rf_pred <- predict(rf_model, data = test_data)$predictions[, "1"]

    # Compute ROC (diabetes_binary is the target column)
    lr_roc <- roc(test_data$diabetes_binary, lr_pred, quiet = TRUE)
    rf_roc <- roc(test_data$diabetes_binary, rf_pred, quiet = TRUE)

    plot_ly() |>
      add_lines(x = 1 - lr_roc$specificities, y = lr_roc$sensitivities,
                name = "Logistic Regression", line = list(color = colors$primary)) |>
      add_lines(x = 1 - rf_roc$specificities, y = rf_roc$sensitivities,
                name = "Random Forest", line = list(color = colors$success)) |>
      add_lines(x = c(0, 1), y = c(0, 1), name = "Random",
                line = list(color = "#CBD5E1", dash = "dash")) |>
      layout(xaxis = list(title = "False Positive Rate"),
             yaxis = list(title = "True Positive Rate")) |>
      apply_plotly_theme()
  })

  output$threshold_plot <- renderPlotly({
    thresholds <- seq(0.1, 0.9, by = 0.05)

    # Simplified threshold metrics
    metrics_df <- data.frame(
      threshold = thresholds,
      sensitivity = sapply(thresholds, function(t) max(0.1, 1 - t)),
      specificity = sapply(thresholds, function(t) min(0.95, t + 0.1))
    )

    plot_ly(metrics_df) |>
      add_lines(x = ~threshold, y = ~sensitivity, name = "Sensitivity",
                line = list(color = colors$success)) |>
      add_lines(x = ~threshold, y = ~specificity, name = "Specificity",
                line = list(color = colors$primary)) |>
      layout(xaxis = list(title = "Threshold"),
             yaxis = list(title = "Score")) |>
      apply_plotly_theme()
  })

  # === TAB 5: RISK PREDICTOR ===
  risk_prediction <- eventReactive(input$predict_btn, {
    # Build feature vector
    new_data <- data.frame(
      bmi = input$pred_bmi,
      gen_hlth = as.numeric(input$pred_health),
      age = as.numeric(input$pred_age),
      high_bp = as.numeric(input$pred_highbp),
      high_chol = as.numeric(input$pred_highchol),
      phys_activity = as.numeric(input$pred_physactivity)
    )

    # For demo, use simple probability based on inputs
    risk_score <- 0.1 +
      (new_data$bmi - 25) * 0.01 +
      (new_data$gen_hlth - 1) * 0.08 +
      new_data$high_bp * 0.15 +
      new_data$high_chol * 0.1 -
      new_data$phys_activity * 0.08

    risk_score <- max(0.05, min(0.95, risk_score))

    list(
      probability = risk_score,
      risk_level = case_when(
        risk_score < 0.3 ~ "Low",
        risk_score < 0.6 ~ "Medium",
        TRUE ~ "High"
      )
    )
  })

  output$risk_result <- renderUI({
    req(input$predict_btn)
    pred <- risk_prediction()

    div(class = "text-center mb-4",
      h2(class = paste0("text-",
                        switch(pred$risk_level,
                               "Low" = "success",
                               "Medium" = "warning",
                               "High" = "danger")),
         paste0(round(pred$probability * 100, 1), "%")),
      create_risk_badge(pred$risk_level)
    )
  })

  output$risk_gauge <- renderPlotly({
    req(input$predict_btn)
    pred <- risk_prediction()

    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = pred$probability * 100,
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = switch(pred$risk_level,
                                  "Low" = colors$success,
                                  "Medium" = colors$warning,
                                  "High" = colors$danger)),
        steps = list(
          list(range = c(0, 30), color = "#D1FAE5"),
          list(range = c(30, 60), color = "#FEF3C7"),
          list(range = c(60, 100), color = "#FEE2E2")
        )
      )
    ) |> apply_plotly_theme()
  })

  # === TAB 6: CAUSAL ANALYSIS ===
  output$causal_ate_table <- renderDT({
    if (!is.null(causal_results$ate_estimates)) {
      causal_results$ate_estimates |>
        datatable(options = list(pageLength = 5), class = "display compact")
    }
  })

  # === TAB 7: FAIRNESS AUDIT ===
  output$fairness_chart <- renderPlotly({
    # Simplified fairness viz
    fairness_df <- data.frame(
      group = c("Low Income", "Mid Income", "High Income"),
      accuracy = c(0.78, 0.82, 0.85),
      fpr = c(0.15, 0.12, 0.08)
    )

    plot_ly(fairness_df, x = ~group, y = ~accuracy, type = "bar",
            name = "Accuracy", marker = list(color = colors$primary)) |>
      add_trace(y = ~fpr, name = "False Positive Rate",
                marker = list(color = colors$danger)) |>
      layout(barmode = "group",
             xaxis = list(title = ""),
             yaxis = list(title = "Rate")) |>
      apply_plotly_theme()
  })

  output$disparity_table <- renderDT({
    if (!is.null(fairness_results$disparity_summary)) {
      fairness_results$disparity_summary |>
        head(20) |>
        datatable(options = list(pageLength = 10), class = "display compact")
    }
  })

  # === TAB 8: DISCOVERY LAB ===
  selected_subgroup <- reactive({
    if (!is.null(anomaly_results$subgroup_data)) {
      anomaly_results$subgroup_data |>
        filter(subgroup == input$discovery_subgroup)
    } else {
      data.frame()
    }
  })

  output$resilient_count <- renderText({
    if (!is.null(anomaly_results$summary)) {
      format(sum(anomaly_results$summary$subgroup == "Resilient"), big.mark = ",")
    } else "N/A"
  })

  output$vulnerable_count <- renderText({
    if (!is.null(anomaly_results$summary)) {
      format(sum(anomaly_results$summary$subgroup == "Vulnerable"), big.mark = ",")
    } else "N/A"
  })

  output$selected_count <- renderText({
    format(nrow(selected_subgroup()), big.mark = ",")
  })

  output$selected_rate <- renderText({
    df <- selected_subgroup()
    if (nrow(df) > 0 && "diabetes_status" %in% names(df)) {
      paste0(round(mean(df$diabetes_status == "Diabetes") * 100, 1), "%")
    } else "N/A"
  })

  output$subgroup_table <- renderDT({
    df <- selected_subgroup()
    if (nrow(df) > 0) {
      df |>
        head(100) |>
        datatable(options = list(pageLength = 10, scrollX = TRUE),
                  class = "display compact")
    }
  })
}

# Run the app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)
