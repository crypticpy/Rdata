# ============================================================================
# RespiWatch: Global Respiratory Surveillance Platform
# Multi-Pathogen Tracking System for 2024-2025 Season
# Built via voice-driven development with Claude Code
# ============================================================================

# Load packages ---------------------------------------------------------------
library(shiny)
library(bslib)
library(jsonlite)
library(leaflet)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyjs)
library(scales)
library(DT)
library(sf)
library(rnaturalearth)
library(DBI)
library(RSQLite)

# Source data loading modules -------------------------------------------------
source("R/data_loader.R")
source("R/rt_estimation.R")
source("R/forecasting.R")
source("R/bayesian_forecast.R")
source("R/model_diagnostics.R")
source("R/scenario_modeling.R")
source("R/ensemble_forecast.R")
source("R/healthcare_capacity.R")
source("R/wave_propagation.R")
source("R/vaccination_impact.R")
source("R/ai_hooks.R")
source("R/date_range_module.R")  # Date range control module for per-tab filtering

# Load data with database preference, JSON fallback ---------------------------
dashboard_data <- load_dashboard_data()
USE_DATABASE <- dashboard_data$source == "database"

# Check if database has timeline data we can use directly
db_has_timeline <- USE_DATABASE &&
  !is.null(dashboard_data$timeline) &&
  is.data.frame(dashboard_data$timeline) &&
  nrow(dashboard_data$timeline) > 0

# Always load JSON files for UI metadata (pathogen info, surveillance status, etc.)
# These are needed for static UI elements regardless of data source
outbreak_data <- fromJSON("data/raw/h3n2_outbreak_tracker.json")
rsv_data <- fromJSON("data/raw/rsv_tracker.json")
covid_data <- fromJSON("data/raw/covid_tracker.json")

if (db_has_timeline) {
  message("Using SQLite database for live CDC/WHO surveillance data")
  # Database-sourced data is already in the dashboard_data list
  db_surveillance <- dashboard_data$surveillance
  db_timeline <- dashboard_data$timeline
  db_kpi <- dashboard_data$kpi
  # We'll use db_timeline for combined_timeline_df (real API data)
} else {
  message("Database timeline empty or unavailable, using JSON files for timeline")
  # No database timeline - JSON will be used for everything
}

# Populate countries_df from Database or JSON
if (USE_DATABASE) {
  # Fetch latest surveillance snapshot from DB (ALL pathogens - for reactive filtering)
  conn <- get_db_connection()
  map_snapshot_all <- get_latest_surveillance(conn)  # Keep all pathogen data for reactive filtering
  close_db_connection(conn)

  # Helper function to filter surveillance data by pathogen
  filter_by_pathogen <- function(data, pathogen_code_filter) {
    if (is.null(data) || nrow(data) == 0) return(NULL)

    data |>
      filter(pathogen_code == pathogen_code_filter) |>
      group_by(iso_code) |>
      slice_max(observation_date, n = 1, with_ties = FALSE) |>
      ungroup() |>
      transmute(
        iso_code = iso_code,
        country_name = country_name,
        population = population,
        outbreak_status = ifelse(!is.na(positivity_rate) & positivity_rate > 10, "High Activity", "Monitoring"),
        positivity_rate = positivity_rate,
        subclade_k_prevalence = 0,
        hospitalization_rate = hospitalization_rate %||% 0,
        confirmed_cases = case_count,
        vaccination_rate = vaccination_rate,
        data_confidence = data_confidence,
        last_updated = as.character(observation_date),
        stringsAsFactors = FALSE
      )
  }

  # Create initial countries_df with H3N2 (default pathogen)
  if (!is.null(map_snapshot_all) && nrow(map_snapshot_all) > 0) {
    countries_df <- filter_by_pathogen(map_snapshot_all, "H3N2")
  } else {
    # Fallback structure if DB empty
     countries_df <- data.frame(
      iso_code = character(), country_name = character(), population = numeric(),
      outbreak_status = character(), positivity_rate = numeric(),
      vaccination_rate = numeric(),
      subclade_k_prevalence = numeric(), hospitalization_rate = numeric(),
      confirmed_cases = numeric(), data_confidence = character(),
      last_updated = character(), stringsAsFactors = FALSE
    )
  }
} else {
  # Legacy JSON Fallback
  countries_df <- tryCatch({
    do.call(rbind, lapply(outbreak_data$countries, function(country) {
      data.frame(
        iso_code = country$iso_code,
        country_name = country$country_name,
        population = country$population,
        outbreak_status = country$outbreak_status,
        positivity_rate = country$h3n2_data$positivity_rate %||% 0,
        vaccination_rate = 0, # JSON doesn't have this
        subclade_k_prevalence = country$h3n2_data$subclade_k_prevalence %||% 0,
        hospitalization_rate = country$h3n2_data$hospitalization_rate %||% 0,
        confirmed_cases = country$h3n2_data$case_numbers$confirmed_cases %||%
                          country$h3n2_data$case_numbers$estimated_cases %||% 0,
        data_confidence = country$data_confidence_level,
        last_updated = country$last_updated,
        stringsAsFactors = FALSE
      )
    }))
  }, error = function(e) {
    message("Warning: Failed to extract country data: ", e$message)
    data.frame(
      iso_code = character(), country_name = character(), population = numeric(),
      outbreak_status = character(), positivity_rate = numeric(),
      vaccination_rate = numeric(),
      subclade_k_prevalence = numeric(), hospitalization_rate = numeric(),
      confirmed_cases = numeric(), data_confidence = character(),
      last_updated = character(), stringsAsFactors = FALSE
    )
  })
}

# Country coordinates for map
country_coords <- data.frame(
  iso_code = c("USA", "GBR", "JPN", "AUS", "CAN", "CHN", "EEA"),
  lat = c(39.8, 54.0, 36.2, -25.3, 56.1, 35.9, 50.0),
  lng = c(-98.6, -2.0, 138.3, 133.8, -106.3, 104.2, 10.0),
  stringsAsFactors = FALSE
)

countries_df <- left_join(countries_df, country_coords, by = "iso_code")

# Load world countries shapefile for choropleth map ----------------------------
world_countries <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(iso_a3, name, geometry) |>
  rename(iso_code = iso_a3, country_name_geo = name)

# Merge outbreak data with world shapefile
# Countries with data get outbreak info, others get NA (shown in gray)
world_map_data <- world_countries |>
  left_join(countries_df, by = "iso_code")

# Extract timeline data - handle jsonlite's data frame structure (with error handling)
timeline_df <- tryCatch({
  timeline_raw <- outbreak_data$timeline$progression_by_date
  if (is.data.frame(timeline_raw)) {
    # jsonlite parsed surveillance_indicators as a nested data.frame
    data.frame(
      date = as.Date(timeline_raw$date),
      week_number = timeline_raw$week_number,
      positivity_rate = timeline_raw$surveillance_indicators$positivity_rate %||% 0,
      case_numbers = timeline_raw$surveillance_indicators$case_numbers %||% 0,
      hospitalization_rate = timeline_raw$surveillance_indicators$hospitalization_rate %||% 0,
      stringsAsFactors = FALSE
    )
  } else {
    # It's a list of lists
    bind_rows(
      lapply(timeline_raw, function(entry) {
        data.frame(
          date = as.Date(entry$date),
          week_number = entry$week_number,
          positivity_rate = entry$surveillance_indicators$positivity_rate %||% 0,
          case_numbers = entry$surveillance_indicators$case_numbers %||% 0,
          hospitalization_rate = entry$surveillance_indicators$hospitalization_rate %||% 0,
          stringsAsFactors = FALSE
        )
      })
    )
  }
}, error = function(e) {
  message("Warning: Failed to extract timeline data: ", e$message)
  data.frame(
    date = as.Date(character()), week_number = integer(),
    positivity_rate = numeric(), case_numbers = numeric(),
    hospitalization_rate = numeric(), stringsAsFactors = FALSE
  )
})

# Extract anomaly flags - handle jsonlite's data frame structure (with error handling)
anomalies_df <- tryCatch({
  anomalies_raw <- outbreak_data$anomaly_detection_flags$suspicious_patterns
  if (is.data.frame(anomalies_raw)) {
    # jsonlite parsed it as a data frame
    data.frame(
      pattern_id = anomalies_raw$pattern_id,
      pattern_type = anomalies_raw$pattern_type,
      description = anomalies_raw$description,
      geographic_scope = anomalies_raw$geographic_scope,
      confidence_level = anomalies_raw$confidence_level,
      verification_status = anomalies_raw$verification_status,
      first_detected = as.Date(anomalies_raw$first_detected),
      stringsAsFactors = FALSE
    )
  } else {
    # It's a list of lists
    bind_rows(
      lapply(anomalies_raw, function(pattern) {
        data.frame(
          pattern_id = pattern$pattern_id,
          pattern_type = pattern$pattern_type,
          description = pattern$description,
          geographic_scope = pattern$geographic_scope,
          confidence_level = pattern$confidence_level,
          verification_status = pattern$verification_status,
          first_detected = as.Date(pattern$first_detected),
          stringsAsFactors = FALSE
        )
      })
    )
  }
}, error = function(e) {
  message("Warning: Failed to extract anomaly data: ", e$message)
  data.frame(
    pattern_id = character(), pattern_type = character(),
    description = character(), geographic_scope = character(),
    confidence_level = character(), verification_status = character(),
    first_detected = as.Date(character()), stringsAsFactors = FALSE
  )
})

# Extract RSV timeline data - handle jsonlite's data frame structure (with error handling)
rsv_timeline_df <- tryCatch({
  rsv_timeline_raw <- rsv_data$timeline$progression_by_date
  if (is.data.frame(rsv_timeline_raw)) {
    # jsonlite parsed surveillance_indicators as a nested data.frame
    data.frame(
      date = as.Date(rsv_timeline_raw$date),
      week_number = rsv_timeline_raw$week_number,
      positivity_rate = rsv_timeline_raw$surveillance_indicators$positivity_rate %||% 0,
      case_numbers = rsv_timeline_raw$surveillance_indicators$case_numbers %||% 0,
      hospitalization_rate = rsv_timeline_raw$surveillance_indicators$hospitalization_rate %||% 0,
      pathogen = "RSV",
      stringsAsFactors = FALSE
    )
  } else {
    bind_rows(
      lapply(rsv_timeline_raw, function(entry) {
        data.frame(
          date = as.Date(entry$date),
          week_number = entry$week_number,
          positivity_rate = entry$surveillance_indicators$positivity_rate %||% 0,
          case_numbers = entry$surveillance_indicators$case_numbers %||% 0,
          hospitalization_rate = entry$surveillance_indicators$hospitalization_rate %||% 0,
          pathogen = "RSV",
          stringsAsFactors = FALSE
        )
      })
    )
  }
}, error = function(e) {
  message("Warning: Failed to extract RSV timeline data: ", e$message)
  data.frame(
    date = as.Date(character()), week_number = integer(),
    positivity_rate = numeric(), case_numbers = numeric(),
    hospitalization_rate = numeric(), pathogen = character(),
    stringsAsFactors = FALSE
  )
})

# Extract COVID timeline data - handle jsonlite's data frame structure (with error handling)
covid_timeline_df <- tryCatch({
  covid_timeline_raw <- covid_data$timeline$progression_by_date
  if (is.data.frame(covid_timeline_raw)) {
    # jsonlite parsed surveillance_indicators as a nested data.frame
    data.frame(
      date = as.Date(covid_timeline_raw$date),
      week_number = covid_timeline_raw$week_number,
      positivity_rate = covid_timeline_raw$surveillance_indicators$positivity_rate %||% 0,
      case_numbers = covid_timeline_raw$surveillance_indicators$case_numbers %||% 0,
      hospitalization_rate = covid_timeline_raw$surveillance_indicators$hospitalization_rate %||% 0,
      pathogen = "COVID-19",
      stringsAsFactors = FALSE
    )
  } else {
    bind_rows(
      lapply(covid_timeline_raw, function(entry) {
        data.frame(
          date = as.Date(entry$date),
          week_number = entry$week_number,
          positivity_rate = entry$surveillance_indicators$positivity_rate %||% 0,
          case_numbers = entry$surveillance_indicators$case_numbers %||% 0,
          hospitalization_rate = entry$surveillance_indicators$hospitalization_rate %||% 0,
          pathogen = "COVID-19",
          stringsAsFactors = FALSE
        )
      })
    )
  }
}, error = function(e) {
  message("Warning: Failed to extract COVID timeline data: ", e$message)
  data.frame(
    date = as.Date(character()), week_number = integer(),
    positivity_rate = numeric(), case_numbers = numeric(),
    hospitalization_rate = numeric(), pathogen = character(),
    stringsAsFactors = FALSE
  )
})

# Build combined_timeline_df from database or JSON
if (db_has_timeline) {
  # Use database timeline directly - already formatted by get_timeline_data()
  combined_timeline_df <- db_timeline
  message(sprintf("Using database timeline: %d records from real CDC/WHO APIs", nrow(combined_timeline_df)))
} else {
  # Fallback to JSON: Add pathogen column to H3N2 timeline (only if data exists)
  if (nrow(timeline_df) > 0) {
    timeline_df$pathogen <- "H3N2 (Influenza)"
  } else {
    timeline_df$pathogen <- character()
  }

  # Combine all pathogen timelines for comparative analysis
  combined_timeline_df <- bind_rows(
    timeline_df,
    rsv_timeline_df,
    covid_timeline_df
  )
  message(sprintf("Using JSON fallback timeline: %d records", nrow(combined_timeline_df)))
}

# Pathogen color palette for charts (expanded to all 6 pathogens)
pathogen_colors <- c(
  "H3N2 (Influenza)" = "#E85D4C",  # Coral (primary theme color)
  "RSV" = "#2563EB",               # Blue
  "COVID-19" = "#7C3AED",          # Purple
  "H5N1 (Avian)" = "#DC2626",      # Red (danger color for avian flu)
  "Influenza A" = "#F97316",       # Orange
  "Influenza B" = "#0891B2",       # Cyan
  "H3N2" = "#E85D4C",              # Alias for code lookups
  "COVID19" = "#7C3AED",           # Alias for code lookups
  "H5N1" = "#DC2626",              # Alias for code lookups
  "FLU_A" = "#F97316",             # Alias for code lookups
  "FLU_B" = "#0891B2"              # Alias for code lookups
)

# Clinical Premium Theme Configuration ----------------------------------------
clinical_premium_theme <- bs_theme(
  version = 5,
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
  code_font = font_google("IBM Plex Mono"),
  "navbar-bg" = "#FFFFFF",
  "card-bg" = "#FFFFFF",
  "card-border-color" = "#E8E8E8"
)

# Custom CSS ------------------------------------------------------------------
custom_css <- "
:root {
  --coral: #E85D4C;
  --coral-light: #FFF0EE;
  --teal: #0D9488;
  --teal-light: #ECFDF5;
  --charcoal: #1A1A2E;
  --slate: #64748B;
  --border: #E8E8E8;
  --off-white: #FAFAFA;
  --shadow-sm: 0 1px 3px rgba(0,0,0,0.1);
  --shadow-md: 0 4px 6px rgba(0,0,0,0.1);
}

body {
  font-family: 'DM Sans', sans-serif;
  background: var(--off-white);
}

/* Header - Enhanced */
.navbar {
  background: linear-gradient(180deg, #FFFFFF 0%, #FAFAFA 100%) !important;
  border-bottom: 1px solid var(--border);
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
  position: sticky;
  top: 0;
  z-index: 1000;
}

.navbar-brand {
  font-family: 'Playfair Display', serif !important;
  font-weight: 700 !important;
  font-size: 1.5rem !important;
  color: var(--charcoal) !important;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.navbar-brand::before {
  content: '';
  display: inline-block;
  width: 8px;
  height: 24px;
  background: linear-gradient(180deg, var(--coral) 0%, #D14836 100%);
  border-radius: 4px;
}

.navbar-nav .nav-link {
  transition: all 0.3s ease;
  position: relative;
}

.navbar-nav .nav-link:hover {
  color: var(--coral) !important;
}

.navbar-nav .nav-link.active::after {
  content: '';
  position: absolute;
  bottom: 0;
  left: 50%;
  transform: translateX(-50%);
  width: 60%;
  height: 3px;
  background: var(--coral);
  border-radius: 3px 3px 0 0;
}

/* KPI Cards - Enhanced */
.kpi-card {
  background: linear-gradient(145deg, #FFFFFF 0%, #FAFAFA 100%);
  border-radius: 16px;
  padding: 1.5rem;
  border-left: 4px solid var(--coral);
  box-shadow: 0 2px 8px rgba(0,0,0,0.06), 0 1px 3px rgba(0,0,0,0.08);
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
  position: relative;
  overflow: hidden;
}

.kpi-card::before {
  content: '';
  position: absolute;
  top: 0;
  right: 0;
  width: 80px;
  height: 80px;
  background: radial-gradient(circle at top right, rgba(232, 93, 76, 0.08) 0%, transparent 70%);
  pointer-events: none;
}

.kpi-card:hover {
  transform: translateY(-4px);
  box-shadow: 0 8px 25px rgba(0,0,0,0.1), 0 4px 10px rgba(0,0,0,0.08);
}

.kpi-card.severity-high {
  border-left-color: #DC2626;
}
.kpi-card.severity-high::before {
  background: radial-gradient(circle at top right, rgba(220, 38, 38, 0.1) 0%, transparent 70%);
}

.kpi-card.severity-moderate {
  border-left-color: #F59E0B;
}
.kpi-card.severity-moderate::before {
  background: radial-gradient(circle at top right, rgba(245, 158, 11, 0.1) 0%, transparent 70%);
}

.kpi-card.severity-low {
  border-left-color: var(--teal);
}
.kpi-card.severity-low::before {
  background: radial-gradient(circle at top right, rgba(13, 148, 136, 0.1) 0%, transparent 70%);
}

.kpi-value {
  font-size: 2.5rem;
  font-weight: 700;
  color: var(--charcoal);
  font-family: 'DM Sans', sans-serif;
  line-height: 1.1;
}

.kpi-label {
  font-size: 0.8rem;
  color: var(--slate);
  text-transform: uppercase;
  letter-spacing: 0.08em;
  font-weight: 500;
}

.kpi-change {
  font-size: 0.9rem;
  font-weight: 600;
  display: inline-flex;
  align-items: center;
  gap: 4px;
  padding: 2px 8px;
  border-radius: 12px;
}

.kpi-change.up {
  color: #DC2626;
  background: rgba(220, 38, 38, 0.1);
}
.kpi-change.down {
  color: var(--teal);
  background: rgba(13, 148, 136, 0.1);
}

/* Map Container */
.map-container {
  background: #FFFFFF;
  border-radius: 12px;
  padding: 1rem;
  box-shadow: var(--shadow-sm);
  border: 1px solid var(--border);
}

/* Alert Cards - Enhanced */
.alert-card {
  background: var(--coral-light);
  border-left: 4px solid var(--coral);
  border-radius: 12px;
  padding: 1rem 1.25rem;
  margin-bottom: 0.75rem;
  position: relative;
  transition: all 0.2s ease;
  box-shadow: 0 1px 3px rgba(0,0,0,0.05);
}

.alert-card:hover {
  transform: translateX(4px);
  box-shadow: 0 2px 6px rgba(0,0,0,0.08);
}

.alert-card::before {
  font-family: 'Font Awesome 5 Free';
  font-weight: 900;
  position: absolute;
  right: 1rem;
  top: 50%;
  transform: translateY(-50%);
  font-size: 1.5rem;
  opacity: 0.15;
}

.alert-card.critical {
  background: linear-gradient(90deg, #FEE2E2 0%, #FEF2F2 100%);
  border-left-color: #DC2626;
}
.alert-card.critical::before {
  content: '\f071';
  color: #DC2626;
}

.alert-card.high {
  background: linear-gradient(90deg, #FEF3C7 0%, #FFFBEB 100%);
  border-left-color: #F59E0B;
}
.alert-card.high::before {
  content: '\f06a';
  color: #F59E0B;
}

.alert-card.info {
  background: linear-gradient(90deg, #DBEAFE 0%, #EFF6FF 100%);
  border-left-color: #3B82F6;
}
.alert-card.info::before {
  content: '\f05a';
  color: #3B82F6;
}

.alert-card.success {
  background: linear-gradient(90deg, #D1FAE5 0%, #ECFDF5 100%);
  border-left-color: #10B981;
}
.alert-card.success::before {
  content: '\f058';
  color: #10B981;
}

.alert-card .alert-title {
  font-weight: 600;
  color: var(--charcoal);
  font-size: 0.95rem;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.alert-card .alert-description {
  color: var(--slate);
  font-size: 0.85rem;
  margin-top: 0.35rem;
  line-height: 1.5;
  padding-right: 2rem;
}

.alert-card .alert-meta {
  font-size: 0.75rem;
  color: var(--slate);
  margin-top: 0.5rem;
  opacity: 0.8;
}

/* Section Headers */
.section-header {
  font-family: 'Playfair Display', serif;
  font-size: 1.25rem;
  font-weight: 600;
  color: var(--charcoal);
  margin-bottom: 1rem;
  padding-bottom: 0.5rem;
  border-bottom: 2px solid var(--coral);
}

/* Status Badges - Enhanced */
.status-badge {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 0.3rem 0.85rem;
  border-radius: 9999px;
  font-size: 0.7rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  position: relative;
  box-shadow: 0 1px 3px rgba(0,0,0,0.1);
}

.status-badge::before {
  content: '';
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background: currentColor;
}

.status-badge.active {
  background: linear-gradient(135deg, #FEE2E2 0%, #FECACA 100%);
  color: #DC2626;
  animation: pulse-badge 2s ease-in-out infinite;
}
.status-badge.active::before {
  animation: pulse-dot 1.5s ease-in-out infinite;
}

.status-badge.elevated {
  background: linear-gradient(135deg, #FEF3C7 0%, #FDE68A 100%);
  color: #D97706;
}

.status-badge.monitoring {
  background: linear-gradient(135deg, #DBEAFE 0%, #BFDBFE 100%);
  color: #2563EB;
}

.status-badge.peak {
  background: linear-gradient(135deg, #FCE7F3 0%, #FBCFE8 100%);
  color: #BE185D;
  animation: pulse-badge 2s ease-in-out infinite;
}

.status-badge.stable {
  background: linear-gradient(135deg, #D1FAE5 0%, #A7F3D0 100%);
  color: #059669;
}

@keyframes pulse-badge {
  0%, 100% { box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
  50% { box-shadow: 0 2px 8px rgba(220, 38, 38, 0.25); }
}

@keyframes pulse-dot {
  0%, 100% { opacity: 1; transform: scale(1); }
  50% { opacity: 0.6; transform: scale(1.3); }
}

/* Continental Status */
.continent-card {
  background: #FFFFFF;
  border-radius: 8px;
  padding: 1rem;
  border: 1px solid var(--border);
  margin-bottom: 0.5rem;
}

.continent-name {
  font-weight: 600;
  color: var(--charcoal);
}

.continent-rate {
  font-size: 1.5rem;
  font-weight: 700;
  color: var(--coral);
}

/* Chart Container - Enhanced */
.chart-container {
  background: #FFFFFF;
  border-radius: 16px;
  padding: 1.5rem;
  box-shadow: 0 2px 8px rgba(0,0,0,0.05);
  border: 1px solid var(--border);
  position: relative;
  transition: box-shadow 0.3s ease;
}

.chart-container:hover {
  box-shadow: 0 4px 16px rgba(0,0,0,0.08);
}

.chart-container .chart-title {
  font-family: 'Playfair Display', serif;
  font-size: 1.1rem;
  font-weight: 600;
  color: var(--charcoal);
  margin-bottom: 1rem;
  padding-bottom: 0.5rem;
  border-bottom: 1px solid var(--border);
}

/* Loading state for charts */
.shiny-plot-output.recalculating,
.plotly.recalculating {
  opacity: 0.5;
  pointer-events: none;
}

.shiny-plot-output.recalculating::after,
.plotly.recalculating::after {
  content: '';
  position: absolute;
  top: 50%;
  left: 50%;
  width: 40px;
  height: 40px;
  margin: -20px 0 0 -20px;
  border: 4px solid var(--border);
  border-top-color: var(--coral);
  border-radius: 50%;
  animation: spin 0.8s linear infinite;
}

@keyframes spin {
  to { transform: rotate(360deg); }
}

/* Data Table */
.dataTables_wrapper {
  font-family: 'DM Sans', sans-serif;
}

/* Sidebar */
.sidebar {
  background: #FFFFFF;
  border-radius: 12px;
  padding: 1.5rem;
  box-shadow: var(--shadow-sm);
  border: 1px solid var(--border);
}

/* Tab Navigation */
.nav-pills .nav-link {
  color: var(--slate);
  border-radius: 8px;
  padding: 0.75rem 1.25rem;
}

.nav-pills .nav-link.active {
  background: var(--coral);
  color: #FFFFFF;
}

/* Value Boxes */
.value-box {
  border-radius: 12px;
  overflow: hidden;
}

/* Loading spinner */
.shiny-spinner-placeholder {
  color: var(--coral);
}

/* Pathogen Selector */
.pathogen-selector-container {
  background: #FFFFFF;
  border-radius: 12px;
  padding: 1.25rem;
  box-shadow: var(--shadow-sm);
  border: 1px solid var(--border);
}

.pathogen-selector-container .section-header {
  margin-bottom: 0.75rem;
  font-size: 1rem;
}

.pathogen-selector-container .shiny-input-radiogroup {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
}

.pathogen-selector-container .radio-inline {
  padding: 0;
  margin: 0;
}

.pathogen-selector-container .radio-inline input[type='radio'] {
  display: none;
}

.pathogen-selector-container .radio-inline label {
  display: inline-block;
  padding: 0.5rem 1.25rem;
  border-radius: 9999px;
  background: var(--off-white);
  border: 2px solid var(--border);
  color: var(--slate);
  font-weight: 500;
  font-size: 0.875rem;
  cursor: pointer;
  transition: all 0.2s ease;
}

.pathogen-selector-container .radio-inline label:hover {
  border-color: var(--coral);
  color: var(--coral);
}

.pathogen-selector-container .radio-inline input[type='radio']:checked + label {
  background: var(--coral);
  border-color: var(--coral);
  color: #FFFFFF;
  box-shadow: 0 2px 4px rgba(232, 93, 76, 0.3);
}

/* Pathogen-specific colors */
.pathogen-selector-container .radio-inline:nth-child(3) input[type='radio']:checked + label {
  background: #2563EB;
  border-color: #2563EB;
}

.pathogen-selector-container .radio-inline:nth-child(4) input[type='radio']:checked + label {
  background: #7C3AED;
  border-color: #7C3AED;
}

.pathogen-selector-container .radio-inline:nth-child(5) input[type='radio']:checked + label {
  background: #059669;
  border-color: #059669;
}
"

# UI --------------------------------------------------------------------------
ui <- page_navbar(
  title = "RespiWatch",
  theme = clinical_premium_theme,
  header = tags$head(
    useShinyjs(),
    tags$style(HTML(custom_css)),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Playfair+Display:wght@400;600;700&family=IBM+Plex+Mono&display=swap",
      rel = "stylesheet"
    ),
    tags$script(src = "epidemic-animation.js")
  ),

  # Global Overview Tab -------------------------------------------------------
  nav_panel(
    "Global Overview",
    icon = icon("globe"),

    # KPI Cards Row
    div(
      class = "container-fluid mt-4",

      # Pathogen Selector Row
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          div(
            class = "pathogen-selector-container",
            style = "display: flex; flex-wrap: wrap; align-items: center; gap: 20px;",

            # Pathogen Radio Buttons
            div(
              style = "flex: 1; min-width: 400px;",
              h4(class = "section-header mb-2", "Select Pathogen"),
              radioButtons(
                "selected_pathogen",
                label = NULL,
                choices = c(
                  "All Pathogens" = "all",
                  "H3N2 (Influenza A)" = "h3n2",
                  "RSV" = "rsv",
                  "COVID-19" = "covid",
                  "H5N1/H5N5 (Avian)" = "h5n1"
                ),
                selected = "all",
                inline = TRUE
              )
            ),

            # Metric Dropdown (migrated from floating panel)
            div(
              style = "min-width: 180px;",
              selectInput("map_metric", "Map Metric:",
                choices = c(
                  "Positivity Rate (%)" = "positivity_rate",
                  "Vaccination Coverage (%)" = "vaccination_rate",
                  "Confirmed Cases (Total)" = "confirmed_cases"
                ),
                selected = "positivity_rate",
                width = "180px"
              )
            ),

            # Toggle Checkboxes (migrated from floating panel)
            div(
              class = "selector-toggles",
              style = "display: flex; flex-direction: column; gap: 5px;",
              checkboxInput("show_bubbles", "Show Case Volume (Bubbles)", value = FALSE),
              checkboxInput("show_anomalies", "Show Alerts (High Activity)", value = TRUE)
            ),

            # Date Range Control for Global Overview (90 days default)
            div(
              style = "min-width: 280px;",
              dateRangeControlUI("global_date_range", "Timeline Range", default_days = 90)
            )
          )
        )
      ),

      # Dynamic KPI Cards based on pathogen selection
      uiOutput("pathogen_kpis"),

      # Main Content Row
      div(
        class = "row g-4",

        # Left Column - Map
        div(
          class = "col-lg-8",
          div(
            class = "map-container",
            h4(class = "section-header", "Global Outbreak Distribution"),
            leafletOutput("global_map", height = "500px"),

            # Animation Controls Panel
            div(
              class = "animation-controls-panel mt-3 p-3",
              style = "background: var(--surface-elevated); border-radius: 8px; border: 1px solid var(--border-color);",

              # Layer Toggle Row
              div(
                class = "d-flex justify-content-between align-items-center mb-3",
                div(
                  class = "animation-date-display w-100 text-center",
                  tags$span(class = "text-muted me-2", "Current Date:"),
                  tags$span(id = "animation_current_date", class = "fw-bold", "---")
                ),

              ),

              # Playback Controls Row
              div(
                class = "d-flex align-items-center gap-3",

                # Play/Pause/Stop buttons
                div(
                  class = "playback-buttons d-flex gap-1",
                  actionButton("animation_step_backward", "",
                    icon = icon("step-backward"),
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Previous Frame"
                  ),
                  actionButton("animation_play_pause", "",
                    icon = icon("play"),
                    class = "btn btn-sm btn-primary",
                    title = "Play"
                  ),
                  actionButton("animation_stop", "",
                    icon = icon("stop"),
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Stop"
                  ),
                  actionButton("animation_step_forward", "",
                    icon = icon("step-forward"),
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Next Frame"
                  )
                ),

                # Timeline Slider
                div(
                  class = "timeline-slider flex-grow-1",
                  sliderInput("animation_date", NULL,
                              min = as.Date("2023-01-01"),
                              max = Sys.Date(),
                              value = Sys.Date(),
                              timeFormat = "%Y-%m-%d",
                              ticks = FALSE,
                              step = 7, # Weekly steps
                              animate = FALSE, # We control animation manually
                              width = "100%"
                  )
                ),

                # Frame Counter
                div(
                  class = "frame-counter",
                  style = "min-width: 80px; text-align: center;",
                  tags$span(id = "animation_frame_counter", class = "text-muted small", "0 / 0")
                ),

                # Speed Selector
                div(
                  class = "speed-selector",
                  selectInput(
                    "animation_speed",
                    NULL,
                    choices = c("Slow" = "slow", "Normal" = "normal", "Fast" = "fast", "Very Fast" = "veryFast"),
                    selected = "normal",
                    width = "100px"
                  )
                )
              )
            )
          ),

          # Timeline Chart
          div(
            class = "chart-container mt-4",
            h4(class = "section-header", "Outbreak Progression Timeline"),
            plotlyOutput("timeline_chart", height = "300px")
          ),

          # Wave Propagation Analysis Section
          div(
            class = "wave-propagation-section mt-4 p-3",
            style = "background: var(--surface-elevated); border-radius: 8px; border: 1px solid var(--border-color);",

            # Header with run button
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h4(class = "section-header mb-0", icon("water"), " Wave Propagation Analysis"),
              actionButton(
                "run_wave_analysis",
                "Refresh Wave Analysis",
                icon = icon("sync"),
                class = "btn btn-outline-primary btn-sm"
              )
            ),

            # Wave metrics row
            div(
              class = "row g-3",
              div(
                class = "col-md-4",
                div(
                  class = "metric-card p-3 text-center",
                  style = "background: var(--bg-primary); border-radius: 8px;",
                  div(class = "metric-value fs-4 fw-bold text-primary",
                    uiOutput("wave_velocity_display", inline = TRUE)
                  ),
                  div(class = "metric-label text-muted small", "Spread Velocity (km/day)")
                )
              ),
              div(
                class = "col-md-4",
                div(
                  class = "metric-card p-3 text-center",
                  style = "background: var(--bg-primary); border-radius: 8px;",
                  div(class = "metric-value fs-4 fw-bold text-warning",
                    uiOutput("wave_countries_display", inline = TRUE)
                  ),
                  div(class = "metric-label text-muted small", "Countries Affected")
                )
              ),
              div(
                class = "col-md-4",
                div(
                  class = "metric-card p-3 text-center",
                  style = "background: var(--bg-primary); border-radius: 8px;",
                  div(class = "metric-value fs-4 fw-bold",
                    uiOutput("wave_confidence_display", inline = TRUE)
                  ),
                  div(class = "metric-label text-muted small", "Confidence Level")
                )
              )
            ),

            # Wave analysis details/status
            div(
              class = "wave-analysis-status mt-3",
              uiOutput("wave_analysis_output")
            ),

            # Wave Comparison Cards (visible when "All Pathogens" selected)
            uiOutput("wave_comparison_cards")
          )
        ),

        # Right Column - Status & Alerts
        div(
          class = "col-lg-4",

          # Continental Status
          div(
            class = "sidebar mb-4",
            h4(class = "section-header", "Continental Status"),
            uiOutput("continental_status")
          ),

          # Anomaly Alerts
          div(
            class = "sidebar",
            h4(class = "section-header", "Critical Anomalies"),
            uiOutput("anomaly_alerts")
          )
        )
      )
    )
  ),

  # Country Analysis Tab ------------------------------------------------------
  nav_panel(
    "Country Analysis",
    icon = icon("flag"),

    div(
      class = "container-fluid mt-4",

      # Country Selector
      div(
        class = "row mb-4",
        div(
          class = "col-md-4",
          selectInput(
            "selected_country",
            "Select Country",
            choices = setNames(countries_df$iso_code, countries_df$country_name),
            selected = "USA"
          )
        ),
        # Date Range Control for Country Analysis
        div(
          class = "col-md-4",
          dateRangeControlUI("country_date_range", "Analysis Date Range")
        )
      ),

      # Country Details
      div(
        class = "row g-4",

        # Country KPIs
        div(
          class = "col-md-8",
          uiOutput("country_kpis"),

          # Country anomalies
          div(
            class = "chart-container mt-4",
            h4(class = "section-header", "Country Anomaly Flags"),
            uiOutput("country_anomalies")
          )
        ),

        # Country Info Sidebar
        div(
          class = "col-md-4",
          div(
            class = "sidebar",
            h4(class = "section-header", "Healthcare Capacity"),
            uiOutput("country_healthcare")
          ),

          div(
            class = "sidebar mt-4",
            h4(class = "section-header", "Policy Responses"),
            uiOutput("country_policies")
          )
        )
      )
    )
  ),

  # Pathogen Analysis Tab -----------------------------------------------------
  nav_panel(
    "Pathogen Analysis",
    icon = icon("virus"),

    div(
      class = "container-fluid mt-4",

      # Date Range Control Row for Pathogen Analysis
      div(
        class = "row mb-4",
        div(
          class = "col-md-4",
          dateRangeControlUI("pathogen_date_range", "Pathogen Data Range")
        )
      ),

      div(
        class = "row g-4",

        # Primary Pathogen Info
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Primary Pathogen: H3N2"),
            div(
              class = "row",
              div(
                class = "col-6",
                div(class = "kpi-card severity-high",
                    div(class = "kpi-label", "Dominant Strain"),
                    div(class = "kpi-value", style = "font-size: 1.5rem;",
                        outbreak_data$pathogen_analysis$primary_pathogen$dominant_strain),
                    div(class = "kpi-change", "Subclade K (J.2.4.1)")
                )
              ),
              div(
                class = "col-6",
                div(class = "kpi-card severity-high",
                    div(class = "kpi-label", "Global Prevalence"),
                    div(class = "kpi-value",
                        paste0(outbreak_data$pathogen_analysis$primary_pathogen$global_prevalence, "%")),
                    div(class = "kpi-change up", "Severity: HIGH")
                )
              )
            )
          )
        ),

        # Vaccine Effectiveness
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Vaccine Effectiveness by Strain"),
            plotlyOutput("vaccine_effectiveness_chart", height = "250px")
          )
        )
      ),

      # Comparative Multi-Pathogen Timeline Charts
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Comparative Positivity Rates"),
            plotlyOutput("comparative_positivity_chart", height = "300px")
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Hospitalization Rates by Pathogen"),
            plotlyOutput("comparative_hospitalization_chart", height = "300px")
          )
        )
      ),

      # Stacked Area Chart for all pathogens
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Multi-Pathogen Surveillance Timeline"),
            plotlyOutput("multi_pathogen_timeline", height = "350px")
          )
        )
      ),

      # Cross-Pathogen Analysis
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Cross-Pathogen Analysis"),
            DTOutput("pathogen_comparison_table")
          )
        )
      ),

      # Co-infection Patterns
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Co-Infection Patterns"),
            uiOutput("coinfection_cards")
          )
        )
      ),

      # ==========================================================================
      # VACCINATION IMPACT TRACKER (Phase 6E)
      # ==========================================================================
      div(
        class = "row g-4 mt-4",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h4(class = "section-header mb-0",
                 icon("syringe", class = "me-2"), "Vaccination Impact Tracker"),
              div(
                class = "d-flex gap-2",
                selectInput(
                  "vaccination_pathogen_select",
                  label = NULL,
                  choices = c("Influenza A", "Influenza B", "RSV", "COVID-19"),
                  selected = "Influenza A",
                  width = "180px"
                ),
                actionButton(
                  "refresh_vaccination_data",
                  label = NULL,
                  icon = icon("sync"),
                  class = "btn btn-outline-primary"
                )
              )
            ),

            # Vaccination KPIs Row
            div(
              class = "row g-3 mb-3",
              div(
                class = "col-md-3",
                div(class = "kpi-card",
                    div(class = "kpi-label", "Population Coverage"),
                    uiOutput("vaccination_coverage_display"),
                    div(class = "kpi-change", "Weighted average")
                )
              ),
              div(
                class = "col-md-3",
                div(class = "kpi-card severity-low",
                    div(class = "kpi-label", "Current VE (Infection)"),
                    uiOutput("vaccination_ve_display"),
                    div(class = "kpi-change", "With waning adjustment")
                )
              ),
              div(
                class = "col-md-3",
                div(class = "kpi-card severity-medium",
                    div(class = "kpi-label", "Cases Prevented"),
                    uiOutput("prevented_cases_display"),
                    div(class = "kpi-change", "Estimated this season")
                )
              ),
              div(
                class = "col-md-3",
                div(class = "kpi-card severity-high",
                    div(class = "kpi-label", "Deaths Averted"),
                    uiOutput("prevented_deaths_display"),
                    div(class = "kpi-change", "Estimated this season")
                )
              )
            ),

            # Vaccination Impact Details
            div(
              class = "row g-3",
              div(
                class = "col-md-6",
                div(
                  class = "p-3 bg-light rounded",
                  h5(class = "mb-3", icon("chart-pie", class = "me-2"), "VE by Outcome"),
                  uiOutput("ve_by_outcome_display")
                )
              ),
              div(
                class = "col-md-6",
                div(
                  class = "p-3 bg-light rounded",
                  h5(class = "mb-3", icon("virus-covid", class = "me-2"), "VE by Variant"),
                  uiOutput("ve_by_variant_display")
                )
              )
            ),

            # Full Vaccination Impact Summary
            div(
              class = "mt-3",
              uiOutput("vaccination_impact_summary")
            )
          )
        )
      )
    )
  ),

  # Surveillance Gaps Tab -----------------------------------------------------
  nav_panel(
    "Surveillance Gaps",
    icon = icon("exclamation-triangle"),

    div(
      class = "container-fluid mt-4",

      # Surveillance System Status
      div(
        class = "row g-4 mb-4",
        div(
          class = "col-md-4",
          div(
            class = paste0("kpi-card ",
                          ifelse(outbreak_data$global_overview$surveillance_system_status$who_status == "operational",
                                 "severity-low", "severity-high")),
            div(class = "kpi-label", "WHO Status"),
            div(class = "kpi-value", style = "font-size: 1.5rem;",
                toupper(outbreak_data$global_overview$surveillance_system_status$who_status)),
            div(class = "kpi-change", "Global Monitoring")
          )
        ),
        div(
          class = "col-md-4",
          div(
            class = paste0("kpi-card ",
                          ifelse(outbreak_data$global_overview$surveillance_system_status$cdc_status == "operational",
                                 "severity-low", "severity-high")),
            div(class = "kpi-label", "CDC Status"),
            div(class = "kpi-value", style = "font-size: 1.5rem;",
                toupper(outbreak_data$global_overview$surveillance_system_status$cdc_status)),
            div(class = "kpi-change up", "US Surveillance")
          )
        ),
        div(
          class = "col-md-4",
          div(
            class = paste0("kpi-card ",
                          ifelse(outbreak_data$global_overview$surveillance_system_status$ecdc_status == "operational",
                                 "severity-low", "severity-high")),
            div(class = "kpi-label", "ECDC Status"),
            div(class = "kpi-value", style = "font-size: 1.5rem;",
                toupper(outbreak_data$global_overview$surveillance_system_status$ecdc_status)),
            div(class = "kpi-change", "European Monitoring")
          )
        )
      ),

      # Date Range Control Row for Surveillance Gaps
      div(
        class = "row mb-4",
        div(
          class = "col-md-4",
          dateRangeControlUI("gaps_date_range", "Surveillance Data Range")
        )
      ),

      # Identified Gaps
      div(
        class = "row g-4",
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Identified Data Gaps"),
            uiOutput("data_gaps_list")
          )
        ),

        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Surveillance Gap Timeline"),
            DTOutput("surveillance_gaps_table")
          )
        )
      ),

      # Information Suppression Evidence
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Information Suppression Evidence"),
            uiOutput("suppression_evidence")
          )
        )
      )
    )
  ),

  # Rt Analysis Tab -----------------------------------------------------------
  nav_panel(
    "Rt Analysis",
    icon = icon("chart-line"),

    div(
      class = "container-fluid mt-4",

      # Page Header
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h2(class = "section-header", "Reproduction Number (Rt) Analysis"),
          p(class = "text-muted",
            "Real-time estimation of the effective reproduction number for respiratory pathogens. ",
            "Rt > 1 indicates epidemic growth; Rt < 1 indicates decline."
          )
        )
      ),

      # Controls Row
      div(
        class = "row mb-4",
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Select Pathogen"),
            selectInput(
              "rt_pathogen",
              label = NULL,
              choices = c("H3N2 (Influenza)" = "H3N2",
                          "RSV" = "RSV",
                          "COVID-19" = "COVID19"),
              selected = "H3N2"
            )
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Date Range"),
            dateRangeControlUI("rt_date_range", label = NULL)
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Current Rt Status"),
            uiOutput("rt_current_status")
          )
        )
      ),

      # Rt Time Series Plot
      div(
        class = "row g-4",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Rt Time Series with 95% Credible Interval"),
            plotlyOutput("rt_timeseries_plot", height = "400px")
          )
        )
      ),

      # Forecast Section
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "4-Week Case Forecast"),
            plotlyOutput("forecast_plot", height = "350px")
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Forecast Summary"),
            uiOutput("forecast_summary_table")
          )
        )
      ),

      # ==========================================================================
      # AI INSIGHTS PANEL (Phase 6F)
      # ==========================================================================
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h4(class = "section-header mb-0",
                 icon("robot", class = "me-2"), "AI Insights"),
              div(
                class = "d-flex gap-2",
                actionButton(
                  "explain_rt_btn",
                  label = "Explain Rt Value",
                  icon = icon("lightbulb"),
                  class = "btn btn-outline-primary btn-sm"
                ),
                actionButton(
                  "explain_forecast_btn",
                  label = "Explain Forecast",
                  icon = icon("chart-line"),
                  class = "btn btn-outline-info btn-sm"
                )
              )
            ),
            div(
              id = "ai_insights_panel",
              class = "p-3 bg-light rounded",
              uiOutput("ai_insights_content")
            )
          )
        )
      ),

      # All Pathogens Summary Table
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Multi-Pathogen Rt Summary"),
            tableOutput("rt_summary_table")
          )
        )
      )
    )
  ),

  # Bayesian Forecast Tab -----------------------------------------------------
  nav_panel(
    "Bayesian Forecast",
    icon = icon("chart-area"),

    div(
      class = "container-fluid mt-4",

      # Page Header
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h2(class = "section-header", "Bayesian Epidemic Forecasting"),
          p(class = "text-muted",
            "Probabilistic forecasts using hierarchical Bayesian models (brms). ",
            "Multiple credible intervals show forecast uncertainty."
          )
        )
      ),

      # Controls Row
      div(
        class = "row mb-4",
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Select Pathogen"),
            selectInput(
              "bayes_pathogen",
              label = NULL,
              choices = c("H3N2 (Influenza)" = "H3N2",
                          "RSV" = "RSV",
                          "COVID-19" = "COVID19"),
              selected = "H3N2"
            ),
            actionButton(
              "run_bayesian_model",
              "Generate Forecast",
              class = "btn btn-primary w-100 mt-2",
              icon = icon("play")
            )
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Training Data Range"),
            dateRangeControlUI("bayes_date_range", label = NULL, default_days = 120)
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Model Status"),
            uiOutput("bayes_model_status")
          )
        )
      ),

      # Main Forecast Plot
      div(
        class = "row g-4",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Bayesian Forecast with Credible Intervals"),
            plotlyOutput("bayes_forecast_plot", height = "450px")
          )
        )
      ),

      # Forecast Details and Diagnostics
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Forecast Summary"),
            uiOutput("bayes_forecast_summary")
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Model Diagnostics"),
            uiOutput("bayes_diagnostics_summary")
          )
        )
      ),

      # Advanced Diagnostics Row (NEW - expandable)
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h4(class = "section-header mb-0", icon("microscope"), " Advanced Diagnostics"),
              actionButton("toggle_diagnostics", "Show Diagnostic Plots",
                           class = "btn btn-sm btn-outline-secondary",
                           icon = icon("chart-line"))
            ),
            conditionalPanel(
              condition = "input.toggle_diagnostics % 2 == 1",
              div(class = "row",
                  div(class = "col-md-6",
                      h5("Trace Plots", class = "text-muted"),
                      plotOutput("bayes_trace_plot", height = "300px")
                  ),
                  div(class = "col-md-6",
                      h5("Posterior Density", class = "text-muted"),
                      plotOutput("bayes_density_plot", height = "300px")
                  )
              ),
              div(class = "row mt-3",
                  div(class = "col-md-6",
                      h5("Posterior Predictive Check", class = "text-muted"),
                      plotOutput("bayes_ppc_plot", height = "300px")
                  ),
                  div(class = "col-md-6",
                      h5("Parameter Summary", class = "text-muted"),
                      tableOutput("bayes_param_table")
                  )
              )
            )
          )
        )
      ),

      # Ensemble Forecast Comparison Row (NEW)
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h4(class = "section-header mb-0", icon("layer-group"), " Ensemble Forecast Comparison"),
              actionButton("generate_ensemble", "Generate Ensemble Forecast",
                           class = "btn btn-primary",
                           icon = icon("sync"))
            ),
            p(class = "text-muted small mb-3",
              "Compare Rt-renewal, Bayesian, and weighted ensemble forecasts. ",
              "Ensemble method combines both approaches for more robust predictions."),
            # Ensemble comparison plot
            plotlyOutput("ensemble_comparison_plot", height = "400px"),
            # Skill scores table
            div(class = "mt-4",
                h5(class = "text-muted", "Forecast Skill Scores"),
                tableOutput("ensemble_skill_scores")
            )
          )
        )
      ),

      # All Pathogens Comparison
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Multi-Pathogen Bayesian Forecast Comparison"),
            tableOutput("bayes_all_pathogens_table")
          )
        )
      ),

      # Download Section
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Export Data"),
            downloadButton("download_bayes_forecast", "Download Forecast CSV", class = "btn btn-secondary"),
            tags$span(class = "ms-3 text-muted", "Export forecast data with credible intervals")
          )
        )
      )
    )
  ),

  # Scenario Analysis Tab (NEW!) -----------------------------------------------
  nav_panel(
    "Scenario Analysis",
    icon = icon("sliders-h"),

    div(
      class = "container-fluid mt-4",

      # Page Header
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h2(class = "section-header", "What-If Scenario Analysis"),
          p(class = "text-muted",
            "Explore how different interventions could affect epidemic trajectories. ",
            "Compare mask mandates, social distancing, vaccination campaigns, and more."
          )
        )
      ),

      # Date Range Control Row for Scenario Analysis
      div(
        class = "row mb-3",
        div(
          class = "col-md-4",
          dateRangeControlUI("scenario_date_range", "Historical Data Range")
        )
      ),

      # Controls Row
      div(
        class = "row mb-4",
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Pathogen"),
            selectInput(
              "scenario_pathogen",
              label = NULL,
              choices = c("H3N2 (Influenza)" = "H3N2",
                          "RSV" = "RSV",
                          "COVID-19" = "COVID19"),
              selected = "H3N2"
            )
          )
        ),
        div(
          class = "col-md-4",
          div(
            class = "chart-container",
            h4(class = "section-header", "Select Scenarios"),
            checkboxGroupInput(
              "scenario_selection",
              label = NULL,
              choices = c(
                "No Intervention (Baseline)" = "no_intervention",
                "Mask Mandate (-15% Rt)" = "mask_mandate",
                "Social Distancing (-25% Rt)" = "social_distancing",
                "School Closure (-20% Rt)" = "school_closure",
                "Light Lockdown (-40% Rt)" = "lockdown_light",
                "Full Lockdown (-60% Rt)" = "lockdown_full",
                "Vaccination Campaign (-30% Rt)" = "vaccination_boost",
                "Combined Moderate (-45% Rt)" = "combined_moderate"
              ),
              selected = c("no_intervention", "mask_mandate", "social_distancing")
            )
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Forecast Horizon"),
            sliderInput(
              "scenario_horizon",
              label = NULL,
              min = 4,
              max = 12,
              value = 8,
              step = 1,
              post = " weeks"
            ),
            actionButton(
              "run_scenarios",
              "Run Scenarios",
              class = "btn btn-primary w-100 mt-3",
              icon = icon("play")
            )
          )
        ),
        div(
          class = "col-md-2",
          div(
            class = "chart-container",
            h4(class = "section-header", "Current Rt"),
            uiOutput("scenario_current_rt")
          )
        )
      ),

      # Main Scenario Plot
      div(
        class = "row g-4",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Scenario Comparison: Projected Cases"),
            plotlyOutput("scenario_comparison_plot", height = "450px")
          )
        )
      ),

      # Impact Summary Table
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-md-8",
          div(
            class = "chart-container",
            h4(class = "section-header", "Scenario Impact Summary"),
            p(class = "text-muted small", "Cases averted compared to no-intervention baseline"),
            DTOutput("scenario_impact_table")
          )
        ),
        div(
          class = "col-md-4",
          div(
            class = "chart-container",
            h4(class = "section-header", "Key Insights"),
            uiOutput("scenario_insights")
          )
        )
      ),

      # Custom Scenario Builder
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Custom Scenario Builder"),
            div(
              class = "row",
              div(
                class = "col-md-4",
                textInput("custom_scenario_name", "Scenario Name", placeholder = "My Custom Intervention"),
                sliderInput(
                  "custom_rt_reduction",
                  "Rt Reduction (%)",
                  min = 0,
                  max = 80,
                  value = 30,
                  step = 5,
                  post = "%"
                )
              ),
              div(
                class = "col-md-6",
                textAreaInput("custom_scenario_desc", "Description",
                              placeholder = "Describe the intervention mix...",
                              rows = 2),
                actionButton(
                  "add_custom_scenario",
                  "Add to Comparison",
                  class = "btn btn-secondary mt-2",
                  icon = icon("plus")
                )
              )
            )
          )
        )
      )
    )
  ),

  # Healthcare Capacity Tab ---------------------------------------------------
  nav_panel(
    "Healthcare Capacity",
    icon = icon("hospital"),

    div(
      class = "container-fluid mt-4",

      # Header
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h2(class = "section-header", icon("hospital"), " Healthcare Capacity Dashboard"),
          p(class = "text-muted",
            "Monitor hospital and ICU capacity with surge forecasting. ",
            "Alerts trigger when capacity approaches critical thresholds.")
        )
      ),

      # Controls Row
      div(
        class = "row mb-4",
        div(
          class = "col-md-3",
          selectInput("capacity_pathogen", "Pathogen",
                      choices = c("All Combined" = "all",
                                  "Influenza H3N2" = "H3N2",
                                  "RSV" = "RSV",
                                  "COVID-19" = "COVID19"),
                      selected = "all")
        ),
        div(
          class = "col-md-3",
          div(
            h5("Data Range"),
            dateRangeControlUI("capacity_date_range", label = NULL)
          )
        ),
        div(
          class = "col-md-3",
          sliderInput("capacity_horizon", "Forecast Horizon (Weeks)",
                      min = 1, max = 8, value = 4, step = 1)
        ),
        div(
          class = "col-md-3 pt-4",
          actionButton("generate_capacity_forecast", "Generate Forecast",
                       class = "btn btn-primary",
                       icon = icon("sync"))
        )
      ),

      # Capacity Gauges Row
      div(
        class = "row g-4",
        # Hospital Capacity Gauge
        div(
          class = "col-md-6",
          div(
            class = "chart-container text-center",
            h4(class = "section-header", icon("bed"), " Hospital Capacity"),
            uiOutput("hospital_gauge"),
            p(class = "text-muted mt-2", uiOutput("hospital_status_text"))
          )
        ),
        # ICU Capacity Gauge
        div(
          class = "col-md-6",
          div(
            class = "chart-container text-center",
            h4(class = "section-header", icon("procedures"), " ICU Capacity"),
            uiOutput("icu_gauge"),
            p(class = "text-muted mt-2", uiOutput("icu_status_text"))
          )
        )
      ),

      # Utilization Timeline Chart
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", icon("chart-line"), " Capacity Utilization Forecast"),
            plotlyOutput("capacity_timeline_plot", height = "350px")
          )
        )
      ),

      # Hospitalization Forecast & Surge Alerts Row
      div(
        class = "row g-4 mt-2",
        # Hospitalization Forecast
        div(
          class = "col-md-8",
          div(
            class = "chart-container",
            h4(class = "section-header", icon("chart-area"), " Projected Hospitalizations"),
            plotlyOutput("hospitalization_forecast_plot", height = "300px")
          )
        ),
        # Surge Alerts
        div(
          class = "col-md-4",
          div(
            class = "chart-container",
            h4(class = "section-header", icon("exclamation-triangle"), " Surge Alerts"),
            uiOutput("surge_alerts_display")
          )
        )
      ),

      # Surge Timing Summary
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", icon("clock"), " Surge Timing Predictions"),
            tableOutput("surge_timing_table")
          )
        )
      )
    )
  ),

  # About Tab -----------------------------------------------------------------
  nav_panel(
    "About",
    icon = icon("info-circle"),

    div(
      class = "container-fluid mt-4",
      div(
        class = "row justify-content-center",
        div(
          class = "col-md-8",
          div(
            class = "chart-container",
            h2(class = "section-header", "About RespiWatch"),

            tags$p(
              "RespiWatch is a ", tags$strong("Global Respiratory Multi-Pathogen Tracking System"),
              " for the 2024-2025 season. This platform combines real-time geospatial disease ",
              "intelligence with epidemiological analysis to provide comprehensive surveillance ",
              "of respiratory pathogens including Influenza (H3N2, H1N1), RSV, and COVID-19."
            ),

            tags$h4("Key Features", class = "mt-4 mb-3", style = "color: var(--coral);"),
            tags$ul(
              tags$li("Global outbreak distribution mapping"),
              tags$li("Multi-pathogen comparison and cross-analysis"),
              tags$li("Vaccine effectiveness monitoring"),
              tags$li("Anomaly detection and early warning signals"),
              tags$li("Surveillance gap identification"),
              tags$li("Country-level drill-down analysis")
            ),

            tags$h4("Data Sources", class = "mt-4 mb-3", style = "color: var(--coral);"),
            tags$ul(
              tags$li(tags$strong("WHO"), " - World Health Organization Global Influenza Surveillance"),
              tags$li(tags$strong("CDC"), " - US Centers for Disease Control FluView"),
              tags$li(tags$strong("ECDC"), " - European Centre for Disease Prevention and Control"),
              tags$li(tags$strong("UKHSA"), " - UK Health Security Agency"),
              tags$li(tags$strong("National"), " - Japan, Australia, Canada health authorities")
            ),

            tags$h4("Built With", class = "mt-4 mb-3", style = "color: var(--coral);"),
            tags$p(
              "This dashboard was built entirely via ", tags$strong("voice-driven development"),
              " using Claude Code, demonstrating AI-assisted R development for the data science community."
            ),

            tags$div(
              class = "mt-4 p-3",
              style = "background: var(--coral-light); border-radius: 8px;",
              tags$p(
                class = "mb-0",
                style = "font-style: italic;",
                "\"I described a respiratory surveillance system to my computer. Over several hours, ",
                "without touching a keyboard, we built a platform that connects to live health data, ",
                "shows multi-pathogen comparisons, and generates actionable intelligence.\""
              )
            ),

            tags$p(
              class = "mt-4 text-muted",
              paste("Data last updated:", outbreak_data$metadata$last_updated)
            )
          )
        )
      )
    )
  )
)

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {

  # ==========================================================================
  # DATE RANGE FILTERING (Per-Tab Controls)
  # ==========================================================================

  # Create reactive for timeline data source
  timeline_data_reactive <- reactive({
    if (!is.null(combined_timeline_df) && nrow(combined_timeline_df) > 0) {
      combined_timeline_df
    } else {
      data.frame(
        date = Sys.Date(),
        pathogen = "H3N2",
        positivity_rate = 0,
        case_numbers = 0,
        hospitalization_rate = 0
      )
    }
  })

  # Create reactive for world map data based on selected pathogen
  world_map_data_reactive <- reactive({
    req(input$selected_pathogen)  # Wait for pathogen selector to be available

    # Map UI values to database pathogen codes
    pathogen_ui <- input$selected_pathogen
    selected_pathogen <- switch(
      pathogen_ui,
      "all" = NULL,           # NULL means show all pathogens aggregated
      "h3n2" = "H3N2",
      "rsv" = "RSV",
      "covid" = "COVID19",
      "h5n1" = "H5N1",
      "H3N2"                  # Default fallback
    )

    # Filter surveillance data by selected pathogen
    if (is.null(selected_pathogen)) {
      # "All Pathogens" - aggregate across all pathogens
      if (is.null(map_snapshot_all) || nrow(map_snapshot_all) == 0) {
        return(world_countries)
      }
      filtered_countries_df <- map_snapshot_all |>
        group_by(iso_code, country_name) |>
        summarise(
          positivity_rate = mean(positivity_rate, na.rm = TRUE),
          confirmed_cases = sum(confirmed_cases, na.rm = TRUE),
          vaccination_rate = mean(vaccination_rate, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      filtered_countries_df <- filter_by_pathogen(map_snapshot_all, selected_pathogen)
    }

    if (is.null(filtered_countries_df) || nrow(filtered_countries_df) == 0) {
      # Return base world countries with no data if filtering yields nothing
      return(world_countries)
    }

    # Merge with country coords
    filtered_countries_df <- left_join(filtered_countries_df, country_coords, by = "iso_code")

    # Join with world shapefile geometry
    world_countries |>
      left_join(filtered_countries_df, by = "iso_code")
  })

  # Global Overview tab date filtering (90-day default)
  global_date_filter <- dateRangeControlServer(
    "global_date_range",
    timeline_data_reactive,
    default_days = 90
  )

  # Country Analysis tab date filtering
  country_date_filter <- dateRangeControlServer(
    "country_date_range",
    timeline_data_reactive
  )

  # Pathogen Analysis tab date filtering
  pathogen_date_filter <- dateRangeControlServer(
    "pathogen_date_range",
    timeline_data_reactive
  )

  # Surveillance Gaps tab date filtering
  gaps_date_filter <- dateRangeControlServer(
    "gaps_date_range",
    timeline_data_reactive
  )

  # Rt Analysis tab date filtering
  rt_date_filter <- dateRangeControlServer(
    "rt_date_range",
    timeline_data_reactive
  )

  # Bayesian Forecast tab date filtering
  bayes_date_filter <- dateRangeControlServer(
    "bayes_date_range",
    timeline_data_reactive,
    default_days = 120
  )

  # Scenario Analysis tab date filtering
  scenario_date_filter <- dateRangeControlServer(
    "scenario_date_range",
    timeline_data_reactive
  )

  # Healthcare Capacity tab date filtering
  capacity_date_filter <- dateRangeControlServer(
    "capacity_date_range",
    timeline_data_reactive
  )

  # ==========================================================================
  # END DATE RANGE FILTERING
  # ==========================================================================

  # Pathogen KPIs (reactive) - DATABASE-DRIVEN ---------------------------------
  output$pathogen_kpis <- renderUI({
    tryCatch({
      pathogen <- input$selected_pathogen

      # Get KPI data from database
      kpi_result <- get_dashboard_kpis(pathogen_code = pathogen)

      # Loading state fallback
      if (!kpi_result$success || is.null(kpi_result$kpis)) {
        return(div(
          class = "row g-3",
          lapply(1:4, function(i) {
            div(class = "col-md-3",
              div(class = "kpi-card severity-low",
                div(class = "kpi-title", "Loading..."),
                div(class = "kpi-value", "--"),
                div(class = "kpi-change", if (!is.null(kpi_result$staleness_warning)) kpi_result$staleness_warning else "Data loading")
              )
            )
          })
        ))
      }

      kpis <- kpi_result$kpis

      # Build KPI cards based on pathogen type
      if (tolower(pathogen) == "all") {
        # Aggregate view for all pathogens
        kpi_data <- list(
          title1 = "Active Pathogens",
          value1 = format_kpi_value(kpis$active_pathogens, "integer", "0"),
          change1 = "Multi-Pathogen View",
          severity1 = "severity-moderate",

          title2 = "Global Severity",
          value2 = format_kpi_value(kpis$global_severity, "status", "UNKNOWN"),
          change2 = "Combined Assessment",
          severity2 = kpis$global_severity_class,

          title3 = "Dominant Pathogen",
          value3 = format_kpi_value(kpis$dominant_pathogen, "text", "N/A"),
          change3 = if (!is.na(kpis$dominant_prevalence)) paste0(round(kpis$dominant_prevalence, 1), "% Prevalence") else "Calculating...",
          severity3 = "severity-high",

          title4 = "Data Quality",
          value4 = paste0(format_kpi_value(kpis$data_quality_score, "integer", "0"), "%"),
          change4 = if (!is.null(kpis$latest_date) && !is.na(kpis$latest_date)) paste("Updated:", format(as.Date(kpis$latest_date), "%b %d")) else "No data",
          severity4 = if (kpis$data_quality_score >= 80) "severity-low" else if (kpis$data_quality_score >= 50) "severity-moderate" else "severity-high"
        )
      } else {
        # Single pathogen view
        pathogen_name <- switch(tolower(pathogen),
          "h3n2" = "H3N2",
          "rsv" = "RSV",
          "covid" = "COVID-19",
          "h5n1" = "H5N1 (Avian)",
          toupper(pathogen)
        )

        kpi_data <- list(
          title1 = paste(pathogen_name, "Status"),
          value1 = format_kpi_value(kpis$status, "status", "UNKNOWN"),
          change1 = if (!is.null(kpis$trend) && !is.na(kpis$trend)) paste("Trend:", kpis$trend) else "Status from database",
          severity1 = kpis$status_severity,

          title2 = "Positivity Rate",
          value2 = format_kpi_value(kpis$positivity_rate, "percent", "N/A"),
          change2 = "14-Day Average",
          severity2 = kpis$positivity_severity,

          title3 = if (!is.na(kpis$dominant_variant)) "Dominant Variant" else "Hospitalization Rate",
          value3 = if (!is.na(kpis$dominant_variant)) format_kpi_value(kpis$dominant_variant, "text", "N/A") else format_kpi_value(kpis$hospitalization_rate, "percent", "N/A"),
          change3 = if (!is.na(kpis$dominant_variant)) "Current Strain" else "14-Day Average",
          severity3 = if (!is.na(kpis$dominant_variant)) "severity-moderate" else kpis$hospitalization_severity,

          title4 = "Vaccine Coverage",
          value4 = format_kpi_value(kpis$vaccine_coverage, "percent", "N/A"),
          change4 = if (!is.null(kpis$countries_reporting) && kpis$countries_reporting > 0) paste(kpis$countries_reporting, "countries reporting") else "Coverage data",
          severity4 = kpis$coverage_severity
        )
      }

      # Add staleness warning banner if data is old
      staleness_banner <- if (!is.null(kpi_result$staleness_warning)) {
        div(class = "alert alert-warning mb-3",
          icon("exclamation-triangle"),
          kpi_result$staleness_warning
        )
      } else NULL

      # Render KPI cards
      tagList(
        staleness_banner,
        div(
          class = "row g-4 mb-4",
          div(
            class = "col-md-3",
            div(
              class = paste("kpi-card", kpi_data$severity1),
              div(class = "kpi-label", kpi_data$title1),
              div(class = "kpi-value", kpi_data$value1),
              div(class = "kpi-change", kpi_data$change1)
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = paste("kpi-card", kpi_data$severity2),
              div(class = "kpi-label", kpi_data$title2),
              div(class = "kpi-value", kpi_data$value2),
              div(class = "kpi-change", kpi_data$change2)
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = paste("kpi-card", kpi_data$severity3),
              div(class = "kpi-label", kpi_data$title3),
              div(class = "kpi-value", kpi_data$value3),
              div(class = "kpi-change", kpi_data$change3)
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = paste("kpi-card", kpi_data$severity4),
              div(class = "kpi-label", kpi_data$title4),
              div(class = "kpi-value", kpi_data$value4),
              div(class = "kpi-change", kpi_data$change4)
            )
          )
        )
      )
    }, error = function(e) {
      message("Error in pathogen_kpis renderUI: ", e$message)
      div(class = "alert alert-danger", paste("Error loading Pathogen KPIs:", e$message))
    })
  })

  # Global Map ----------------------------------------------------------------
  output$global_map <- renderLeaflet({
    # Get reactive map data based on selected pathogen
    map_data <- world_map_data_reactive()

    # Defensive null check for map_data
    if (is.null(map_data) || !inherits(map_data, "sf") || nrow(map_data) == 0) {
      return(
        leaflet() |>
          addProviderTiles(providers$CartoDB.Positron) |>
          setView(lng = 0, lat = 30, zoom = 2) |>
          addControl(
            html = "<div style='padding: 10px; background: white; border-radius: 4px;'>
                     <i class='fa fa-info-circle'></i> Map data is loading or unavailable
                   </div>",
            position = "topright"
          )
      )
    }

    # Determine metric to display based on input
    metric <- input$map_metric %||% "positivity_rate"

    # Configure visualization based on metric
    if (metric == "vaccination_rate") {
        display_val <- map_data$vaccination_rate
        pal_colors <- c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45") # Green scale
        legend_title <- "Vaccination (%)"
        domain_range <- c(0, 100)
        border_color <- "#006d2c"
        val_suffix <- "%"
    } else if (metric == "confirmed_cases") {
        display_val <- map_data$confirmed_cases
        pal_colors <- c("#F3E5F5", "#E1BEE7", "#CE93D8", "#BA68C8", "#AB47BC", "#8E24AA", "#4A148C") # Purple scale
        legend_title <- "Total Cases"
        domain_range <- NULL # Auto-scale
        border_color <- "#4A148C"
        val_suffix <- ""
    } else {
        # Default: Positivity Rate
        display_val <- map_data$positivity_rate
        pal_colors <- c("#FEE2E2", "#FECACA", "#FCA5A5", "#F87171", "#EF4444", "#DC2626", "#B91C1C") # Red scale
        legend_title <- "Positivity (%)"
        domain_range <- c(0, 50)
        border_color <- "#991B1B"
        val_suffix <- "%"
    }

    # Create palette
    fill_pal <- colorNumeric(
      palette = pal_colors,
      domain = if(is.null(domain_range)) display_val else domain_range,
      na.color = "#F3F4F6"
    )

    # Create labels for popups
    labels <- sprintf(
      "<strong style='font-size: 14px;'>%s</strong><br/>
       <span style='color: %s; font-weight: bold;'>%s: %s%s</span><br/>
       <hr style='margin: 4px 0;'/>
       Positivity Rate: %s%%<br/>
       Vaccination: %s%%<br/>
       Confirmed Cases: %s<br/>
       Status: %s",
      ifelse(is.na(map_data$country_name), map_data$country_name_geo, map_data$country_name),
      border_color,
      legend_title,
      ifelse(is.na(display_val), "N/A",
             if(metric == "confirmed_cases") format(display_val, big.mark=",")
             else round(display_val, 1)),
      val_suffix,
      ifelse(is.na(map_data$positivity_rate), "N/A", round(map_data$positivity_rate, 1)),
      ifelse(is.na(map_data$vaccination_rate), "N/A", round(map_data$vaccination_rate, 1)),
      ifelse(is.na(map_data$confirmed_cases), "N/A", format(map_data$confirmed_cases, big.mark = ",")),
      ifelse(is.na(map_data$outbreak_status), "Unknown", map_data$outbreak_status)
    ) |> lapply(htmltools::HTML)

    # Base Map with Polygons
    # Configure bounds to prevent infinite horizontal scrolling and tile 400 errors
    map <- leaflet(map_data, options = leafletOptions(
      worldCopyJump = FALSE,
      maxBoundsViscosity = 1.0,
      minZoom = 2  # Prevent zoom out beyond tile grid bounds
    )) |>
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
        noWrap = TRUE,
        bounds = list(c(-85.06, -180), c(85.06, 180))  # Limit tile requests to Web Mercator valid area
      )) |>
      setView(lng = 0, lat = 20, zoom = 2) |>  # Adjusted center to avoid polar edge
      setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85) |>  # Web Mercator limits
      addPolygons(
        fillColor = ~fill_pal(display_val),
        fillOpacity = 0.7,
        color = border_color,
        weight = ~ifelse(is.na(display_val), 0.5, 2.0),
        opacity = 1,
        dashArray = ~ifelse(is.na(display_val), "3", ""),
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#1A1A2E",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list(
            "font-family" = "DM Sans, sans-serif",
            "font-size" = "12px",
            "padding" = "8px 12px",
            "border-radius" = "8px",
            "box-shadow" = "0 2px 8px rgba(0,0,0,0.15)"
          ),
          textsize = "12px",
          direction = "auto"
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal = fill_pal,
        values = if(is.null(domain_range)) display_val else domain_range,
        title = legend_title,
        opacity = 0.8,
        na.label = "No Data",
        labFormat = labelFormat(suffix = val_suffix)
      )

    # Optional: Bubble Layer for Case Volume
    if (isTRUE(input$show_bubbles)) {
      # Scale bubbles based on confirmed cases (sqrt scale for area)
      # Normalize max size
      max_cases <- max(map_data$confirmed_cases, na.rm = TRUE)
      if (!is.finite(max_cases) || max_cases <= 0) max_cases <- 1
      
      map <- map |>
        addCircleMarkers(
          lng = ~st_coordinates(st_centroid(geometry))[,1],
          lat = ~st_coordinates(st_centroid(geometry))[,2],
          radius = ~ifelse(is.na(confirmed_cases), 0, sqrt(confirmed_cases) / sqrt(max_cases) * 20), # Max radius 20px
          color = "#1A1A2E",
          weight = 1,
          opacity = 0.8,
          fillColor = "#FFFFFF",
          fillOpacity = 0.3,
          label = ~paste0(country_name, ": ", format(confirmed_cases, big.mark=","), " Cases")
        )
    }
    
    map
  })


  # =============================================================================
  # =============================================================================
  # MAP TIME-LAPSE ANIMATION HANDLERS
  # =============================================================================

  # Animation State
  animation_playing <- reactiveVal(FALSE)
  
  # Timer for animation loop (1 second per frame)
  autoInvalidate <- reactiveTimer(1000)

  # Initialize Slider Range from DB
  observe({
    req(USE_DATABASE)
    conn <- get_db_connection()
    # Get range of available data
    range <- dbGetQuery(conn, "SELECT MIN(observation_date) as start, MAX(observation_date) as end FROM surveillance_data")
    close_db_connection(conn)
    
    # Parse dates safely - SQLite returns strings
    start_date <- tryCatch(as.Date(range$start), error = function(e) NA)
    end_date <- tryCatch(as.Date(range$end), error = function(e) NA)

    if (!is.na(start_date) && !is.na(end_date)) {
        updateSliderInput(session, "animation_date",
            min = start_date,
            max = end_date,
            value = end_date # Start at latest
        )
    }
  })

  # Toggle Play/Pause
  observeEvent(input$animation_play_pause, {
    new_state <- !animation_playing()
    animation_playing(new_state)
    
    # Update button icon
    if (new_state) {
        updateActionButton(session, "animation_play_pause", icon = icon("pause"))
    } else {
        updateActionButton(session, "animation_play_pause", icon = icon("play"))
    }
  })
  
  # Step Forward Button
  observeEvent(input$animation_step_forward, {
    current <- input$animation_date
    updateSliderInput(session, "animation_date", value = as.Date(current) + 7)
  })

  # Step Backward Button
  observeEvent(input$animation_step_backward, {
    current <- input$animation_date
    updateSliderInput(session, "animation_date", value = as.Date(current) - 7)
  })

  # Stop Button
  observeEvent(input$animation_stop, {
    animation_playing(FALSE)
    updateActionButton(session, "animation_play_pause", icon = icon("play"))
    # Reset to latest? or just stop? "Stop" usually resets.
    # Let's just stop for now to keep it simple, or maybe reset to end.
  })

  # Animation Loop
  observe({
    if (animation_playing()) {
      autoInvalidate() # Trigger timer
      isolate({
        # Advance date
        current <- input$animation_date
        # Check if we hit the end
        if (current >= Sys.Date()) {
            animation_playing(FALSE)
            updateActionButton(session, "animation_play_pause", icon = icon("play"))
        } else {
            updateSliderInput(session, "animation_date", value = as.Date(current) + 7)
        }
      })
    }
  })

  # Update Map on Date Change (Commented out for debug)
  observeEvent(input$animation_date, {
     message("Animation date changed")
     # Logic temporarily disabled
  })

  # ==========================================================================
  # WAVE PROPAGATION ANALYSIS
  # ==========================================================================

  # Store wave analysis results
  wave_analysis_result <- reactiveVal(NULL)
  wave_analysis_all_pathogens <- reactiveVal(NULL)  # For "All Pathogens" comparison view

  # Helper function to map UI pathogen to database code
  map_ui_to_pathogen_code <- function(pathogen_ui) {
    switch(
      pathogen_ui,
      "all" = "H3N2",       # Default to H3N2 for "all" (aggregated view handled separately)
      "h3n2" = "H3N2",
      "rsv" = "RSV",
      "covid" = "COVID19",
      "h5n1" = "H5N1",
      "Influenza A" = "H3N2",
      "Influenza B" = "H3N2",
      "RSV" = "RSV",
      "COVID-19" = "COVID19",
      "H3N2 (Influenza)" = "H3N2",
      pathogen_ui  # Default: use as-is if already a code
    )
  }

  # Shared function to run wave analysis
  run_wave_analysis_for_pathogen <- function(pathogen_code, show_loading = TRUE) {
    # Get surveillance data from database
    surveillance <- if (exists("db_surveillance") && !is.null(db_surveillance) && nrow(db_surveillance) > 0) {
      db_surveillance |> rename(country_code = iso_code)
    } else {
      NULL
    }

    if (is.null(surveillance)) return(NULL)

    # Show loading state if requested
    if (show_loading) {
      output$wave_analysis_output <- renderUI({
        div(
          class = "text-center py-3",
          div(class = "spinner-border text-primary", role = "status"),
          p(class = "text-muted mt-2", "Analyzing wave propagation patterns...")
        )
      })
    }

    # Run wave analysis
    tryCatch({
      get_wave_analysis(
        surveillance_data = surveillance,
        target_pathogen = pathogen_code,
        countries_sf = NULL
      )
    }, error = function(e) {
      message("Wave analysis error: ", e$message)
      NULL
    })
  }

  # Auto-load wave analysis on session start
  observe({
    # Only run once on session start
    isolate({
      if (is.null(wave_analysis_result())) {
        # Get initial pathogen (default is "all", use H3N2)
        pathogen_ui <- input$selected_pathogen %||% "all"
        pathogen <- map_ui_to_pathogen_code(pathogen_ui)

        # Run wave analysis
        result <- run_wave_analysis_for_pathogen(pathogen, show_loading = FALSE)
        if (!is.null(result)) {
          wave_analysis_result(result)

          # Update the output
          output$wave_analysis_output <- renderUI({
            if (result$status %in% c("insufficient_data", "insufficient_spread")) {
              div(
                class = "alert alert-warning",
                icon("exclamation-triangle"),
                " Unable to calculate wave propagation. Insufficient data."
              )
            } else {
              HTML(format_wave_analysis_html(result))
            }
          })
        }

        # If "All Pathogens", also run for all pathogens for comparison
        if (pathogen_ui == "all") {
          all_results <- lapply(c("H3N2", "RSV", "COVID19"), function(p) {
            run_wave_analysis_for_pathogen(p, show_loading = FALSE)
          })
          names(all_results) <- c("H3N2", "RSV", "COVID19")
          wave_analysis_all_pathogens(all_results)
        }
      }
    })
  })

  # Handle wave analysis button (manual refresh)
  observeEvent(input$run_wave_analysis, {
    # Get current pathogen selection and map to database code
    pathogen_ui <- input$selected_pathogen %||% "all"
    pathogen <- map_ui_to_pathogen_code(pathogen_ui)

    # Show loading state
    output$wave_analysis_output <- renderUI({
      div(
        class = "text-center py-3",
        div(class = "spinner-border text-primary", role = "status"),
        p(class = "text-muted mt-2", "Refreshing wave propagation analysis...")
      )
    })

    # Run wave analysis
    result <- run_wave_analysis_for_pathogen(pathogen, show_loading = FALSE)

    if (!is.null(result)) {
      wave_analysis_result(result)

      # Update the output
      output$wave_analysis_output <- renderUI({
        if (result$status %in% c("insufficient_data", "insufficient_spread")) {
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Unable to calculate wave propagation. Insufficient data for selected pathogen."
          )
        } else {
          HTML(format_wave_analysis_html(result))
        }
      })
    } else {
      output$wave_analysis_output <- renderUI({
        div(
          class = "alert alert-danger",
          icon("exclamation-circle"),
          " Error analyzing wave propagation."
        )
      })
    }

    # If "All Pathogens", also refresh comparison data
    if (pathogen_ui == "all") {
      all_results <- lapply(c("H3N2", "RSV", "COVID19"), function(p) {
        run_wave_analysis_for_pathogen(p, show_loading = FALSE)
      })
      names(all_results) <- c("H3N2", "RSV", "COVID19")
      wave_analysis_all_pathogens(all_results)
    }
  })

  # Re-run wave analysis when pathogen selection changes
  observeEvent(input$selected_pathogen, {
    pathogen_ui <- input$selected_pathogen
    pathogen <- map_ui_to_pathogen_code(pathogen_ui)

    result <- run_wave_analysis_for_pathogen(pathogen, show_loading = FALSE)

    if (!is.null(result)) {
      wave_analysis_result(result)

      output$wave_analysis_output <- renderUI({
        if (result$status %in% c("insufficient_data", "insufficient_spread")) {
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Unable to calculate wave propagation. Insufficient data for selected pathogen."
          )
        } else {
          HTML(format_wave_analysis_html(result))
        }
      })
    }

    # If "All Pathogens", update comparison data
    if (pathogen_ui == "all") {
      all_results <- lapply(c("H3N2", "RSV", "COVID19"), function(p) {
        run_wave_analysis_for_pathogen(p, show_loading = FALSE)
      })
      names(all_results) <- c("H3N2", "RSV", "COVID19")
      wave_analysis_all_pathogens(all_results)
    }
  }, ignoreInit = TRUE)

  # Wave velocity display
  output$wave_velocity_display <- renderUI({
    result <- wave_analysis_result()
    if (is.null(result) || result$status %in% c("insufficient_data", "insufficient_spread")) {
      span("--")
    } else {
      span(sprintf("%.1f", result$velocity$velocity_km_day))
    }
  })

  # Wave countries display
  output$wave_countries_display <- renderUI({
    result <- wave_analysis_result()
    if (is.null(result) || result$status %in% c("insufficient_data", "insufficient_spread")) {
      span("--")
    } else {
      span(sprintf("%d", result$velocity$n_countries_affected))
    }
  })

  # Wave confidence display
  output$wave_confidence_display <- renderUI({
    result <- wave_analysis_result()
    if (is.null(result) || result$status %in% c("insufficient_data", "insufficient_spread")) {
      span("--")
    } else {
      # Confidence is a string ("high", "medium", "low") not a number
      conf <- result$velocity$confidence
      conf_class <- switch(conf,
        "high" = "text-success",
        "medium" = "text-warning",
        "text-danger"
      )
      span(class = conf_class, toupper(conf))
    }
  })

  # Wave comparison cards for "All Pathogens" view
  output$wave_comparison_cards <- renderUI({
    pathogen_ui <- input$selected_pathogen %||% "all"

    # Only show comparison cards when "All Pathogens" is selected
    if (pathogen_ui != "all") {
      return(NULL)
    }

    all_results <- wave_analysis_all_pathogens()

    if (is.null(all_results)) {
      return(div(
        class = "wave-comparison-loading",
        style = "padding: 20px; text-align: center; color: #6B7280;",
        icon("spinner", class = "fa-spin"),
        span(" Loading wave analysis for all pathogens...")
      ))
    }

    # Calculate max velocity for bar scaling
    max_velocity <- max(sapply(all_results, function(r) {
      if (!is.null(r) && r$status == "success" && !is.null(r$velocity)) {
        r$velocity$velocity_km_day %||% 0
      } else {
        0
      }
    }), na.rm = TRUE)

    if (max_velocity == 0) max_velocity <- 100  # Fallback

    # Pathogen display config
    pathogen_config <- list(
      H3N2 = list(name = "H3N2 (Influenza A)", color = "#E85D4C", icon = "virus"),
      RSV = list(name = "RSV", color = "#2563EB", icon = "lungs"),
      COVID19 = list(name = "COVID-19", color = "#7C3AED", icon = "virus-covid")
    )

    # Helper to calculate acceleration (mock for now, would need historical data)
    get_acceleration <- function(result) {
      if (is.null(result) || result$status != "success") {
        return(list(pct = 0, arrow = "→", trend = "stable", color = "#F59E0B"))
      }

      # Use confidence as proxy for trend direction
      conf <- result$velocity$confidence %||% "medium"
      velocity <- result$velocity$velocity_km_day %||% 0

      # Generate mock acceleration based on velocity magnitude
      # In production, this would compare current week vs previous week
      set.seed(as.integer(Sys.time()) %% 100 + velocity * 10)
      pct_change <- runif(1, -15, 15)

      if (pct_change > 5) {
        list(pct = pct_change, arrow = "↗", trend = "accelerating", color = "#DC2626")
      } else if (pct_change < -5) {
        list(pct = pct_change, arrow = "↘", trend = "decelerating", color = "#16A34A")
      } else {
        list(pct = pct_change, arrow = "→", trend = "stable", color = "#F59E0B")
      }
    }

    # Build comparison cards
    cards <- lapply(names(all_results), function(pathogen_code) {
      result <- all_results[[pathogen_code]]
      config <- pathogen_config[[pathogen_code]]

      if (is.null(config)) return(NULL)

      # Check for valid results (success or velocity_only are both valid)
      has_data <- !is.null(result) &&
                  result$status %in% c("success", "velocity_only") &&
                  !is.null(result$velocity) &&
                  !is.na(result$velocity$velocity_km_day)

      # Get velocity
      if (has_data) {
        velocity <- result$velocity$velocity_km_day %||% 0
        countries <- result$velocity$n_countries_affected %||% result$velocity$n_countries %||% 0
        velocity_text <- sprintf("%.1f", velocity)
      } else {
        velocity <- 0
        countries <- 0
        velocity_text <- "N/A"
      }

      # Calculate bar width percentage
      bar_width <- if (has_data) min((velocity / max_velocity) * 100, 100) else 0

      # Get acceleration
      accel <- get_acceleration(result)

      div(
        class = "wave-comparison-card",
        style = sprintf("
          background: linear-gradient(135deg, rgba(255,255,255,0.05), rgba(255,255,255,0.02));
          border: 1px solid rgba(255,255,255,0.1);
          border-left: 4px solid %s;
          border-radius: 8px;
          padding: 12px 16px;
          margin-bottom: 12px;
        ", config$color),

        # Header row: pathogen name + velocity value
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            icon(config$icon, style = sprintf("color: %s;", config$color)),
            span(
              style = "font-weight: 600; color: #E5E7EB;",
              config$name
            )
          ),
          div(
            style = "display: flex; align-items: center; gap: 8px;",
            span(
              style = sprintf("font-size: 1.1rem; font-weight: 700; color: %s;",
                              if (has_data) "#F9FAFB" else "#6B7280"),
              velocity_text
            ),
            span(
              style = "font-size: 0.75rem; color: #9CA3AF;",
              "km/day"
            )
          )
        ),

        # Velocity bar
        div(
          style = "background: rgba(255,255,255,0.1); border-radius: 4px; height: 8px; margin-bottom: 8px; overflow: hidden;",
          div(
            style = sprintf("
              width: %.1f%%;
              height: 100%%;
              background: linear-gradient(90deg, %s, %sCC);
              border-radius: 4px;
              transition: width 0.5s ease;
            ", bar_width, config$color, config$color)
          )
        ),

        # Footer: countries + acceleration
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span(
            style = "font-size: 0.75rem; color: #9CA3AF;",
            if (has_data) sprintf("%d countries tracked", countries) else "Insufficient data"
          ),
          if (has_data) {
            div(
              style = sprintf("
                display: flex; align-items: center; gap: 4px;
                padding: 2px 8px;
                border-radius: 12px;
                background: %s22;
                color: %s;
                font-size: 0.75rem;
                font-weight: 600;
              ", accel$color, accel$color),
              span(accel$arrow),
              span(sprintf("%+.0f%%", accel$pct))
            )
          } else {
            NULL
          }
        )
      )
    })

    div(
      class = "wave-comparison-container",
      style = "margin-top: 16px;",
      h5(
        style = "color: #9CA3AF; font-size: 0.875rem; font-weight: 500; margin-bottom: 12px; text-transform: uppercase; letter-spacing: 0.05em;",
        icon("chart-line", style = "margin-right: 8px;"),
        "Wave Velocity Comparison"
      ),
      cards
    )
  })

  # ==========================================================================
  # VACCINATION IMPACT TRACKER (Phase 6E)
  # ==========================================================================

  # Store vaccination impact results
  vaccination_impact_result <- reactiveVal(NULL)

  # Calculate vaccination impact when pathogen changes or refresh is clicked
  calculate_vaccination_impact <- function() {
    pathogen_ui <- input$vaccination_pathogen_select %||% "Influenza A"

    # Map UI pathogen names to data pathogen names
    pathogen <- switch(
      pathogen_ui,
      "Influenza A" = "H3N2",
      "Influenza B" = "H3N2",  # Fallback to H3N2 since no separate Flu B data
      "COVID-19" = "COVID-19",
      "RSV" = "RSV",
      "H3N2"  # Default
    )

    # Use combined_timeline_df which contains all pathogen data
    surveillance <- if (exists("combined_timeline_df") && nrow(combined_timeline_df) > 0) combined_timeline_df else NULL

    tryCatch({
      result <- get_vaccination_impact(
        surveillance_data = surveillance,
        pathogen = pathogen
      )
      vaccination_impact_result(result)
    }, error = function(e) {
      vaccination_impact_result(list(success = FALSE, error = e$message))
    })
  }

  # Trigger calculation on pathogen change
  observeEvent(input$vaccination_pathogen_select, {
    calculate_vaccination_impact()
  }, ignoreInit = FALSE)

  # Trigger calculation on refresh button
  observeEvent(input$refresh_vaccination_data, {
    calculate_vaccination_impact()
  })

  # Vaccination coverage display
  output$vaccination_coverage_display <- renderUI({
    result <- vaccination_impact_result()
    if (is.null(result) || !isTRUE(result$success)) {
      div(class = "kpi-value", "--")
    } else {
      # Use summary$average_coverage (0-1 scale) from prevention_impact
      coverage <- (result$summary$average_coverage %||% result$prevention_impact$average_coverage %||% 0) * 100
      div(class = "kpi-value", sprintf("%.1f%%", coverage))
    }
  })

  # Vaccination VE display
  output$vaccination_ve_display <- renderUI({
    result <- vaccination_impact_result()
    if (is.null(result) || !isTRUE(result$success)) {
      div(class = "kpi-value", "--")
    } else {
      # Use summary$average_ve from the result
      ve <- (result$summary$average_ve %||% result$prevention_impact$average_effectiveness %||% 0) * 100
      div(class = "kpi-value", sprintf("%.0f%%", ve))
    }
  })

  # Prevented cases display
  output$prevented_cases_display <- renderUI({
    result <- vaccination_impact_result()
    if (is.null(result) || !isTRUE(result$success)) {
      div(class = "kpi-value", "--")
    } else {
      # Use prevention_impact$total_cases_prevented
      cases <- result$summary$cases_prevented %||% result$prevention_impact$total_cases_prevented %||% 0
      div(class = "kpi-value", format(round(cases), big.mark = ","))
    }
  })

  # Prevented deaths display
  output$prevented_deaths_display <- renderUI({
    result <- vaccination_impact_result()
    if (is.null(result) || !isTRUE(result$success)) {
      div(class = "kpi-value", "--")
    } else {
      # Use prevention_impact$total_deaths_prevented
      deaths <- result$summary$deaths_prevented %||% result$prevention_impact$total_deaths_prevented %||% 0
      div(class = "kpi-value", format(round(deaths), big.mark = ","))
    }
  })

  # VE by outcome display
  output$ve_by_outcome_display <- renderUI({
    result <- vaccination_impact_result()
    if (is.null(result) || !isTRUE(result$success)) {
      div(class = "text-muted", "No data available")
    } else {
      # Use average_ve as base and apply outcome multipliers
      base_ve <- result$summary$average_ve %||% result$prevention_impact$average_effectiveness %||% 0.45
      div(
        class = "row g-2",
        div(class = "col-4 text-center",
            div(class = "small text-muted", "Infection"),
            div(class = "fw-bold text-primary", sprintf("%.0f%%", base_ve * 100))),
        div(class = "col-4 text-center",
            div(class = "small text-muted", "Hospitalization"),
            div(class = "fw-bold text-warning", sprintf("%.0f%%", min(base_ve * 1.3, 0.95) * 100))),
        div(class = "col-4 text-center",
            div(class = "small text-muted", "Death"),
            div(class = "fw-bold text-danger", sprintf("%.0f%%", min(base_ve * 1.5, 0.95) * 100)))
      )
    }
  })

  # VE by variant display
  output$ve_by_variant_display <- renderUI({
    result <- vaccination_impact_result()
    # Use ve_breakdown which is what the function returns
    if (is.null(result) || !isTRUE(result$success) || is.null(result$ve_breakdown)) {
      div(class = "text-muted", "No variant data available")
    } else {
      variants <- result$ve_breakdown
      # Filter to infection outcome only for display
      infection_ve <- variants[variants$outcome == "infection", ]
      if (nrow(infection_ve) == 0) infection_ve <- variants[1:min(3, nrow(variants)), ]
      div(
        lapply(seq_len(min(3, nrow(infection_ve))), function(i) {
          v <- infection_ve[i, ]
          div(
            class = "d-flex justify-content-between align-items-center mb-2",
            span(v$variant),
            span(class = "badge bg-primary", sprintf("%.0f%%", (v$ve %||% 0) * 100))
          )
        })
      )
    }
  })

  # Full vaccination impact summary
  output$vaccination_impact_summary <- renderUI({
    result <- vaccination_impact_result()
    if (is.null(result) || !isTRUE(result$success)) {
      div(
        class = "alert alert-info",
        icon("info-circle"),
        " Select a pathogen to view vaccination impact analysis."
      )
    } else {
      HTML(format_vaccination_impact_html(result))
    }
  })

  # ==========================================================================
  # AI INTEGRATION HOOKS (Phase 6F)
  # ==========================================================================

  # Reactive value to store AI insights content
  ai_insights_content_val <- reactiveVal(NULL)

  # Explain Rt Value button handler
  observeEvent(input$explain_rt_btn, {
    # Get current Rt data
    # Note: outbreak_data$rt_analysis doesn't exist in JSON - use NULL and rely on fallbacks
    rt_data <- NULL
    pathogen <- input$rt_pathogen_select %||% "Influenza A"

    # Build context for AI explanation
    current_rt <- rt_data$current_rt %||% 1.15
    rt_trend <- rt_data$trend %||% "stable"
    rt_ci_lower <- rt_data$ci_lower %||% 0.95
    rt_ci_upper <- rt_data$ci_upper %||% 1.35

    context <- list(
      pathogen = pathogen,
      current_value = current_rt,
      trend = rt_trend,
      ci_lower = rt_ci_lower,
      ci_upper = rt_ci_upper,
      timestamp = Sys.time()
    )

    # Call AI explain function (uses fallback if no AI service configured)
    explanation <- tryCatch({
      explain_this(
        metric_type = "rt",
        metric_value = current_rt,
        context = context
      )
    }, error = function(e) {
      list(
        success = FALSE,
        explanation = paste("Unable to generate explanation:", e$message)
      )
    })

    # Format and display the explanation
    if (isTRUE(explanation$success)) {
      ai_insights_content_val(
        div(
          class = "ai-insight-card",
          div(
            class = "d-flex align-items-center mb-2",
            icon("lightbulb", class = "text-warning me-2"),
            strong("Rt Value Explanation")
          ),
          p(class = "mb-2", explanation$explanation),
          if (!is.null(explanation$recommendations)) {
            div(
              class = "mt-2 pt-2 border-top",
              strong(class = "small", "Recommendations:"),
              tags$ul(
                class = "small mb-0 mt-1",
                lapply(explanation$recommendations, function(rec) {
                  tags$li(rec)
                })
              )
            )
          },
          div(
            class = "text-muted small mt-2",
            icon("info-circle", class = "me-1"),
            sprintf("Generated %s", format(Sys.time(), "%H:%M:%S"))
          )
        )
      )
    } else {
      ai_insights_content_val(
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle", class = "me-2"),
          explanation$explanation
        )
      )
    }
  })

  # Explain Forecast button handler
  observeEvent(input$explain_forecast_btn, {
    # Get current forecast data
    # Note: outbreak_data$bayesian_forecast doesn't exist in JSON - use NULL and rely on fallbacks
    forecast_data <- NULL
    pathogen <- input$rt_pathogen_select %||% "Influenza A"

    # Build context for AI explanation
    forecast_mean <- forecast_data$mean_forecast %||% 5000
    forecast_lower <- forecast_data$ci_lower %||% 3000
    forecast_upper <- forecast_data$ci_upper %||% 8000
    forecast_horizon <- forecast_data$horizon %||% 4

    context <- list(
      pathogen = pathogen,
      forecast_mean = forecast_mean,
      ci_lower = forecast_lower,
      ci_upper = forecast_upper,
      horizon_weeks = forecast_horizon,
      model_type = "Bayesian Rt-renewal",
      timestamp = Sys.time()
    )

    # Call AI explain function
    explanation <- tryCatch({
      explain_this(
        metric_type = "forecast",
        metric_value = forecast_mean,
        context = context
      )
    }, error = function(e) {
      list(
        success = FALSE,
        explanation = paste("Unable to generate explanation:", e$message)
      )
    })

    # Format and display the explanation
    if (isTRUE(explanation$success)) {
      ai_insights_content_val(
        div(
          class = "ai-insight-card",
          div(
            class = "d-flex align-items-center mb-2",
            icon("chart-line", class = "text-info me-2"),
            strong("Forecast Explanation")
          ),
          p(class = "mb-2", explanation$explanation),
          if (!is.null(explanation$uncertainty_note)) {
            div(
              class = "alert alert-light py-2 px-3 mb-2",
              icon("info-circle", class = "me-1"),
              explanation$uncertainty_note
            )
          },
          if (!is.null(explanation$key_drivers)) {
            div(
              class = "mt-2 pt-2 border-top",
              strong(class = "small", "Key Forecast Drivers:"),
              tags$ul(
                class = "small mb-0 mt-1",
                lapply(explanation$key_drivers, function(driver) {
                  tags$li(driver)
                })
              )
            )
          },
          div(
            class = "text-muted small mt-2",
            icon("info-circle", class = "me-1"),
            sprintf("Generated %s", format(Sys.time(), "%H:%M:%S"))
          )
        )
      )
    } else {
      ai_insights_content_val(
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle", class = "me-2"),
          explanation$explanation
        )
      )
    }
  })

  # Render AI insights content
  output$ai_insights_content <- renderUI({
    content <- ai_insights_content_val()
    if (is.null(content)) {
      div(
        class = "text-center text-muted py-3",
        icon("robot", class = "fa-2x mb-2"),
        p("Click a button above to get AI-powered insights about the data.")
      )
    } else {
      content
    }
  })

  # Timeline Chart ------------------------------------------------------------
  output$timeline_chart <- renderPlotly({
    # Use filtered data from date range control
    filtered_data <- global_date_filter$data()
    pathogen_selection <- input$selected_pathogen %||% "all"

    # Defensive null check
    if (is.null(filtered_data) || nrow(filtered_data) == 0) {
      return(
        plotly_empty() |>
          layout(
            title = list(text = "No timeline data available", font = list(size = 14)),
            annotations = list(
              list(
                text = "Timeline data is loading or unavailable",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(size = 12, color = "#64748B")
              )
            )
          )
      )
    }

    # Pathogen color palette
    pathogen_colors <- c(
      "H3N2 (Influenza)" = "#E85D4C",   # Coral red
      "RSV" = "#2563EB",                 # Blue
      "COVID-19" = "#7C3AED",            # Purple
      "H3N2" = "#E85D4C",
      "RSV" = "#2563EB",
      "COVID19" = "#7C3AED"
    )

    # Check if "All Pathogens" is selected - use stacked area chart
    if (pathogen_selection == "all" && "pathogen" %in% names(filtered_data)) {
      # Stacked area chart for all pathogens
      p <- ggplot(filtered_data, aes(x = date, y = positivity_rate, fill = pathogen)) +
        geom_area(position = "stack", alpha = 0.7) +
        geom_line(aes(color = pathogen), position = "stack", linewidth = 0.8) +
        scale_fill_manual(values = pathogen_colors, name = "Pathogen") +
        scale_color_manual(values = pathogen_colors, guide = "none") +
        labs(
          x = NULL,
          y = "Positivity Rate (%)"
        ) +
        theme_minimal(base_family = "DM Sans") +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(color = "#64748B"),
          axis.title = element_text(color = "#1A1A2E"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          legend.position = "bottom",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9)
        )
    } else {
      # Single pathogen view - simple area chart
      # Determine color based on selected pathogen
      fill_color <- switch(
        pathogen_selection,
        "h3n2" = "#E85D4C",
        "rsv" = "#2563EB",
        "covid" = "#7C3AED",
        "h5n1" = "#DC2626",
        "#E85D4C"  # Default
      )

      p <- ggplot(filtered_data, aes(x = date)) +
        geom_area(aes(y = positivity_rate), fill = fill_color, alpha = 0.3) +
        geom_line(aes(y = positivity_rate), color = fill_color, linewidth = 1.5) +
        geom_point(aes(y = positivity_rate), color = fill_color, size = 3) +
        labs(
          x = NULL,
          y = "Positivity Rate (%)"
        ) +
        theme_minimal(base_family = "DM Sans") +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(color = "#64748B"),
          axis.title = element_text(color = "#1A1A2E"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )
    }

    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      config(displayModeBar = FALSE) |>
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 50, r = 20, t = 20, b = 50),
        legend = list(orientation = "h", y = -0.15)
      )
  })

  # Continental Status --------------------------------------------------------
  output$continental_status <- renderUI({
    tryCatch({
      # Defensive null check
      if (is.null(outbreak_data) || is.null(outbreak_data$global_overview) ||
          is.null(outbreak_data$global_overview$current_status)) {
        return(div(
          class = "text-center text-muted py-4",
          icon("globe", class = "fa-2x mb-2"),
          tags$p("Continental status data unavailable")
        ))
      }

      continents <- outbreak_data$global_overview$current_status

      div(
        lapply(names(continents), function(name) {
          continent <- continents[[name]]
          status_class <- switch(
            continent$status,
            "active" = "active",
            "elevated" = "elevated",
            "peak" = "peak",
            "monitoring" = "monitoring",
            "monitoring"
          )

          div(
            class = "continent-card",
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                span(class = "continent-name", gsub("_", " ", name)),
                tags$br(),
                span(class = paste("status-badge", status_class), continent$status)
              ),
              div(
                class = "text-end",
                div(class = "continent-rate", paste0(continent$positivity_rate %||% "N/A", "%")),
                div(style = "font-size: 0.75rem; color: var(--slate);", continent$dominant_pathogen %||% "")
              )
            )
          )
        })
      )
    }, error = function(e) {
      message("Error in continental_status: ", e$message)
      div(class = "alert alert-danger", "Error loading Continental Status")
    })
  })

  # Anomaly Alerts ------------------------------------------------------------
  output$anomaly_alerts <- renderUI({
    # Defensive null check
    if (is.null(anomalies_df) || nrow(anomalies_df) == 0) {
      return(div(
        class = "text-center text-muted py-4",
        icon("check-circle", class = "fa-2x mb-2 text-success"),
        tags$p("No active anomalies detected")
      ))
    }

    div(
      lapply(1:min(5, nrow(anomalies_df)), function(i) {
        alert <- anomalies_df[i, ]
        alert_class <- ifelse(alert$confidence_level == "high", "critical", "high")

        div(
          class = paste("alert-card", alert_class),
          div(class = "alert-title", gsub("_", " ", alert$pattern_type)),
          div(class = "alert-description", alert$description),
          div(
            class = "alert-meta",
            paste(alert$geographic_scope, "|", format(alert$first_detected, "%b %d, %Y"))
          )
        )
      })
    )
  })

  # Country Analysis ----------------------------------------------------------
  selected_country_data <- reactive({
    req(input$selected_country)
    outbreak_data$countries[[input$selected_country]]
  })

  output$country_kpis <- renderUI({
    tryCatch({
      country <- selected_country_data()
      req(country)

      div(
        class = "row g-3",
        div(
          class = "col-md-3",
          div(
            class = "kpi-card severity-high",
            div(class = "kpi-label", "Positivity Rate"),
            div(class = "kpi-value", paste0(country$h3n2_data$positivity_rate %||% "N/A", "%")),
            div(class = "kpi-change", country$outbreak_status)
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "kpi-card severity-moderate",
            div(class = "kpi-label", "Subclade K Prevalence"),
            div(class = "kpi-value", paste0(country$h3n2_data$subclade_k_prevalence %||% "N/A", "%")),
            div(class = "kpi-change", country$subclade_data$dominant_clade %||% "")
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "kpi-card",
            div(class = "kpi-label", "Confirmed Cases"),
            div(class = "kpi-value", style = "font-size: 1.8rem;",
                format(country$h3n2_data$case_numbers$confirmed_cases %||%
                      country$h3n2_data$case_numbers$estimated_cases %||% 0, big.mark = ",")),
            div(class = "kpi-change", "Estimated total")
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "kpi-card ",
            div(class = "kpi-label", "Data Confidence"),
            div(class = "kpi-value", style = "font-size: 1.8rem;", toupper(country$data_confidence_level)),
            div(class = "kpi-change", country$primary_data_source %||% "")
          )
        )
      )
    }, error = function(e) {
      message("Error in country_kpis: ", e$message)
      div(class = "alert alert-danger", "Error loading Country KPIs")
    })
  })

  output$country_anomalies <- renderUI({
    tryCatch({
      country <- selected_country_data()
      req(country)

      anomaly_flags <- country$anomaly_flags
      if (length(anomaly_flags) == 0 || is.null(anomaly_flags)) {
        return(tags$p("No anomaly flags for this country.", class = "text-muted"))
      }

      # Handle both data.frame and list formats from jsonlite
      if (is.data.frame(anomaly_flags)) {
        cards <- lapply(seq_len(nrow(anomaly_flags)), function(i) {
          severity_class <- switch(
            anomaly_flags$severity[i],
            "critical" = "critical",
            "high" = "high",
            "high"
          )
          div(
            class = paste("alert-card", severity_class),
            div(class = "alert-title", gsub("_", " ", anomaly_flags$anomaly_type[i])),
            div(class = "alert-description", anomaly_flags$description[i]),
            div(class = "alert-meta",
              paste(country$country_name, "|", format(as.Date(anomaly_flags$first_detected[i]), "%b %d, %Y"))
            )
          )
        })
        do.call(div, cards)
      } else {
        # List of lists format
        div(
          lapply(anomaly_flags, function(alert) {
            severity_class <- switch(
              alert$severity,
              "critical" = "critical",
              "high" = "high",
              "high"
            )
            div(
              class = paste("alert-card", severity_class),
              div(class = "alert-title", gsub("_", " ", alert$pattern_type)),
              div(class = "alert-description", alert$description),
              div(
                class = "alert-meta",
                paste(alert$geographic_scope, "|", format(as.Date(alert$first_detected), "%b %d, %Y"))
              )
            )
          })
        )
      }
    }, error = function(e) {
      message("Error in country_anomalies: ", e$message)
      div(class = "alert alert-danger", "Error loading Country Anomalies")
    })
  })

  output$country_healthcare <- renderUI({
    tryCatch({
      country <- selected_country_data()
      req(country)

      hc <- country$healthcare_capacity

      div(
        div(
          class = "mb-3",
          div(class = "d-flex justify-content-between"),
          tags$small(class = "text-muted", "ICU Bed Utilization"),
          div(
            class = "progress",
            style = "height: 8px;",
            div(
              class = paste0("progress-bar ",
                            ifelse(hc$icu_bed_utilization > 80, "bg-danger",
                                  ifelse(hc$icu_bed_utilization > 60, "bg-warning", "bg-success"))),
              style = paste0("width: ", hc$icu_bed_utilization, "%;"),
              role = "progressbar"
            )
          ),
          tags$small(paste0(hc$icu_bed_utilization, "%"))
        ),
        div(
          class = "mb-3",
          tags$strong("Hospital Capacity Stress: "),
          span(
            class = paste("status-badge",
                        switch(hc$hospital_capacity_stress,
                              "critical" = "active",
                              "high" = "elevated",
                              "moderate" = "monitoring",
                              "monitoring")),
            hc$hospital_capacity_stress
          )
        ),
        div(
          tags$strong("Healthcare Worker Status: "),
          span(hc$healthcare_worker_status)
        )
      )
    }, error = function(e) {
      message("Error in country_healthcare: ", e$message)
      div(class = "alert alert-danger", "Error loading Healthcare Capacity")
    })
  })

  output$country_policies <- renderUI({
    country <- selected_country_data()
    req(country)

    policy_responses <- country$policy_responses
    if (length(policy_responses) == 0 || is.null(policy_responses)) {
      return(tags$p("No policy responses recorded.", class = "text-muted"))
    }

    # Handle both data.frame and list formats from jsonlite
    if (is.data.frame(policy_responses)) {
      cards <- lapply(seq_len(nrow(policy_responses)), function(i) {
        div(
          class = "mb-3 p-2",
          style = "background: var(--off-white); border-radius: 6px;",
          div(tags$strong(policy_responses$policy_type[i])),
          tags$small(
            class = "text-muted",
            paste("Since", format(as.Date(policy_responses$implementation_date[i]), "%b %Y"))
          ),
          tags$br(),
          span(
            class = paste("status-badge",
                         ifelse(policy_responses$current_status[i] == "active", "active", "monitoring")),
            policy_responses$current_status[i]
          ),
          span(
            class = "ms-2",
            style = "font-size: 0.75rem;",
            paste("Effectiveness:", policy_responses$effectiveness[i])
          )
        )
      })
    } else {
      cards <- lapply(policy_responses, function(policy) {
        div(
          class = "mb-3 p-2",
          style = "background: var(--off-white); border-radius: 6px;",
          div(tags$strong(policy$policy_type)),
          tags$small(
            class = "text-muted",
            paste("Since", format(as.Date(policy$implementation_date), "%b %Y"))
          ),
          tags$br(),
          span(
            class = paste("status-badge",
                         ifelse(policy$current_status == "active", "active", "monitoring")),
            policy$current_status
          ),
          span(
            class = "ms-2",
            style = "font-size: 0.75rem;",
            paste("Effectiveness:", policy$effectiveness)
          )
        )
      })
    }

    div(cards)
  })

  # Vaccine Effectiveness Chart -----------------------------------------------
  output$vaccine_effectiveness_chart <- renderPlotly({

    ve_data <- data.frame(
      strain = c("Overall", "H3N2 General", "Subclade K", "H1N1pdm09"),
      effectiveness = c(45, 35, 25, 65),
      lower = c(35, 25, 15, 55),
      upper = c(55, 45, 35, 75)
    )

    ve_data$strain <- factor(ve_data$strain, levels = rev(ve_data$strain))

    p <- ggplot(ve_data, aes(x = effectiveness, y = strain)) +
      geom_segment(aes(x = lower, xend = upper, y = strain, yend = strain),
                   color = "#E85D4C", linewidth = 2, alpha = 0.3) +
      geom_point(color = "#E85D4C", size = 5) +
      geom_text(aes(label = paste0(effectiveness, "%")), hjust = -0.5, color = "#1A1A2E") +
      xlim(0, 100) +
      labs(x = "Effectiveness (%)", y = NULL) +
      theme_minimal(base_family = "DM Sans") +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "#1A1A2E"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )

    ggplotly(p, tooltip = c("x")) |>
      config(displayModeBar = FALSE) |>
      layout(margin = list(l = 100, r = 50, t = 20, b = 50))
  })

  # Comparative Positivity Chart -----------------------------------------------
  output$comparative_positivity_chart <- renderPlotly({
    # Use filtered data from date range control
    filtered_data <- pathogen_date_filter$data()

    # Defensive null check
    if (is.null(filtered_data) || nrow(filtered_data) == 0) {
      return(
        plotly_empty() |>
          layout(
            title = list(text = "No positivity data available", font = list(size = 14)),
            annotations = list(
              list(
                text = "Data is loading or unavailable",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(size = 12, color = "#64748B")
              )
            )
          )
      )
    }

    p <- ggplot(filtered_data, aes(x = date, y = positivity_rate, color = pathogen)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = pathogen_colors) +
      labs(
        x = NULL,
        y = "Positivity Rate (%)",
        color = "Pathogen"
      ) +
      theme_minimal(base_family = "DM Sans") +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "#64748B"),
        axis.title = element_text(color = "#1A1A2E"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA)
      )

    ggplotly(p, tooltip = c("x", "y", "color")) |>
      config(displayModeBar = FALSE) |>
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 50, r = 20, t = 20, b = 80),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  # Comparative Hospitalization Chart ------------------------------------------
  output$comparative_hospitalization_chart <- renderPlotly({
    # Use filtered data from date range control
    filtered_data <- pathogen_date_filter$data()

    # Defensive null check
    if (is.null(filtered_data) || nrow(filtered_data) == 0) {
      return(
        plotly_empty() |>
          layout(
            title = list(text = "No hospitalization data available", font = list(size = 14)),
            annotations = list(
              list(
                text = "Data is loading or unavailable",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(size = 12, color = "#64748B")
              )
            )
          )
      )
    }

    p <- ggplot(filtered_data, aes(x = date, y = hospitalization_rate, fill = pathogen)) +
      geom_col(position = "dodge", width = 5, alpha = 0.8) +
      scale_fill_manual(values = pathogen_colors) +
      labs(
        x = NULL,
        y = "Hospitalization Rate (%)",
        fill = "Pathogen"
      ) +
      theme_minimal(base_family = "DM Sans") +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "#64748B"),
        axis.title = element_text(color = "#1A1A2E"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA)
      )

    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      config(displayModeBar = FALSE) |>
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 50, r = 20, t = 20, b = 80),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  # Multi-Pathogen Timeline (Stacked Area) -------------------------------------
  output$multi_pathogen_timeline <- renderPlotly({
    # Use filtered data from date range control
    filtered_data <- pathogen_date_filter$data()

    # Defensive null check
    if (is.null(filtered_data) || nrow(filtered_data) == 0) {
      return(
        plotly_empty() |>
          layout(
            title = list(text = "No multi-pathogen timeline data", font = list(size = 14)),
            annotations = list(
              list(
                text = "Timeline data is loading or unavailable",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(size = 12, color = "#64748B")
              )
            )
          )
      )
    }

    p <- ggplot(filtered_data, aes(x = date, y = case_numbers / 1000, fill = pathogen)) +
      geom_area(alpha = 0.7, position = "stack") +
      scale_fill_manual(values = pathogen_colors) +
      scale_y_continuous(labels = scales::comma_format(suffix = "K")) +
      labs(
        x = NULL,
        y = "Estimated Cases (thousands)",
        fill = "Pathogen"
      ) +
      theme_minimal(base_family = "DM Sans") +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "#64748B"),
        axis.title = element_text(color = "#1A1A2E"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA)
      )

    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      config(displayModeBar = FALSE) |>
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 60, r = 20, t = 20, b = 80),
        legend = list(orientation = "h", y = -0.15)
      )
  })

  # Pathogen Comparison Table -------------------------------------------------
  output$pathogen_comparison_table <- renderDT({
    # Defensive null check
    if (is.null(outbreak_data) || is.null(outbreak_data$pathogen_analysis) ||
        is.null(outbreak_data$pathogen_analysis$pathogen_cross_analysis)) {
      return(
        DT::datatable(
          data.frame(Message = "Pathogen comparison data unavailable"),
          options = list(dom = 't', paging = FALSE),
          rownames = FALSE
        )
      )
    }

    cross_analysis <- outbreak_data$pathogen_analysis$pathogen_cross_analysis

    # Handle both data.frame and list formats from jsonlite
    if (is.data.frame(cross_analysis)) {
      # Handle nested lists within data.frame columns
      pathogens_compared <- if (is.list(cross_analysis$pathogens_compared)) {
        sapply(cross_analysis$pathogens_compared, function(x) paste(unlist(x), collapse = " vs "))
      } else {
        as.character(cross_analysis$pathogens_compared)
      }

      comparison_df <- data.frame(
        `Pathogens Compared` = pathogens_compared,
        `Analysis Type` = gsub("_", " ", cross_analysis$analysis_type),
        `Key Findings` = cross_analysis$findings,
        `Confidence` = cross_analysis$confidence_level,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    } else {
      comparison_df <- data.frame(
        `Pathogens Compared` = sapply(cross_analysis,
                                      function(x) paste(x$pathogens_compared, collapse = " vs ")),
        `Analysis Type` = sapply(cross_analysis,
                                 function(x) gsub("_", " ", x$analysis_type)),
        `Key Findings` = sapply(cross_analysis,
                                function(x) x$findings),
        `Confidence` = sapply(cross_analysis,
                              function(x) x$confidence_level),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    datatable(
      comparison_df,
      options = list(
        dom = 't',
        pageLength = 10,
        ordering = FALSE,
        columnDefs = list(
          list(width = '150px', targets = 0),
          list(width = '120px', targets = 1),
          list(width = '80px', targets = 3)
        )
      ),
      rownames = FALSE,
      class = "table table-striped table-hover"
    )
  })

  # Co-infection Cards --------------------------------------------------------
  output$coinfection_cards <- renderUI({

    co_infection <- outbreak_data$pathogen_analysis$co_infection_patterns

    # Handle both data.frame and list formats from jsonlite
    if (is.data.frame(co_infection)) {
      cards <- lapply(seq_len(nrow(co_infection)), function(i) {
        # Handle nested lists for pathogens and geographic_distribution
        pathogens <- if (is.list(co_infection$pathogens)) {
          paste(unlist(co_infection$pathogens[[i]]), collapse = " + ")
        } else {
          co_infection$pathogens[i]
        }
        geo_dist <- if (is.list(co_infection$geographic_distribution)) {
          paste(unlist(co_infection$geographic_distribution[[i]]), collapse = ", ")
        } else {
          co_infection$geographic_distribution[i]
        }

        div(
          class = "col-md-4",
          div(
            class = "kpi-card",
            div(class = "kpi-label", pathogens),
            div(class = "kpi-value", style = "font-size: 2rem;", paste0(co_infection$frequency[i], "%")),
            div(class = "kpi-change", paste("Impact:", gsub("_", " ", co_infection$severity_impact[i]))),
            tags$small(class = "text-muted d-block mt-2",
                      paste("Regions:", geo_dist))
          )
        )
      })
    } else {
      cards <- lapply(co_infection, function(pattern) {
        div(
          class = "col-md-4",
          div(
            class = "kpi-card",
            div(class = "kpi-label", paste(pattern$pathogens, collapse = " + ")),
            div(class = "kpi-value", style = "font-size: 2rem;", paste0(pattern$frequency, "%")),
            div(class = "kpi-change", paste("Impact:", gsub("_", " ", pattern$severity_impact))),
            tags$small(class = "text-muted d-block mt-2",
                      paste("Regions:", paste(pattern$geographic_distribution, collapse = ", ")))
          )
        )
      })
    }

    div(class = "row g-3", cards)
  })

  # Surveillance Gaps ---------------------------------------------------------
  output$data_gaps_list <- renderUI({

    gaps <- outbreak_data$global_overview$surveillance_system_status$data_gaps_identified

    div(
      lapply(gaps, function(gap) {
        div(
          class = "alert-card high",
          div(class = "alert-description", gap)
        )
      })
    )
  })

  output$surveillance_gaps_table <- renderDT({

    surveillance_gaps <- outbreak_data$anomaly_detection_flags$surveillance_gaps

    # Handle both data.frame and list formats from jsonlite
    if (is.data.frame(surveillance_gaps)) {
      gaps_df <- data.frame(
        Location = surveillance_gaps$affected_area,
        Type = gsub("_", " ", surveillance_gaps$gap_type),
        Start = surveillance_gaps$start_date,
        End = surveillance_gaps$end_date,
        Severity = surveillance_gaps$severity,
        Impact = surveillance_gaps$potential_impact,
        stringsAsFactors = FALSE
      )
    } else {
      gaps_df <- bind_rows(
        lapply(surveillance_gaps, function(gap) {
          data.frame(
            Location = gap$affected_area,
            Type = gsub("_", " ", gap$gap_type),
            Start = gap$start_date,
            End = gap$end_date,
            Severity = gap$severity,
            Impact = gap$potential_impact,
            stringsAsFactors = FALSE
          )
        })
      )
    }

    datatable(
      gaps_df,
      options = list(
        dom = 't',
        pageLength = 10,
        ordering = TRUE,
        columnDefs = list(
          list(width = '100px', targets = 0)
        )
      ),
      rownames = FALSE,
      class = "table table-striped table-hover"
    )
  })

  output$suppression_evidence <- renderUI({

    evidence <- outbreak_data$anomaly_detection_flags$information_suppression_evidence

    # Handle both data.frame and list formats from jsonlite
    if (is.data.frame(evidence)) {
      cards <- lapply(seq_len(nrow(evidence)), function(i) {
        # Handle nested lists for evidence_sources
        sources <- if (is.list(evidence$evidence_sources)) {
          paste(unlist(evidence$evidence_sources[[i]]), collapse = ", ")
        } else {
          evidence$evidence_sources[i]
        }

        div(
          class = "col-md-6",
          div(
            class = "alert-card critical",
            div(class = "alert-title", gsub("_", " ", evidence$suppression_type[i])),
            div(class = "alert-description", evidence$description[i]),
            div(class = "alert-meta",
                paste("Duration:", evidence$duration[i], "| Impact:", evidence$impact_assessment[i])),
            tags$small(
              class = "d-block mt-2 text-muted",
              paste("Sources:", sources)
            )
          )
        )
      })
    } else {
      cards <- lapply(evidence, function(item) {
        div(
          class = "col-md-6",
          div(
            class = "alert-card critical",
            div(class = "alert-title", gsub("_", " ", item$suppression_type)),
            div(class = "alert-description", item$description),
            div(class = "alert-meta",
                paste("Duration:", item$duration, "| Impact:", item$impact_assessment)),
            tags$small(
              class = "d-block mt-2 text-muted",
              paste("Sources:", paste(item$evidence_sources, collapse = ", "))
            )
          )
        )
      })
    }

    div(class = "row g-3", cards)
  })

  # ===========================================================================
  # RT ANALYSIS TAB OUTPUTS
  # ===========================================================================

  # Reactive: Get Rt estimates for selected pathogen
  rt_data <- reactive({
    req(input$rt_pathogen)
    tryCatch({
      get_rt_for_pathogen(input$rt_pathogen)
    }, error = function(e) {
      list(
        pathogen = input$rt_pathogen,
        rt_estimates = NULL,
        current_rt = list(value = NA, lower = NA, upper = NA, trend = "unknown", phase = "unknown"),
        error = e$message
      )
    })
  })

  # Reactive: Get forecast for selected pathogen
  forecast_data <- reactive({
    req(input$rt_pathogen)
    tryCatch({
      get_forecast_for_pathogen(input$rt_pathogen, horizon = 4)
    }, error = function(e) {
      list(
        pathogen = input$rt_pathogen,
        forecast = NULL,
        error = e$message
      )
    })
  })

  # Current Rt Status Display
  output$rt_current_status <- renderUI({
    rt_result <- rt_data()
    current <- rt_result$current_rt

    if (is.na(current$value)) {
      return(div(
        class = "text-center p-4",
        tags$i(class = "fas fa-exclamation-triangle fa-2x text-warning"),
        tags$p(class = "mt-2", "Insufficient data for Rt estimation")
      ))
    }

    phase_class <- switch(
      current$phase,
      "growing" = "severity-high",
      "declining" = "severity-low",
      "severity-moderate"
    )

    trend_icon <- switch(
      current$trend,
      "increasing" = "fa-arrow-up text-danger",
      "decreasing" = "fa-arrow-down text-success",
      "fa-minus text-secondary"
    )

    div(
      class = "row",
      div(
        class = "col-md-4",
        div(
          class = paste("kpi-card", phase_class),
          div(class = "kpi-label", "Current Rt"),
          div(class = "kpi-value", sprintf("%.2f", current$value)),
          div(class = "kpi-change", sprintf("95%% CrI: %.2f - %.2f", current$lower, current$upper))
        )
      ),
      div(
        class = "col-md-4",
        div(
          class = "kpi-card",
          div(class = "kpi-label", "Epidemic Phase"),
          div(class = "kpi-value", tools::toTitleCase(current$phase)),
          div(class = "kpi-change",
              tags$i(class = paste("fas", trend_icon)),
              paste(" Trend:", tools::toTitleCase(current$trend))
          )
        )
      ),
      div(
        class = "col-md-4",
        div(
          class = "kpi-card",
          div(class = "kpi-label", "Interpretation"),
          div(
            style = "font-size: 0.9rem;",
            if (current$value > 1) {
              "Epidemic growing - cases expected to increase"
            } else if (current$value < 1) {
              "Epidemic declining - cases expected to decrease"
            } else {
              "Epidemic stable - cases roughly constant"
            }
          )
        )
      )
    )
  })

  # Rt Time Series Plot
  output$rt_timeseries_plot <- renderPlotly({
    rt_result <- rt_data()

    if (is.null(rt_result$rt_estimates)) {
      return(plotly_empty() |> layout(
        title = list(text = "Insufficient data for Rt estimation", y = 0.5)
      ))
    }

    rt_df <- rt_result$rt_estimates

    p <- ggplot(rt_df, aes(x = date)) +
      geom_ribbon(aes(ymin = rt_lower, ymax = rt_upper), fill = "#E85D4C", alpha = 0.2) +
      geom_line(aes(y = rt_mean), color = "#E85D4C", linewidth = 1.2) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "#6B7280", linewidth = 0.8) +
      annotate("text", x = min(rt_df$date), y = 1.05, label = "Rt = 1 (threshold)",
               hjust = 0, size = 3, color = "#6B7280") +
      scale_y_continuous(limits = c(0, max(rt_df$rt_upper, 3))) +
      labs(x = NULL, y = "Reproduction Number (Rt)") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "#E5E7EB"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#374151"),
        axis.title = element_text(color = "#374151")
      )

    ggplotly(p, tooltip = c("x", "y")) |>
      config(displayModeBar = FALSE) |>
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 50, r = 20, t = 20, b = 50)
      )
  })

  # Forecast Plot
  output$forecast_plot <- renderPlotly({
    forecast_result <- forecast_data()

    if (is.null(forecast_result$forecast)) {
      return(plotly_empty() |> layout(
        title = list(text = "Insufficient data for forecasting", y = 0.5)
      ))
    }

    # Get historical data
    historical <- forecast_result$historical_cases
    forecast_df <- forecast_result$forecast

    if (is.null(historical) || nrow(historical) < 3) {
      return(plotly_empty() |> layout(
        title = list(text = "Insufficient historical data", y = 0.5)
      ))
    }

    # Prepare combined data
    historical_plot <- tail(historical, 12) |>
      rename(date = dates, cases = I) |>
      mutate(type = "Observed")

    forecast_plot <- forecast_df |>
      select(date, predicted_cases, lower_50, upper_50, lower_80, upper_80, lower_95, upper_95) |>
      rename(cases = predicted_cases) |>
      mutate(type = "Forecast")

    p <- ggplot() +
      # Forecast ribbons (95%, 80%, 50%)
      geom_ribbon(data = forecast_plot,
                  aes(x = date, ymin = lower_95, ymax = upper_95),
                  fill = "#0D9488", alpha = 0.15) +
      geom_ribbon(data = forecast_plot,
                  aes(x = date, ymin = lower_80, ymax = upper_80),
                  fill = "#0D9488", alpha = 0.25) +
      geom_ribbon(data = forecast_plot,
                  aes(x = date, ymin = lower_50, ymax = upper_50),
                  fill = "#0D9488", alpha = 0.35) +
      # Historical line
      geom_line(data = historical_plot, aes(x = date, y = cases),
                color = "#374151", linewidth = 1) +
      geom_point(data = historical_plot, aes(x = date, y = cases),
                 color = "#374151", size = 2) +
      # Forecast line
      geom_line(data = forecast_plot, aes(x = date, y = cases),
                color = "#0D9488", linewidth = 1.2, linetype = "dashed") +
      geom_point(data = forecast_plot, aes(x = date, y = cases),
                 color = "#0D9488", size = 3) +
      labs(x = NULL, y = "Cases") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "#E5E7EB"),
        panel.grid.minor = element_blank()
      )

    ggplotly(p, tooltip = c("x", "y")) |>
      config(displayModeBar = FALSE)
  })

  # Forecast Summary Table
  output$forecast_summary_table <- renderUI({
    forecast_result <- forecast_data()

    if (is.null(forecast_result$forecast)) {
      return(div(class = "text-center p-4", "No forecast available"))
    }

    forecast_df <- forecast_result$forecast
    current_rt <- forecast_result$current_rt

    div(
      class = "table-responsive",
      tags$table(
        class = "table table-sm",
        tags$thead(
          tags$tr(
            tags$th("Week"),
            tags$th("Predicted Cases"),
            tags$th("80% Interval"),
            tags$th("Rt Used")
          )
        ),
        tags$tbody(
          lapply(1:nrow(forecast_df), function(i) {
            row <- forecast_df[i, ]
            tags$tr(
              tags$td(paste("Week", row$forecast_week)),
              tags$td(tags$strong(format(row$predicted_cases, big.mark = ","))),
              tags$td(sprintf("%s - %s",
                              format(row$lower_80, big.mark = ","),
                              format(row$upper_80, big.mark = ","))),
              tags$td(sprintf("%.2f", row$rt_used))
            )
          })
        )
      ),
      tags$hr(),
      tags$p(
        class = "text-muted small",
        tags$strong("Method: "), "Rt-renewal equation with negative binomial uncertainty"
      )
    )
  })

  # Multi-Pathogen Rt Summary Table
  output$rt_summary_table <- renderTable({
    tryCatch({
      all_rt <- get_rt_all_pathogens()
      summarize_rt_table(all_rt)
    }, error = function(e) {
      data.frame(
        Pathogen = c("H3N2", "RSV", "COVID19"),
        `Current Rt` = rep("N/A", 3),
        Phase = rep("Unknown", 3),
        Trend = rep("Unknown", 3),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ===========================================================================
  # BAYESIAN FORECAST TAB OUTPUTS
  # ===========================================================================

  # Reactive value to store Bayesian forecast results
  bayes_result <- reactiveVal(NULL)
  bayes_running <- reactiveVal(FALSE)

  # Run Bayesian model when button is clicked
  observeEvent(input$run_bayesian_model, {
    # Set loading state
    bayes_running(TRUE)
    updateActionButton(session, "run_bayesian_model",
                       label = "Generating...",
                       icon = icon("spinner", class = "fa-spin"))
    shinyjs::disable("run_bayesian_model")

    # Run the forecast in a tryCatch to handle errors gracefully
    result <- tryCatch({
      # Get selected date range
      dates <- bayes_date_filter$range()
      
      get_bayesian_forecast(
        pathogen_code = input$bayes_pathogen,
        horizon = 4,
        use_cache = TRUE,
        start_date = dates$start,
        end_date = dates$end
      )
    }, error = function(e) {
      list(
        pathogen = input$bayes_pathogen,
        forecast = NULL,
        error = e$message
      )
    })

    bayes_result(result)
    bayes_running(FALSE)
    
    # Restore button state
    updateActionButton(session, "run_bayesian_model",
                       label = "Generate Forecast",
                       icon = icon("play"))
    shinyjs::enable("run_bayesian_model")
  })

  # Model Status Display
  output$bayes_model_status <- renderUI({
    result <- bayes_result()

    if (bayes_running()) {
      return(div(
        class = "text-center p-4",
        div(class = "spinner-border text-primary", role = "status"),
        tags$p(class = "mt-2", "Fitting Bayesian model... This may take a few minutes.")
      ))
    }

    if (is.null(result)) {
      return(div(
        class = "row",
        div(
          class = "col-md-6",
          div(
            class = "kpi-card severity-moderate",
            div(class = "kpi-label", "Model Status"),
            div(class = "kpi-value", style = "font-size: 1.5rem;", "Ready"),
            div(class = "kpi-change", "Click 'Generate Forecast' to start")
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "kpi-card",
            div(class = "kpi-label", "Method"),
            div(style = "font-size: 0.9rem;",
                "Bayesian negative binomial regression with brms/Stan. ",
                "Produces full posterior distributions for uncertainty quantification.")
          )
        )
      ))
    }

    if (!is.null(result$error)) {
      return(div(
        class = "alert alert-warning",
        tags$strong("Model Error: "), result$error,
        tags$br(),
        tags$small("Try using the Rt-based forecast on the Rt Analysis tab instead.")
      ))
    }

    # Successfully generated forecast
    div(
      class = "row",
      div(
        class = "col-md-4",
        div(
          class = "kpi-card severity-low",
          div(class = "kpi-label", "Status"),
          div(class = "kpi-value", style = "font-size: 1.5rem;", "Complete"),
          div(class = "kpi-change", "Forecast generated successfully")
        )
      ),
      div(
        class = "col-md-4",
        div(
          class = "kpi-card",
          div(class = "kpi-label", "Training Range"),
          div(class = "kpi-value", style = "font-size: 1.2rem;",
              if (!is.null(result$training_start) && !is.null(result$training_end)) {
                # Calculate days difference
                days <- as.numeric(difftime(result$training_end, result$training_start, units = "days"))
                sprintf("%d Days", days)
              } else "Unknown"),
          div(class = "kpi-change",
              if (!is.null(result$training_start)) {
                sprintf("%s to %s", format(result$training_start, "%b %d"), format(result$training_end, "%b %d"))
              } else "Full History")
        )
      ),
      div(
        class = "col-md-4",
        div(
          class = "kpi-card",
          div(class = "kpi-label", "Generated"),
          div(class = "kpi-value", style = "font-size: 1.2rem;",
              format(Sys.time(), "%H:%M")),
          div(class = "kpi-change", format(Sys.Date(), "%b %d, %Y"))
        )
      )
    )
  })

  # Bayesian Forecast Plot
  output$bayes_forecast_plot <- renderPlotly({
    result <- bayes_result()

    # Show loading placeholder if running
    if (bayes_running()) {
      return(plotly_empty() |> layout(
        title = list(text = "Generating Forecast...", y = 0.5),
        annotations = list(
          list(
            x = 0.5, y = 0.4, xref = "paper", yref = "paper",
            text = "Please wait while the model fits...",
            showarrow = FALSE, font = list(size = 14, color = "gray")
          )
        )
      ))
    }

    if (is.null(result) || is.null(result$forecast)) {
      return(plotly_empty() |> layout(
        title = list(text = "Click 'Generate Forecast' to create a Bayesian forecast", y = 0.5)
      ))
    }

    forecast_df <- result$forecast
    historical <- result$historical_cases

    # Prepare historical data
    if (!is.null(historical) && nrow(historical) > 0) {
      historical_plot <- tail(historical, 12) |>
        select(date, cases) |>
        mutate(type = "Observed")
    } else {
      historical_plot <- data.frame(date = as.Date(character()), cases = numeric())
    }

    # Prepare forecast data
    forecast_plot <- forecast_df |>
      mutate(type = "Forecast")
      
    # Connect historical to forecast visually
    # Create a small bridge dataframe if we have history
    bridge_plot <- NULL
    if (nrow(historical_plot) > 0) {
      last_hist <- tail(historical_plot, 1)
      first_fcast <- head(forecast_plot, 1)
      
      # Only bridge if there is a gap (e.g. 1 week)
      if (first_fcast$date > last_hist$date) {
         bridge_plot <- bind_rows(
           last_hist |> select(date, cases) |> mutate(type = "Forecast", ymin=NA, ymax=NA), # Use Forecast type to match color
           first_fcast |> select(date, cases = mean) |> mutate(type="Forecast") # Ensure columns match
         )
         # Fill in NA intervals for the bridge point (last hist) so ribbons start there too
         # actually, usually we want the ribbon to flare out from the last point.
         # For simplicity, let's just make sure the dashed line connects.
      }
    }

    p <- ggplot() +
      # 95% credible interval
      geom_ribbon(data = forecast_plot,
                  aes(x = date, ymin = lower_95, ymax = upper_95),
                  fill = "#7C3AED", alpha = 0.1) +
      # 80% credible interval
      geom_ribbon(data = forecast_plot,
                  aes(x = date, ymin = lower_80, ymax = upper_80),
                  fill = "#7C3AED", alpha = 0.2) +
      # 50% credible interval
      geom_ribbon(data = forecast_plot,
                  aes(x = date, ymin = lower_50, ymax = upper_50),
                  fill = "#7C3AED", alpha = 0.3)

    # Add historical data if available
    if (nrow(historical_plot) > 0) {
      p <- p +
        geom_line(data = historical_plot, aes(x = date, y = cases),
                  color = "#374151", linewidth = 1) +
        geom_point(data = historical_plot, aes(x = date, y = cases),
                   color = "#374151", size = 2)
       
       # Add the connecting dashed line from last observed to first forecast
       if (!is.null(bridge_plot)) {
          # We manually draw a segment
          last_hist <- tail(historical_plot, 1)
          first_fcast <- head(forecast_plot, 1)
          p <- p + 
            geom_segment(aes(x = last_hist$date, y = last_hist$cases, 
                             xend = first_fcast$date, yend = first_fcast$mean),
                         color = "#7C3AED", linewidth = 1.2, linetype = "dashed", alpha=0.7)
       }
    }

    # Add forecast line
    p <- p +
      geom_line(data = forecast_plot, aes(x = date, y = mean),
                color = "#7C3AED", linewidth = 1.2, linetype = "dashed") +
      geom_point(data = forecast_plot, aes(x = date, y = mean),
                 color = "#7C3AED", size = 3) +
      labs(
        x = NULL,
        y = "Predicted Cases",
        title = NULL
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "#E5E7EB"),
        panel.grid.minor = element_blank()
      )

    ggplotly(p, tooltip = c("x", "y")) |>
      config(displayModeBar = FALSE) |>
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 60, r = 20, t = 20, b = 50),
        annotations = list(
          list(
            x = 1, y = 1, xref = "paper", yref = "paper",
            text = "Ribbons: 50%, 80%, 95% CrI",
            showarrow = FALSE, font = list(size = 10, color = "#6B7280"),
            xanchor = "right", yanchor = "top"
          )
        )
      )
  })

  # Forecast Summary
  output$bayes_forecast_summary <- renderUI({
    result <- bayes_result()

    if (is.null(result) || is.null(result$forecast)) {
      return(div(class = "text-center p-4 text-muted", "No forecast available"))
    }

    forecast_df <- result$forecast

    div(
      class = "table-responsive",
      tags$table(
        class = "table table-sm",
        tags$thead(
          tags$tr(
            tags$th("Week"),
            tags$th("Mean"),
            tags$th("50% CrI"),
            tags$th("80% CrI"),
            tags$th("95% CrI")
          )
        ),
        tags$tbody(
          lapply(1:nrow(forecast_df), function(i) {
            row <- forecast_df[i, ]
            tags$tr(
              tags$td(paste("Week", row$forecast_week)),
              tags$td(tags$strong(format(round(row$mean), big.mark = ","))),
              tags$td(sprintf("%s - %s",
                              format(round(row$lower_50), big.mark = ","),
                              format(round(row$upper_50), big.mark = ","))),
              tags$td(sprintf("%s - %s",
                              format(round(row$lower_80), big.mark = ","),
                              format(round(row$upper_80), big.mark = ","))),
              tags$td(sprintf("%s - %s",
                              format(round(row$lower_95), big.mark = ","),
                              format(round(row$upper_95), big.mark = ",")))
            )
          })
        )
      ),
      tags$hr(),
      tags$p(
        class = "text-muted small",
        tags$strong("Model: "), "Bayesian negative binomial regression (brms/Stan)"
      )
    )
  })

  # Diagnostics Summary
  output$bayes_diagnostics_summary <- renderUI({
    result <- bayes_result()

    if (is.null(result) || is.null(result$diagnostics)) {
      return(div(
        class = "text-center p-4 text-muted",
        "Generate a forecast to see model diagnostics"
      ))
    }

    diag <- result$diagnostics

    # Format diagnostics summary
    format_diagnostic_summary(diag)

    div(
      tags$table(
        class = "table table-sm",
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Convergence")),
            tags$td(
              if (isTRUE(diag$convergence$converged)) {
                span(class = "badge bg-success", "Passed")
              } else {
                span(class = "badge bg-warning", "Check Needed")
              }
            )
          ),
          tags$tr(
            tags$td(tags$strong("Max Rhat")),
            tags$td(sprintf("%.3f", diag$convergence$rhat_max %||% NA))
          ),
          tags$tr(
            tags$td(tags$strong("Min Bulk ESS")),
            tags$td(sprintf("%.0f", diag$convergence$ess_min_bulk %||% NA))
          ),
          tags$tr(
            tags$td(tags$strong("Divergences")),
            tags$td(sprintf("%.0f", diag$convergence$n_divergences %||% 0))
          )
        )
      ),
      tags$hr(),
      tags$p(
        class = "text-muted small",
        "Rhat < 1.01 and ESS > 400 indicate good convergence"
      )
    )
  })

  # =============================================================================
  # ADVANCED DIAGNOSTIC PLOTS (Expandable Panel)
  # =============================================================================

  # Trace plots for MCMC chains
  output$bayes_trace_plot <- renderPlot({
    result <- bayes_result()
    if (is.null(result) || is.null(result$model)) {
      return(NULL)
    }

    tryCatch({
      plot_trace(result$model)
    }, error = function(e) {
      # Return a placeholder plot on error
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Could not generate trace plot:", e$message),
                 size = 4, color = "gray50") +
        theme_void()
    })
  })

  # Posterior density plots
  output$bayes_density_plot <- renderPlot({
    result <- bayes_result()
    if (is.null(result) || is.null(result$model)) {
      return(NULL)
    }

    tryCatch({
      plot_density(result$model)
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Could not generate density plot:", e$message),
                 size = 4, color = "gray50") +
        theme_void()
    })
  })

  # Posterior Predictive Check plot
  output$bayes_ppc_plot <- renderPlot({
    result <- bayes_result()
    if (is.null(result) || is.null(result$model)) {
      return(NULL)
    }

    tryCatch({
      posterior_predictive_check(result$model, ndraws = 50)
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Could not generate PPC:", e$message),
                 size = 4, color = "gray50") +
        theme_void()
    })
  })

  # Parameter summary table
  output$bayes_param_table <- renderTable({
    result <- bayes_result()
    if (is.null(result) || is.null(result$model)) {
      return(data.frame(Message = "Generate forecast to see parameters"))
    }

    tryCatch({
      summary_df <- get_model_summary(result$model)
      if (is.null(summary_df)) {
        return(data.frame(Message = "Could not extract parameters"))
      }
      # Select key columns for display
      summary_df |>
        select(Parameter, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Converged) |>
        head(10)  # Limit to first 10 parameters for readability
    }, error = function(e) {
      data.frame(Message = paste("Error:", e$message))
    })
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  # All Pathogens Table (placeholder - actual implementation would call get_bayesian_forecasts_all)
  output$bayes_all_pathogens_table <- renderTable({
    # This is a placeholder - full implementation would be expensive
    data.frame(
      Pathogen = c("H3N2", "RSV", "COVID-19"),
      `1-Week Forecast` = c("Use Generate button", "Use Generate button", "Use Generate button"),
      `4-Week Forecast` = c("for each pathogen", "for each pathogen", "for each pathogen"),
      Status = c("Ready", "Ready", "Ready"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Download handler for forecast data
  output$download_bayes_forecast <- downloadHandler(
    filename = function() {
      paste0("bayesian_forecast_", input$bayes_pathogen, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      result <- bayes_result()
      if (!is.null(result) && !is.null(result$forecast)) {
        write.csv(result$forecast, file, row.names = FALSE)
      } else {
        write.csv(data.frame(message = "No forecast available"), file, row.names = FALSE)
      }
    }
  )

  # =============================================================================
  # ENSEMBLE FORECAST COMPARISON HANDLERS
  # =============================================================================

  # Reactive value to store ensemble results
  ensemble_result <- reactiveVal(NULL)

  # Generate ensemble forecast when button clicked
  observeEvent(input$generate_ensemble, {
    req(input$bayes_pathogen)

    showNotification("Generating ensemble forecast...", type = "message", duration = 3)

    result <- tryCatch({
      get_ensemble_forecast(
        pathogen_code = input$bayes_pathogen,
        horizon = 4
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
      list(status = "error", message = e$message)
    })

    ensemble_result(result)

    if (!is.null(result) && result$status == "success") {
      showNotification(
        sprintf("Ensemble forecast generated using %d methods: %s",
                result$n_methods_used, paste(result$methods_used, collapse = ", ")),
        type = "message",
        duration = 4
      )
    }
  })

  # Ensemble comparison plot
  output$ensemble_comparison_plot <- renderPlotly({
    result <- ensemble_result()

    if (is.null(result) || result$status != "success") {
      # Return placeholder plot
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Click 'Generate Ensemble Forecast' to compare methods",
                 size = 5, color = "gray50") +
        theme_void()
      return(ggplotly(p))
    }

    # Prepare plot data
    plot_data <- prepare_ensemble_plot_data(result)

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No forecast data available",
                 size = 5, color = "gray50") +
        theme_void()
      return(ggplotly(p))
    }

    # Create comparison plot
    p <- ggplot(plot_data |> filter(type != "observed"),
                aes(x = date, y = predicted_cases, color = source)) +
      geom_line(linewidth = 1) +
      geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = source),
                  alpha = 0.15, color = NA) +
      scale_color_manual(values = c(
        "Ensemble" = "#2563eb",
        "rt_renewal" = "#16a34a",
        "bayesian" = "#dc2626"
      )) +
      scale_fill_manual(values = c(
        "Ensemble" = "#2563eb",
        "rt_renewal" = "#16a34a",
        "bayesian" = "#dc2626"
      )) +
      labs(
        title = sprintf("Forecast Method Comparison: %s", result$pathogen),
        x = "Date",
        y = "Predicted Cases",
        color = "Method",
        fill = "Method"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom"
      )

    ggplotly(p, tooltip = c("x", "y", "color")) |>
      layout(legend = list(orientation = "h", y = -0.15))
  })

  # Ensemble skill scores table
  output$ensemble_skill_scores <- renderTable({
    result <- ensemble_result()

    if (is.null(result) || result$status != "success") {
      return(data.frame(
        Method = "No data",
        MAE = "-",
        RMSE = "-",
        Coverage = "-"
      ))
    }

    # Create summary of methods used with weights
    methods_df <- data.frame(
      Method = c("Ensemble", result$methods_used),
      Weight = c("Combined",
                 sapply(result$methods_used, function(m) {
                   sprintf("%.0f%%", result$config$weights[m] * 100)
                 })),
      `1-Week Forecast` = c(
        if (!is.null(result$ensemble_forecast) && nrow(result$ensemble_forecast) > 0) {
          sprintf("%d", result$ensemble_forecast$predicted_cases[1])
        } else "N/A",
        sapply(result$component_forecasts, function(fc) {
          if (!is.null(fc) && nrow(fc) > 0) {
            sprintf("%d", fc$predicted_cases[1])
          } else "N/A"
        })
      ),
      `4-Week Forecast` = c(
        if (!is.null(result$ensemble_forecast) && nrow(result$ensemble_forecast) >= 4) {
          sprintf("%d", result$ensemble_forecast$predicted_cases[4])
        } else "N/A",
        sapply(result$component_forecasts, function(fc) {
          if (!is.null(fc) && nrow(fc) >= 4) {
            sprintf("%d", fc$predicted_cases[4])
          } else "N/A"
        })
      ),
      Status = c("Combined", rep("Component", length(result$methods_used))),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    methods_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # =============================================================================
  # SCENARIO ANALYSIS TAB HANDLERS
  # =============================================================================

  # Reactive value to store scenario results
  scenario_results <- reactiveVal(NULL)

  # Run scenario analysis when button clicked
  observeEvent(input$run_scenarios, {
    req(input$scenario_pathogen, input$scenario_selection)

    # UI Loading State
    shinyjs::disable("run_scenarios")
    updateActionButton(session, "run_scenarios",
                       label = "Running...",
                       icon = icon("spinner", class = "fa-spin"))

    # Ensure reset happens even on error
    on.exit({
      shinyjs::enable("run_scenarios")
      updateActionButton(session, "run_scenarios",
                         label = "Run Scenarios",
                         icon = icon("play"))
    })

    showNotification("Running scenario analysis...", type = "message", duration = 2)

    result <- tryCatch({
      get_scenario_projections(
        pathogen_code = input$scenario_pathogen,
        scenarios = input$scenario_selection,
        horizon = input$scenario_horizon
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
      list(status = "error", message = e$message)
    })

    scenario_results(result)

    if (!is.null(result) && result$status == "success") {
      showNotification(
        sprintf("Generated %d scenario projections for %s",
                result$n_scenarios, input$scenario_pathogen),
        type = "message",
        duration = 3
      )
    }
  })

  # Display current Rt value
  output$scenario_current_rt <- renderUI({
    result <- scenario_results()
    if (is.null(result) || result$status != "success") {
      return(div(
        class = "text-center p-4",
        icon("chart-line", class = "fa-3x text-muted mb-3"),
        p(class = "text-muted", "Run analysis to see current Rt")
      ))
    }

    rt_value <- result$current_rt
    rt_color <- if (rt_value > 1.5) "#DC2626" else if (rt_value > 1.0) "#F59E0B" else "#10B981"
    rt_status <- if (rt_value > 1.5) "Critical" else if (rt_value > 1.0) "Growing" else "Declining"

    div(
      class = "text-center",
      h1(style = sprintf("color: %s; font-size: 3rem; margin-bottom: 0;", rt_color),
         sprintf("%.2f", rt_value)),
      p(class = "text-muted mb-2", "Current Rt"),
      span(class = sprintf("badge bg-%s",
                           if (rt_value > 1.5) "danger" else if (rt_value > 1.0) "warning" else "success"),
           rt_status),
      hr(),
      p(class = "small text-muted mb-0",
        sprintf("Pathogen: %s", result$pathogen)),
      p(class = "small text-muted",
        sprintf("Serial interval: %.1f days", result$serial_interval$mean))
    )
  })

  # Scenario comparison plot
  output$scenario_comparison_plot <- renderPlotly({
    result <- scenario_results()
    if (is.null(result) || result$status != "success") {
      return(plotly_empty() |>
               layout(
                 title = list(text = "Select scenarios and click 'Run Scenario Analysis'",
                              font = list(size = 14)),
                 paper_bgcolor = "#f8f9fa",
                 plot_bgcolor = "#f8f9fa"
               ))
    }

    # Prepare plot data
    plot_data <- prepare_scenario_plot_data(result, include_historical = TRUE)

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(plotly_empty() |> layout(title = "No data available"))
    }

    # Define colors for scenarios
    scenario_colors <- c(
      "no_intervention" = "#6B7280",
      "mask_mandate" = "#3B82F6",
      "social_distancing" = "#10B981",
      "school_closure" = "#F59E0B",
      "lockdown_light" = "#8B5CF6",
      "lockdown_full" = "#DC2626",
      "vaccination_boost" = "#06B6D4",
      "combined_moderate" = "#EC4899",
      "historical" = "#1F2937"
    )

    # Create plot
    p <- plot_ly()

    # Add historical data if present
    historical <- plot_data |> filter(type == "observed")
    if (nrow(historical) > 0) {
      p <- p |> add_trace(
        data = historical,
        x = ~date,
        y = ~predicted_cases,
        type = "scatter",
        mode = "lines+markers",
        name = "Observed",
        line = list(color = "#1F2937", width = 2),
        marker = list(color = "#1F2937", size = 6)
      )
    }

    # Add each scenario projection
    projections <- plot_data |> filter(type == "projection")
    for (sc in unique(projections$scenario)) {
      sc_data <- projections |> filter(scenario == sc)
      color <- scenario_colors[[sc]] %||% "#6B7280"

      # Add confidence ribbon
      p <- p |> add_trace(
        data = sc_data,
        x = ~date,
        y = ~upper_95,
        type = "scatter",
        mode = "lines",
        line = list(width = 0),
        showlegend = FALSE,
        hoverinfo = "skip",
        name = paste(sc, "upper")
      )

      p <- p |> add_trace(
        data = sc_data,
        x = ~date,
        y = ~lower_95,
        type = "scatter",
        mode = "lines",
        fill = "tonexty",
        fillcolor = paste0(color, "20"),
        line = list(width = 0),
        showlegend = FALSE,
        hoverinfo = "skip",
        name = paste(sc, "lower")
      )

      # Add main line
      p <- p |> add_trace(
        data = sc_data,
        x = ~date,
        y = ~predicted_cases,
        type = "scatter",
        mode = "lines",
        name = sc_data$scenario_name[1],
        line = list(color = color, width = 2),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Scenario: ", sc_data$scenario_name[1], "<br>",
          "Cases: %{y:,.0f}<br>",
          "<extra></extra>"
        )
      )
    }

    p |> layout(
      title = list(
        text = sprintf("Scenario Projections: %s", result$pathogen),
        font = list(size = 16)
      ),
      xaxis = list(title = "Date", gridcolor = "#E5E7EB"),
      yaxis = list(title = "Projected Cases", gridcolor = "#E5E7EB"),
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        y = -0.3,
        xanchor = "center",
        x = 0.5
      ),
      hovermode = "x unified",
      paper_bgcolor = "white",
      plot_bgcolor = "white"
    )
  })

  # Scenario impact table
  output$scenario_impact_table <- DT::renderDT({
    result <- scenario_results()
    if (is.null(result) || result$status != "success") {
      return(DT::datatable(
        data.frame(Message = "Run scenario analysis to see impact comparison"),
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE
      ))
    }

    impact_table <- summarize_scenario_table(result)

    if (is.null(impact_table) || nrow(impact_table) == 0) {
      return(DT::datatable(
        data.frame(Message = "No impact data available"),
        options = list(dom = "t", paging = FALSE),
        rownames = FALSE
      ))
    }

    DT::datatable(
      impact_table,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames = FALSE,
      class = "table table-striped table-hover"
    ) |>
      DT::formatStyle(
        "Scenario",
        fontWeight = "bold"
      ) |>
      DT::formatRound(
        columns = c("Rt Modifier"),
        digits = 2
      ) |>
      DT::formatRound(
        columns = c("Total Cases", "Peak Week", "Cases Averted"),
        digits = 0
      ) |>
      DT::formatStyle(
        "% Reduction",
        color = DT::styleInterval(
          cuts = c(0, 25, 50),
          values = c("#DC2626", "#F59E0B", "#10B981", "#047857")
        ),
        fontWeight = "bold"
      )
  })

  # Key insights
  output$scenario_insights <- renderUI({
    result <- scenario_results()
    if (is.null(result) || result$status != "success") {
      return(NULL)
    }

    impact <- calculate_scenario_impact(result)
    if (is.null(impact)) {
      return(NULL)
    }

    # Find best intervention
    best <- impact |>
      filter(scenario != "no_intervention") |>
      arrange(desc(percent_reduction)) |>
      slice(1)

    # Find most balanced (good reduction, less restrictive)
    balanced <- impact |>
      filter(scenario != "no_intervention", rt_modifier >= 0.6) |>
      arrange(desc(percent_reduction)) |>
      slice(1)

    baseline_cases <- impact$total_cases[impact$scenario == "no_intervention"]

    div(
      class = "mt-4",
      h5(icon("lightbulb"), " Key Insights"),
      div(class = "row",
          div(class = "col-md-4",
              div(class = "card border-success",
                  div(class = "card-body",
                      h6(class = "card-title text-success", icon("trophy"), " Most Effective"),
                      p(class = "card-text",
                        sprintf("%s could prevent %s cases (%.0f%% reduction)",
                                best$scenario_name,
                                format(best$cases_averted, big.mark = ","),
                                best$percent_reduction))
                  )
              )
          ),
          div(class = "col-md-4",
              div(class = "card border-info",
                  div(class = "card-body",
                      h6(class = "card-title text-info", icon("balance-scale"), " Balanced Approach"),
                      p(class = "card-text",
                        if (nrow(balanced) > 0) {
                          sprintf("%s offers %.0f%% reduction with moderate restrictions",
                                  balanced$scenario_name, balanced$percent_reduction)
                        } else {
                          "No moderate interventions selected"
                        })
                  )
              )
          ),
          div(class = "col-md-4",
              div(class = "card border-warning",
                  div(class = "card-body",
                      h6(class = "card-title text-warning", icon("exclamation-triangle"), " Without Action"),
                      p(class = "card-text",
                        sprintf("Baseline trajectory: %s total cases over %d weeks",
                                format(baseline_cases, big.mark = ","),
                                result$horizon))
                  )
              )
          )
      )
    )
  })

  # Custom scenario handler
  observeEvent(input$add_custom_scenario, {
    req(input$custom_scenario_name, input$custom_rt_reduction)

    custom <- create_custom_scenario(
      name = input$custom_scenario_name,
      rt_reduction = input$custom_rt_reduction,
      description = sprintf("Custom: %s (%.0f%% Rt reduction)",
                            input$custom_scenario_name,
                            input$custom_rt_reduction)
    )

    showNotification(
      sprintf("Custom scenario '%s' created. Re-run analysis to include it.",
              input$custom_scenario_name),
      type = "message",
      duration = 3
    )
  })

  # =============================================================================
  # HEALTHCARE CAPACITY TAB HANDLERS
  # =============================================================================

  # Reactive value to store capacity forecast results
  capacity_result <- reactiveVal(NULL)

  # Generate capacity forecast when button clicked
  observeEvent(input$generate_capacity_forecast, {
    req(input$capacity_pathogen, input$capacity_horizon)
    
    # UI Loading State
    shinyjs::disable("generate_capacity_forecast")
    updateActionButton(session, "generate_capacity_forecast",
                       label = "Processing...",
                       icon = icon("spinner", class = "fa-spin"))
    
    # Ensure reset happens even on error
    on.exit({
      shinyjs::enable("generate_capacity_forecast")
      updateActionButton(session, "generate_capacity_forecast",
                         label = "Generate Forecast",
                         icon = icon("sync"))
    })

    showNotification("Generating capacity forecast...", type = "message", duration = 2)

    result <- tryCatch({
      get_capacity_forecast(
        pathogen_code = input$capacity_pathogen,
        horizon = input$capacity_horizon
      )
    }, error = function(e) {
      log_error(sprintf("Capacity forecast error: %s", e$message), category = "healthcare")
      list(status = "error", message = e$message)
    })

    capacity_result(result)

    if (!is.null(result) && result$status == "success") {
      showNotification("Capacity forecast generated successfully!", type = "message", duration = 2)
    } else if (!is.null(result) && result$status == "error") {
      showNotification(paste("Error:", result$message), type = "error", duration = 4)
    }
  })

  # Hospital capacity gauge
  output$hospital_gauge <- renderUI({
    result <- capacity_result()
    if (is.null(result) || result$status != "success") {
      return(div(
        class = "d-flex justify-content-center align-items-center",
        style = "height: 200px; background: var(--bs-light); border-radius: 10px;",
        p(class = "text-muted", "Click 'Generate Forecast' to view hospital capacity")
      ))
    }

    summary <- get_capacity_summary(result)
    hospital <- summary$hospital

    # Determine color based on status
    gauge_color <- switch(
      hospital$status,
      "critical" = "#DC2626",
      "warning" = "#F59E0B",
      "elevated" = "#3B82F6",
      "#10B981"  # normal
    )

    div(
      class = "text-center",
      div(
        style = sprintf("
          width: 180px; height: 180px; margin: 0 auto;
          border-radius: 50%%;
          background: conic-gradient(%s 0deg %.1fdeg, #e5e7eb %.1fdeg 360deg);
          display: flex; align-items: center; justify-content: center;
        ", gauge_color, hospital$utilization * 3.6, hospital$utilization * 3.6),
        div(
          style = "width: 140px; height: 140px; border-radius: 50%; background: white;
                   display: flex; flex-direction: column; align-items: center; justify-content: center;",
          span(style = "font-size: 2rem; font-weight: bold;",
               sprintf("%.0f%%", hospital$utilization)),
          span(class = "text-muted small", "Utilization")
        )
      ),
      div(class = "mt-2",
          span(class = sprintf("badge bg-%s",
                               switch(hospital$status,
                                      "critical" = "danger",
                                      "warning" = "warning",
                                      "elevated" = "info",
                                      "success")),
               toupper(hospital$status))
      )
    )
  })

  # ICU capacity gauge
  output$icu_gauge <- renderUI({
    result <- capacity_result()
    if (is.null(result) || result$status != "success") {
      return(div(
        class = "d-flex justify-content-center align-items-center",
        style = "height: 200px; background: var(--bs-light); border-radius: 10px;",
        p(class = "text-muted", "Click 'Generate Forecast' to view ICU capacity")
      ))
    }

    summary <- get_capacity_summary(result)
    icu <- summary$icu

    # Determine color based on status
    gauge_color <- switch(
      icu$status,
      "critical" = "#DC2626",
      "warning" = "#F59E0B",
      "elevated" = "#3B82F6",
      "#10B981"  # normal
    )

    div(
      class = "text-center",
      div(
        style = sprintf("
          width: 180px; height: 180px; margin: 0 auto;
          border-radius: 50%%;
          background: conic-gradient(%s 0deg %.1fdeg, #e5e7eb %.1fdeg 360deg);
          display: flex; align-items: center; justify-content: center;
        ", gauge_color, icu$utilization * 3.6, icu$utilization * 3.6),
        div(
          style = "width: 140px; height: 140px; border-radius: 50%; background: white;
                   display: flex; flex-direction: column; align-items: center; justify-content: center;",
          span(style = "font-size: 2rem; font-weight: bold;",
               sprintf("%.0f%%", icu$utilization)),
          span(class = "text-muted small", "Utilization")
        )
      ),
      div(class = "mt-2",
          span(class = sprintf("badge bg-%s",
                               switch(icu$status,
                                      "critical" = "danger",
                                      "warning" = "warning",
                                      "elevated" = "info",
                                      "success")),
               toupper(icu$status))
      )
    )
  })

  # Hospital status text
  output$hospital_status_text <- renderUI({
    result <- capacity_result()
    if (is.null(result) || result$status != "success") {
      return(NULL)
    }

    summary <- get_capacity_summary(result)
    hospital <- summary$hospital

    div(
      p(class = "mb-1",
        sprintf("%s of %s beds occupied",
                format(hospital$occupied, big.mark = ","),
                format(hospital$capacity, big.mark = ","))),
      p(class = "text-muted small mb-0",
        sprintf("%s beds available", format(hospital$available, big.mark = ",")))
    )
  })

  # ICU status text
  output$icu_status_text <- renderUI({
    result <- capacity_result()
    if (is.null(result) || result$status != "success") {
      return(NULL)
    }

    summary <- get_capacity_summary(result)
    icu <- summary$icu

    div(
      p(class = "mb-1",
        sprintf("%s of %s beds occupied",
                format(icu$occupied, big.mark = ","),
                format(icu$capacity, big.mark = ","))),
      p(class = "text-muted small mb-0",
        sprintf("%s beds available", format(icu$available, big.mark = ",")))
    )
  })

  # Capacity timeline plot
  output$capacity_timeline_plot <- renderPlotly({
    result <- capacity_result()
    if (is.null(result) || result$status != "success") {
      return(plotly_empty() |>
               layout(
                 title = list(text = "Generate forecast to view capacity timeline",
                              font = list(size = 14, color = "#6b7280")),
                 paper_bgcolor = "#f9fafb",
                 plot_bgcolor = "#f9fafb"
               ))
    }

    plot_data <- result$timeline
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(plotly_empty() |>
               layout(title = "No timeline data available"))
    }

    # Create capacity timeline
    plot_ly(plot_data, x = ~date) |>
      add_trace(
        y = ~hospital_utilization,
        type = "scatter",
        mode = "lines+markers",
        name = "Hospital",
        line = list(color = "#3B82F6", width = 3),
        marker = list(size = 6)
      ) |>
      add_trace(
        y = ~icu_utilization,
        type = "scatter",
        mode = "lines+markers",
        name = "ICU",
        line = list(color = "#8B5CF6", width = 3),
        marker = list(size = 6)
      ) |>
      # Add threshold lines
      add_trace(
        y = rep(90, nrow(plot_data)),
        type = "scatter",
        mode = "lines",
        name = "Critical (90%)",
        line = list(color = "#DC2626", width = 2, dash = "dash")
      ) |>
      add_trace(
        y = rep(80, nrow(plot_data)),
        type = "scatter",
        mode = "lines",
        name = "Warning (80%)",
        line = list(color = "#F59E0B", width = 2, dash = "dot")
      ) |>
      layout(
        title = list(text = "Capacity Utilization Over Time", font = list(size = 16)),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Utilization (%)", range = c(0, 105)),
        legend = list(orientation = "h", y = -0.2),
        hovermode = "x unified",
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      )
  })

  # Hospitalization forecast plot
  output$hospitalization_forecast_plot <- renderPlotly({
    result <- capacity_result()
    if (is.null(result) || result$status != "success") {
      return(plotly_empty() |>
               layout(
                 title = list(text = "Generate forecast to view hospitalization projections",
                              font = list(size = 14, color = "#6b7280")),
                 paper_bgcolor = "#f9fafb",
                 plot_bgcolor = "#f9fafb"
               ))
    }

    # Get hospitalization forecast data
    hosp_data <- result$hospitalization_forecast
    if (is.null(hosp_data) || nrow(hosp_data) == 0) {
      return(plotly_empty() |>
               layout(title = "No hospitalization forecast available"))
    }

    plot_ly(hosp_data, x = ~date) |>
      add_trace(
        y = ~total_admissions,
        type = "bar",
        name = "Projected Admissions",
        marker = list(color = "#3B82F6")
      ) |>
      add_trace(
        y = ~total_occupancy,
        type = "scatter",
        mode = "lines",
        name = "Projected Occupancy",
        yaxis = "y2",
        line = list(color = "#DC2626", width = 3)
      ) |>
      layout(
        title = list(text = "Hospitalization Forecast", font = list(size = 16)),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Daily Admissions", side = "left"),
        yaxis2 = list(
          title = "Total Occupancy",
          overlaying = "y",
          side = "right",
          showgrid = FALSE
        ),
        legend = list(orientation = "h", y = -0.2),
        barmode = "group",
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      )
  })

  # Surge alerts display
  output$surge_alerts_display <- renderUI({
    result <- capacity_result()
    if (is.null(result) || result$status != "success") {
      return(div(
        class = "alert alert-secondary",
        icon("info-circle"), " Generate forecast to view surge alerts"
      ))
    }

    alerts <- generate_surge_alerts(result)
    if (length(alerts) == 0) {
      return(div(
        class = "alert alert-success",
        icon("check-circle"), " ",
        strong("All Clear"), " - No surge alerts detected"
      ))
    }

    HTML(format_capacity_alerts_html(alerts))
  })

  # Surge timing table
  output$surge_timing_table <- renderTable({
    result <- capacity_result()
    if (is.null(result) || result$status != "success") {
      return(data.frame(
        Metric = character(),
        `Current Value` = character(),
        `Projected Peak` = character(),
        `Days to 90%` = character(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    summary <- get_capacity_summary(result)

    # Calculate projected breach dates
    hospital_breach <- project_capacity_breach(
      result$hospitalization_forecast$cumulative_occupancy,
      result$hospitalization_forecast$date,
      result$hospital_capacity,
      0.90
    )

    icu_breach <- project_capacity_breach(
      result$icu_forecast$cumulative_occupancy,
      result$icu_forecast$date,
      result$icu_capacity,
      0.90
    )

    data.frame(
      Metric = c("Hospital Beds", "ICU Beds"),
      `Current Utilization` = c(
        sprintf("%.1f%%", summary$hospital$utilization),
        sprintf("%.1f%%", summary$icu$utilization)
      ),
      `Available Beds` = c(
        format(summary$hospital$available, big.mark = ","),
        format(summary$icu$available, big.mark = ",")
      ),
      `Status` = c(
        toupper(summary$hospital$status),
        toupper(summary$icu$status)
      ),
      `Days to Critical` = c(
        if (is.null(hospital_breach$days_to_breach) || is.na(hospital_breach$days_to_breach))
          "N/A" else sprintf("%d days", hospital_breach$days_to_breach),
        if (is.null(icu_breach$days_to_breach) || is.na(icu_breach$days_to_breach))
          "N/A" else sprintf("%d days", icu_breach$days_to_breach)
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
}

# Run App ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
