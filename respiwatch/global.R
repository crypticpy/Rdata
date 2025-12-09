# ============================================================================
# RespiWatch: Global Respiratory Surveillance Platform
# global.R - Packages, Theme, and Data Initialization
# Multi-Pathogen Tracking System for 2024-2025 Season
# ============================================================================

# Startup error logging for debugging deployment issues -----------------------
.startup_log <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entry <- sprintf("[%s] [%s] %s\n", timestamp, level, msg)
  cat(entry)
  # Also write to file for HF Spaces debugging

  tryCatch({
    log_dir <- if (dir.exists("/var/log/shiny-server")) "/var/log/shiny-server" else "logs"
    if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    cat(entry, file = file.path(log_dir, "startup.log"), append = TRUE)
  }, error = function(e) NULL)
}

.startup_log("RespiWatch initialization starting...")

# Load packages with error handling -------------------------------------------
.load_package <- function(pkg) {
  tryCatch({
    library(pkg, character.only = TRUE)
    .startup_log(sprintf("Loaded package: %s", pkg))
    TRUE
  }, error = function(e) {
    .startup_log(sprintf("FAILED to load package %s: %s", pkg, e$message), "ERROR")
    FALSE
  })
}

required_packages <- c("shiny", "bslib", "jsonlite", "leaflet", "plotly", 
                       "dplyr", "tidyr", "ggplot2", "shinyjs", "scales", 
                       "DT", "sf", "rnaturalearth", "DBI", "RSQLite", "zoo")

failed_packages <- c()
for (pkg in required_packages) {
  if (!.load_package(pkg)) {
    failed_packages <- c(failed_packages, pkg)
  }
}

if (length(failed_packages) > 0) {
  stop(sprintf("Failed to load required packages: %s", paste(failed_packages, collapse = ", ")))
}

# Legacy compatibility: keep explicit library calls for reference
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
library(zoo)

# Source data loading modules with error handling -----------------------------
.safe_source <- function(file) {
  tryCatch({
    source(file)
    .startup_log(sprintf("Sourced: %s", file))
    TRUE
  }, error = function(e) {
    .startup_log(sprintf("FAILED to source %s: %s", file, e$message), "ERROR")
    FALSE
  })
}

source_files <- c(
  "R/logging.R",
  "R/ui_helpers.R",
  "R/schema_definitions.R",
  "R/data_loader.R",
  "R/rt_estimation.R",
  "R/forecasting.R",
  "R/bayesian_forecast.R",
  "R/model_diagnostics.R",
  "R/scenario_modeling.R",
  "R/ensemble_forecast.R",
  "R/healthcare_capacity.R",
  "R/wave_propagation.R",
  "R/vaccination_impact.R",
  "R/ai_hooks.R",
  "R/date_range_module.R",
  "R/data_fallback.R"
)

module_files <- c(
  "R/modules/mod_about.R",
  "R/modules/mod_surveillance_gaps.R",
  "R/modules/mod_country_analysis.R",
  "R/modules/mod_pathogen_analysis.R",
  "R/modules/mod_rt_analysis.R",
  "R/modules/mod_bayesian_forecast.R",
  "R/modules/mod_scenario_analysis.R",
  "R/modules/mod_healthcare_capacity.R",
  "R/modules/mod_global_overview.R"
)

failed_sources <- c()
for (f in c(source_files, module_files)) {
  if (!.safe_source(f)) {
    failed_sources <- c(failed_sources, f)
  }
}

if (length(failed_sources) > 0) {
  .startup_log(sprintf("WARNING: Failed to source %d files: %s", 
                       length(failed_sources), paste(failed_sources, collapse = ", ")), "WARNING")
}

# =============================================================================
# DATA LOADING
# =============================================================================

# Load data with database preference, JSON fallback
dashboard_data <- load_dashboard_data()
USE_DATABASE <- dashboard_data$source == "database"

# Check if database has timeline data we can use directly
db_has_timeline <- USE_DATABASE &&
  !is.null(dashboard_data$timeline) &&
  is.data.frame(dashboard_data$timeline) &&
  nrow(dashboard_data$timeline) > 0

# Always load JSON files for UI metadata (pathogen info, surveillance status, etc.)
outbreak_data <- fromJSON("data/raw/h3n2_outbreak_tracker.json")
rsv_data <- fromJSON("data/raw/rsv_tracker.json")
covid_data <- fromJSON("data/raw/covid_tracker.json")

if (db_has_timeline) {
  message("Using SQLite database for live CDC/WHO surveillance data")
  db_surveillance <- dashboard_data$surveillance
  db_timeline <- dashboard_data$timeline
  db_kpi <- dashboard_data$kpi
} else {
  message("Database timeline empty or unavailable, using JSON files for timeline")
}

# =============================================================================
# COUNTRIES DATA
# =============================================================================

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

# Populate countries_df from Database or JSON
if (USE_DATABASE) {
  conn <- get_db_connection()
  map_snapshot_all <- get_latest_surveillance(conn)
  close_db_connection(conn)

  if (!is.null(map_snapshot_all) && nrow(map_snapshot_all) > 0) {
    countries_df <- filter_by_pathogen(map_snapshot_all, "H3N2")
  } else {
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
  countries_df <- tryCatch({
    do.call(rbind, lapply(outbreak_data$countries, function(country) {
      data.frame(
        iso_code = country$iso_code,
        country_name = country$country_name,
        population = country$population,
        outbreak_status = country$outbreak_status,
        positivity_rate = country$h3n2_data$positivity_rate %||% 0,
        vaccination_rate = 0,
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
    log_error(sprintf("Failed to extract country data: %s", e$message), category = "data")
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

# =============================================================================
# WORLD MAP DATA
# =============================================================================

world_countries <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(iso_a3, name, geometry) |>
  rename(iso_code = iso_a3, country_name_geo = name)

world_map_data <- world_countries |>
  left_join(countries_df, by = "iso_code")

# =============================================================================
# TIMELINE DATA EXTRACTION
# =============================================================================

# H3N2 timeline
timeline_df <- tryCatch({
  timeline_raw <- outbreak_data$timeline$progression_by_date
  if (is.data.frame(timeline_raw)) {
    data.frame(
      date = as.Date(timeline_raw$date),
      week_number = timeline_raw$week_number,
      positivity_rate = timeline_raw$surveillance_indicators$positivity_rate %||% 0,
      case_numbers = timeline_raw$surveillance_indicators$case_numbers %||% 0,
      hospitalization_rate = timeline_raw$surveillance_indicators$hospitalization_rate %||% 0,
      stringsAsFactors = FALSE
    )
  } else {
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
  log_error(sprintf("Failed to extract timeline data: %s", e$message), category = "data")
  data.frame(
    date = as.Date(character()), week_number = integer(),
    positivity_rate = numeric(), case_numbers = numeric(),
    hospitalization_rate = numeric(), stringsAsFactors = FALSE
  )
})

# Anomalies
anomalies_df <- tryCatch({
  anomalies_raw <- outbreak_data$anomaly_detection_flags$suspicious_patterns
  if (is.data.frame(anomalies_raw)) {
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
  log_error(sprintf("Failed to extract anomaly data: %s", e$message), category = "data")
  data.frame(
    pattern_id = character(), pattern_type = character(),
    description = character(), geographic_scope = character(),
    confidence_level = character(), verification_status = character(),
    first_detected = as.Date(character()), stringsAsFactors = FALSE
  )
})

# RSV timeline
rsv_timeline_df <- tryCatch({
  rsv_timeline_raw <- rsv_data$timeline$progression_by_date
  if (is.data.frame(rsv_timeline_raw)) {
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
  log_error(sprintf("Failed to extract RSV timeline data: %s", e$message), category = "data")
  data.frame(
    date = as.Date(character()), week_number = integer(),
    positivity_rate = numeric(), case_numbers = numeric(),
    hospitalization_rate = numeric(), pathogen = character(),
    stringsAsFactors = FALSE
  )
})

# COVID timeline
covid_timeline_df <- tryCatch({
  covid_timeline_raw <- covid_data$timeline$progression_by_date
  if (is.data.frame(covid_timeline_raw)) {
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
  log_error(sprintf("Failed to extract COVID timeline data: %s", e$message), category = "data")
  data.frame(
    date = as.Date(character()), week_number = integer(),
    positivity_rate = numeric(), case_numbers = numeric(),
    hospitalization_rate = numeric(), pathogen = character(),
    stringsAsFactors = FALSE
  )
})

# Build combined timeline
if (db_has_timeline) {
  combined_timeline_df <- db_timeline
  message(sprintf("Using database timeline: %d records from real CDC/WHO APIs", nrow(combined_timeline_df)))
} else {
  if (nrow(timeline_df) > 0) {
    timeline_df$pathogen <- "H3N2 (Influenza)"
  } else {
    timeline_df$pathogen <- character()
  }
  combined_timeline_df <- bind_rows(timeline_df, rsv_timeline_df, covid_timeline_df)
  message(sprintf("Using JSON fallback timeline: %d records", nrow(combined_timeline_df)))
}

# Apply intelligent fallback to fill gaps using real alternate sources
# Priority: CDC → Wastewater → FluSight → Delphi → State → Interpolate (last resort)
combined_timeline_df <- apply_timeline_fallback(combined_timeline_df)
if (attr(combined_timeline_df, "has_fallback") %||% FALSE) {
  fallback_pct <- attr(combined_timeline_df, "fallback_pct") %||% 0
  interpolated_pct <- attr(combined_timeline_df, "interpolated_pct") %||% 0
  source_desc <- attr(combined_timeline_df, "source_description") %||% ""

  if (interpolated_pct > 0 && interpolated_pct < fallback_pct) {
    message(sprintf("Timeline gaps filled: %.1f%% from alternate sources, %.1f%% interpolated",
                    fallback_pct - interpolated_pct, interpolated_pct))
  } else if (interpolated_pct > 0) {
    message(sprintf("Timeline gaps filled: %.1f%% interpolated (no alternate sources available)",
                    interpolated_pct))
  } else {
    message(sprintf("Timeline gaps filled: %.1f%% from alternate sources", fallback_pct))
  }
}

# =============================================================================
# THEME AND STYLING
# =============================================================================

# Pathogen color palette for charts
pathogen_colors <- c(
  "H3N2 (Influenza)" = "#E85D4C",
  "RSV" = "#2563EB",
  "COVID-19" = "#7C3AED",
  "H5N1 (Avian)" = "#DC2626",
  "Influenza A" = "#F97316",
  "Influenza B" = "#0891B2",
  "H3N2" = "#E85D4C",
  "COVID19" = "#7C3AED",
  "H5N1" = "#DC2626",
  "FLU_A" = "#F97316",
  "FLU_B" = "#0891B2"
)

# Clinical Premium Theme Configuration
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

message("RespiWatch global.R loaded successfully")
