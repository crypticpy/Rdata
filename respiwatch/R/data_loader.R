# ============================================================================
# Title: RespiWatch Data Loader
# Purpose: Load data from SQLite database with JSON fallback for Shiny app
# Output: Data frames ready for dashboard visualization
# ============================================================================

# Load required packages -------------------------------------------------------
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(jsonlite)

# Source database modules
source("R/db_schema.R")
source("R/db_operations.R")

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Check if database has sufficient data
#' @return TRUE if database has surveillance data
has_database_data <- function() {
  if (!file.exists(DB_PATH)) return(FALSE)

  tryCatch({
    conn <- get_db_connection()
    count <- dbGetQuery(conn, "SELECT COUNT(*) as n FROM surveillance_data")$n
    close_db_connection(conn)
    count > 0
  }, error = function(e) {
    FALSE
  })
}

#' Load surveillance data from database
#' @return Data frame with surveillance data for all pathogens
load_surveillance_from_db <- function() {
  conn <- get_db_connection()

  data <- tryCatch({
    dbGetQuery(conn, "
      SELECT
        sd.data_id,
        p.pathogen_code,
        p.pathogen_name,
        c.iso_code,
        c.country_name,
        r.region_name,
        sd.observation_date,
        sd.week_number,
        sd.year,
        sd.positivity_rate,
        sd.case_count,
        sd.estimated_cases,
        sd.hospitalizations,
        sd.hospitalization_rate,
        sd.icu_admissions,
        sd.deaths,
        sd.death_rate,
        sd.test_volume,
        sd.data_confidence,
        sd.fetch_timestamp
      FROM surveillance_data sd
      JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
      JOIN countries c ON sd.country_id = c.country_id
      LEFT JOIN regions r ON c.region_id = r.region_id
      ORDER BY sd.observation_date DESC, p.pathogen_code
    ")
  }, finally = {
    close_db_connection(conn)
  })

  # Convert date columns
  if (nrow(data) > 0) {
    data$observation_date <- as.Date(data$observation_date)
  }

  data
}

#' Load regional overview from database
#' @return Data frame with regional aggregated data
load_regional_from_db <- function() {
  conn <- get_db_connection()

  data <- tryCatch({
    dbGetQuery(conn, "
      SELECT
        ro.overview_id,
        p.pathogen_code,
        r.region_name,
        ro.observation_date,
        ro.status,
        ro.positivity_rate,
        ro.dominant_strain,
        ro.trend,
        ro.week_over_week_change,
        ro.confidence_level
      FROM regional_overview ro
      JOIN pathogens p ON ro.pathogen_id = p.pathogen_id
      JOIN regions r ON ro.region_id = r.region_id
      ORDER BY ro.observation_date DESC
    ")
  }, finally = {
    close_db_connection(conn)
  })

  if (nrow(data) > 0) {
    data$observation_date <- as.Date(data$observation_date)
  }

  data
}

#' Load anomalies from database
#' @return Data frame with anomaly detection data
load_anomalies_from_db <- function() {
  conn <- get_db_connection()

  data <- tryCatch({
    dbGetQuery(conn, "
      SELECT
        a.anomaly_id,
        at.type_name as pattern_type,
        p.pathogen_code,
        c.country_name,
        a.description,
        a.severity,
        a.detected_date,
        a.created_at,
        a.verification_status,
        CASE WHEN a.resolved_date IS NULL THEN 1 ELSE 0 END as is_active
      FROM anomalies a
      JOIN anomaly_types at ON a.type_id = at.type_id
      LEFT JOIN pathogens p ON a.pathogen_id = p.pathogen_id
      LEFT JOIN countries c ON a.country_id = c.country_id
      WHERE a.resolved_date IS NULL
      ORDER BY a.detected_date DESC
    ")
  }, finally = {
    close_db_connection(conn)
  })

  if (nrow(data) > 0) {
    data$detected_date <- as.Date(data$detected_date)
  }

  data
}

#' Get data freshness status
#' @return Data frame with last update times per source
load_freshness_status <- function() {
  if (!file.exists(DB_PATH)) {
    return(data.frame(
      source_code = character(),
      source_name = character(),
      last_fetch_timestamp = character(),
      fetch_status = character()
    ))
  }

  conn <- get_db_connection()

  data <- tryCatch({
    dbGetQuery(conn, "
      SELECT
        ds.source_code,
        ds.source_name,
        df.last_fetch_timestamp,
        df.records_fetched,
        df.fetch_status,
        df.next_scheduled_fetch
      FROM data_sources ds
      LEFT JOIN data_freshness df ON ds.source_id = df.source_id
      WHERE ds.is_active = 1
      ORDER BY ds.source_name
    ")
  }, finally = {
    close_db_connection(conn)
  })

  data
}

#' Load country populations from database
#' @return Data frame with iso_code, country_name, population
load_country_populations <- function() {
  conn <- get_db_connection()
  
  data <- tryCatch({
    dbGetQuery(conn, "
      SELECT iso_code, country_name, population
      FROM countries
      WHERE population IS NOT NULL
    ")
  }, finally = {
    close_db_connection(conn)
  })
  
  data
}

#' Load vaccination coverage from database
#' @return Data frame with vaccination coverage
load_vaccination_from_db <- function() {
  conn <- get_db_connection()
  
  data <- tryCatch({
    dbGetQuery(conn, "
      SELECT 
        vc.observation_date,
        c.country_name,
        p.pathogen_name,
        vc.age_group,
        vc.coverage_pct,
        vc.doses_administered
      FROM vaccine_coverage vc
      JOIN countries c ON vc.country_id = c.country_id
      JOIN vaccines v ON vc.vaccine_id = v.vaccine_id
      JOIN pathogens p ON v.pathogen_id = p.pathogen_id
      ORDER BY vc.observation_date DESC
    ")
  }, finally = {
    close_db_connection(conn)
  })
  
  if (nrow(data) > 0) {
    data$observation_date <- as.Date(data$observation_date)
  }
  
  data
}

# =============================================================================
# SHINY-READY DATA FUNCTIONS
# =============================================================================

#' Get countries data frame for map visualization
#' Combines database surveillance with country coordinates
#' @return Data frame ready for Leaflet map
get_countries_map_data <- function() {
  if (!has_database_data()) {
    return(NULL)
  }

  # Get latest surveillance data per country/pathogen
  surv_data <- load_surveillance_from_db()

  if (nrow(surv_data) == 0) return(NULL)

  # Get latest record per country (prioritize H3N2 for influenza display)
  countries_df <- surv_data |>
    filter(pathogen_code == "H3N2" | pathogen_code == "COVID19" | pathogen_code == "RSV") |>
    group_by(iso_code, country_name, pathogen_code) |>
    filter(observation_date == max(observation_date)) |>
    ungroup() |>
    select(iso_code, country_name, pathogen_code,
           positivity_rate, hospitalization_rate, case_count,
           data_confidence, observation_date)

  # Add coordinates
  country_coords <- data.frame(
    iso_code = c("USA", "GBR", "JPN", "AUS", "CAN", "CHN", "EEA"),
    lat = c(39.8, 54.0, 36.2, -25.3, 56.1, 35.9, 50.0),
    lng = c(-98.6, -2.0, 138.3, 133.8, -106.3, 104.2, 10.0),
    stringsAsFactors = FALSE
  )

  countries_df <- left_join(countries_df, country_coords, by = "iso_code")

  # Determine outbreak status based on positivity rate
  countries_df <- countries_df |>
    mutate(
      outbreak_status = case_when(
        positivity_rate >= 15 ~ "epidemic",
        positivity_rate >= 10 ~ "high",
        positivity_rate >= 5 ~ "moderate",
        positivity_rate >= 2 ~ "low",
        TRUE ~ "baseline"
      )
    )

  countries_df
}

#' Get timeline data for multi-pathogen chart
#' @return Data frame with time series for all pathogens
get_timeline_data <- function() {
  surv_data <- load_surveillance_from_db()

  if (nrow(surv_data) == 0) return(NULL)

  # Aggregate by date and pathogen
  timeline_df <- surv_data |>
    mutate(
      pathogen = case_when(
        pathogen_code == "H3N2" ~ "H3N2 (Influenza)",
        pathogen_code == "RSV" ~ "RSV",
        pathogen_code == "COVID19" ~ "COVID-19",
        TRUE ~ pathogen_name
      )
    ) |>
    group_by(observation_date, pathogen) |>
    summarize(
      positivity_rate = mean(positivity_rate, na.rm = TRUE),
      case_numbers = sum(coalesce(case_count, estimated_cases, 0), na.rm = TRUE),
      hospitalization_rate = mean(hospitalization_rate, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(date = observation_date) |>
    arrange(date)

  timeline_df
}

#' Get KPI summary statistics
#' @return Named list with KPI values
get_kpi_summary <- function() {
  surv_data <- load_surveillance_from_db()

  if (nrow(surv_data) == 0) {
    return(list(
      global_cases = 0,
      positivity_rate = 0,
      hospitalization_rate = 0,
      countries_reporting = 0,
      last_updated = Sys.Date()
    ))
  }

  # Get latest week's data
  latest_date <- max(surv_data$observation_date, na.rm = TRUE)
  latest_data <- surv_data |>
    filter(observation_date >= latest_date - 7)

  list(
    global_cases = sum(latest_data$case_count, na.rm = TRUE),
    positivity_rate = mean(latest_data$positivity_rate, na.rm = TRUE),
    hospitalization_rate = mean(latest_data$hospitalization_rate, na.rm = TRUE),
    countries_reporting = n_distinct(latest_data$iso_code),
    last_updated = latest_date
  )
}

# =============================================================================
# HYBRID LOADING: DATABASE WITH JSON FALLBACK
# =============================================================================

#' Load all data for dashboard with database preference
#' Falls back to JSON files if database is empty
#' @return List with all data frames needed by the app
load_dashboard_data <- function() {
  use_database <- has_database_data()

  if (use_database) {
    message("Loading data from SQLite database...")

    return(list(
      source = "database",
      surveillance = load_surveillance_from_db(),
      regional = load_regional_from_db(),
      anomalies = load_anomalies_from_db(),
      freshness = load_freshness_status(),
      countries_map = get_countries_map_data(),
      timeline = get_timeline_data(),
      kpi = get_kpi_summary()
    ))
  }

  message("Database empty or missing. Loading from JSON files...")

  # Fallback to JSON loading
  tryCatch({
    outbreak_data <- fromJSON("data/raw/h3n2_outbreak_tracker.json")
    rsv_data <- fromJSON("data/raw/rsv_tracker.json")
    covid_data <- fromJSON("data/raw/covid_tracker.json")

    list(
      source = "json",
      outbreak_data = outbreak_data,
      rsv_data = rsv_data,
      covid_data = covid_data
    )
  }, error = function(e) {
    warning("Failed to load JSON files: ", e$message)
    list(source = "empty")
  })
}

#' Check if data needs refresh
#' @return TRUE if data is stale (>1 hour old)
data_needs_refresh <- function() {
  status <- load_freshness_status()

  if (nrow(status) == 0) return(TRUE)

  # Check if any source was fetched in the last hour
  latest_fetch <- max(as.POSIXct(status$last_fetch_timestamp, na.rm = TRUE),
                      na.rm = TRUE)

  if (is.na(latest_fetch)) return(TRUE)

  hours_since <- as.numeric(difftime(Sys.time(), latest_fetch, units = "hours"))
  hours_since >= 1
}
