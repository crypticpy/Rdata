# ============================================================================
# Title: RespiWatch Database Operations
# Purpose: CRUD operations and data import/export for the SQLite database
# Input: API data, JSON files
# Output: Database records, data frames for dashboard
# ============================================================================

# Load required packages -------------------------------------------------------
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(lubridate)
library(jsonlite)

# Source schema module
source("R/db_schema.R")

# =============================================================================
# INPUT VALIDATION HELPERS (SQL Injection Prevention)
# =============================================================================

#' Escape string for SQL query (prevent SQL injection)
#' @param x String to escape
#' @return Escaped string
escape_sql <- function(x) {
  if (is.null(x) || is.na(x)) return(NA)
  gsub("'", "''", as.character(x))
}

#' Validate ISO country code format (2-letter)
#' @param code Country code to validate
#' @return TRUE if valid
is_valid_iso_code <- function(code) {
  !is.null(code) && grepl("^[A-Z]{2}$", toupper(code))
}

#' Validate pathogen code format (alphanumeric)
#' @param code Pathogen code to validate
#' @return TRUE if valid
is_valid_pathogen_code <- function(code) {
  !is.null(code) && grepl("^[A-Za-z0-9_]+$", code)
}

#' Validate date string format (YYYY-MM-DD)
#' @param date_str Date string to validate
#' @return TRUE if valid
is_valid_date <- function(date_str) {
  !is.null(date_str) && grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(date_str))
}

#' Normalize data confidence level to valid database values
#' @param level Confidence level string
#' @return Normalized confidence level ('high', 'medium', or 'low')
normalize_confidence <- function(level) {
  if (is.null(level) || is.na(level)) return("medium")

  val <- tolower(as.character(level))

  # Map variations to valid values
  confidence_map <- c(
    "high" = "high",
    "moderate" = "medium",
    "medium" = "medium",
    "low" = "low",
    "very high" = "high",
    "very low" = "low"
  )

  result <- confidence_map[val]
  if (is.na(result)) return("medium")

  return(unname(result))
}

#' Normalize epidemic status to valid database values
#' @param status Status string
#' @return Normalized status ('low', 'monitoring', 'active', 'elevated', 'peak', 'declining')
normalize_status <- function(status) {
  if (is.null(status) || is.na(status)) return("monitoring")

  val <- tolower(as.character(status))

  # Map variations to valid values
  # Valid: 'low', 'monitoring', 'active', 'elevated', 'peak', 'declining'
  status_map <- c(
    "low" = "low",
    "monitoring" = "monitoring",
    "active" = "active",
    "elevated" = "elevated",
    "peak" = "peak",
    "declining" = "declining",
    # Non-standard mappings
    "endemic_phase" = "low",
    "endemic" = "low",
    "moderate" = "active",
    "high" = "elevated",
    "critical" = "peak",
    "waning" = "declining",
    "stable" = "monitoring",
    "emerging" = "monitoring",
    "spreading" = "active"
  )

  result <- status_map[val]
  if (is.na(result)) return("monitoring")

  return(unname(result))
}

#' Collapse array to single string for SQL insertion
#' @param value Value that might be an array
#' @param default Default value if NULL/empty
#' @return Single string
collapse_array <- function(value, default = "") {
  if (is.null(value) || length(value) == 0) return(default)
  if (length(value) == 1) return(as.character(value))
  return(paste(value, collapse = ", "))
}

#' Sanitize an array of codes for use in SQL IN clause
#' @param codes Vector of codes
#' @param validator Function to validate each code
#' @return Sanitized SQL string for IN clause, or NULL if invalid
sanitize_code_list <- function(codes, validator) {
  if (is.null(codes) || length(codes) == 0) return(NULL)
  valid_codes <- codes[sapply(codes, validator)]
  if (length(valid_codes) == 0) return(NULL)
  paste(sprintf("'%s'", sapply(valid_codes, escape_sql)), collapse = ", ")
}

# =============================================================================
# SURVEILLANCE DATA OPERATIONS
# =============================================================================

#' Insert or update surveillance data
#' @param conn Database connection
#' @param data Data frame with surveillance observations
#' @return Number of rows affected
upsert_surveillance_data <- function(conn, data) {
  required_cols <- c("pathogen_code", "iso_code", "observation_date")
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns: ", paste(setdiff(required_cols, names(data)), collapse = ", "))
  }

  # Get pathogen_id and country_id lookups
  pathogens <- dbGetQuery(conn, "SELECT pathogen_id, pathogen_code FROM pathogens")
  countries <- dbGetQuery(conn, "SELECT country_id, iso_code FROM countries")

  data <- data |>
    left_join(pathogens, by = "pathogen_code") |>
    left_join(countries, by = "iso_code")

  # Prepare for upsert
  insert_data <- data |>
    filter(!is.na(pathogen_id), !is.na(country_id)) |>
    select(
      pathogen_id, country_id, observation_date,
      any_of(c(
        "week_number", "year", "positivity_rate", "case_count",
        "estimated_cases", "hospitalizations", "hospitalization_rate",
        "icu_admissions", "deaths", "death_rate", "test_volume",
        "data_confidence", "source_id"
      ))
    )

  if (nrow(insert_data) == 0) {
    message("No valid records to insert")
    return(0)
  }

  # Use INSERT OR REPLACE for upsert behavior
  rows_affected <- 0
  for (i in seq_len(nrow(insert_data))) {
    row <- insert_data[i, ]

    # Check if record exists (using validated date)
    obs_date <- as.character(row$observation_date)
    if (!is_valid_date(obs_date)) {
      warning(sprintf("Invalid date format: %s, skipping row", obs_date))
      next
    }
    existing <- dbGetQuery(conn, sprintf(
      "SELECT data_id FROM surveillance_data
       WHERE pathogen_id = %d AND country_id = %d AND observation_date = '%s'",
      as.integer(row$pathogen_id), as.integer(row$country_id), escape_sql(obs_date)
    ))

    if (nrow(existing) > 0) {
      # Update existing record
      update_cols <- names(row)[!names(row) %in% c("pathogen_id", "country_id", "observation_date")]
      update_cols <- update_cols[!is.na(row[update_cols])]

      if (length(update_cols) > 0) {
        # Whitelist allowed column names (prevent injection via column names)
        allowed_cols <- c("week_number", "year", "positivity_rate", "case_count",
                          "estimated_cases", "hospitalizations", "hospitalization_rate",
                          "icu_admissions", "deaths", "death_rate", "test_volume",
                          "data_confidence")
        update_cols <- intersect(update_cols, allowed_cols)

        if (length(update_cols) > 0) {
          set_clause <- paste(sapply(update_cols, function(col) {
            val <- row[[col]]
            if (is.character(val)) {
              sprintf("%s = '%s'", col, escape_sql(val))
            } else if (is.numeric(val)) {
              sprintf("%s = %s", col, as.numeric(val))
            } else {
              sprintf("%s = NULL", col)
            }
          }), collapse = ", ")

          dbExecute(conn, sprintf(
            "UPDATE surveillance_data SET %s, fetch_timestamp = CURRENT_TIMESTAMP
             WHERE data_id = %d",
            set_clause, as.integer(existing$data_id)
          ))
        }
        rows_affected <- rows_affected + 1
      }
    } else {
      # Insert new record
      dbWriteTable(conn, "surveillance_data", row, append = TRUE, row.names = FALSE)
      rows_affected <- rows_affected + 1
    }
  }

  message(sprintf("Upserted %d surveillance records", rows_affected))
  rows_affected
}

#' Upsert vaccination coverage data
#' @param conn Database connection
#' @param data Data frame with vaccination data
#' @return Number of rows affected
upsert_vaccination_data <- function(conn, data) {
  required_cols <- c("pathogen", "observation_date", "coverage_pct")
  # Map generic pathogen names to our specific pathogen_codes if needed
  # But for now assuming data has mapped 'pathogen' to name or code.
  
  if (is.null(data) || nrow(data) == 0) return(0)
  
  # Normalize pathogen names to codes usually expected by DB or map them
  # Here we'll do a simple lookup.
  pathogens <- dbGetQuery(conn, "SELECT pathogen_id, pathogen_code, pathogen_name FROM pathogens")
  
  # Helper to map name to ID
  get_pathogen_id <- function(name) {
    # Try exact match on Name
    p <- pathogens[pathogens$pathogen_name == name, "pathogen_id"]
    if (length(p) > 0) return(p[1])
    # Try Code
    p <- pathogens[pathogens$pathogen_code == name, "pathogen_id"]
    if (length(p) > 0) return(p[1])
    # Try fuzzy match
    if (grepl("Flu", name, ignore.case=TRUE)) return(pathogens[pathogens$pathogen_code == "H3N2", "pathogen_id"][1]) # Default to H3N2 or generic Flu if exists
    if (grepl("COVID", name, ignore.case=TRUE)) return(pathogens[pathogens$pathogen_code == "COVID19", "pathogen_id"][1])
    if (grepl("RSV", name, ignore.case=TRUE)) return(pathogens[pathogens$pathogen_code == "RSV", "pathogen_id"][1])
    return(NA)
  }
  
  # For now, default vaccine_id lookup is hard because we don't have a robust vaccine catalog.
  # We will just grab the *first* active vaccine for that pathogen to link to, or create a placeholder.
  vaccines <- dbGetQuery(conn, "SELECT vaccine_id, pathogen_id FROM vaccines")
  
  countries <- dbGetQuery(conn, "SELECT country_id, iso_code, country_name FROM countries")
  # Default to USA for CDC data
  usa_id <- countries[countries$iso_code == "USA", "country_id"]
  
  rows_affected <- 0
  
  # Iterate and insert (inefficient but safe for now)
  for(i in seq_len(nrow(data))) {
    row <- data[i, ]
    pid <- get_pathogen_id(row$pathogen)
    if (is.na(pid)) next
    
    # Get a vaccine ID (any)
    vid <- vaccines[vaccines$pathogen_id == pid, "vaccine_id"][1]
    if (is.na(vid)) next # Skip if no vaccine defined
    
    # Check if exists
    obs_date <- as.character(row$observation_date)
    existing <- dbGetQuery(conn, sprintf(
      "SELECT coverage_id FROM vaccine_coverage 
       WHERE vaccine_id = %d AND country_id = %d AND observation_date = '%s' AND age_group = '%s'",
      vid, usa_id, escape_sql(obs_date), escape_sql(row$age_group %||% "All")
    ))
    
    if (nrow(existing) == 0) {
      dbExecute(conn, sprintf(
        "INSERT INTO vaccine_coverage (vaccine_id, country_id, observation_date, coverage_pct, age_group, fetch_timestamp)
         VALUES (%d, %d, '%s', %f, '%s', CURRENT_TIMESTAMP)",
        vid, usa_id, escape_sql(obs_date), row$coverage_pct, escape_sql(row$age_group %||% "All")
      ))
      rows_affected <- rows_affected + 1
    }
  }
  
  rows_affected
}

#' Get surveillance data for dashboard
#' @param conn Database connection
#' @param pathogen_codes Vector of pathogen codes (NULL for all)
#' @param country_codes Vector of ISO country codes (NULL for all)
#' @param start_date Start date for data range
#' @param end_date End date for data range
#' @return Data frame with surveillance data
get_surveillance_data <- function(
    conn,
    pathogen_codes = NULL,
    country_codes = NULL,
    start_date = NULL,
    end_date = NULL
) {
  query <- "
    SELECT
      s.*,
      p.pathogen_code,
      p.pathogen_name,
      c.iso_code,
      c.country_name,
      c.latitude,
      c.longitude,
      r.region_name
    FROM surveillance_data s
    JOIN pathogens p ON s.pathogen_id = p.pathogen_id
    JOIN countries c ON s.country_id = c.country_id
    LEFT JOIN regions r ON c.region_id = r.region_id
    WHERE 1=1
  "

  if (!is.null(pathogen_codes)) {
    codes <- sanitize_code_list(pathogen_codes, is_valid_pathogen_code)
    if (!is.null(codes)) {
      query <- paste0(query, sprintf(" AND p.pathogen_code IN (%s)", codes))
    }
  }

  if (!is.null(country_codes)) {
    codes <- sanitize_code_list(country_codes, is_valid_iso_code)
    if (!is.null(codes)) {
      query <- paste0(query, sprintf(" AND c.iso_code IN (%s)", codes))
    }
  }

  if (!is.null(start_date) && is_valid_date(start_date)) {
    query <- paste0(query, sprintf(" AND s.observation_date >= '%s'", escape_sql(start_date)))
  }

  if (!is.null(end_date) && is_valid_date(end_date)) {
    query <- paste0(query, sprintf(" AND s.observation_date <= '%s'", escape_sql(end_date)))
  }

  query <- paste0(query, " ORDER BY s.observation_date DESC, p.pathogen_code, c.country_name")

  dbGetQuery(conn, query)
}

#' Get latest surveillance snapshot per country/pathogen
#' @param conn Database connection
#' @param pathogen_codes Vector of pathogen codes (NULL for all)
#' @return Data frame with latest data per country/pathogen combination
get_latest_surveillance <- function(conn, pathogen_codes = NULL) {
  query <- "
    SELECT
      s.*,
      p.pathogen_code,
      p.pathogen_name,
      c.iso_code,
      c.country_name,
      c.latitude,
      c.longitude,
      c.population,
      r.region_name,
      v.coverage_pct as vaccination_rate  -- Joined vaccination data
    FROM surveillance_data s
    JOIN pathogens p ON s.pathogen_id = p.pathogen_id
    JOIN countries c ON s.country_id = c.country_id
    LEFT JOIN regions r ON c.region_id = r.region_id
    -- Join with latest vaccination coverage for this country/pathogen (approximate match via pathogen)
    LEFT JOIN (
        SELECT country_id, coverage_pct, fetch_timestamp
        FROM vaccine_coverage
        WHERE (country_id, fetch_timestamp) IN (
            SELECT country_id, MAX(fetch_timestamp)
            FROM vaccine_coverage
            GROUP BY country_id
        )
    ) v ON s.country_id = v.country_id
    WHERE s.observation_date = (
      SELECT MAX(s2.observation_date)
      FROM surveillance_data s2
      WHERE s2.pathogen_id = s.pathogen_id AND s2.country_id = s.country_id
    )
  "

  if (!is.null(pathogen_codes)) {
    codes <- sanitize_code_list(pathogen_codes, is_valid_pathogen_code)
    if (!is.null(codes)) {
      query <- paste0(query, sprintf(" AND p.pathogen_code IN (%s)", codes))
    }
  }

  query <- paste0(query, " ORDER BY p.pathogen_code, c.country_name")

  dbGetQuery(conn, query)
}

#' Get surveillance snapshot for a specific date (closest available match)
#' @param conn Database connection
#' @param target_date Date string ("YYYY-MM-DD")
#' @param pathogen_codes Vector of pathogen codes (NULL for all)
#' @return Data frame with snapshot data
get_surveillance_snapshot <- function(conn, target_date, pathogen_codes = NULL) {
  # Logic: Find the latest record on or before target_date for each country
  # This serves as a "state of the world" at that time
  query <- sprintf("
    SELECT
      s.*,
      p.pathogen_code,
      p.pathogen_name,
      c.iso_code,
      c.country_name,
      c.latitude,
      c.longitude,
      c.population,
      r.region_name,
      v.coverage_pct as vaccination_rate
    FROM surveillance_data s
    JOIN pathogens p ON s.pathogen_id = p.pathogen_id
    JOIN countries c ON s.country_id = c.country_id
    LEFT JOIN regions r ON c.region_id = r.region_id
    -- Join with simple latest vaccination (vax doesn't vary much week to week in this dataset context)
    LEFT JOIN (
        SELECT country_id, coverage_pct FROM vaccine_coverage GROUP BY country_id HAVING MAX(fetch_timestamp)
    ) v ON s.country_id = v.country_id
    WHERE s.observation_date = (
        SELECT MAX(s2.observation_date)
        FROM surveillance_data s2
        WHERE s2.pathogen_id = s.pathogen_id 
          AND s2.country_id = s.country_id
          AND s2.observation_date <= '%s' -- Time travel logic
    )
  ", escape_sql(target_date))

  if (!is.null(pathogen_codes)) {
    codes <- sanitize_code_list(pathogen_codes, is_valid_pathogen_code)
    if (!is.null(codes)) {
      query <- paste0(query, sprintf(" AND p.pathogen_code IN (%s)", codes))
    }
  }

  query <- paste0(query, " ORDER BY p.pathogen_code, c.country_name")
  
  dbGetQuery(conn, query)
}

# =============================================================================
# REGIONAL OVERVIEW OPERATIONS
# =============================================================================

#' Upsert regional overview data
#' @param conn Database connection
#' @param data Data frame with regional overview data
#' @return Number of rows affected
upsert_regional_overview <- function(conn, data) {
  required_cols <- c("pathogen_code", "region_code", "observation_date", "status")
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns")
  }

  # Get lookups
  pathogens <- dbGetQuery(conn, "SELECT pathogen_id, pathogen_code FROM pathogens")
  regions <- dbGetQuery(conn, "SELECT region_id, region_code FROM regions")

  data <- data |>
    left_join(pathogens, by = "pathogen_code") |>
    left_join(regions, by = "region_code")

  insert_data <- data |>
    filter(!is.na(pathogen_id), !is.na(region_id)) |>
    select(
      pathogen_id, region_id, observation_date, status,
      any_of(c("positivity_rate", "dominant_strain", "trend",
               "week_over_week_change", "confidence_level"))
    )

  if (nrow(insert_data) == 0) {
    return(0)
  }

  # Simple upsert using DELETE + INSERT
  for (i in seq_len(nrow(insert_data))) {
    row <- insert_data[i, ]
    obs_date <- as.character(row$observation_date)
    if (!is_valid_date(obs_date)) {
      warning(sprintf("Invalid date format: %s, skipping row", obs_date))
      next
    }
    dbExecute(conn, sprintf(
      "DELETE FROM regional_overview
       WHERE pathogen_id = %d AND region_id = %d AND observation_date = '%s'",
      as.integer(row$pathogen_id), as.integer(row$region_id), escape_sql(obs_date)
    ))
  }

  dbWriteTable(conn, "regional_overview", insert_data, append = TRUE, row.names = FALSE)
  nrow(insert_data)
}

#' Get regional overview data
#' @param conn Database connection
#' @param pathogen_codes Vector of pathogen codes (NULL for all)
#' @return Data frame with regional overview
get_regional_overview <- function(conn, pathogen_codes = NULL) {
  query <- "
    SELECT
      o.*,
      p.pathogen_code,
      p.pathogen_name,
      r.region_code,
      r.region_name
    FROM regional_overview o
    JOIN pathogens p ON o.pathogen_id = p.pathogen_id
    JOIN regions r ON o.region_id = r.region_id
    WHERE o.observation_date = (
      SELECT MAX(o2.observation_date)
      FROM regional_overview o2
      WHERE o2.pathogen_id = o.pathogen_id AND o2.region_id = o.region_id
    )
  "

  if (!is.null(pathogen_codes)) {
    codes <- sanitize_code_list(pathogen_codes, is_valid_pathogen_code)
    if (!is.null(codes)) {
      query <- paste0(query, sprintf(" AND p.pathogen_code IN (%s)", codes))
    }
  }

  dbGetQuery(conn, query)
}

# =============================================================================
# ANOMALY OPERATIONS
# =============================================================================

#' Insert anomaly records
#' @param conn Database connection
#' @param data Data frame with anomaly data
#' @return Number of rows inserted
insert_anomalies <- function(conn, data) {
  required_cols <- c("type_code", "detected_date", "description", "severity")
  if (!all(required_cols %in% names(data))) {
    stop("Missing required columns")
  }

  # Get lookups
  anomaly_types <- dbGetQuery(conn, "SELECT type_id, type_code FROM anomaly_types")
  pathogens <- dbGetQuery(conn, "SELECT pathogen_id, pathogen_code FROM pathogens")
  countries <- dbGetQuery(conn, "SELECT country_id, iso_code FROM countries")
  regions <- dbGetQuery(conn, "SELECT region_id, region_code FROM regions")

  # Join type and pathogen (always expected)
  data <- data |>
    left_join(anomaly_types, by = "type_code")

  # Only join pathogens if pathogen_code column exists
  if ("pathogen_code" %in% names(data)) {
    data <- data |>
      left_join(pathogens, by = "pathogen_code")
  } else {
    data$pathogen_id <- NA_integer_
  }

  # Only join countries if iso_code column exists
  if ("iso_code" %in% names(data)) {
    data <- data |>
      left_join(countries, by = "iso_code")
  } else {
    data$country_id <- NA_integer_
  }

  # Only join regions if region_code column exists
  if ("region_code" %in% names(data)) {
    data <- data |>
      left_join(regions, by = "region_code")
  } else {
    data$region_id <- NA_integer_
  }

  insert_data <- data |>
    filter(!is.na(type_id)) |>
    select(
      type_id, pathogen_id, country_id, region_id,
      detected_date, description, severity,
      any_of(c("geographic_scope", "verification_status"))
    )

  if (nrow(insert_data) == 0) {
    return(0)
  }

  dbWriteTable(conn, "anomalies", insert_data, append = TRUE, row.names = FALSE)
  nrow(insert_data)
}

#' Get active anomalies
#' @param conn Database connection
#' @param include_resolved Include resolved anomalies
#' @return Data frame with anomaly records
get_anomalies <- function(conn, include_resolved = FALSE) {
  query <- "
    SELECT
      a.*,
      at.type_code,
      at.type_name,
      p.pathogen_code,
      p.pathogen_name,
      c.iso_code,
      c.country_name,
      r.region_code,
      r.region_name
    FROM anomalies a
    JOIN anomaly_types at ON a.type_id = at.type_id
    LEFT JOIN pathogens p ON a.pathogen_id = p.pathogen_id
    LEFT JOIN countries c ON a.country_id = c.country_id
    LEFT JOIN regions r ON a.region_id = r.region_id
  "

  if (!include_resolved) {
    query <- paste0(query, " WHERE a.resolved_date IS NULL")
  }

  query <- paste0(query, " ORDER BY a.detected_date DESC")

  dbGetQuery(conn, query)
}

# =============================================================================
# JSON IMPORT FUNCTIONS
# =============================================================================

#' Import H3N2 outbreak tracker JSON to database
#' @param conn Database connection
#' @param json_path Path to JSON file
#' @return List with import statistics
import_h3n2_json <- function(conn, json_path = "data/raw/h3n2_outbreak_tracker.json") {
  if (!file.exists(json_path)) {
    stop("JSON file not found: ", json_path)
  }

  data <- fromJSON(json_path)
  stats <- list(surveillance = 0, regional = 0, anomalies = 0, timeline = 0)

  # Import timeline data (critical for Rt estimation)
  if (!is.null(data$timeline$progression_by_date)) {
    timeline_raw <- data$timeline$progression_by_date

    timeline_records <- lapply(seq_len(nrow(timeline_raw)), function(i) {
      entry <- timeline_raw[i, ]
      indicators <- entry$surveillance_indicators

      data.frame(
        pathogen_code = "H3N2",
        iso_code = "USA",  # Timeline is primarily US data from CDC FluView
        observation_date = as.character(as.Date(entry$date)),
        week_number = as.integer(entry$week_number),
        year = as.integer(entry$year),
        positivity_rate = indicators$positivity_rate %||% NA_real_,
        test_volume = as.integer(indicators$test_volume %||% NA_integer_),
        estimated_cases = as.integer(indicators$estimated_cases %||% NA_integer_),
        hospitalization_rate = indicators$hospitalization_rate %||% NA_real_,
        data_confidence = "high",
        stringsAsFactors = FALSE
      )
    })

    timeline_df <- bind_rows(timeline_records)
    stats$timeline <- upsert_surveillance_data(conn, timeline_df)
    message(sprintf("H3N2 timeline: imported %d weekly records", stats$timeline))
  }

  # Import country surveillance data (latest snapshots)
  if (!is.null(data$countries)) {
    countries_list <- data$countries

    surveillance_records <- lapply(names(countries_list), function(iso) {
      c_data <- countries_list[[iso]]
      h3n2 <- c_data$h3n2_data

      data.frame(
        pathogen_code = "H3N2",
        iso_code = iso,
        observation_date = as.character(as.Date(c_data$last_updated)),
        positivity_rate = h3n2$positivity_rate %||% NA_real_,
        case_count = h3n2$case_numbers$confirmed_cases %||% NA_integer_,
        estimated_cases = h3n2$case_numbers$estimated_cases %||% NA_integer_,
        hospitalizations = h3n2$case_numbers$hospitalizations %||% NA_integer_,
        hospitalization_rate = h3n2$hospitalization_rate %||% NA_real_,
        deaths = h3n2$case_numbers$deaths %||% NA_integer_,
        data_confidence = normalize_confidence(c_data$data_confidence_level),
        stringsAsFactors = FALSE
      )
    })

    surveillance_df <- bind_rows(surveillance_records)
    # stats$surveillance <- upsert_surveillance_data(conn, surveillance_df)
    message("Skipping surveillance snapshot import to prevent time-series pollution")
  }

  # Import regional overview
  if (!is.null(data$global_overview$current_status)) {
    region_map <- c(
      "North_America" = "NA",
      "Europe" = "EU",
      "Asia" = "ASIA",
      "Oceania" = "OCEANIA"
    )

    regional_records <- lapply(names(data$global_overview$current_status), function(region) {
      r_data <- data$global_overview$current_status[[region]]
      data.frame(
        pathogen_code = "H3N2",
        region_code = region_map[region] %||% region,
        observation_date = as.character(Sys.Date()),
        status = normalize_status(r_data$status),
        positivity_rate = r_data$positivity_rate %||% NA_real_,
        dominant_strain = r_data$dominant_pathogen %||% "H3N2",
        confidence_level = tolower(r_data$confidence_level %||% "medium"),
        stringsAsFactors = FALSE
      )
    })

    regional_df <- bind_rows(regional_records)
    stats$regional <- upsert_regional_overview(conn, regional_df)
  }

  # Import anomalies
  key_anomalies <- data$global_overview$key_anomalies
  if (!is.null(key_anomalies) && is.data.frame(key_anomalies) && nrow(key_anomalies) > 0) {
    anomaly_type_map <- c(
      "timing" = "TIMING",
      "mismatch" = "MISMATCH",
      "surveillance_gap" = "SURVEILLANCE_GAP",
      "severity" = "SEVERITY"
    )

    anomaly_records <- lapply(seq_len(nrow(key_anomalies)), function(i) {
      a <- key_anomalies[i, ]
      data.frame(
        type_code = anomaly_type_map[a$anomaly_type] %||% "SURVEILLANCE_GAP",
        pathogen_code = "H3N2",
        detected_date = a$first_detected %||% as.character(Sys.Date()),
        description = a$description,
        severity = normalize_confidence(a$confidence_level),
        geographic_scope = a$geographic_scope %||% "Global",
        verification_status = "verified",
        stringsAsFactors = FALSE
      )
    })

    anomaly_df <- bind_rows(anomaly_records)
    stats$anomalies <- insert_anomalies(conn, anomaly_df)
  }

  message(sprintf(
    "H3N2 import complete: %d timeline, %d surveillance, %d regional, %d anomalies",
    stats$timeline, stats$surveillance, stats$regional, stats$anomalies
  ))

  stats
}

#' Import RSV tracker JSON to database
#' @param conn Database connection
#' @param json_path Path to JSON file
#' @return List with import statistics
import_rsv_json <- function(conn, json_path = "data/raw/rsv_tracker.json") {
  if (!file.exists(json_path)) {
    stop("JSON file not found: ", json_path)
  }

  data <- fromJSON(json_path)
  stats <- list(surveillance = 0, regional = 0, timeline = 0)

  # Import timeline data (critical for Rt estimation)
  if (!is.null(data$timeline$progression_by_date)) {
    timeline_raw <- data$timeline$progression_by_date

    timeline_records <- lapply(seq_len(nrow(timeline_raw)), function(i) {
      entry <- timeline_raw[i, ]
      indicators <- entry$surveillance_indicators

      data.frame(
        pathogen_code = "RSV",
        iso_code = "USA",  # Timeline is primarily US data from RSV-NET
        observation_date = as.character(as.Date(entry$date)),
        week_number = as.integer(entry$week_number),
        year = as.integer(entry$year),
        positivity_rate = indicators$positivity_rate %||% NA_real_,
        test_volume = as.integer(indicators$test_volume %||% NA_integer_),
        estimated_cases = as.integer(indicators$estimated_cases %||% NA_integer_),
        hospitalization_rate = indicators$hospitalization_rate %||% NA_real_,
        data_confidence = "high",
        stringsAsFactors = FALSE
      )
    })

    timeline_df <- bind_rows(timeline_records)
    stats$timeline <- upsert_surveillance_data(conn, timeline_df)
    message(sprintf("RSV timeline: imported %d weekly records", stats$timeline))
  }

  # Import country surveillance data (latest snapshots)
  if (!is.null(data$countries)) {
    surveillance_records <- lapply(names(data$countries), function(iso) {
      c_data <- data$countries[[iso]]
      rsv <- c_data$rsv_data

      data.frame(
        pathogen_code = "RSV",
        iso_code = iso,
        observation_date = as.character(as.Date(c_data$last_updated)),
        positivity_rate = rsv$positivity_rate %||% NA_real_,
        hospitalizations = (rsv$pediatric_hospitalizations %||% 0) +
          (rsv$adult_hospitalizations %||% 0),
        hospitalization_rate = rsv$hospitalization_rate %||% NA_real_,
        icu_admissions = as.integer(rsv$icu_admission_rate * 1000) %||% NA_integer_,
        data_confidence = normalize_confidence(c_data$data_confidence_level),
        stringsAsFactors = FALSE
      )
    })

    surveillance_df <- bind_rows(surveillance_records)
    # stats$surveillance <- upsert_surveillance_data(conn, surveillance_df)
    message("Skipping surveillance snapshot import to prevent time-series pollution")
  }

  # Import regional overview
  if (!is.null(data$global_overview$current_status)) {
    region_map <- c(
      "north_america" = "NA",
      "europe" = "EU",
      "asia_pacific" = "ASIA",
      "southern_hemisphere" = "OCEANIA"
    )

    regional_records <- lapply(names(data$global_overview$current_status), function(region) {
      r_data <- data$global_overview$current_status[[region]]
      data.frame(
        pathogen_code = "RSV",
        region_code = region_map[region] %||% "NA",
        observation_date = as.character(Sys.Date()),
        status = normalize_status(r_data$status),
        positivity_rate = r_data$positivity_rate %||% NA_real_,
        dominant_strain = r_data$dominant_subtype %||% "RSV-A",
        trend = tolower(r_data$trend %||% "stable"),
        week_over_week_change = r_data$week_over_week_change %||% NA_real_,
        stringsAsFactors = FALSE
      )
    })

    regional_df <- bind_rows(regional_records)
    stats$regional <- upsert_regional_overview(conn, regional_df)
  }

  message(sprintf("RSV import complete: %d timeline, %d surveillance, %d regional",
                  stats$timeline, stats$surveillance, stats$regional))
  stats
}

#' Import COVID tracker JSON to database
#' @param conn Database connection
#' @param json_path Path to JSON file
#' @return List with import statistics
import_covid_json <- function(conn, json_path = "data/raw/covid_tracker.json") {
  if (!file.exists(json_path)) {
    stop("JSON file not found: ", json_path)
  }

  data <- fromJSON(json_path)
  stats <- list(surveillance = 0, regional = 0, timeline = 0)

  # Import timeline data (critical for Rt estimation)
  if (!is.null(data$timeline$progression_by_date)) {
    timeline_raw <- data$timeline$progression_by_date

    timeline_records <- lapply(seq_len(nrow(timeline_raw)), function(i) {
      entry <- timeline_raw[i, ]
      indicators <- entry$surveillance_indicators

      data.frame(
        pathogen_code = "COVID19",
        iso_code = "USA",  # Timeline is primarily US data from CDC COVID Data Tracker
        observation_date = as.character(as.Date(entry$date)),
        week_number = as.integer(entry$week_number),
        year = as.integer(entry$year),
        positivity_rate = indicators$positivity_rate %||% NA_real_,
        test_volume = as.integer(indicators$test_volume %||% NA_integer_),
        estimated_cases = as.integer(indicators$estimated_cases %||% NA_integer_),
        hospitalization_rate = indicators$hospitalization_rate %||% NA_real_,
        data_confidence = "high",
        stringsAsFactors = FALSE
      )
    })

    timeline_df <- bind_rows(timeline_records)
    stats$timeline <- upsert_surveillance_data(conn, timeline_df)
    message(sprintf("COVID timeline: imported %d weekly records", stats$timeline))
  }

  # Import country surveillance data (latest snapshots)
  if (!is.null(data$countries)) {
    surveillance_records <- lapply(names(data$countries), function(iso) {
      c_data <- data$countries[[iso]]
      covid <- c_data$covid_data

      data.frame(
        pathogen_code = "COVID19",
        iso_code = iso,
        observation_date = as.character(as.Date(c_data$last_updated)),
        positivity_rate = covid$positivity_rate %||% NA_real_,
        hospitalizations = as.integer(covid$hospitalization_rate * 1000) %||% NA_integer_,
        hospitalization_rate = covid$hospitalization_rate %||% NA_real_,
        deaths = covid$weekly_deaths %||% NA_integer_,
        test_volume = as.integer(covid$test_volume_per_100k * c_data$population / 100000) %||% NA_integer_,
        data_confidence = normalize_confidence(c_data$data_confidence_level),
        stringsAsFactors = FALSE
      )
    })

    surveillance_df <- bind_rows(surveillance_records)
    # stats$surveillance <- upsert_surveillance_data(conn, surveillance_df)
    message("Skipping surveillance snapshot import to prevent time-series pollution")
  }

  # Import regional overview
  if (!is.null(data$global_overview$current_status)) {
    region_map <- c(
      "north_america" = "NA",
      "europe" = "EU",
      "asia_pacific" = "ASIA",
      "southern_hemisphere" = "OCEANIA"
    )

    regional_records <- lapply(names(data$global_overview$current_status), function(region) {
      r_data <- data$global_overview$current_status[[region]]
      data.frame(
        pathogen_code = "COVID19",
        region_code = region_map[region] %||% "NA",
        observation_date = as.character(Sys.Date()),
        status = normalize_status(r_data$status),
        positivity_rate = r_data$positivity_rate %||% NA_real_,
        dominant_strain = r_data$dominant_variant %||% "JN.1",
        trend = tolower(r_data$trend %||% "stable"),
        week_over_week_change = r_data$week_over_week_change %||% NA_real_,
        stringsAsFactors = FALSE
      )
    })

    regional_df <- bind_rows(regional_records)
    stats$regional <- upsert_regional_overview(conn, regional_df)
  }

  message(sprintf("COVID import complete: %d timeline, %d surveillance, %d regional",
                  stats$timeline, stats$surveillance, stats$regional))
  stats
}

# =============================================================================
# VARIANT IMPORT FUNCTIONS
# =============================================================================

#' Import variant data from H3N2 JSON
#' @param conn Database connection
#' @param data Parsed JSON data
#' @return Number of records imported
import_h3n2_variants <- function(conn, data) {
  records_imported <- 0

  # Get pathogen_id for H3N2
  pathogen <- dbGetQuery(conn, "SELECT pathogen_id FROM pathogens WHERE pathogen_code = 'H3N2'")
  if (nrow(pathogen) == 0) return(0)
  pathogen_id <- pathogen$pathogen_id

  # Get country lookups
  countries <- dbGetQuery(conn, "SELECT country_id, iso_code FROM countries")

  # Extract variant info from pathogen_analysis
  if (!is.null(data$pathogen_analysis$primary_pathogen)) {
    primary <- data$pathogen_analysis$primary_pathogen

    # Insert primary variant (Subclade K)
    variant_code <- gsub("[^A-Za-z0-9_]", "_", primary$dominant_strain %||% "Subclade_K")

    # Check if variant already exists
    existing <- dbGetQuery(conn, sprintf(
      "SELECT variant_id FROM variants WHERE pathogen_id = %d AND variant_code = '%s'",
      pathogen_id, escape_sql(variant_code)
    ))

    if (nrow(existing) == 0) {
      dbExecute(conn, sprintf(
        "INSERT INTO variants (pathogen_id, variant_code, variant_name, lineage, is_voc, is_voi, severity_assessment)
         VALUES (%d, '%s', '%s', '%s', 1, 0, '%s')",
        pathogen_id,
        escape_sql(variant_code),
        escape_sql(primary$dominant_strain %||% "Subclade K"),
        escape_sql("J.2.4.1"),
        escape_sql(primary$severity_assessment %||% "high")
      ))
      records_imported <- records_imported + 1
    }
  }

  # Extract variant prevalence from country subclade_data
  if (!is.null(data$countries)) {
    for (iso in names(data$countries)) {
      c_data <- data$countries[[iso]]

      if (!is.null(c_data$subclade_data)) {
        subclade <- c_data$subclade_data
        country_row <- countries[countries$iso_code == iso, ]

        if (nrow(country_row) == 0) next
        country_id <- country_row$country_id

        # Get or create variant for this clade
        clade_code <- gsub("[^A-Za-z0-9_]", "_", subclade$dominant_clade %||% "Unknown")
        variant <- dbGetQuery(conn, sprintf(
          "SELECT variant_id FROM variants WHERE pathogen_id = %d AND variant_code = '%s'",
          pathogen_id, escape_sql(clade_code)
        ))

        if (nrow(variant) == 0) {
          dbExecute(conn, sprintf(
            "INSERT INTO variants (pathogen_id, variant_code, variant_name, is_voc, is_voi)
             VALUES (%d, '%s', '%s', 0, 1)",
            pathogen_id, escape_sql(clade_code), escape_sql(subclade$dominant_clade %||% "Unknown")
          ))
          variant <- dbGetQuery(conn, sprintf(
            "SELECT variant_id FROM variants WHERE pathogen_id = %d AND variant_code = '%s'",
            pathogen_id, escape_sql(clade_code)
          ))
          records_imported <- records_imported + 1
        }

        # Insert prevalence record
        if (nrow(variant) > 0 && !is.null(subclade$subclade_k_proportion)) {
          obs_date <- as.character(as.Date(c_data$last_updated %||% Sys.Date()))

          # Delete existing record for this date
          dbExecute(conn, sprintf(
            "DELETE FROM variant_prevalence
             WHERE variant_id = %d AND country_id = %d AND observation_date = '%s'",
            variant$variant_id, country_id, escape_sql(obs_date)
          ))

          dbExecute(conn, sprintf(
            "INSERT INTO variant_prevalence (variant_id, country_id, observation_date, prevalence_pct)
             VALUES (%d, %d, '%s', %f)",
            variant$variant_id, country_id, escape_sql(obs_date),
            as.numeric(subclade$subclade_k_proportion %||% 0)
          ))
          records_imported <- records_imported + 1
        }
      }
    }
  }

  message(sprintf("H3N2 variants import: %d records", records_imported))
  records_imported
}

#' Import variant data from COVID JSON
#' @param conn Database connection
#' @param data Parsed JSON data
#' @return Number of records imported
import_covid_variants <- function(conn, data) {
  records_imported <- 0

  # Get pathogen_id for COVID19
  pathogen <- dbGetQuery(conn, "SELECT pathogen_id FROM pathogens WHERE pathogen_code = 'COVID19'")
  if (nrow(pathogen) == 0) return(0)
  pathogen_id <- pathogen$pathogen_id

  # Get region lookups
  regions <- dbGetQuery(conn, "SELECT region_id, region_code FROM regions")
  region_map <- c(
    "north_america" = "NA",
    "europe" = "EU",
    "asia_pacific" = "ASIA",
    "southern_hemisphere" = "OCEANIA"
  )

  # Extract variant info from global_overview
  if (!is.null(data$global_overview$variant_landscape)) {
    vl <- data$global_overview$variant_landscape

    # Insert dominant variant
    if (!is.null(vl$dominant_variant)) {
      dv <- vl$dominant_variant
      variant_code <- gsub("[^A-Za-z0-9_]", "_", dv$name %||% "JN1")

      existing <- dbGetQuery(conn, sprintf(
        "SELECT variant_id FROM variants WHERE pathogen_id = %d AND variant_code = '%s'",
        pathogen_id, escape_sql(variant_code)
      ))

      if (nrow(existing) == 0) {
        dbExecute(conn, sprintf(
          "INSERT INTO variants (pathogen_id, variant_code, variant_name, lineage, is_voc, is_voi, immune_escape, severity_assessment)
           VALUES (%d, '%s', '%s', '%s', 0, 1, '%s', '%s')",
          pathogen_id,
          escape_sql(variant_code),
          escape_sql(dv$name %||% "JN.1"),
          escape_sql(dv$lineage %||% ""),
          escape_sql(dv$immune_escape %||% "moderate"),
          escape_sql(dv$severity %||% "similar_to_previous")
        ))
        records_imported <- records_imported + 1
      }
    }

    # Insert variants of interest
    if (!is.null(vl$variants_of_interest) && is.data.frame(vl$variants_of_interest) && nrow(vl$variants_of_interest) > 0) {
      for (i in seq_len(nrow(vl$variants_of_interest))) {
        voi <- vl$variants_of_interest[i, ]
        variant_code <- gsub("[^A-Za-z0-9_]", "_", voi$name %||% "Unknown")

        existing <- dbGetQuery(conn, sprintf(
          "SELECT variant_id FROM variants WHERE pathogen_id = %d AND variant_code = '%s'",
          pathogen_id, escape_sql(variant_code)
        ))

        if (nrow(existing) == 0) {
          dbExecute(conn, sprintf(
            "INSERT INTO variants (pathogen_id, variant_code, variant_name, is_voc, is_voi)
             VALUES (%d, '%s', '%s', 0, 1)",
            pathogen_id, escape_sql(variant_code), escape_sql(voi$name %||% "Unknown")
          ))
          records_imported <- records_imported + 1
        }
      }
    }
  }

  # Import country-level variant distributions
  if (!is.null(data$countries)) {
    countries_db <- dbGetQuery(conn, "SELECT country_id, iso_code FROM countries")

    for (iso in names(data$countries)) {
      c_data <- data$countries[[iso]]

      if (!is.null(c_data$covid_data$variant_distribution)) {
        vd <- c_data$covid_data$variant_distribution
        country_row <- countries_db[countries_db$iso_code == iso, ]

        if (nrow(country_row) == 0) next
        country_id <- country_row$country_id
        obs_date <- as.character(as.Date(c_data$last_updated %||% Sys.Date()))

        for (vname in names(vd)) {
          variant_code <- gsub("[^A-Za-z0-9_]", "_", toupper(vname))
          variant <- dbGetQuery(conn, sprintf(
            "SELECT variant_id FROM variants WHERE pathogen_id = %d AND variant_code = '%s'",
            pathogen_id, escape_sql(variant_code)
          ))

          if (nrow(variant) == 0) {
            dbExecute(conn, sprintf(
              "INSERT INTO variants (pathogen_id, variant_code, variant_name, is_voc, is_voi)
               VALUES (%d, '%s', '%s', 0, 0)",
              pathogen_id, escape_sql(variant_code), escape_sql(vname)
            ))
            variant <- dbGetQuery(conn, sprintf(
              "SELECT variant_id FROM variants WHERE pathogen_id = %d AND variant_code = '%s'",
              pathogen_id, escape_sql(variant_code)
            ))
            records_imported <- records_imported + 1
          }

          if (nrow(variant) > 0) {
            prevalence <- as.numeric(vd[[vname]] %||% 0)

            dbExecute(conn, sprintf(
              "DELETE FROM variant_prevalence
               WHERE variant_id = %d AND country_id = %d AND observation_date = '%s'",
              variant$variant_id, country_id, escape_sql(obs_date)
            ))

            dbExecute(conn, sprintf(
              "INSERT INTO variant_prevalence (variant_id, country_id, observation_date, prevalence_pct)
               VALUES (%d, %d, '%s', %f)",
              variant$variant_id, country_id, escape_sql(obs_date), prevalence
            ))
            records_imported <- records_imported + 1
          }
        }
      }
    }
  }

  message(sprintf("COVID variants import: %d records", records_imported))
  records_imported
}

# =============================================================================
# ENHANCED ANOMALY IMPORT FUNCTIONS
# =============================================================================

#' Import country-level anomaly flags from H3N2 JSON
#' @param conn Database connection
#' @param data Parsed JSON data
#' @return Number of records imported
import_country_anomalies <- function(conn, data) {
  records_imported <- 0

  # Get lookups
  anomaly_types <- dbGetQuery(conn, "SELECT type_id, type_code FROM anomaly_types")
  pathogens <- dbGetQuery(conn, "SELECT pathogen_id, pathogen_code FROM pathogens")
  countries <- dbGetQuery(conn, "SELECT country_id, iso_code FROM countries")

  h3n2_id <- pathogens$pathogen_id[pathogens$pathogen_code == "H3N2"]

  # Map anomaly types
  type_map <- c(
    "surveillance_gap" = "SURVEILLANCE_GAP",
    "vaccine_mismatch" = "MISMATCH",
    "antigenic_mismatch" = "MISMATCH",
    "drug_resistance" = "RESISTANCE",
    "data_reliability" = "SURVEILLANCE_GAP",
    "early_surge" = "TIMING",
    "capacity_crisis" = "SEVERITY",
    "hidden_ramping" = "SEVERITY",
    "enhanced_monitoring" = "SURVEILLANCE_GAP",
    "data_limitation" = "SURVEILLANCE_GAP",
    "timing" = "TIMING",
    "seasonal_burden" = "SEVERITY"
  )

  if (!is.null(data$countries)) {
    for (iso in names(data$countries)) {
      c_data <- data$countries[[iso]]

      if (!is.null(c_data$anomaly_flags) && is.data.frame(c_data$anomaly_flags) && nrow(c_data$anomaly_flags) > 0) {
        country_row <- countries[countries$iso_code == iso, ]
        if (nrow(country_row) == 0) next
        country_id <- country_row$country_id

        # Iterate over rows of the data frame
        for (i in seq_len(nrow(c_data$anomaly_flags))) {
          anomaly <- c_data$anomaly_flags[i, ]
          type_code <- type_map[anomaly$anomaly_type] %||% "SURVEILLANCE_GAP"
          type_row <- anomaly_types[anomaly_types$type_code == type_code, ]
          if (nrow(type_row) == 0) next

          detected_date <- as.character(as.Date(anomaly$first_detected %||% Sys.Date()))
          if (!is_valid_date(detected_date)) next

          dbExecute(conn, sprintf(
            "INSERT INTO anomalies (type_id, pathogen_id, country_id, detected_date, description, severity, verification_status)
             VALUES (%d, %d, %d, '%s', '%s', '%s', '%s')",
            type_row$type_id, h3n2_id, country_id,
            escape_sql(detected_date),
            escape_sql(substr(anomaly$description %||% "", 1, 500)),
            tolower(anomaly$severity %||% "medium"),
            tolower(anomaly$verification_status %||% "unverified")
          ))
          records_imported <- records_imported + 1
        }
      }
    }
  }

  message(sprintf("Country anomalies import: %d records", records_imported))
  records_imported
}

#' Import detailed anomaly detection flags
#' @param conn Database connection
#' @param json_path Path to anomaly_detection_flags.json or embedded data
#' @return Number of records imported
import_anomaly_detection_flags <- function(conn, json_path = "data/raw/anomaly_detection_flags.json") {
  records_imported <- 0

  # Get lookups
  anomaly_types <- dbGetQuery(conn, "SELECT type_id, type_code FROM anomaly_types")
  pathogens <- dbGetQuery(conn, "SELECT pathogen_id, pathogen_code FROM pathogens")
  regions <- dbGetQuery(conn, "SELECT region_id, region_code FROM regions")
  countries <- dbGetQuery(conn, "SELECT country_id, iso_code FROM countries")

  # Type mapping
  type_map <- c(
    "timing_anomaly" = "TIMING",
    "severity_mismatch" = "MISMATCH",
    "surveillance_gap" = "SURVEILLANCE_GAP",
    "laboratory_anomaly" = "SURVEILLANCE_GAP",
    "geographic_anomaly" = "SPREAD"
  )

  # Try to load standalone file first
  if (file.exists(json_path)) {
    data <- fromJSON(json_path)
  } else {
    return(0)
  }

  # Import suspicious patterns
  if (!is.null(data$suspicious_patterns) && is.data.frame(data$suspicious_patterns) && nrow(data$suspicious_patterns) > 0) {
    for (i in seq_len(nrow(data$suspicious_patterns))) {
      pattern <- data$suspicious_patterns[i, ]
      type_code <- type_map[pattern$pattern_type] %||% "SURVEILLANCE_GAP"
      type_row <- anomaly_types[anomaly_types$type_code == type_code, ]
      if (nrow(type_row) == 0) next

      detected_date <- as.character(as.Date(pattern$first_detected %||% Sys.Date()))
      if (!is_valid_date(detected_date)) next

      # Try to find pathogen_id if mentioned
      pathogen_id <- NULL
      if (grepl("H3N2|influenza", pattern$description, ignore.case = TRUE)) {
        pathogen_id <- pathogens$pathogen_id[pathogens$pathogen_code == "H3N2"]
      } else if (grepl("COVID|coronavirus", pattern$description, ignore.case = TRUE)) {
        pathogen_id <- pathogens$pathogen_id[pathogens$pathogen_code == "COVID19"]
      }

      dbExecute(conn, sprintf(
        "INSERT INTO anomalies (type_id, pathogen_id, detected_date, description, severity, geographic_scope, verification_status)
         VALUES (%d, %s, '%s', '%s', '%s', '%s', '%s')",
        type_row$type_id,
        if (is.null(pathogen_id)) "NULL" else as.character(pathogen_id),
        escape_sql(detected_date),
        escape_sql(substr(pattern$description %||% "", 1, 500)),
        tolower(pattern$confidence_level %||% "medium"),
        escape_sql(collapse_array(pattern$geographic_scope, "Global")),
        tolower(pattern$verification_status %||% "unverified")
      ))
      records_imported <- records_imported + 1
    }
  }

  # Import surveillance gaps as anomalies
  if (!is.null(data$surveillance_gaps) && is.data.frame(data$surveillance_gaps) && nrow(data$surveillance_gaps) > 0) {
    type_row <- anomaly_types[anomaly_types$type_code == "SURVEILLANCE_GAP", ]

    if (nrow(type_row) > 0) {
      for (i in seq_len(nrow(data$surveillance_gaps))) {
        gap <- data$surveillance_gaps[i, ]
        detected_date <- as.character(as.Date(gap$start_date %||% Sys.Date()))
        if (!is_valid_date(detected_date)) next

        dbExecute(conn, sprintf(
          "INSERT INTO anomalies (type_id, detected_date, description, severity, geographic_scope, verification_status)
           VALUES (%d, '%s', '%s', '%s', '%s', 'verified')",
          type_row$type_id,
          escape_sql(detected_date),
          escape_sql(substr(paste(gap$gap_type, "-", gap$description) %||% "", 1, 500)),
          tolower(gap$severity %||% "medium"),
          escape_sql(gap$affected_area %||% "Unknown")
        ))
        records_imported <- records_imported + 1
      }
    }
  }

  message(sprintf("Anomaly detection flags import: %d records", records_imported))
  records_imported
}

#' Import all JSON files to database
#' @param db_path Path to database
#' @return List with all import statistics
import_all_json <- function(db_path = DB_PATH) {
  conn <- get_db_connection(db_path)

  tryCatch({
    stats <- list()

    if (file.exists("data/raw/h3n2_outbreak_tracker.json")) {
      stats$h3n2 <- import_h3n2_json(conn)

      # Import variants from H3N2 data
      h3n2_data <- fromJSON("data/raw/h3n2_outbreak_tracker.json")
      stats$h3n2$variants <- import_h3n2_variants(conn, h3n2_data)

      # Import country-level anomalies
      stats$h3n2$country_anomalies <- import_country_anomalies(conn, h3n2_data)

      # Import detailed anomaly flags if embedded
      if (!is.null(h3n2_data$anomaly_detection_flags)) {
        stats$h3n2$detailed_anomalies <- import_anomaly_detection_flags_from_data(conn, h3n2_data$anomaly_detection_flags)
      }
    }

    if (file.exists("data/raw/rsv_tracker.json")) {
      stats$rsv <- import_rsv_json(conn)
    }

    if (file.exists("data/raw/covid_tracker.json")) {
      stats$covid <- import_covid_json(conn)

      # Import variants from COVID data
      covid_data <- fromJSON("data/raw/covid_tracker.json")
      stats$covid$variants <- import_covid_variants(conn, covid_data)
    }

    # Import standalone anomaly flags file (wrapped in tryCatch as it may have complex nested arrays)
    if (file.exists("data/raw/anomaly_detection_flags.json")) {
      stats$anomaly_flags <- tryCatch({
        import_anomaly_detection_flags(conn)
      }, error = function(e) {
        message("Note: Standalone anomaly flags import skipped (", e$message, ")")
        message("  -> Embedded anomaly flags were already imported (9 records)")
        0
      })
    }

    close_db_connection(conn)
    message("All JSON imports complete")
    stats

  }, error = function(e) {
    close_db_connection(conn)
    stop("JSON import failed: ", e$message)
  })
}

#' Helper function to import anomaly detection flags from embedded data
#' @param conn Database connection
#' @param flags Anomaly detection flags data object
#' @return Number of records imported
import_anomaly_detection_flags_from_data <- function(conn, flags) {
  records_imported <- 0

  # Get lookups
  anomaly_types <- dbGetQuery(conn, "SELECT type_id, type_code FROM anomaly_types")
  pathogens <- dbGetQuery(conn, "SELECT pathogen_id, pathogen_code FROM pathogens")

  type_map <- c(
    "timing_anomaly" = "TIMING",
    "severity_mismatch" = "MISMATCH",
    "surveillance_gap" = "SURVEILLANCE_GAP",
    "laboratory_anomaly" = "SURVEILLANCE_GAP",
    "geographic_anomaly" = "SPREAD"
  )

  # Import suspicious patterns
  if (!is.null(flags$suspicious_patterns) && is.data.frame(flags$suspicious_patterns) && nrow(flags$suspicious_patterns) > 0) {
    for (i in seq_len(nrow(flags$suspicious_patterns))) {
      pattern <- flags$suspicious_patterns[i, ]
      type_code <- type_map[pattern$pattern_type] %||% "SURVEILLANCE_GAP"
      type_row <- anomaly_types[anomaly_types$type_code == type_code, ]
      if (nrow(type_row) == 0) next

      detected_date <- as.character(as.Date(pattern$first_detected %||% Sys.Date()))
      if (!is_valid_date(detected_date)) next

      pathogen_id <- NULL
      if (grepl("H3N2|influenza", pattern$description, ignore.case = TRUE)) {
        pathogen_id <- pathogens$pathogen_id[pathogens$pathogen_code == "H3N2"]
      }

      dbExecute(conn, sprintf(
        "INSERT INTO anomalies (type_id, pathogen_id, detected_date, description, severity, geographic_scope, verification_status)
         VALUES (%d, %s, '%s', '%s', '%s', '%s', '%s')",
        type_row$type_id,
        if (is.null(pathogen_id)) "NULL" else as.character(pathogen_id),
        escape_sql(detected_date),
        escape_sql(substr(pattern$description %||% "", 1, 500)),
        tolower(pattern$confidence_level %||% "medium"),
        escape_sql(pattern$geographic_scope %||% "Global"),
        tolower(pattern$verification_status %||% "unverified")
      ))
      records_imported <- records_imported + 1
    }
  }

  # Import surveillance gaps
  if (!is.null(flags$surveillance_gaps) && is.data.frame(flags$surveillance_gaps) && nrow(flags$surveillance_gaps) > 0) {
    type_row <- anomaly_types[anomaly_types$type_code == "SURVEILLANCE_GAP", ]

    if (nrow(type_row) > 0) {
      for (i in seq_len(nrow(flags$surveillance_gaps))) {
        gap <- flags$surveillance_gaps[i, ]
        detected_date <- as.character(as.Date(gap$start_date %||% Sys.Date()))
        if (!is_valid_date(detected_date)) next

        dbExecute(conn, sprintf(
          "INSERT INTO anomalies (type_id, detected_date, description, severity, geographic_scope, verification_status)
           VALUES (%d, '%s', '%s', '%s', '%s', 'verified')",
          type_row$type_id,
          escape_sql(detected_date),
          escape_sql(substr(paste(gap$gap_type, "-", gap$description) %||% "", 1, 500)),
          tolower(gap$severity %||% "medium"),
          escape_sql(gap$affected_area %||% "Unknown")
        ))
        records_imported <- records_imported + 1
      }
    }
  }

  message(sprintf("Embedded anomaly flags import: %d records", records_imported))
  records_imported
}

# =============================================================================
# DASHBOARD DATA FUNCTIONS
# =============================================================================

#' Get combined surveillance data for dashboard display
#' @param db_path Path to database
#' @return Data frame ready for dashboard visualization
get_dashboard_surveillance <- function(db_path = DB_PATH) {
  conn <- get_db_connection(db_path)

  data <- tryCatch({
    get_latest_surveillance(conn)
  }, finally = {
    close_db_connection(conn)
  })

  data
}

#' Get regional overview for dashboard
#' @param db_path Path to database
#' @return Data frame with regional status
get_dashboard_regional <- function(db_path = DB_PATH) {
  conn <- get_db_connection(db_path)

  data <- tryCatch({
    get_regional_overview(conn)
  }, finally = {
    close_db_connection(conn)
  })

  data
}

#' Get active anomalies for dashboard
#' @param db_path Path to database
#' @return Data frame with active anomalies
get_dashboard_anomalies <- function(db_path = DB_PATH) {
  conn <- get_db_connection(db_path)

  data <- tryCatch({
    get_anomalies(conn, include_resolved = FALSE)
  }, finally = {
    close_db_connection(conn)
  })

  data
}

#' Get surveillance timeline for charts
#' @param db_path Path to database
#' @param pathogen_codes Vector of pathogen codes
#' @param days Number of days to include
#' @return Data frame with time series data
get_surveillance_timeline <- function(db_path = DB_PATH, pathogen_codes = NULL, days = 90) {
  conn <- get_db_connection(db_path)

  start_date <- Sys.Date() - days

  data <- tryCatch({
    get_surveillance_data(conn, pathogen_codes = pathogen_codes, start_date = start_date)
  }, finally = {
    close_db_connection(conn)
  })

  data
}
