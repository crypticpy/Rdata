# ============================================================================
# Title: Enhanced Flu Surveillance Data Fetcher
# Purpose: Fetch enhanced influenza data from CDC FluView, WHO, and Delphi APIs
# Input: CDC FluView, WHO FluMart, CMU Delphi Epidata APIs
# Output: Enhanced surveillance_data table with flu-specific metrics
# ============================================================================

# Set working directory to project root
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# Load required packages
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(httr2)
library(jsonlite)
library(lubridate)

# Source dependencies
source("R/api_fetcher.R")
source("R/db_schema.R")
source("R/logging.R")

# =============================================================================
# API ENDPOINTS
# =============================================================================

FLU_API_CONFIG <- list(
  # Delphi FluView API - Primary ILI source (working endpoint)
  delphi_fluview_url = "https://api.delphi.cmu.edu/epidata/fluview/",

  # CDC Respiratory Virus Surveillance - includes Influenza positivity (working)
  cdc_respiratory_url = "https://data.cdc.gov/resource/r229-z6ma.json",

  # CDC Hospitalization Metrics by Jurisdiction - includes flu hospitalizations (working)
  cdc_hospitalization_url = "https://data.cdc.gov/resource/aemt-mg7g.json",

  # Legacy CDC FluView endpoints (DEPRECATED - returning 404)
  # cdc_fluview_ili_url = "https://data.cdc.gov/resource/vw8d-6c9p.json",
  # cdc_fluview_clinical_url = "https://data.cdc.gov/resource/3rnt-5p5v.json",

  # WHO FluNet (DEPRECATED - now requires xMart4 authentication)
  # Use synthetic data fallback for international coverage
  who_flunet_url = NULL,  # No longer publicly accessible

  # CMU Delphi Epidata - COVIDcast (ILI-related signals)
  delphi_covidcast_url = "https://api.covidcast.cmu.edu/epidata/covidcast/"
)

# =============================================================================
# DELPHI FLUVIEW FETCHER (Primary ILI Source)
# =============================================================================

#' Fetch ILI data from Delphi FluView API
#' @param weeks_back Weeks of historical data
#' @return Data frame with ILI activity
fetch_delphi_fluview <- function(weeks_back = 52) {
  message("Fetching Delphi FluView ILI Activity data...")

  # Calculate epiweek range
  current_date <- Sys.Date()
  current_year <- as.numeric(format(current_date, "%Y"))
  current_week <- as.numeric(format(current_date, "%V"))

  # Build epiweek range (YYYYWW format)
  start_year <- current_year - 1
  epiweeks <- c()
  for (year in start_year:current_year) {
    for (week in 1:53) {
      epiweeks <- c(epiweeks, sprintf("%d%02d", year, week))
    }
  }
  epiweek_range <- paste(epiweeks, collapse = ",")

  # Delphi API accepts comma-separated epiweeks
  url <- sprintf("%s?regions=nat&epiweeks=%d01-%d53",
                 FLU_API_CONFIG$delphi_fluview_url,
                 start_year, current_year)

  data <- tryCatch({
    response <- httr2::request(url) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()

    result <- httr2::resp_body_json(response)

    if (!is.null(result$epidata) && length(result$epidata) > 0) {
      bind_rows(result$epidata)
    } else {
      NULL
    }
  }, error = function(e) {
    log_error(sprintf("Delphi FluView fetch failed: %s", e$message), category = "api")
    return(NULL)
  })

  if (is.null(data) || nrow(data) == 0) {
    warning("No Delphi FluView data returned, trying CDC Respiratory endpoint")
    return(fetch_cdc_respiratory_flu())
  }

  message(sprintf("Fetched %d Delphi FluView records", nrow(data)))

  # Standardize Delphi data
  standardize_delphi_fluview_data(data)
}

#' Standardize Delphi FluView data
#' @param df Raw Delphi data
#' @return Standardized data frame
standardize_delphi_fluview_data <- function(df) {
  if (nrow(df) == 0) return(df)

  df |>
    mutate(
      epiweek_str = as.character(epiweek),
      year = as.numeric(substr(epiweek_str, 1, 4)),
      week_number = as.numeric(substr(epiweek_str, 5, 6)),
      observation_date = as.Date(paste0(year, "-01-01")) + weeks(week_number - 1),
      ili_percent = ili,
      weighted_ili = wili,
      iso_code = "USA",
      pathogen_code = "H3N2",
      data_source = "DELPHI_FLUVIEW",
      data_confidence = "high",
      # Convert ILI% to approximate positivity rate (scaling factor)
      positivity_rate = ili * 2.5
    ) |>
    select(observation_date, week_number, year, iso_code, pathogen_code,
           ili_percent, weighted_ili, positivity_rate, data_source, data_confidence) |>
    filter(!is.na(observation_date))
}

#' Fetch CDC Respiratory Virus Surveillance (Influenza positivity)
#' @return Data frame with influenza positivity
fetch_cdc_respiratory_flu <- function() {
  message("Fetching CDC Respiratory Surveillance Influenza data...")

  params <- list(
    `$where` = "pathogen='Influenza'",
    `$order` = "mmwr_week_end DESC",
    `$limit` = 1000
  )

  data <- tryCatch({
    fetch_api_data(
      FLU_API_CONFIG$cdc_respiratory_url,
      params = params,
      cache_hours = 6
    )
  }, error = function(e) {
    log_error(sprintf("CDC Respiratory Flu fetch failed: %s", e$message), category = "api")
    return(NULL)
  })

  if (is.null(data) || length(data) == 0) {
    warning("No CDC Respiratory Influenza data returned")
    return(data.frame())
  }

  df <- if (is.data.frame(data)) data else bind_rows(data)

  message(sprintf("Fetched %d CDC Respiratory Influenza records", nrow(df)))

  # Standardize
  df |>
    mutate(
      observation_date = as.Date(mmwr_week_end),
      week_number = as.numeric(mmwr_week),
      year = as.numeric(format(observation_date, "%Y")),
      positivity_rate = as.numeric(pct_positive),
      iso_code = "USA",
      pathogen_code = "H3N2",
      data_source = "CDC_RESPIRATORY",
      data_confidence = "high"
    ) |>
    filter(!is.na(observation_date)) |>
    select(observation_date, week_number, year, iso_code, pathogen_code,
           positivity_rate, data_source, data_confidence)
}

# Legacy function for backwards compatibility
fetch_cdc_fluview_ili <- function(limit = 10000, weeks_back = 52) {
  fetch_delphi_fluview(weeks_back)
}

#' Standardize FluView ILI data
#' @param df Raw FluView data
#' @return Standardized data frame
standardize_fluview_ili_data <- function(df) {
  if (nrow(df) == 0) return(df)

  # Common column candidates
  date_cols <- c("week_end", "weekend", "week_ending", "epiweek")
  ili_cols <- c("ilitotal", "ili_total", "total_ili")
  total_cols <- c("total_patients", "num_of_providers", "total")
  pct_cols <- c("unweighted_ili", "weighted_ili", "ili_pct")

  find_col <- function(candidates) {
    for (col in candidates) {
      if (col %in% names(df)) return(col)
    }
    NA_character_
  }

  date_col <- find_col(date_cols)
  pct_col <- find_col(pct_cols)

  if (is.na(date_col)) {
    # Try to construct from year + week
    if (all(c("year", "week") %in% names(df))) {
      df <- df |>
        mutate(observation_date = as.Date(paste0(year, "-01-01")) + weeks(as.numeric(week) - 1))
    } else {
      warning("Could not find date column in FluView data")
      return(data.frame())
    }
  } else {
    df$observation_date <- as.Date(df[[date_col]])
  }

  # Aggregate to national level
  result <- df |>
    filter(!is.na(observation_date)) |>
    mutate(
      ili_percent = if (!is.na(pct_col)) {
        suppressWarnings(as.numeric(.data[[pct_col]]))
      } else {
        NA_real_
      },
      year = as.numeric(format(observation_date, "%Y")),
      week_number = as.numeric(format(observation_date, "%V"))
    ) |>
    group_by(observation_date) |>
    summarize(
      ili_percent = mean(ili_percent, na.rm = TRUE),
      n_regions = n(),
      .groups = "drop"
    ) |>
    mutate(
      iso_code = "USA",
      pathogen_code = "H3N2",  # Primary influenza tracked
      data_source = "CDC_FLUVIEW",
      data_confidence = "high",
      # Convert ILI% to approximate positivity rate
      positivity_rate = ili_percent * 3  # Rough scaling factor
    )

  result
}

#' Fetch CDC FluView Clinical Lab data
#' @param limit Maximum records
#' @return Data frame with clinical lab results
#' @note Legacy CDC FluView Clinical endpoints are deprecated (404).
#'       This function now returns empty data frame to maintain compatibility.
fetch_cdc_fluview_clinical <- function(limit = 5000) {
  message("CDC FluView Clinical endpoint is deprecated, using Delphi FluView instead")

  # Legacy endpoints return 404, so we just return empty and let main function

  # use Delphi data instead

  # NOTE: Original endpoint was: https://data.cdc.gov/resource/3rnt-5p5v.json
  # This endpoint no longer exists as of 2024

  data.frame()
}

#' Standardize FluView Clinical Lab data
#' @param df Raw data
#' @return Standardized data frame
standardize_fluview_clinical_data <- function(df) {
  if (nrow(df) == 0) return(df)

  # Find date column
  date_cols <- c("week_end", "weekend", "wk_date")
  date_col <- NULL
  for (col in date_cols) {
    if (col %in% names(df)) {
      date_col <- col
      break
    }
  }

  if (is.null(date_col)) {
    if (all(c("year", "week") %in% names(df))) {
      df$observation_date <- as.Date(paste0(df$year, "-01-01")) + weeks(as.numeric(df$week) - 1)
    } else {
      return(data.frame())
    }
  } else {
    df$observation_date <- as.Date(df[[date_col]])
  }

  # Look for test and positive columns
  test_candidates <- c("total_specimens", "total_tested", "specimens")
  pos_candidates <- c("total_positive", "total_a", "total_b", "positive")
  pct_candidates <- c("percent_positive", "pct_positive")

  find_col <- function(candidates) {
    for (col in candidates) {
      if (col %in% names(df)) return(col)
    }
    NA_character_
  }

  tests_col <- find_col(test_candidates)
  pos_col <- find_col(pos_candidates)
  pct_col <- find_col(pct_candidates)

  result <- df |>
    filter(!is.na(observation_date)) |>
    mutate(
      test_volume = if (!is.na(tests_col)) suppressWarnings(as.numeric(.data[[tests_col]])) else NA_real_,
      positive = if (!is.na(pos_col)) suppressWarnings(as.numeric(.data[[pos_col]])) else NA_real_,
      positivity_rate = if (!is.na(pct_col)) {
        suppressWarnings(as.numeric(.data[[pct_col]]))
      } else if (!is.na(tests_col) && !is.na(pos_col)) {
        (positive / test_volume) * 100
      } else {
        NA_real_
      }
    ) |>
    group_by(observation_date) |>
    summarize(
      positivity_rate = mean(positivity_rate, na.rm = TRUE),
      test_volume = sum(test_volume, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      iso_code = "USA",
      pathogen_code = "H3N2",
      data_source = "CDC_FLUVIEW_CLINICAL",
      data_confidence = "high"
    )

  result
}

# =============================================================================
# WHO FLUNET FETCHER
# =============================================================================

#' Fetch WHO FluNet global surveillance data
#' @param countries Vector of ISO codes to filter
#' @param weeks_back Weeks of historical data
#' @return Data frame with global flu surveillance data
fetch_who_flunet <- function(countries = NULL, weeks_back = 52) {
  message("Fetching WHO FluNet global flu surveillance data...")

  # WHO FluNet API now requires xMart4 authentication

  # The old Azure Front Door endpoint is no longer publicly accessible
  # See: https://www.who.int/tools/flunet
  # For API access, register at: https://extranet.who.int/xmart4/docs/xmart_api/register_client_app.html

  if (is.null(FLU_API_CONFIG$who_flunet_url)) {
    message("WHO FluNet API requires authentication (xMart4), using synthetic global data")
    message("For live WHO data, register at https://extranet.who.int/xmart4/")
    return(generate_who_synthetic(countries, weeks_back))
  }

  # Calculate year filter for recent data
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  min_year <- current_year - 1

  # Try fetching from WHO FluNet API (if endpoint is configured)
  data <- tryCatch({
    url <- sprintf(
      "%s&$filter=YEAR%%20ge%%20%d&$top=10000",
      FLU_API_CONFIG$who_flunet_url,
      min_year
    )

    response <- fetch_api_data(url, cache_hours = 12)
    if (!is.null(response) && "value" %in% names(response)) {
      response$value
    } else if (is.data.frame(response)) {
      response
    } else {
      NULL
    }
  }, error = function(e) {
    log_error(sprintf("WHO FluNet fetch failed: %s", e$message), category = "api")
    NULL
  })

  if (is.null(data) || length(data) == 0) {
    message("WHO FluNet API returned no data, generating synthetic global data")
    return(generate_who_synthetic(countries, weeks_back))
  }

  df <- if (is.data.frame(data)) data else bind_rows(data)
  message(sprintf("Fetched %d WHO FluNet records", nrow(df)))

  # Standardize the data
  standardize_who_flunet_data(df, countries)
}

#' Standardize WHO FluNet data
#' @param df Raw WHO FluNet data
#' @param countries Countries to filter (NULL for all)
#' @return Standardized data frame
standardize_who_flunet_data <- function(df, countries = NULL) {
  if (nrow(df) == 0) return(df)

  # WHO FluNet columns typically include:
  # COUNTRY_CODE, COUNTRY_AREA_TERRITORY, YEAR, WEEK, SPEC_PROCESSED_NB,
  # SPEC_RECEIVED_NB, INF_A, INF_B, INF_ALL, AH1, AH1N12009, AH3, AH5, etc.

  # Find relevant columns
  country_col <- intersect(c("COUNTRY_CODE", "ISO_COUNTRY", "ISO3"), names(df))[1]
  year_col <- intersect(c("YEAR", "ISO_YEAR", "MMWR_YEAR"), names(df))[1]
  week_col <- intersect(c("WEEK", "ISO_WEEK", "MMWR_WEEK", "SDATE"), names(df))[1]
  tests_col <- intersect(c("SPEC_PROCESSED_NB", "SPEC_RECEIVED_NB", "SPECIMENS", "TOTAL_SPEC"), names(df))[1]
  positive_col <- intersect(c("INF_ALL", "ALL_INF", "TOTAL_POSITIVE", "INF_A", "AH3"), names(df))[1]

  if (is.na(country_col) || is.na(year_col) || is.na(week_col)) {
    warning("WHO FluNet data missing required columns")
    return(data.frame())
  }

  result <- df |>
    mutate(
      iso_code = .data[[country_col]],
      year = suppressWarnings(as.numeric(.data[[year_col]])),
      week_number = suppressWarnings(as.numeric(.data[[week_col]])),
      test_volume = if (!is.na(tests_col)) suppressWarnings(as.numeric(.data[[tests_col]])) else NA_real_,
      positive = if (!is.na(positive_col)) suppressWarnings(as.numeric(.data[[positive_col]])) else NA_real_
    ) |>
    filter(!is.na(year), !is.na(week_number), week_number >= 1, week_number <= 53)

  # Filter by countries if specified
  if (!is.null(countries)) {
    result <- result |> filter(iso_code %in% countries)
  }

  # Calculate observation date and positivity
  result <- result |>
    mutate(
      observation_date = as.Date(paste0(year, "-01-01")) + weeks(week_number - 1),
      positivity_rate = if_else(
        !is.na(test_volume) & !is.na(positive) & test_volume > 0,
        (positive / test_volume) * 100,
        NA_real_
      ),
      pathogen_code = "H3N2",
      data_source = "WHO_FLUNET",
      data_confidence = "high"
    ) |>
    select(observation_date, week_number, year, iso_code, pathogen_code,
           positivity_rate, test_volume, data_source, data_confidence) |>
    filter(!is.na(observation_date))

  # Aggregate by country and week (some countries report multiple regions)
  result |>
    group_by(observation_date, week_number, year, iso_code, pathogen_code, data_source, data_confidence) |>
    summarize(
      positivity_rate = mean(positivity_rate, na.rm = TRUE),
      test_volume = sum(test_volume, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Generate synthetic WHO-style global flu data
#' @param countries Vector of ISO codes
#' @param weeks Number of weeks
#' @return Data frame with synthetic data
generate_who_synthetic <- function(countries = NULL, weeks = 52) {
  if (is.null(countries)) {
    countries <- c("GBR", "DEU", "FRA", "ITA", "ESP", "JPN", "KOR", "CHN",
                   "IND", "BRA", "ARG", "ZAF", "AUS", "NZL")
  }

  message(sprintf("Generating synthetic WHO FluNet data for %d countries...", length(countries)))

  southern <- c("AUS", "NZL", "ARG", "BRA", "ZAF")

  result <- data.frame()

  for (iso in countries) {
    hemisphere <- if (iso %in% southern) "south" else "north"
    peak_month <- if (hemisphere == "north") 1 else 7

    base_date <- Sys.Date() - (weeks * 7)

    for (week in 0:(weeks - 1)) {
      obs_date <- base_date + (week * 7)
      month <- as.numeric(format(obs_date, "%m"))

      # Seasonal wave pattern
      seasonal <- 1 + 0.8 * cos(2 * pi * (month - peak_month) / 12)
      seasonal <- pmax(0.1, seasonal)

      positivity <- 2 + seasonal * 20 + runif(1, -4, 4)
      positivity <- pmax(0.5, pmin(40, positivity))

      result <- bind_rows(result, data.frame(
        observation_date = obs_date,
        week_number = as.numeric(format(obs_date, "%V")),
        year = as.numeric(format(obs_date, "%Y")),
        iso_code = iso,
        pathogen_code = "H3N2",
        positivity_rate = round(positivity, 1),
        test_volume = round(runif(1, 500, 5000)),
        data_source = "WHO_FLUNET_SYNTHETIC",
        data_confidence = "medium"
      ))
    }
  }

  result
}

# =============================================================================
# DELPHI EPIDATA FETCHER
# =============================================================================

#' Fetch Delphi COVIDcast ILI signals
#' @param signal Signal name (e.g., "smoothed_cli", "smoothed_ili")
#' @param data_source Source name (e.g., "doctor-visits", "fb-survey")
#' @param geo_type Geographic granularity ("nation", "state")
#' @return Data frame with signal values
fetch_delphi_ili_signal <- function(signal = "smoothed_cli",
                                     data_source = "doctor-visits",
                                     geo_type = "nation") {
  message(sprintf("Fetching Delphi %s signal from %s...", signal, data_source))

  # Calculate time range - use full year for better historical coverage
  end_date <- format(Sys.Date(), "%Y%m%d")
  start_date <- format(Sys.Date() - 365, "%Y%m%d")

  params <- list(
    data_source = data_source,
    signal = signal,
    time_type = "day",
    time_values = paste0(start_date, "-", end_date),
    geo_type = geo_type,
    geo_value = if (geo_type == "nation") "us" else "*"
  )

  data <- tryCatch({
    fetch_delphi_epidata("covidcast", params)
  }, error = function(e) {
    log_error(sprintf("Delphi %s fetch failed: %s", signal, e$message), category = "api")
    return(data.frame())
  })

  if (nrow(data) == 0) {
    warning(sprintf("No Delphi %s data returned", signal))
    return(data.frame())
  }

  message(sprintf("Fetched %d Delphi records", nrow(data)))

  # Standardize
  data |>
    mutate(
      observation_date = as.Date(as.character(time_value), format = "%Y%m%d"),
      iso_code = "USA",
      signal_value = value,
      signal_name = signal,
      data_source = paste0("DELPHI_", toupper(data_source))
    ) |>
    filter(!is.na(observation_date))
}

# =============================================================================
# SYNTHETIC FLU DATA
# =============================================================================

#' Generate synthetic flu data for international countries
#' @param countries Vector of ISO codes
#' @param weeks Number of weeks
#' @return Data frame with synthetic flu data
generate_synthetic_flu <- function(countries, weeks = 52) {
  message("Generating synthetic flu data for international comparison...")

  southern <- c("AUS", "NZL", "ARG", "BRA", "ZAF")

  result <- data.frame()

  for (iso in countries) {
    hemisphere <- if (iso %in% southern) "south" else "north"
    peak_month <- if (hemisphere == "north") 1 else 7  # Jan vs July

    base_date <- Sys.Date() - (weeks * 7)

    for (week in 0:(weeks - 1)) {
      obs_date <- base_date + (week * 7)
      month <- as.numeric(format(obs_date, "%m"))

      # Seasonal pattern
      seasonal <- 1 + 0.7 * cos(2 * pi * (month - peak_month) / 12)
      seasonal <- pmax(0.2, seasonal)

      # Base positivity (5-25% at peak)
      positivity <- 3 + seasonal * 18 + runif(1, -3, 3)
      positivity <- pmax(1, pmin(35, positivity))

      # Hospitalization rate
      hosp_rate <- seasonal * 3 + runif(1, -0.5, 0.5)
      hosp_rate <- pmax(0.1, hosp_rate)

      result <- bind_rows(result, data.frame(
        observation_date = obs_date,
        week_number = as.numeric(format(obs_date, "%V")),
        year = as.numeric(format(obs_date, "%Y")),
        iso_code = iso,
        pathogen_code = "H3N2",
        positivity_rate = round(positivity, 1),
        hospitalization_rate = round(hosp_rate, 2),
        data_source = "SYNTHETIC",
        data_confidence = "medium"
      ))
    }
  }

  result
}

# =============================================================================
# DATABASE INSERTION
# =============================================================================

#' Insert enhanced flu surveillance data
#' @param data Standardized data frame
#' @return Number of records inserted
insert_flu_surveillance <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    message("No flu data to insert")
    return(0)
  }

  conn <- get_db_connection()

  tryCatch({
    # Get pathogen_id for H3N2
    pathogen_id <- dbGetQuery(
      conn,
      "SELECT pathogen_id FROM pathogens WHERE pathogen_code = 'H3N2'"
    )$pathogen_id

    if (length(pathogen_id) == 0) {
      warning("H3N2 pathogen not found in database")
      close_db_connection(conn)
      return(0)
    }

    # Build source_id lookup map from data_sources table
    # Also ensure new sources exist (DELPHI_FLUVIEW, DELPHI_DOCTOR-VISITS, etc.)
    ensure_source_exists <- function(conn, source_code, source_name) {
      existing <- dbGetQuery(
        conn,
        sprintf("SELECT source_id FROM data_sources WHERE source_code = '%s'", source_code)
      )$source_id
      if (length(existing) == 0) {
        dbExecute(conn, sprintf(
          "INSERT INTO data_sources (source_code, source_name, source_type, is_active)
           VALUES ('%s', '%s', 'api', 1)",
          source_code, source_name
        ))
        dbGetQuery(conn, sprintf(
          "SELECT source_id FROM data_sources WHERE source_code = '%s'", source_code
        ))$source_id
      } else {
        existing
      }
    }

    # Ensure all potential Delphi sources exist
    ensure_source_exists(conn, "DELPHI_FLUVIEW", "Delphi FluView ILI")
    ensure_source_exists(conn, "DELPHI_DOCTOR-VISITS", "Delphi Doctor Visits")
    ensure_source_exists(conn, "CDC_RESPIRATORY", "CDC Respiratory Surveillance")
    ensure_source_exists(conn, "WHO_FLUNET_SYNTHETIC", "WHO FluNet (Synthetic)")
    ensure_source_exists(conn, "SYNTHETIC", "Synthetic Data")

    # Get all source IDs for mapping
    source_map <- dbGetQuery(conn, "SELECT source_code, source_id FROM data_sources")
    source_lookup <- setNames(source_map$source_id, source_map$source_code)

    # Default source_id for fallback
    default_source_id <- source_lookup["CDC_FLUVIEW"]
    if (is.na(default_source_id)) default_source_id <- NA_integer_

    total_inserted <- 0

    for (iso in unique(data$iso_code)) {
      country_data <- data |> filter(iso_code == iso)

      country_id <- dbGetQuery(
        conn,
        sprintf("SELECT country_id FROM countries WHERE iso_code = '%s'", iso)
      )$country_id

      if (length(country_id) == 0) next

      # Resolve source_id for each record based on its data_source column
      # If data_source is missing or not found, use default
      insert_data <- country_data |>
        rowwise() |>
        mutate(
          resolved_source_id = ifelse(
            "data_source" %in% names(country_data) && data_source %in% names(source_lookup),
            as.integer(source_lookup[data_source]),
            as.integer(default_source_id)
          )
        ) |>
        ungroup() |>
        transmute(
          pathogen_id = pathogen_id,
          country_id = country_id,
          source_id = resolved_source_id,
          observation_date = as.character(observation_date),
          week_number = if ("week_number" %in% names(country_data)) week_number else as.numeric(format(observation_date, "%V")),
          year = if ("year" %in% names(country_data)) year else as.numeric(format(observation_date, "%Y")),
          positivity_rate = if ("positivity_rate" %in% names(country_data)) positivity_rate else NA_real_,
          case_count = NA_integer_,
          estimated_cases = NA_integer_,
          hospitalizations = NA_integer_,
          hospitalization_rate = if ("hospitalization_rate" %in% names(country_data)) hospitalization_rate else NA_real_,
          icu_admissions = NA_integer_,
          deaths = NA_integer_,
          death_rate = NA_real_,
          test_volume = if ("test_volume" %in% names(country_data)) test_volume else NA_integer_,
          data_confidence = if ("data_confidence" %in% names(country_data)) data_confidence else "medium",
          fetch_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )

      # Delete existing records
      min_date <- min(country_data$observation_date)
      max_date <- max(country_data$observation_date)

      dbExecute(conn, sprintf(
        "DELETE FROM surveillance_data
         WHERE pathogen_id = %d AND country_id = %d
         AND observation_date >= '%s' AND observation_date <= '%s'",
        pathogen_id, country_id, min_date, max_date
      ))

      dbWriteTable(conn, "surveillance_data", insert_data, append = TRUE, row.names = FALSE)
      total_inserted <- total_inserted + nrow(insert_data)
    }

    message(sprintf("Inserted %d flu surveillance records", total_inserted))
    close_db_connection(conn)
    total_inserted

  }, error = function(e) {
    close_db_connection(conn)
    log_error(sprintf("Flu surveillance insert failed: %s", e$message), category = "database")
    0
  })
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

#' Main function to fetch enhanced flu surveillance data
#' @param use_synthetic Use synthetic data only
#' @param weeks_back Weeks of historical data
#' @return Total records inserted
fetch_all_flu_data <- function(use_synthetic = FALSE, weeks_back = 52) {
  message("=" |> rep(60) |> paste(collapse = ""))
  message("Enhanced Flu Surveillance Data Fetch")
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  message("=" |> rep(60) |> paste(collapse = ""))

  all_data <- data.frame()

  target_countries <- c("USA", "GBR", "CAN", "AUS", "DEU", "FRA", "JPN", "ITA", "ESP", "NZL")

  if (!use_synthetic) {
    # Fetch CDC FluView ILI
    ili_data <- fetch_cdc_fluview_ili(weeks_back = weeks_back)
    if (nrow(ili_data) > 0) {
      all_data <- bind_rows(all_data, ili_data)
    }

    # Fetch CDC FluView Clinical
    clinical_data <- fetch_cdc_fluview_clinical()
    if (nrow(clinical_data) > 0) {
      # Merge with existing USA data or add
      if (nrow(all_data |> filter(iso_code == "USA")) > 0) {
        all_data <- all_data |>
          left_join(
            clinical_data |> select(observation_date, test_volume),
            by = "observation_date"
          )
      } else {
        all_data <- bind_rows(all_data, clinical_data)
      }
    }

    # Fetch WHO FluNet for global coverage
    who_data <- fetch_who_flunet(countries = target_countries, weeks_back = weeks_back)
    if (nrow(who_data) > 0) {
      # Add countries not already covered by CDC
      existing_countries <- unique(all_data$iso_code)
      who_new <- who_data |> filter(!iso_code %in% existing_countries)
      if (nrow(who_new) > 0) {
        all_data <- bind_rows(all_data, who_new)
      }
    }

    # Try Delphi signals as supplement
    delphi_data <- tryCatch({
      fetch_delphi_ili_signal()
    }, error = function(e) {
      data.frame()
    })

    # Generate synthetic for countries without API data
    countries_with_data <- unique(all_data$iso_code)
    countries_needing_synthetic <- setdiff(target_countries, countries_with_data)

    if (length(countries_needing_synthetic) > 0) {
      synthetic_data <- generate_synthetic_flu(countries_needing_synthetic, weeks = weeks_back)
      all_data <- bind_rows(all_data, synthetic_data)
    }
  } else {
    all_data <- generate_synthetic_flu(target_countries, weeks = weeks_back)
  }

  # Insert all data
  total_records <- insert_flu_surveillance(all_data)

  message("=" |> rep(60) |> paste(collapse = ""))
  message(sprintf("Total flu surveillance records: %d", total_records))
  message("=" |> rep(60) |> paste(collapse = ""))

  total_records
}

# Run if executed directly
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  use_synthetic <- "--synthetic" %in% args

  result <- fetch_all_flu_data(use_synthetic = use_synthetic)
  message(sprintf("Fetch complete: %d records", result))
}
