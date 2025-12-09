# ============================================================================
# Title: RespiWatch Intelligent Data Fallback System
# Purpose: Central engine for automatically filling surveillance gaps with
#          alternate signals (wastewater, syndromic, forecasts, state data)
# Key Differentiator: When primary sources fail, this system auto-falls back
#          to alternate signals and transparently communicates this to users
# ============================================================================

# Load required packages
library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)

# Source database functions
source("R/db_schema.R")

# =============================================================================
# DATA SOURCE PRIORITY CONFIGURATION
# =============================================================================

#' Data source priority order for fallback cascade
#' Lower numbers = higher priority (tried first)
DATA_SOURCE_PRIORITY <- list(
  CDC_FLUVIEW = 1,      # CDC FluView - primary flu surveillance
  CDC_NREVSS = 1,       # CDC NREVSS - primary lab surveillance
  CDC_COVID = 1,        # CDC COVID Data Tracker - primary COVID
  RSV_NET = 1,          # RSV-NET - primary RSV surveillance
  DELPHI_FLUVIEW = 1,   # Delphi FluView ILI - primary flu data
  CDC_RESPIRATORY = 1,  # CDC Respiratory Surveillance - primary
  WHO_FLUMART = 2,      # WHO FluMart - international primary
  ECDC = 2,             # ECDC - European primary
  NWSS_WASTEWATER = 3,  # CDC NWSS Wastewater - first fallback
  FLUSIGHT_FORECAST = 4, # CDC FluSight ensemble - forecast fallback
  DELPHI = 5,           # CMU Delphi syndromic - syndromic fallback
  STATE_HEALTH = 6,     # State health departments
  INTERPOLATED = 99     # Last resort - linear interpolation
)

#' Human-readable source names for display
SOURCE_DISPLAY_NAMES <- list(
  CDC_FLUVIEW = "CDC FluView",
  CDC_NREVSS = "CDC NREVSS",
  CDC_COVID = "CDC COVID Tracker",
  RSV_NET = "RSV-NET",
  DELPHI_FLUVIEW = "Delphi FluView",
  CDC_RESPIRATORY = "CDC Respiratory",
  WHO_FLUMART = "WHO FluNet",
  ECDC = "ECDC Surveillance",
  NWSS_WASTEWATER = "Wastewater Surveillance",
  FLUSIGHT_FORECAST = "CDC FluSight Forecast",
  DELPHI = "Syndromic Signals",
  STATE_HEALTH = "State Health Depts",
  INTERPOLATED = "Interpolated"
)

#' Source reliability ratings
SOURCE_RELIABILITY <- list(
  CDC_FLUVIEW = "high",
  CDC_NREVSS = "high",
  CDC_COVID = "high",
  RSV_NET = "high",
  DELPHI_FLUVIEW = "high",
  CDC_RESPIRATORY = "high",
  WHO_FLUMART = "high",
  ECDC = "high",
  NWSS_WASTEWATER = "high",
  FLUSIGHT_FORECAST = "high",
  DELPHI = "medium",
  STATE_HEALTH = "high",
  INTERPOLATED = "low"
)

# =============================================================================
# CORE FALLBACK ENGINE
# =============================================================================

#' Get surveillance data with automatic fallback to alternate signals
#'
#' This is the KEY DIFFERENTIATOR for RespiWatch. When primary surveillance
#' data is unavailable (reporting lag, holidays, etc.), this function
#' automatically cascades through alternate signals to fill gaps.
#'
#' @param pathogen Pathogen code (e.g., "H3N2", "RSV", "COVID19")
#' @param date_range Vector of dates needed (or single date for point query)
#' @param country ISO country code (default: "USA")
#' @param metric Which metric to retrieve: "positivity_rate", "case_count",
#'        "hospitalization_rate", or "all"
#' @return Data frame with data and source metadata. Includes attributes:
#'         - "source_coverage": list of how many points from each source
#'         - "has_fallback": logical, TRUE if any non-primary data used
#'         - "gap_periods": data frame of primary data gaps
#' @examples
#' # Get 12 weeks of H3N2 data with automatic fallback
#' data <- get_surveillance_with_fallback("H3N2", seq(Sys.Date() - 84, Sys.Date(), by = "day"))
#'
#' # Check if fallback was used
#' if (attr(data, "has_fallback")) {
#'   message("Using alternate signals for some dates")
#' }
get_surveillance_with_fallback <- function(pathogen,
                                           date_range,
                                           country = "USA",
                                           metric = "all") {

  # Ensure date_range is a sequence
  if (length(date_range) == 1) {
    date_range <- as.Date(date_range)
  } else {
    date_range <- as.Date(date_range)
  }

  conn <- get_db_connection()

  result <- tryCatch({
    # Get pathogen_id
    pathogen_id <- dbGetQuery(
      conn,
      sprintf("SELECT pathogen_id FROM pathogens WHERE pathogen_code = '%s'", pathogen)
    )$pathogen_id

    if (length(pathogen_id) == 0) {
      close_db_connection(conn)
      warning(sprintf("Pathogen %s not found", pathogen))
      return(empty_fallback_result(date_range))
    }

    # Get country_id
    country_id <- dbGetQuery(
      conn,
      sprintf("SELECT country_id FROM countries WHERE iso_code = '%s'", country)
    )$country_id

    if (length(country_id) == 0) {
      close_db_connection(conn)
      warning(sprintf("Country %s not found", country))
      return(empty_fallback_result(date_range))
    }

    # Fetch all available data for this pathogen/country/date range
    # Join with data_sources to get source_code
    all_data <- dbGetQuery(conn, sprintf("
      SELECT
        sd.observation_date,
        sd.positivity_rate,
        sd.case_count,
        sd.estimated_cases,
        sd.hospitalizations,
        sd.hospitalization_rate,
        sd.data_confidence,
        ds.source_code
      FROM surveillance_data sd
      LEFT JOIN data_sources ds ON sd.source_id = ds.source_id
      WHERE sd.pathogen_id = %d
        AND sd.country_id = %d
        AND sd.observation_date >= '%s'
        AND sd.observation_date <= '%s'
      ORDER BY sd.observation_date, ds.source_id
    ", pathogen_id, country_id, min(date_range), max(date_range)))

    close_db_connection(conn)

    if (nrow(all_data) == 0) {
      return(empty_fallback_result(date_range))
    }

    # Convert date
    all_data$observation_date <- as.Date(all_data$observation_date)

    # Apply cascade: for each date, pick highest priority source
    cascaded_data <- cascade_by_priority(all_data, date_range)

    cascaded_data

  }, error = function(e) {
    close_db_connection(conn)
    warning(sprintf("Fallback query failed: %s", e$message))
    empty_fallback_result(date_range)
  })

  result
}

#' Apply cascade logic to select best source for each date
#' @param data All available data with source_code column
#' @param date_range Full date range requested
#' @return Data frame with one row per date, best source selected
cascade_by_priority <- function(data, date_range) {
  if (nrow(data) == 0) {
    return(empty_fallback_result(date_range))
  }

  # Add priority score to each row
  data <- data |>
    mutate(
      priority = sapply(source_code, function(s) {
        DATA_SOURCE_PRIORITY[[s]] %||% 50
      })
    )

  # For each date, keep only the highest priority (lowest number) source
  best_per_date <- data |>
    group_by(observation_date) |>
    slice_min(priority, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      is_primary = priority <= 2,
      is_fallback = priority > 2,
      source_display = sapply(source_code, function(s) {
        SOURCE_DISPLAY_NAMES[[s]] %||% s
      }),
      source_reliability = sapply(source_code, function(s) {
        SOURCE_RELIABILITY[[s]] %||% "unknown"
      })
    )

  # Fill in missing dates with interpolation if needed
  all_dates <- data.frame(observation_date = as.Date(date_range))

  result <- all_dates |>
    left_join(best_per_date, by = "observation_date")

  # For dates with no data, mark as interpolated
  missing_mask <- is.na(result$source_code)
  if (any(missing_mask)) {
    result$source_code[missing_mask] <- "INTERPOLATED"
    result$is_primary[missing_mask] <- FALSE
    result$is_fallback[missing_mask] <- TRUE
    result$source_display[missing_mask] <- "Interpolated"
    result$source_reliability[missing_mask] <- "low"
    result$priority[missing_mask] <- 99

    # Interpolate numeric values
    for (col in c("positivity_rate", "case_count", "hospitalization_rate")) {
      if (col %in% names(result)) {
        result[[col]] <- interpolate_gaps(result[[col]], result$observation_date)
      }
    }
  }

  # Calculate source coverage statistics
  coverage <- result |>
    group_by(source_code) |>
    summarize(count = n(), .groups = "drop") |>
    as.list() |>
    (\(x) setNames(x$count, x$source_code))()

  # Identify gap periods in primary data
  gaps <- detect_primary_gaps(result)

  # Attach metadata as attributes
  attr(result, "source_coverage") <- coverage
  attr(result, "has_fallback") <- any(result$is_fallback, na.rm = TRUE)
  attr(result, "gap_periods") <- gaps
  attr(result, "pathogen") <- unique(na.omit(result$pathogen_code))[1]

  result
}

#' Interpolate missing values in a numeric vector
#' @param x Numeric vector with NAs
#' @param dates Date vector for ordering
#' @return Vector with NAs filled by linear interpolation
interpolate_gaps <- function(x, dates) {
  if (all(is.na(x))) return(x)

  # Use approx for linear interpolation
  valid <- !is.na(x)
  if (sum(valid) < 2) return(x)

  approx_result <- approx(
    x = as.numeric(dates[valid]),
    y = x[valid],
    xout = as.numeric(dates),
    rule = 2  # Extend edge values
  )

  approx_result$y
}

#' Create empty result with proper structure
#' @param date_range Date range for empty result
#' @return Empty data frame with correct columns and attributes
empty_fallback_result <- function(date_range) {
  result <- data.frame(
    observation_date = as.Date(date_range),
    positivity_rate = NA_real_,
    case_count = NA_integer_,
    hospitalization_rate = NA_real_,
    source_code = "NONE",
    is_primary = FALSE,
    is_fallback = FALSE,
    source_display = "No Data",
    source_reliability = "none"
  )

  attr(result, "source_coverage") <- list()
  attr(result, "has_fallback") <- FALSE
  attr(result, "gap_periods") <- data.frame()

  result
}

# =============================================================================
# GAP DETECTION UTILITIES
# =============================================================================

#' Detect gaps in primary surveillance data
#' @param data Data frame with observation_date and is_primary columns
#' @return Data frame with gap periods (start, end, duration, fallback_sources)
detect_primary_gaps <- function(data) {
  if (nrow(data) == 0 || !("is_primary" %in% names(data))) {
    return(data.frame(
      start = as.Date(character()),
      end = as.Date(character()),
      duration = integer(),
      fallback_sources = character()
    ))
  }

  # Find dates without primary data
  gap_dates <- data |>
    filter(!is_primary | is.na(is_primary)) |>
    pull(observation_date) |>
    sort()

  if (length(gap_dates) == 0) {
    return(data.frame(
      start = as.Date(character()),
      end = as.Date(character()),
      duration = integer(),
      fallback_sources = character()
    ))
  }

  # Group consecutive dates into gap periods
  gap_groups <- cumsum(c(1, diff(as.numeric(gap_dates)) > 1))

  gap_df <- data.frame(
    gap_date = gap_dates,
    group = gap_groups
  )

  gap_periods <- gap_df |>
    group_by(group) |>
    summarize(
      start = min(gap_date),
      end = max(gap_date),
      duration = n(),
      .groups = "drop"
    ) |>
    select(-group)

  # Get fallback sources used for each gap period
  gap_periods$fallback_sources <- sapply(1:nrow(gap_periods), function(i) {
    sources <- data |>
      filter(observation_date >= gap_periods$start[i] &
               observation_date <= gap_periods$end[i]) |>
      pull(source_code) |>
      unique()
    paste(sources, collapse = ", ")
  })

  gap_periods
}

#' Check if data has significant gaps that needed fallback
#' @param data Data frame from get_surveillance_with_fallback
#' @return Logical TRUE if fallback was used for > 10% of data
has_significant_fallback <- function(data) {
  if (is.null(attr(data, "has_fallback"))) return(FALSE)

  if (!attr(data, "has_fallback")) return(FALSE)

  # Check if more than 10% of data is from fallback
  fallback_pct <- sum(data$is_fallback, na.rm = TRUE) / nrow(data)
  fallback_pct > 0.10
}

# =============================================================================
# SOURCE DESCRIPTION UTILITIES
# =============================================================================

#' Get human-readable description of data sources used
#' @param data Data frame from get_surveillance_with_fallback
#' @return Character string describing sources
describe_data_sources <- function(data) {
  coverage <- attr(data, "source_coverage")

  if (is.null(coverage) || length(coverage) == 0) {
    return("No data available")
  }

  # Build description
  source_parts <- sapply(names(coverage), function(src) {
    display_name <- SOURCE_DISPLAY_NAMES[[src]] %||% src
    count <- coverage[[src]]
    sprintf("%s (%d days)", display_name, count)
  })

  paste(source_parts, collapse = " + ")
}

#' Get source breakdown for UI display
#' @param data Data frame from get_surveillance_with_fallback
#' @return Data frame with source details for display
get_source_breakdown <- function(data) {
  coverage <- attr(data, "source_coverage")

  if (is.null(coverage) || length(coverage) == 0) {
    return(data.frame(
      source_code = character(),
      source_name = character(),
      data_points = integer(),
      reliability = character(),
      is_primary = logical()
    ))
  }

  data.frame(
    source_code = names(coverage),
    source_name = sapply(names(coverage), function(s) SOURCE_DISPLAY_NAMES[[s]] %||% s),
    data_points = unlist(coverage),
    reliability = sapply(names(coverage), function(s) SOURCE_RELIABILITY[[s]] %||% "unknown"),
    is_primary = sapply(names(coverage), function(s) {
      (DATA_SOURCE_PRIORITY[[s]] %||% 50) <= 2
    })
  )
}

#' Calculate fallback percentage
#' @param data Data frame from get_surveillance_with_fallback
#' @return Numeric percentage of data from fallback sources (0-100)
get_fallback_percentage <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(0)

  fallback_count <- sum(data$is_fallback, na.rm = TRUE)
  total_count <- nrow(data)

  round(fallback_count / total_count * 100, 1)
}

# =============================================================================
# CONVENIENCE WRAPPERS
# =============================================================================

#' Get surveillance data for Rt estimation with fallback
#' Convenience wrapper that returns data in format expected by rt_estimation.R
#'
#' @param pathogen Pathogen code
#' @param date_range Date range
#' @param country Country code
#' @return Data frame with I (incidence) column for EpiEstim
get_incidence_with_fallback <- function(pathogen, date_range, country = "USA") {

  data <- get_surveillance_with_fallback(pathogen, date_range, country)

  # Convert to incidence format for EpiEstim
  result <- data |>
    transmute(
      dates = observation_date,
      I = coalesce(case_count, estimated_cases,
                   round(positivity_rate * 100)),  # Fallback to positivity
      source_code = source_code,
      is_fallback = is_fallback
    ) |>
    filter(!is.na(I))

  # Carry over attributes
  attr(result, "source_coverage") <- attr(data, "source_coverage")
  attr(result, "has_fallback") <- attr(data, "has_fallback")
  attr(result, "gap_periods") <- attr(data, "gap_periods")

  result
}

#' Get surveillance data for timeline visualization with fallback
#' @param pathogen Pathogen code
#' @param date_range Date range
#' @param country Country code
#' @return Data frame ready for timeline plotting
get_timeline_with_fallback <- function(pathogen, date_range, country = "USA") {

  data <- get_surveillance_with_fallback(pathogen, date_range, country)

  result <- data |>
    transmute(
      date = observation_date,
      pathogen = pathogen,
      value = coalesce(positivity_rate, case_count / 1000),
      source_code = source_code,
      source_display = source_display,
      is_fallback = is_fallback,
      line_type = if_else(source_code == "INTERPOLATED", "dashed", "solid"),
      alpha = if_else(is_fallback, 0.7, 1.0)
    )

  # Carry over attributes
  attr(result, "source_coverage") <- attr(data, "source_coverage")
  attr(result, "has_fallback") <- attr(data, "has_fallback")
  attr(result, "gap_periods") <- attr(data, "gap_periods")

  result
}

# =============================================================================
# TIMELINE FALLBACK FOR GLOBAL DATA
# =============================================================================

#' Apply intelligent fallback to timeline data using real alternate sources
#'
#' This function fills gaps in combined_timeline_df by first attempting to fetch
#' data from alternate sources (wastewater, FluSight, Delphi, state data) via
#' get_surveillance_with_fallback(). Only uses linear interpolation as LAST RESORT
#' when no alternate sources have data.
#'
#' Priority cascade:
#' 1. CDC FluView / NREVSS / COVID Tracker (primary)
#' 2. WHO FluMart / ECDC (international)
#' 3. NWSS Wastewater (first fallback)
#' 4. FluSight Forecasts (forecast fallback)
#' 5. Delphi Syndromic (syndromic fallback)
#' 6. State Health Departments
#' 7. Interpolation (LAST RESORT ONLY)
#'
#' @param timeline_df Data frame with date, pathogen, and metric columns
#' @param pathogens Character vector of pathogen codes to process (default: all)
#' @return Data frame with gaps filled from real sources and source metadata
#' @examples
#' filled_timeline <- apply_timeline_fallback(combined_timeline_df)
apply_timeline_fallback <- function(timeline_df, pathogens = NULL) {
  if (is.null(timeline_df) || nrow(timeline_df) == 0) {
    return(timeline_df)
  }

  # Ensure date column exists and is Date type
  if (!"date" %in% names(timeline_df)) {
    if ("observation_date" %in% names(timeline_df)) {
      timeline_df <- timeline_df |> rename(date = observation_date)
    } else {
      warning("Timeline data has no date column")
      return(timeline_df)
    }
  }
  timeline_df$date <- as.Date(timeline_df$date)

  # Get list of pathogens to process
  if (is.null(pathogens)) {
    pathogens <- unique(timeline_df$pathogen)
  }

  # Pathogen name to code mapping for database queries
  pathogen_code_map <- list(
    "H3N2 (Influenza)" = "H3N2",
    "H3N2" = "H3N2",
    "Influenza A" = "H3N2",
    "RSV" = "RSV",
    "COVID-19" = "COVID19",
    "COVID19" = "COVID19"
  )

  # Initialize source tracking columns if not present
  if (!"source_code" %in% names(timeline_df)) {
    timeline_df$source_code <- "PRIMARY"
  }
  if (!"is_fallback" %in% names(timeline_df)) {
    timeline_df$is_fallback <- FALSE
  }
  if (!"is_primary" %in% names(timeline_df)) {
    timeline_df$is_primary <- TRUE
  }

  result_list <- lapply(pathogens, function(p) {
    pathogen_data <- timeline_df |> filter(pathogen == p)

    if (nrow(pathogen_data) == 0) return(NULL)

    # Create complete date sequence
    date_range <- seq(
      min(pathogen_data$date, na.rm = TRUE),
      max(pathogen_data$date, na.rm = TRUE),
      by = "day"
    )

    complete_dates <- data.frame(date = date_range)

    # Join with existing data
    filled_data <- complete_dates |>
      left_join(pathogen_data, by = "date")

    # Identify gaps (dates missing from primary data)
    is_gap <- is.na(filled_data$positivity_rate) & is.na(filled_data$case_numbers)
    gap_dates <- filled_data$date[is_gap]

    if (length(gap_dates) > 0) {
      # Get pathogen code for database query
      pathogen_code <- pathogen_code_map[[p]] %||% p

      # Try to fill gaps using the real fallback cascade
      tryCatch({
        fallback_data <- get_surveillance_with_fallback(
          pathogen = pathogen_code,
          date_range = gap_dates,
          country = "USA"
        )

        if (!is.null(fallback_data) && nrow(fallback_data) > 0) {
          # Merge fallback data into our results
          for (i in which(is_gap)) {
            gap_date <- filled_data$date[i]
            fb_row <- fallback_data |> filter(observation_date == gap_date)

            if (nrow(fb_row) > 0) {
              # Fill from fallback source
              filled_data$positivity_rate[i] <- fb_row$positivity_rate[1]
              filled_data$case_numbers[i] <- fb_row$case_count[1] %||% fb_row$estimated_cases[1]
              filled_data$hospitalization_rate[i] <- fb_row$hospitalization_rate[1]
              filled_data$source_code[i] <- fb_row$source_code[1]
              filled_data$is_fallback[i] <- fb_row$is_fallback[1]
              filled_data$is_primary[i] <- fb_row$is_primary[1]
              is_gap[i] <- FALSE  # No longer a gap
            }
          }
        }
      }, error = function(e) {
        # If database query fails, continue to interpolation
        message(sprintf("Fallback query failed for %s: %s", p, e$message))
      })
    }

    # Fill pathogen column for remaining gaps
    filled_data$pathogen[is.na(filled_data$pathogen)] <- p

    # LAST RESORT: Interpolate only for dates that still have no data
    still_missing <- is.na(filled_data$positivity_rate) & is.na(filled_data$case_numbers)

    if (any(still_missing)) {
      # Interpolate numeric columns for remaining gaps ONLY
      for (col in c("positivity_rate", "case_numbers", "hospitalization_rate")) {
        if (col %in% names(filled_data)) {
          filled_data[[col]] <- interpolate_gaps(filled_data[[col]], filled_data$date)
        }
      }

      # Mark interpolated rows (only the ones that had NO fallback data)
      filled_data$source_code[still_missing] <- "INTERPOLATED"
      filled_data$is_fallback[still_missing] <- TRUE
      filled_data$is_primary[still_missing] <- FALSE
    }

    # Fill week_number if present
    if ("week_number" %in% names(filled_data)) {
      filled_data$week_number <- as.integer(format(filled_data$date, "%V"))
    }

    filled_data
  })

  # Combine results
  result <- bind_rows(result_list)

  # Calculate source coverage statistics
  if (nrow(result) > 0) {
    source_coverage <- result |>
      group_by(source_code) |>
      summarize(count = n(), .groups = "drop") |>
      as.list() |>
      (\(x) setNames(x$count, x$source_code))()

    fallback_count <- sum(result$is_fallback, na.rm = TRUE)
    interpolated_count <- sum(result$source_code == "INTERPOLATED", na.rm = TRUE)
    total_count <- nrow(result)
    fallback_pct <- fallback_count / total_count * 100
    interpolated_pct <- interpolated_count / total_count * 100

    attr(result, "has_fallback") <- any(result$is_fallback, na.rm = TRUE)
    attr(result, "fallback_pct") <- fallback_pct
    attr(result, "interpolated_pct") <- interpolated_pct
    attr(result, "source_coverage") <- source_coverage

    # Build source description
    if (interpolated_pct > 0 && fallback_pct > interpolated_pct) {
      attr(result, "source_description") <- sprintf(
        "Alternate sources (%.0f%%) + Interpolated (%.0f%%)",
        fallback_pct - interpolated_pct, interpolated_pct
      )
    } else if (interpolated_pct > 0) {
      attr(result, "source_description") <- sprintf("Interpolated (%.0f%%)", interpolated_pct)
    } else if (fallback_pct > 0) {
      attr(result, "source_description") <- "Alternate sources"
    } else {
      attr(result, "source_description") <- "Primary sources"
    }
  }

  result
}

#' Get timeline data for a specific pathogen with fallback
#'
#' Convenience function for modules that need single-pathogen timeline
#'
#' @param timeline_df Full timeline data frame
#' @param pathogen_code Pathogen to filter for
#' @return Filtered and filled timeline data
get_pathogen_timeline_with_fallback <- function(timeline_df, pathogen_code) {
  if (is.null(timeline_df) || nrow(timeline_df) == 0) {
    return(data.frame(
      date = as.Date(character()),
      pathogen = character(),
      positivity_rate = numeric(),
      case_numbers = numeric(),
      is_fallback = logical()
    ))
  }

  # Normalize pathogen code for matching
  pathogen_patterns <- list(
    "H3N2" = c("H3N2", "H3N2 (Influenza)", "Influenza A"),
    "RSV" = c("RSV"),
    "COVID19" = c("COVID19", "COVID-19")
  )

  matching_names <- pathogen_patterns[[pathogen_code]] %||% pathogen_code

  filtered <- timeline_df |>
    filter(pathogen %in% matching_names)

  if (nrow(filtered) == 0) {
    return(data.frame(
      date = as.Date(character()),
      pathogen = character(),
      positivity_rate = numeric(),
      case_numbers = numeric(),
      is_fallback = logical()
    ))
  }

  apply_timeline_fallback(filtered, pathogens = unique(filtered$pathogen))
}

message("Data Fallback System loaded successfully")
