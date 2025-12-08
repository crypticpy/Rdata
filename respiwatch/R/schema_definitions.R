# ============================================================================
# Title: RespiWatch Schema Definitions
# Purpose: Type validators for data structures used throughout the app
# ============================================================================

# Source logging for error reporting
# Assumes logging.R is already sourced

# =============================================================================
# SCHEMA DEFINITIONS
# =============================================================================

# Expected timeline data columns
TIMELINE_SCHEMA <- list(
  required = c("date", "pathogen"),
  optional = c("week_number", "positivity_rate", "case_numbers", "hospitalization_rate"),
  types = list(
    date = "Date",
    pathogen = "character",
    week_number = "integer",
    positivity_rate = "numeric",
    case_numbers = "numeric",
    hospitalization_rate = "numeric"
  )
)

# Expected surveillance data columns
SURVEILLANCE_SCHEMA <- list(
  required = c("pathogen_code", "iso_code", "observation_date"),
  optional = c(
    "pathogen_name", "country_name", "positivity_rate", "case_count",
    "hospitalization_rate", "data_confidence", "population", "vaccination_rate"
  ),
  types = list(
    pathogen_code = "character",
    iso_code = "character",
    observation_date = "Date",
    pathogen_name = "character",
    country_name = "character",
    positivity_rate = "numeric",
    case_count = "numeric",
    hospitalization_rate = "numeric",
    data_confidence = "character",
    population = "numeric",
    vaccination_rate = "numeric"
  )
)

# Expected KPI structure
KPI_SCHEMA <- list(
  required = c("global_cases", "positivity_rate", "hospitalization_rate", "countries_reporting"),
  optional = c("last_updated"),
  types = list(
    global_cases = "numeric",
    positivity_rate = "numeric",
    hospitalization_rate = "numeric",
    countries_reporting = "numeric",
    last_updated = "Date"
  )
)

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

#' Validate data frame against schema
#'
#' @param df Data frame to validate
#' @param schema Schema list with required, optional, and types
#' @param context String for logging context
#' @return List with valid (boolean) and errors (character vector)
validate_schema <- function(df, schema, context = "data") {
  errors <- character()

  # Check if data frame

if (!is.data.frame(df)) {
    errors <- c(errors, sprintf("%s: Expected data frame, got %s", context, class(df)[1]))
    return(list(valid = FALSE, errors = errors))
  }

  # Check required columns
  missing_required <- setdiff(schema$required, names(df))
  if (length(missing_required) > 0) {
    errors <- c(errors, sprintf(
      "%s: Missing required columns: %s",
      context, paste(missing_required, collapse = ", ")
    ))
  }

  # Check column types for present columns
  for (col in intersect(names(df), names(schema$types))) {
    expected_type <- schema$types[[col]]
    actual_class <- class(df[[col]])[1]

    # Map R classes to expected types
    type_match <- switch(expected_type,
      "Date" = inherits(df[[col]], "Date"),
      "character" = is.character(df[[col]]),
      "numeric" = is.numeric(df[[col]]),
      "integer" = is.integer(df[[col]]) || is.numeric(df[[col]]),
      TRUE
    )

    if (!type_match) {
      errors <- c(errors, sprintf(
        "%s: Column '%s' expected %s, got %s",
        context, col, expected_type, actual_class
      ))
    }
  }

  list(valid = length(errors) == 0, errors = errors)
}

#' Validate timeline data
#'
#' @param df Timeline data frame
#' @return TRUE if valid, FALSE otherwise (logs errors)
validate_timeline_schema <- function(df) {
  result <- validate_schema(df, TIMELINE_SCHEMA, "timeline")

  if (!result$valid && exists("log_warning", mode = "function")) {
    for (err in result$errors) {
      log_warning(err, category = "schema")
    }
  }

  result$valid
}

#' Validate surveillance data
#'
#' @param df Surveillance data frame
#' @return TRUE if valid, FALSE otherwise (logs errors)
validate_surveillance_schema <- function(df) {
  result <- validate_schema(df, SURVEILLANCE_SCHEMA, "surveillance")

  if (!result$valid && exists("log_warning", mode = "function")) {
    for (err in result$errors) {
      log_warning(err, category = "schema")
    }
  }

  result$valid
}

#' Validate KPI list structure
#'
#' @param kpi_list Named list with KPI values
#' @return TRUE if valid, FALSE otherwise (logs errors)
validate_kpi_structure <- function(kpi_list) {
  errors <- character()

  if (!is.list(kpi_list)) {
    errors <- c(errors, sprintf("KPI: Expected list, got %s", class(kpi_list)[1]))
    if (exists("log_warning", mode = "function")) {
      log_warning(errors[1], category = "schema")
    }
    return(FALSE)
  }

  # Check required keys
  missing_keys <- setdiff(KPI_SCHEMA$required, names(kpi_list))
  if (length(missing_keys) > 0) {
    errors <- c(errors, sprintf(
      "KPI: Missing required keys: %s",
      paste(missing_keys, collapse = ", ")
    ))
  }

  # Check types for present keys
  for (key in intersect(names(kpi_list), names(KPI_SCHEMA$types))) {
    expected_type <- KPI_SCHEMA$types[[key]]
    value <- kpi_list[[key]]

    type_match <- switch(expected_type,
      "Date" = inherits(value, "Date"),
      "character" = is.character(value),
      "numeric" = is.numeric(value),
      TRUE
    )

    if (!type_match && !is.null(value)) {
      errors <- c(errors, sprintf(
        "KPI: Key '%s' expected %s, got %s",
        key, expected_type, class(value)[1]
      ))
    }
  }

  if (length(errors) > 0 && exists("log_warning", mode = "function")) {
    for (err in errors) {
      log_warning(err, category = "schema")
    }
  }

  length(errors) == 0
}

#' Coerce data frame to timeline schema
#'
#' Attempts to fix common type issues
#'
#' @param df Data frame to coerce
#' @return Coerced data frame
coerce_timeline_schema <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)

  # Coerce date column
  if ("date" %in% names(df) && !inherits(df$date, "Date")) {
    df$date <- tryCatch(
      as.Date(df$date),
      error = function(e) df$date
    )
  }

  # Coerce numeric columns
  numeric_cols <- c("positivity_rate", "case_numbers", "hospitalization_rate")
  for (col in intersect(numeric_cols, names(df))) {
    if (!is.numeric(df[[col]])) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }

  df
}

#' Coerce data frame to surveillance schema
#'
#' @param df Data frame to coerce
#' @return Coerced data frame
coerce_surveillance_schema <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)

  # Coerce date column
  if ("observation_date" %in% names(df) && !inherits(df$observation_date, "Date")) {
    df$observation_date <- tryCatch(
      as.Date(df$observation_date),
      error = function(e) df$observation_date
    )
  }

  # Coerce numeric columns
  numeric_cols <- c("positivity_rate", "case_count", "hospitalization_rate", "population", "vaccination_rate")
  for (col in intersect(numeric_cols, names(df))) {
    if (!is.numeric(df[[col]])) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }

  df
}
