# ============================================================================
# Title: RespiWatch Data Validation Module
# Purpose: Validate surveillance data completeness and quality
# Input: Surveillance data from database
# Output: Validation results with issues list
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)

# =============================================================================
# DATA VALIDATION FUNCTIONS
# =============================================================================

#' Validate surveillance data completeness
#' @param data Data frame from database
#' @param min_records Minimum required records (default 10)
#' @param max_age_days Maximum acceptable data age in days (default 7)
#' @return List with is_valid boolean and issues vector
validate_surveillance_data <- function(data, min_records = 10, max_age_days = 7) {
  issues <- character()

  # Check for NULL or empty data

  if (is.null(data) || !is.data.frame(data)) {
    return(list(
      is_valid = FALSE,
      issues = "No data available",
      summary = list(
        record_count = 0,
        data_age_days = NA,
        positivity_coverage = 0,
        hospitalization_coverage = 0
      )
    ))
  }

  if (nrow(data) == 0) {
    return(list(
      is_valid = FALSE,
      issues = "Empty dataset",
      summary = list(
        record_count = 0,
        data_age_days = NA,
        positivity_coverage = 0,
        hospitalization_coverage = 0
      )
    ))
  }

  # Check minimum record count
  if (nrow(data) < min_records) {
    issues <- c(issues, sprintf("Insufficient records (%d < %d required)", nrow(data), min_records))
  }

  # Check data recency
  if ("observation_date" %in% names(data)) {
    max_date <- max(as.Date(data$observation_date), na.rm = TRUE)
    data_age <- as.numeric(Sys.Date() - max_date)

    if (data_age > max_age_days) {
      issues <- c(issues, sprintf("Data may be stale (%d days old, max %d allowed)", data_age, max_age_days))
    }
  } else {
    issues <- c(issues, "Missing observation_date column")
    data_age <- NA
  }

  # Check key metrics populated
  positivity_coverage <- 0
  if ("positivity_rate" %in% names(data)) {
    positivity_coverage <- sum(!is.na(data$positivity_rate)) / nrow(data) * 100
    if (positivity_coverage < 50) {
      issues <- c(issues, sprintf("Low positivity data coverage (%.0f%%)", positivity_coverage))
    }
  } else {
    issues <- c(issues, "Missing positivity_rate column")
  }

  hospitalization_coverage <- 0
  if ("hospitalization_rate" %in% names(data)) {
    hospitalization_coverage <- sum(!is.na(data$hospitalization_rate)) / nrow(data) * 100
    if (hospitalization_coverage < 30) {
      issues <- c(issues, sprintf("Low hospitalization data coverage (%.0f%%)", hospitalization_coverage))
    }
  }

  list(
    is_valid = length(issues) == 0,
    issues = issues,
    summary = list(
      record_count = nrow(data),
      data_age_days = if (exists("data_age")) data_age else NA,
      positivity_coverage = round(positivity_coverage, 1),
      hospitalization_coverage = round(hospitalization_coverage, 1)
    )
  )
}

#' Validate data for a specific pathogen
#' @param conn Database connection
#' @param pathogen_code Pathogen code to validate
#' @return Validation result list
validate_pathogen_data <- function(conn, pathogen_code) {
  query <- sprintf("
    SELECT s.*, p.pathogen_code
    FROM surveillance_data s
    JOIN pathogens p ON s.pathogen_id = p.pathogen_id
    WHERE p.pathogen_code = '%s'
    ORDER BY s.observation_date DESC
  ", toupper(pathogen_code))

  data <- tryCatch(
    DBI::dbGetQuery(conn, query),
    error = function(e) NULL
  )

  result <- validate_surveillance_data(data)
  result$pathogen_code <- toupper(pathogen_code)
  result
}

#' Validate all pathogens and return comprehensive report
#' @param conn Database connection
#' @return List of validation results by pathogen
validate_all_pathogens <- function(conn) {
  pathogens_query <- "SELECT pathogen_code FROM pathogens WHERE is_active = 1"
  pathogens <- tryCatch(
    DBI::dbGetQuery(conn, pathogens_query),
    error = function(e) data.frame(pathogen_code = character())
  )

  results <- lapply(pathogens$pathogen_code, function(code) {
    validate_pathogen_data(conn, code)
  })

  names(results) <- pathogens$pathogen_code

  # Calculate overall summary
  all_valid <- all(sapply(results, function(r) r$is_valid))
  total_issues <- sum(sapply(results, function(r) length(r$issues)))

  list(
    by_pathogen = results,
    overall = list(
      all_valid = all_valid,
      total_issues = total_issues,
      pathogens_checked = length(results),
      pathogens_valid = sum(sapply(results, function(r) r$is_valid))
    )
  )
}

#' Check if API data fetch was successful
#' @param api_result Result from API fetch function
#' @return Validation result list
validate_api_result <- function(api_result) {
  issues <- character()

  if (is.null(api_result)) {
    return(list(is_valid = FALSE, issues = "API returned NULL"))
  }

  if (is.data.frame(api_result)) {
    if (nrow(api_result) == 0) {
      issues <- c(issues, "API returned empty data frame")
    }
  }

  list(
    is_valid = length(issues) == 0,
    issues = issues
  )
}

#' Generate validation report as formatted text
#' @param validation_result Result from validate_all_pathogens()
#' @return Character string with formatted report
generate_validation_report <- function(validation_result) {
  lines <- c(
    "=== RespiWatch Data Validation Report ===",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    ""
  )

  # Overall summary
  overall <- validation_result$overall
  lines <- c(lines,
    "OVERALL SUMMARY:",
    sprintf("  Pathogens Checked: %d", overall$pathogens_checked),
    sprintf("  Pathogens Valid: %d", overall$pathogens_valid),
    sprintf("  Total Issues: %d", overall$total_issues),
    sprintf("  Status: %s", if (overall$all_valid) "ALL VALID" else "ISSUES FOUND"),
    ""
  )

  # Per-pathogen details
  lines <- c(lines, "PATHOGEN DETAILS:")
  for (code in names(validation_result$by_pathogen)) {
    result <- validation_result$by_pathogen[[code]]
    status <- if (result$is_valid) "OK" else "ISSUES"
    lines <- c(lines, sprintf("  %s: %s", code, status))

    if (!result$is_valid) {
      for (issue in result$issues) {
        lines <- c(lines, sprintf("    - %s", issue))
      }
    }

    if (!is.null(result$summary)) {
      lines <- c(lines,
        sprintf("    Records: %d, Age: %s days, Positivity: %.0f%%",
          result$summary$record_count,
          if (is.na(result$summary$data_age_days)) "N/A" else result$summary$data_age_days,
          result$summary$positivity_coverage
        )
      )
    }
  }

  paste(lines, collapse = "\n")
}
