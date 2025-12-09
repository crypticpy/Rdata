# ============================================================================
# Title: Pre-compute RT Estimates
# Purpose: Compute and store Rt estimates for all pathogens/countries
# Input: Surveillance data from SQLite database
# Output: rt_estimates table populated with pre-computed values
# ============================================================================

# Set working directory to project root
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# Load required packages
library(DBI)
library(RSQLite)
library(dplyr)
library(EpiEstim)

# Source dependencies
source("R/db_schema.R")
source("R/rt_estimation.R")
source("R/data_fallback.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

RT_CONFIG <- list(
  # Countries to compute for (ISO 3-letter codes)
  countries = c("USA"),


  # Pathogens to compute for
  pathogens = c("H3N2", "RSV", "COVID19"),

  # Weeks of historical data to use
  weeks_back = 12,

  # Window size for Rt estimation
  window_size = 4
)

# =============================================================================
# COMPUTE RT FOR SINGLE PATHOGEN/COUNTRY
# =============================================================================

#' Compute and store RT estimates for a pathogen/country combination
#' @param pathogen_code Pathogen code (H3N2, RSV, COVID19)
#' @param country_code ISO country code (USA, etc.)
#' @param weeks_back Weeks of historical data
#' @return Number of estimates inserted
compute_and_store_rt <- function(pathogen_code, country_code, weeks_back = 12) {
  message(sprintf("Computing Rt for %s / %s...", pathogen_code, country_code))

  conn <- get_db_connection()

  # Get pathogen_id
  pathogen_id <- dbGetQuery(conn, sprintf(
    "SELECT pathogen_id FROM pathogens WHERE pathogen_code = '%s'",
    pathogen_code
  ))$pathogen_id

  if (length(pathogen_id) == 0) {
    close_db_connection(conn)
    warning(sprintf("Pathogen %s not found", pathogen_code))
    return(0)
  }

  # Get country_id
  country_id <- dbGetQuery(conn, sprintf(
    "SELECT country_id FROM countries WHERE iso_code = '%s'",
    country_code
  ))$country_id

  if (length(country_id) == 0) {
    close_db_connection(conn)
    warning(sprintf("Country %s not found", country_code))
    return(0)
  }

  # Calculate date range
  end_date <- Sys.Date()
  start_date <- end_date - (weeks_back * 7)
  date_range <- seq(start_date, end_date, by = "day")

  # Get surveillance data with fallback
  incidence_data <- tryCatch({
    prepare_incidence_data_with_fallback(
      pathogen_code = pathogen_code,
      date_range = date_range,
      country = country_code
    )
  }, error = function(e) {
    warning(sprintf("Failed to get incidence data: %s", e$message))
    return(NULL)
  })

  if (is.null(incidence_data) || nrow(incidence_data) < 10) {
    close_db_connection(conn)
    warning(sprintf("Insufficient data for %s / %s", pathogen_code, country_code))
    return(0)
  }

  # CRITICAL FIX: Filter out INTERPOLATED data for accurate Rt estimation

  # Interpolated data is flat (constant extrapolation) which artificially
  # produces Rt ≈ 1.0. Only use actual surveillance data for Rt.
  if ("source_code" %in% names(incidence_data)) {
    real_data <- incidence_data[incidence_data$source_code != "INTERPOLATED", ]
    if (nrow(real_data) >= 10) {
      message(sprintf("  Filtering: %d total rows -> %d real data rows (excluding %d interpolated)",
                      nrow(incidence_data), nrow(real_data),
                      nrow(incidence_data) - nrow(real_data)))
      incidence_data <- real_data
    }
  }

  # Get source metadata
  has_fallback <- attr(incidence_data, "has_fallback") %||% FALSE
  source_coverage <- attr(incidence_data, "source_coverage") %||% list()
  source_description <- describe_data_sources_for_rt(source_coverage)

  # Get serial interval parameters
  si_params <- get_serial_interval(pathogen_code)

  # Estimate Rt
  rt_estimates <- tryCatch({
    estimate_rt(
      incidence_data,
      si_mean = si_params$mean,
      si_sd = si_params$sd,
      window_size = RT_CONFIG$window_size
    )
  }, error = function(e) {
    warning(sprintf("Rt estimation failed: %s", e$message))
    return(NULL)
  })

  if (is.null(rt_estimates) || nrow(rt_estimates) == 0) {
    close_db_connection(conn)
    warning(sprintf("No Rt estimates generated for %s / %s", pathogen_code, country_code))
    return(0)
  }

  # Delete existing estimates for this pathogen/country
  dbExecute(conn, sprintf(
    "DELETE FROM rt_estimates WHERE pathogen_id = %d AND country_id = %d",
    pathogen_id, country_id
  ))

  # Prepare data for insertion
  estimates_df <- rt_estimates |>
    transmute(
      pathogen_id = pathogen_id,
      country_id = country_id,
      estimation_date = as.character(date),
      rt_mean = rt_mean,
      rt_median = rt_median,
      rt_lower = rt_lower,
      rt_upper = rt_upper,
      rt_sd = rt_sd,
      data_points = nrow(incidence_data),
      source_description = source_description,
      has_fallback = as.integer(has_fallback),
      computed_at = as.character(Sys.time())
    )

  # Insert estimates
  dbWriteTable(conn, "rt_estimates", estimates_df, append = TRUE, row.names = FALSE)

  close_db_connection(conn)

  message(sprintf("  Inserted %d Rt estimates for %s / %s", nrow(estimates_df), pathogen_code, country_code))

  nrow(estimates_df)
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

#' Compute Rt estimates for all configured pathogens and countries
#' @return Summary of computation results
compute_all_rt_estimates <- function() {
  message("=" |> rep(60) |> paste(collapse = ""))
  message("RespiWatch Rt Pre-computation")
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  message("=" |> rep(60) |> paste(collapse = ""))

  results <- list()

  for (country in RT_CONFIG$countries) {
    for (pathogen in RT_CONFIG$pathogens) {
      key <- paste(pathogen, country, sep = "_")

      count <- tryCatch({
        compute_and_store_rt(pathogen, country, RT_CONFIG$weeks_back)
      }, error = function(e) {
        warning(sprintf("Error computing %s / %s: %s", pathogen, country, e$message))
        0
      })

      results[[key]] <- list(
        pathogen = pathogen,
        country = country,
        estimates = count,
        status = if (count > 0) "success" else "failed"
      )
    }
  }

  # Print summary
  message("\n" |> paste(rep("=", 60), collapse = ""))
  message("Summary:")

  total <- 0
  for (key in names(results)) {
    r <- results[[key]]
    message(sprintf("  %s: %d estimates (%s)", key, r$estimates, r$status))
    total <- total + r$estimates
  }

  message(sprintf("\nTotal: %d Rt estimates computed", total))
  message("=" |> rep(60) |> paste(collapse = ""))

  results
}

# =============================================================================
# HELPER: Get current Rt summary from pre-computed data
# =============================================================================

#' Load pre-computed Rt estimates from database
#' @param pathogen_code Pathogen code
#' @param country_code Country ISO code
#' @return List with rt_estimates, current_rt, and metadata
load_precomputed_rt <- function(pathogen_code, country_code = "USA") {
  conn <- get_db_connection()

  # Get IDs
  pathogen_id <- dbGetQuery(conn, sprintf(
    "SELECT pathogen_id FROM pathogens WHERE pathogen_code = '%s'",
    pathogen_code
  ))$pathogen_id

  country_id <- dbGetQuery(conn, sprintf(
    "SELECT country_id FROM countries WHERE iso_code = '%s'",
    country_code
  ))$country_id

  if (length(pathogen_id) == 0 || length(country_id) == 0) {
    close_db_connection(conn)
    return(NULL)
  }

  # Load estimates
  estimates <- dbGetQuery(conn, sprintf("
    SELECT
      estimation_date as date,
      rt_mean,
      rt_median,
      rt_lower,
      rt_upper,
      rt_sd,
      data_points,
      source_description,
      has_fallback,
      computed_at
    FROM rt_estimates
    WHERE pathogen_id = %d AND country_id = %d
    ORDER BY estimation_date
  ", pathogen_id, country_id))

  close_db_connection(conn)

  if (nrow(estimates) == 0) {
    return(NULL)
  }

  # Convert date
  estimates$date <- as.Date(estimates$date)

  # Get current (most recent) Rt
  current <- estimates[nrow(estimates), ]

  current_rt <- list(
    value = current$rt_mean,
    lower = current$rt_lower,
    upper = current$rt_upper,
    trend = if (nrow(estimates) >= 2) {
      prev <- estimates[nrow(estimates) - 1, ]$rt_mean
      if (current$rt_mean > prev * 1.05) "increasing"
      else if (current$rt_mean < prev * 0.95) "decreasing"
      else "stable"
    } else "unknown",
    phase = if (current$rt_mean > 1) "growing" else if (current$rt_mean < 1) "declining" else "stable"
  )

  list(
    pathogen = pathogen_code,
    country = country_code,
    rt_estimates = estimates,
    current_rt = current_rt,
    data_points = current$data_points,
    has_fallback = as.logical(current$has_fallback),
    source_description = current$source_description,
    computed_at = current$computed_at,
    fallback_pct = 0  # Not tracked in pre-computed
  )
}

# =============================================================================
# RUN IF CALLED DIRECTLY
# =============================================================================

if (sys.nframe() == 0) {
  compute_all_rt_estimates()
}
