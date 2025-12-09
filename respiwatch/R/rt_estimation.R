# ============================================================================
# Title: RespiWatch Rt Estimation Module
# Purpose: Estimate reproduction number (Rt) using EpiEstim package
# Input: Surveillance data from SQLite database
# Output: Rt estimates with credible intervals for all pathogens
# ============================================================================

# Load required packages -------------------------------------------------------
library(EpiEstim)
library(dplyr)
library(tidyr)
library(lubridate)

# =============================================================================
# INPUT VALIDATION HELPERS
# Note: escape_sql, is_valid_pathogen_code, is_valid_iso_code are defined in
# db_operations.R which is sourced via global.R. We don't re-define them here
# to avoid duplication and ensure consistency.
# =============================================================================

# =============================================================================
# SERIAL INTERVAL PARAMETERS
# =============================================================================

#' Get serial interval parameters for a pathogen
#' Serial interval = time between symptom onset in primary and secondary cases
#' @param pathogen_code Pathogen identifier (H3N2, RSV, COVID19)
#' @return Named list with mean and sd of serial interval (in days)
get_serial_interval <- function(pathogen_code) {
  # Literature-based serial interval estimates
  si_params <- list(
    H3N2 = list(
      mean = 2.6,
      sd = 1.5,
      source = "CDC/WHO Influenza surveillance"
    ),
    RSV = list(
      mean = 4.3,
      sd = 2.1,
      source = "NREVSS RSV surveillance"
    ),
    COVID19 = list(
      mean = 3.7,
      sd = 2.0,
      source = "CDC COVID estimates 2024"
    ),
    H5N1 = list(
      mean = 3.5,
      sd = 1.8,
      source = "WHO avian influenza reports"
    )
  )

  if (!pathogen_code %in% names(si_params)) {
    warning(sprintf("Unknown pathogen: %s. Using default H3N2 parameters.", pathogen_code))
    return(si_params$H3N2)
  }

  si_params[[pathogen_code]]
}

# =============================================================================
# DATA PREPARATION
# =============================================================================

#' Prepare surveillance data for EpiEstim incidence format (LEGACY - zero-fills gaps)
#'
#' @description
#' \lifecycle{deprecated}
#' This function zero-fills gaps in surveillance data which can artificially
#' depress Rt estimates during periods of missing data. Use
#' \code{\link{prepare_incidence_data_with_fallback}} instead for production
#' which fills gaps with alternate signals (wastewater, syndromic, forecasts).
#'
#' @param surv_data Data frame with observation_date and case_count columns
#' @param aggregate_by How to aggregate data ("day", "week")
#' @return Data frame with dates and I (incidence) columns
#' @note Use prepare_incidence_data_with_fallback() for production - avoids zero-filling
prepare_incidence_data <- function(surv_data, aggregate_by = "week") {
  .Deprecated("prepare_incidence_data_with_fallback",
              msg = "prepare_incidence_data() zero-fills gaps which distorts Rt. Use prepare_incidence_data_with_fallback() instead.")
  if (nrow(surv_data) == 0) {
    return(NULL)
  }

  # Ensure date column exists and is Date type
  if (!"observation_date" %in% names(surv_data)) {
    stop("Data must contain 'observation_date' column")
  }

  surv_data <- surv_data |>
    mutate(observation_date = as.Date(observation_date))

  # Per-row COALESCE: case_count -> estimated_cases -> derived from positivity

  # This ensures each row uses the best available case data
  # Defensive: check if columns exist before using them
  has_positivity <- "positivity_rate" %in% names(surv_data)
  has_test_volume <- "test_volume" %in% names(surv_data)
  has_case_count <- "case_count" %in% names(surv_data)
  has_estimated <- "estimated_cases" %in% names(surv_data)

  incidence_df <- surv_data |>
    mutate(
      effective_cases = coalesce(
        if (has_case_count) as.numeric(case_count) else NA_real_,
        if (has_estimated) as.numeric(estimated_cases) else NA_real_,
        if (has_positivity && has_test_volume) {
          if_else(
            !is.na(positivity_rate) & !is.na(test_volume),
            round(positivity_rate / 100 * test_volume),
            NA_real_
          )
        } else NA_real_
      )
    ) |>
    group_by(observation_date) |>
    summarize(
      I = sum(effective_cases, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(observation_date) |>
    filter(!is.na(I) & I >= 0)

  # Fill in missing dates with 0 cases
  # WARNING: This zero-filling can artificially depress Rt estimates during gaps
  # Use prepare_incidence_data_with_fallback() instead for production
  if (nrow(incidence_df) > 1) {
    date_range <- seq(
      min(incidence_df$observation_date),
      max(incidence_df$observation_date),
      by = ifelse(aggregate_by == "week", 7, 1)
    )

    incidence_df <- data.frame(observation_date = date_range) |>
      left_join(incidence_df, by = "observation_date") |>
      mutate(I = replace_na(I, 0))
  }

  # Rename for EpiEstim compatibility
  incidence_df |>
    rename(dates = observation_date)
}

#' Prepare incidence data using intelligent fallback system (RECOMMENDED)
#'
#' Uses the data fallback system to fill gaps with alternate signals
#' (wastewater, syndromic, forecasts, state data) instead of zero-filling.
#' This is the KEY DIFFERENTIATOR for RespiWatch - gaps are filled with
#' real alternate signals, not zeros that distort Rt estimates.
#'
#' @param pathogen_code Pathogen identifier (H3N2, RSV, COVID19)
#' @param date_range Date range to fetch
#' @param country ISO country code (default: "USA")
#' @return Data frame with dates, I, source metadata, and fallback flags
#' @export
prepare_incidence_data_with_fallback <- function(pathogen_code,
                                                  date_range,
                                                  country = "USA") {
  # Use the central fallback system
  fallback_data <- get_incidence_with_fallback(
    pathogen = pathogen_code,
    date_range = date_range,
    country = country
  )

  if (is.null(fallback_data) || nrow(fallback_data) == 0) {
    return(NULL)
  }

  # Rename for EpiEstim compatibility and preserve source info
  result <- fallback_data |>
    mutate(
      dates = as.Date(dates),
      I = as.numeric(I)
    ) |>
    filter(!is.na(I) & I >= 0)

  # Carry over fallback metadata as attributes
  attr(result, "source_coverage") <- attr(fallback_data, "source_coverage")
  attr(result, "has_fallback") <- attr(fallback_data, "has_fallback")
  attr(result, "gap_periods") <- attr(fallback_data, "gap_periods")

  result
}

# =============================================================================
# RT ESTIMATION
# =============================================================================

#' Estimate Rt using EpiEstim
#' @param incidence_data Data frame with dates and I columns
#' @param si_mean Mean of serial interval distribution
#' @param si_sd Standard deviation of serial interval distribution
#' @param window_size Size of sliding window for estimation (in time units)
#' @return Data frame with Rt estimates and credible intervals
estimate_rt <- function(incidence_data, si_mean, si_sd, window_size = 4) {
  if (is.null(incidence_data) || nrow(incidence_data) < 10) {
    warning("Insufficient data for Rt estimation (need at least 10 time points)")
    return(NULL)
  }

  # EpiEstim requires at least window_size + 1 data points
  if (nrow(incidence_data) <= window_size) {
    warning("Data length must exceed window size")
    return(NULL)
  }

  # Check for valid non-zero incidence data - EpiEstim hangs on all-zero data
  if (sum(incidence_data$I, na.rm = TRUE) == 0) {
    warning("No non-zero incidence data available for Rt estimation")
    return(NULL)
  }

  # Need at least some variation in the data
  if (max(incidence_data$I, na.rm = TRUE) == 0) {
    warning("All incidence values are zero - cannot estimate Rt")
    return(NULL)
  }

  tryCatch({
    # Configure serial interval as parametric (gamma distribution)
    # EpiEstim uses a discretized gamma distribution
    si_config <- make_config(
      mean_si = si_mean,
      std_si = si_sd,
      t_start = seq(2, nrow(incidence_data) - window_size),
      t_end = seq(2 + window_size, nrow(incidence_data))
    )

    # Estimate Rt
    rt_result <- estimate_R(
      incid = incidence_data$I,
      method = "parametric_si",
      config = si_config
    )

    # Extract results
    rt_df <- data.frame(
      t_start = rt_result$R$t_start,
      t_end = rt_result$R$t_end,
      rt_mean = rt_result$R$`Mean(R)`,
      rt_median = rt_result$R$`Median(R)`,
      rt_lower = rt_result$R$`Quantile.0.025(R)`,
      rt_upper = rt_result$R$`Quantile.0.975(R)`,
      rt_sd = rt_result$R$`Std(R)`
    )

    # Add dates based on window end
    rt_df$date <- incidence_data$dates[rt_df$t_end]

    # Classify epidemic phase
    rt_df <- rt_df |>
      mutate(
        phase = case_when(
          rt_lower > 1 ~ "growing",
          rt_upper < 1 ~ "declining",
          TRUE ~ "stable"
        ),
        phase_color = case_when(
          phase == "growing" ~ "#E85D4C",    # Coral (epidemic growth)
          phase == "declining" ~ "#0D9488",  # Teal (controlled)
          TRUE ~ "#6B7280"                   # Gray (stable/uncertain)
        )
      )

    rt_df

  }, error = function(e) {
    warning(paste("Rt estimation failed:", e$message))
    return(NULL)
  })
}

#' Get current Rt value with trend
#' @param rt_estimates Data frame from estimate_rt()
#' @return Named list with current Rt, trend, and status
get_current_rt <- function(rt_estimates) {
  if (is.null(rt_estimates) || nrow(rt_estimates) == 0) {
    return(list(
      value = NA,
      lower = NA,
      upper = NA,
      trend = "unknown",
      phase = "unknown",
      date = Sys.Date()
    ))
  }

  # Get most recent estimate
  latest <- rt_estimates |>
    filter(date == max(date)) |>
    slice(1)

  # Calculate trend from last 4 estimates
  if (nrow(rt_estimates) >= 4) {
    recent <- tail(rt_estimates, 4)
    trend_slope <- coef(lm(rt_mean ~ seq_along(rt_mean), data = recent))[2]
    trend <- case_when(
      trend_slope > 0.05 ~ "increasing",
      trend_slope < -0.05 ~ "decreasing",
      TRUE ~ "stable"
    )
  } else {
    trend <- "insufficient_data"
  }

  list(
    value = round(latest$rt_mean, 2),
    lower = round(latest$rt_lower, 2),
    upper = round(latest$rt_upper, 2),
    trend = trend,
    phase = latest$phase,
    date = latest$date
  )
}

# =============================================================================
# MAIN API FUNCTIONS
# =============================================================================

#' Get Rt estimates for a specific pathogen
#' @param pathogen_code Pathogen identifier (H3N2, RSV, COVID19)
#' @param country Optional country filter (ISO code)
#' @param conn Database connection (optional, will create if not provided)
#' @return List with rt_estimates, current_rt, and metadata
get_rt_for_pathogen <- function(pathogen_code, country = NULL, conn = NULL) {
  # Source database modules if needed
  if (!exists("get_db_connection")) {
    source("R/db_schema.R")
    source("R/db_operations.R")
  }

  # Get database connection
  close_conn <- FALSE
  if (is.null(conn)) {
    conn <- get_db_connection()
    close_conn <- TRUE
  }

  # Validate input parameters to prevent SQL injection
  if (!is_valid_pathogen_code(pathogen_code)) {
    warning(sprintf("Invalid pathogen code format: %s", pathogen_code))
    if (close_conn) close_db_connection(conn)
    return(list(
      pathogen = pathogen_code,
      country = country,
      rt_estimates = NULL,
      current_rt = get_current_rt(NULL),
      error = "Invalid pathogen code format"
    ))
  }

  if (!is.null(country) && !is_valid_iso_code(country)) {
    warning(sprintf("Invalid country code format: %s", country))
    if (close_conn) close_db_connection(conn)
    return(list(
      pathogen = pathogen_code,
      country = country,
      rt_estimates = NULL,
      current_rt = get_current_rt(NULL),
      error = "Invalid country code format"
    ))
  }

  # Build query with validated and escaped inputs
  country_clause <- ""
  if (!is.null(country)) {
    country_clause <- sprintf("AND c.iso_code = '%s'", escape_sql(toupper(country)))
  }

  query <- sprintf("
    SELECT
      sd.observation_date,
      COALESCE(sd.case_count, sd.estimated_cases) as case_count,
      sd.estimated_cases,
      sd.positivity_rate,
      sd.test_volume,
      c.iso_code,
      c.country_name
    FROM surveillance_data sd
    JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
    JOIN countries c ON sd.country_id = c.country_id
    WHERE p.pathogen_code = '%s'
    %s
    ORDER BY sd.observation_date
  ",
    escape_sql(pathogen_code),
    country_clause
  )

  surv_data <- tryCatch({
    DBI::dbGetQuery(conn, query)
  }, error = function(e) {
    warning(paste("Query failed:", e$message))
    data.frame()
  })

  if (close_conn) {
    close_db_connection(conn)
  }

  if (nrow(surv_data) == 0) {
    return(list(
      pathogen = pathogen_code,
      country = country,
      rt_estimates = NULL,
      current_rt = get_current_rt(NULL),
      error = "No surveillance data available"
    ))
  }

  # Get serial interval parameters
  si_params <- get_serial_interval(pathogen_code)

  # Prepare incidence data
  incidence_data <- prepare_incidence_data(surv_data)

  # Estimate Rt
  rt_estimates <- estimate_rt(
    incidence_data,
    si_mean = si_params$mean,
    si_sd = si_params$sd
  )

  # Get current Rt
  current_rt <- get_current_rt(rt_estimates)

  list(
    pathogen = pathogen_code,
    country = country,
    serial_interval = si_params,
    rt_estimates = rt_estimates,
    current_rt = current_rt,
    data_points = nrow(incidence_data),
    date_range = if (!is.null(incidence_data)) {
      c(min(incidence_data$dates), max(incidence_data$dates))
    } else NULL
  )
}

#' Get Rt estimates with intelligent fallback for data gaps (RECOMMENDED)
#'
#' This is the KEY DIFFERENTIATOR for RespiWatch. Uses the intelligent
#' fallback system to fill gaps with alternate signals (wastewater, syndromic,
#' forecasts, state data) instead of zero-filling which distorts Rt estimates.
#'
#' @param pathogen_code Pathogen identifier (H3N2, RSV, COVID19)
#' @param country Country ISO code (default: "USA")
#' @param weeks_back Number of weeks of historical data (default: 12)
#' @return List with rt_estimates, current_rt, metadata, AND source info
#' @export
get_rt_for_pathogen_with_fallback <- function(pathogen_code,
                                               country = "USA",
                                               weeks_back = 12) {
  # Validate input parameters
  if (!is_valid_pathogen_code(pathogen_code)) {
    warning(sprintf("Invalid pathogen code format: %s", pathogen_code))
    return(list(
      pathogen = pathogen_code,
      country = country,
      rt_estimates = NULL,
      current_rt = get_current_rt(NULL),
      has_fallback = FALSE,
      source_coverage = list(),
      error = "Invalid pathogen code format"
    ))
  }

  if (!is_valid_iso_code(country)) {
    warning(sprintf("Invalid country code format: %s", country))
    return(list(
      pathogen = pathogen_code,
      country = country,
      rt_estimates = NULL,
      current_rt = get_current_rt(NULL),
      has_fallback = FALSE,
      source_coverage = list(),
      error = "Invalid country code format"
    ))
  }

  # Calculate date range
  end_date <- Sys.Date()
  start_date <- end_date - (weeks_back * 7)
  date_range <- seq(start_date, end_date, by = "day")

  # Use fallback-aware incidence data preparation
  incidence_data <- prepare_incidence_data_with_fallback(
    pathogen_code = pathogen_code,
    date_range = date_range,
    country = toupper(country)
  )

  if (is.null(incidence_data) || nrow(incidence_data) == 0) {
    return(list(
      pathogen = pathogen_code,
      country = country,
      rt_estimates = NULL,
      current_rt = get_current_rt(NULL),
      has_fallback = FALSE,
      source_coverage = list(),
      gap_periods = data.frame(),
      error = "No surveillance data available (including fallback sources)"
    ))
  }

  # Extract source metadata BEFORE estimation
  has_fallback <- attr(incidence_data, "has_fallback") %||% FALSE
  source_coverage <- attr(incidence_data, "source_coverage") %||% list()
  gap_periods <- attr(incidence_data, "gap_periods") %||% data.frame()

  # Get serial interval parameters
  si_params <- get_serial_interval(pathogen_code)

  # Estimate Rt (uses same estimate_rt function)
  rt_estimates <- estimate_rt(
    incidence_data,
    si_mean = si_params$mean,
    si_sd = si_params$sd
  )

  # If we have Rt estimates AND source info, annotate which Rt points used fallback
  if (!is.null(rt_estimates) && nrow(rt_estimates) > 0 && has_fallback) {
    # Mark Rt estimates that cover dates with fallback data
    rt_estimates <- rt_estimates |>
      mutate(
        uses_fallback = sapply(date, function(d) {
          # Check if any data in the window came from fallback sources
          window_data <- incidence_data |>
            filter(dates >= d - 7, dates <= d)  # Approx window
          any(window_data$is_fallback, na.rm = TRUE)
        }),
        source_note = if_else(
          uses_fallback,
          "Includes alternate signal data",
          "Primary surveillance data"
        )
      )
  }

  # Get current Rt
  current_rt <- get_current_rt(rt_estimates)

  # Build result with full source transparency
  result <- list(
    pathogen = pathogen_code,
    country = country,
    serial_interval = si_params,
    rt_estimates = rt_estimates,
    current_rt = current_rt,
    data_points = nrow(incidence_data),
    date_range = c(min(incidence_data$dates), max(incidence_data$dates)),

    # KEY: Source transparency metadata
    has_fallback = has_fallback,
    source_coverage = source_coverage,
    gap_periods = gap_periods,
    fallback_pct = if (has_fallback) {
      round(sum(incidence_data$is_fallback, na.rm = TRUE) / nrow(incidence_data) * 100, 1)
    } else 0,

    # Human-readable source description
    source_description = describe_data_sources_for_rt(source_coverage)
  )

  result
}

#' Describe data sources used in Rt estimation
#' @param coverage Source coverage list from get_surveillance_with_fallback
#' @return Human-readable string
describe_data_sources_for_rt <- function(coverage) {
  if (is.null(coverage) || length(coverage) == 0) {
    return("No data available")
  }

  source_labels <- list(
    CDC_FLUVIEW = "CDC FluView",
    CDC_NREVSS = "CDC NREVSS",
    CDC_COVID = "CDC COVID",
    RSV_NET = "RSV-NET",
    WHO_FLUMART = "WHO FluNet",
    ECDC = "ECDC",
    NWSS_WASTEWATER = "Wastewater",
    FLUSIGHT_FORECAST = "FluSight Forecast",
    DELPHI = "Syndromic",
    STATE_HEALTH = "State Health Depts",
    INTERPOLATED = "Interpolated"
  )

  used_sources <- names(coverage)
  primary <- used_sources[used_sources %in% c("CDC_FLUVIEW", "CDC_NREVSS", "CDC_COVID", "RSV_NET", "WHO_FLUMART", "ECDC")]
  fallback <- setdiff(used_sources, primary)

  if (length(fallback) == 0) {
    return("Primary surveillance sources")
  }

  fallback_names <- sapply(fallback, function(s) source_labels[[s]] %||% s)
  sprintf("Primary + %s", paste(fallback_names, collapse = ", "))
}

#' Get Rt for all pathogens using fallback system (RECOMMENDED)
#' @param country Country ISO code
#' @param weeks_back Number of weeks of historical data
#' @return List of Rt results by pathogen with source metadata
#' @export
get_rt_all_pathogens_with_fallback <- function(country = "USA", weeks_back = 12) {
  pathogens <- c("H3N2", "RSV", "COVID19")

  results <- lapply(pathogens, function(p) {
    tryCatch({
      get_rt_for_pathogen_with_fallback(p, country = country, weeks_back = weeks_back)
    }, error = function(e) {
      list(
        pathogen = p,
        country = country,
        rt_estimates = NULL,
        current_rt = get_current_rt(NULL),
        has_fallback = FALSE,
        source_coverage = list(),
        error = e$message
      )
    })
  })

  names(results) <- pathogens

  # Add aggregate metadata across all pathogens
  attr(results, "any_fallback") <- any(sapply(results, function(r) r$has_fallback %||% FALSE))
  attr(results, "fetch_timestamp") <- Sys.time()

  results
}

#' Get Rt for all pathogens (for dashboard overview)
#' @param conn Database connection (optional)
#' @return List of Rt results by pathogen
get_rt_all_pathogens <- function(conn = NULL) {
  pathogens <- c("H3N2", "RSV", "COVID19")

  results <- lapply(pathogens, function(p) {
    tryCatch({
      get_rt_for_pathogen(p, conn = conn)
    }, error = function(e) {
      list(
        pathogen = p,
        rt_estimates = NULL,
        current_rt = get_current_rt(NULL),
        error = e$message
      )
    })
  })

  names(results) <- pathogens
  results
}

# =============================================================================
# PRE-COMPUTED RT LOADING
# =============================================================================

#' Load pre-computed Rt estimates from database
#' This is the RECOMMENDED function for the Shiny app - loads fast from
#' pre-computed rt_estimates table instead of computing on-the-fly.
#'
#' @param pathogen_code Pathogen code (H3N2, RSV, COVID19)
#' @param country_code Country ISO code (default: "USA")
#' @return List with rt_estimates, current_rt, and metadata (same format as get_rt_for_pathogen_with_fallback)
#' @export
load_precomputed_rt <- function(pathogen_code, country_code = "USA") {
  conn <- get_db_connection()

  # Get IDs
  pathogen_id <- dbGetQuery(conn, sprintf(
    "SELECT pathogen_id FROM pathogens WHERE pathogen_code = '%s'",
    escape_sql(pathogen_code)
  ))$pathogen_id

  country_id <- dbGetQuery(conn, sprintf(
    "SELECT country_id FROM countries WHERE iso_code = '%s'",
    escape_sql(country_code)
  ))$country_id

  if (length(pathogen_id) == 0 || length(country_id) == 0) {
    close_db_connection(conn)
    return(list(
      pathogen = pathogen_code,
      country = country_code,
      rt_estimates = NULL,
      current_rt = get_current_rt(NULL),
      has_fallback = FALSE,
      source_description = "No data",
      fallback_pct = 0,
      error = "Pathogen or country not found"
    ))
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
    return(list(
      pathogen = pathogen_code,
      country = country_code,
      rt_estimates = NULL,
      current_rt = get_current_rt(NULL),
      has_fallback = FALSE,
      source_description = "No pre-computed data",
      fallback_pct = 0,
      error = "No pre-computed Rt estimates available. Run scripts/compute_rt_estimates.R"
    ))
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
    source_description = current$source_description %||% "Pre-computed",
    fallback_pct = 0,
    computed_at = current$computed_at
  )
}

# =============================================================================
# VISUALIZATION HELPERS
# =============================================================================

#' Create Rt time series data for plotting
#' @param rt_result Result from get_rt_for_pathogen()
#' @return Data frame ready for ggplot/plotly
prepare_rt_plot_data <- function(rt_result) {
  if (is.null(rt_result$rt_estimates)) {
    return(NULL)
  }

  rt_result$rt_estimates |>
    select(date, rt_mean, rt_lower, rt_upper, phase, phase_color) |>
    mutate(
      pathogen = rt_result$pathogen,
      threshold = 1.0
    )
}

#' Summarize Rt estimates for table display
#' @param rt_results List of results from get_rt_all_pathogens()
#' @return Data frame with summary statistics
summarize_rt_table <- function(rt_results) {
  summaries <- lapply(names(rt_results), function(p) {
    result <- rt_results[[p]]
    current <- result$current_rt

    data.frame(
      Pathogen = p,
      `Current Rt` = sprintf("%.2f (%.2f-%.2f)",
                             current$value, current$lower, current$upper),
      Phase = current$phase,
      Trend = current$trend,
      `Last Updated` = as.character(current$date),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summaries)
}
