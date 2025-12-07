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
# =============================================================================

#' Escape string for SQL query (prevent SQL injection)
escape_sql <- function(x) {
  if (is.null(x) || is.na(x)) return(NA)
  gsub("'", "''", as.character(x))
}

#' Validate pathogen code format (alphanumeric)
is_valid_pathogen_code <- function(code) {
  !is.null(code) && grepl("^[A-Za-z0-9_]+$", code)
}

#' Validate ISO country code format (2-letter)
is_valid_iso_code <- function(code) {
  !is.null(code) && grepl("^[A-Z]{2}$", toupper(code))
}

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

#' Prepare surveillance data for EpiEstim incidence format
#' @param surv_data Data frame with observation_date and case_count columns
#' @param aggregate_by How to aggregate data ("day", "week")
#' @return Data frame with dates and I (incidence) columns
prepare_incidence_data <- function(surv_data, aggregate_by = "week") {
  if (nrow(surv_data) == 0) {
    return(NULL)
  }

  # Ensure date column exists and is Date type
  if (!"observation_date" %in% names(surv_data)) {
    stop("Data must contain 'observation_date' column")
  }

  surv_data <- surv_data |>
    mutate(observation_date = as.Date(observation_date))

  # Determine case count column - check for actual data, not just column existence
  has_case_count <- "case_count" %in% names(surv_data) &&
    sum(!is.na(surv_data$case_count)) > 0
  has_estimated_cases <- "estimated_cases" %in% names(surv_data) &&
    sum(!is.na(surv_data$estimated_cases)) > 0

  case_col <- if (has_case_count) {
    "case_count"
  } else if (has_estimated_cases) {
    "estimated_cases"
  } else {
    # If no case count, derive from positivity rate * test volume
    surv_data <- surv_data |>
      mutate(case_count = ifelse(
        !is.na(positivity_rate) & !is.na(test_volume),
        round(positivity_rate / 100 * test_volume),
        NA_real_
      ))
    "case_count"
  }

  # Aggregate by date
  incidence_df <- surv_data |>
    group_by(observation_date) |>
    summarize(
      I = sum(.data[[case_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(observation_date) |>
    filter(!is.na(I) & I >= 0)

  # Fill in missing dates with 0 cases
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
      sd.case_count,
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
