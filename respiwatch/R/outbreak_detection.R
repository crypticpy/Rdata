# ============================================================================
# Title: RespiWatch Outbreak Detection Module
# Purpose: Implement CUSUM, EARS, and other outbreak detection algorithms
# Input: Surveillance data time series
# Output: Outbreak alerts with confidence levels
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(lubridate)

# =============================================================================
# INPUT VALIDATION
# =============================================================================

#' Validate pathogen code format
is_valid_pathogen_code <- function(code) {
  !is.null(code) && grepl("^[A-Za-z0-9_]+$", code)
}

# =============================================================================
# CUSUM (Cumulative Sum) ALGORITHM
# =============================================================================

#' Calculate CUSUM statistic for outbreak detection
#' @param data Numeric vector of case counts or rates
#' @param baseline_mean Mean of baseline period (historical average)
#' @param baseline_sd Standard deviation of baseline period
#' @param k Slack parameter (typically 0.5 to 1 SD)
#' @param h Threshold parameter (typically 4-5 SD)
#' @return Data frame with CUSUM statistics and alerts
calculate_cusum <- function(data, baseline_mean = NULL, baseline_sd = NULL,
                            k = 0.5, h = 5) {
  if (length(data) < 10) {
    warning("Insufficient data for CUSUM (need at least 10 points)")
    return(NULL)
  }

  # Use first 1/3 of data as baseline if not provided
  if (is.null(baseline_mean) || is.null(baseline_sd)) {
    baseline_n <- max(ceiling(length(data) / 3), 5)
    baseline_data <- head(data, baseline_n)
    baseline_mean <- mean(baseline_data, na.rm = TRUE)
    baseline_sd <- sd(baseline_data, na.rm = TRUE)

    # Prevent division by zero
    if (baseline_sd == 0 || is.na(baseline_sd)) {
      baseline_sd <- 1
    }
  }

  # Standardize data
  z <- (data - baseline_mean) / baseline_sd

  # Calculate CUSUM (two-sided)
  n <- length(data)
  cusum_pos <- numeric(n)  # Detects increases

  cusum_neg <- numeric(n)  # Detects decreases

  for (i in 1:n) {
    if (i == 1) {
      cusum_pos[i] <- max(0, z[i] - k)
      cusum_neg[i] <- min(0, z[i] + k)
    } else {
      cusum_pos[i] <- max(0, cusum_pos[i - 1] + z[i] - k)
      cusum_neg[i] <- min(0, cusum_neg[i - 1] + z[i] + k)
    }
  }

  data.frame(
    index = 1:n,
    value = data,
    z_score = z,
    cusum_pos = cusum_pos,
    cusum_neg = cusum_neg,
    alert_increase = cusum_pos > h,
    alert_decrease = cusum_neg < -h,
    alert = cusum_pos > h | cusum_neg < -h,
    baseline_mean = baseline_mean,
    baseline_sd = baseline_sd,
    threshold = h
  )
}

# =============================================================================
# EARS (Early Aberration Reporting System) ALGORITHMS
# =============================================================================

#' EARS C1 Algorithm (simple threshold method)
#' @param data Numeric vector of case counts
#' @param baseline_window Number of days for baseline (default 7)
#' @param threshold Z-score threshold for alert (default 2)
#' @return Data frame with EARS C1 statistics
ears_c1 <- function(data, baseline_window = 7, threshold = 2) {
  n <- length(data)
  if (n < baseline_window + 2) {
    warning("Insufficient data for EARS C1")
    return(NULL)
  }

  result <- data.frame(
    index = 1:n,
    value = data,
    baseline_mean = NA_real_,
    baseline_sd = NA_real_,
    z_score = NA_real_,
    alert = FALSE
  )

  # Calculate for each point after baseline window
  for (i in (baseline_window + 2):n) {
    baseline_start <- i - baseline_window - 1
    baseline_end <- i - 2  # 1-day gap to avoid detecting same event
    baseline_data <- data[baseline_start:baseline_end]

    mean_val <- mean(baseline_data, na.rm = TRUE)
    sd_val <- sd(baseline_data, na.rm = TRUE)

    # Prevent division by zero
    if (sd_val == 0 || is.na(sd_val)) {
      sd_val <- 1
    }

    z <- (data[i] - mean_val) / sd_val

    result$baseline_mean[i] <- mean_val
    result$baseline_sd[i] <- sd_val
    result$z_score[i] <- z
    result$alert[i] <- z > threshold
  }

  result
}

#' EARS C2 Algorithm (with 2-day lag)
#' Similar to C1 but includes lagged comparison
#' @param data Numeric vector of case counts
#' @param baseline_window Number of days for baseline (default 7)
#' @param threshold Z-score threshold (default 2)
#' @return Data frame with EARS C2 statistics
ears_c2 <- function(data, baseline_window = 7, threshold = 2) {
  n <- length(data)
  if (n < baseline_window + 3) {
    warning("Insufficient data for EARS C2")
    return(NULL)
  }

  result <- data.frame(
    index = 1:n,
    value = data,
    baseline_mean = NA_real_,
    baseline_sd = NA_real_,
    z_score = NA_real_,
    c2_stat = NA_real_,
    alert = FALSE
  )

  for (i in (baseline_window + 3):n) {
    baseline_start <- i - baseline_window - 2
    baseline_end <- i - 3  # 2-day gap
    baseline_data <- data[baseline_start:baseline_end]

    mean_val <- mean(baseline_data, na.rm = TRUE)
    sd_val <- sd(baseline_data, na.rm = TRUE)

    if (sd_val == 0 || is.na(sd_val)) {
      sd_val <- 1
    }

    z <- (data[i] - mean_val) / sd_val

    # C2 combines current day with previous 2 days
    if (i >= 3) {
      z_prev <- ifelse(is.na(result$z_score[i - 1]), 0, max(0, result$z_score[i - 1] - 1))
      z_prev2 <- ifelse(is.na(result$z_score[i - 2]), 0, max(0, result$z_score[i - 2] - 1))
      c2_stat <- max(0, z - 1) + z_prev + z_prev2
    } else {
      c2_stat <- max(0, z - 1)
    }

    result$baseline_mean[i] <- mean_val
    result$baseline_sd[i] <- sd_val
    result$z_score[i] <- z
    result$c2_stat[i] <- c2_stat
    result$alert[i] <- c2_stat > threshold
  }

  result
}

#' EARS C3 Algorithm (with longer lookback)
#' Uses 28-day baseline with 2-day gap
#' @param data Numeric vector of case counts
#' @param threshold Z-score threshold (default 2)
#' @return Data frame with EARS C3 statistics
ears_c3 <- function(data, threshold = 2) {
  ears_c2(data, baseline_window = 28, threshold = threshold)
}

# =============================================================================
# ABERRATION DETECTION ENSEMBLE
# =============================================================================

#' Run multiple outbreak detection algorithms
#' @param incidence_data Data frame with dates and I (incidence) columns
#' @param algorithms Character vector of algorithms to run
#' @return List with results from each algorithm
run_outbreak_detection <- function(incidence_data,
                                    algorithms = c("cusum", "ears_c1", "ears_c2")) {
  if (is.null(incidence_data) || nrow(incidence_data) < 10) {
    return(list(
      status = "insufficient_data",
      message = "Need at least 10 data points for outbreak detection"
    ))
  }

  data <- incidence_data$I
  dates <- incidence_data$dates

  results <- list()

  if ("cusum" %in% algorithms) {
    cusum_result <- calculate_cusum(data)
    if (!is.null(cusum_result)) {
      cusum_result$date <- dates
      results$cusum <- cusum_result
    }
  }

  if ("ears_c1" %in% algorithms) {
    c1_result <- ears_c1(data)
    if (!is.null(c1_result)) {
      c1_result$date <- dates
      results$ears_c1 <- c1_result
    }
  }

  if ("ears_c2" %in% algorithms) {
    c2_result <- ears_c2(data)
    if (!is.null(c2_result)) {
      c2_result$date <- dates
      results$ears_c2 <- c2_result
    }
  }

  if ("ears_c3" %in% algorithms) {
    c3_result <- ears_c3(data)
    if (!is.null(c3_result)) {
      c3_result$date <- dates
      results$ears_c3 <- c3_result
    }
  }

  # Combine alerts across algorithms
  results$combined_alerts <- combine_alerts(results)
  results$status <- "success"

  results
}

#' Combine alerts from multiple algorithms
#' @param results List of results from run_outbreak_detection
#' @return Data frame with combined alert information
combine_alerts <- function(results) {
  if (length(results) == 0) {
    return(NULL)
  }

  # Get dates from first available result
  first_result <- results[[1]]
  if (is.null(first_result) || !is.data.frame(first_result)) {
    return(NULL)
  }

  dates <- first_result$date
  n <- length(dates)

  combined <- data.frame(
    date = dates,
    cusum_alert = rep(FALSE, n),
    ears_c1_alert = rep(FALSE, n),
    ears_c2_alert = rep(FALSE, n),
    ears_c3_alert = rep(FALSE, n)
  )

  if (!is.null(results$cusum)) {
    combined$cusum_alert <- results$cusum$alert
  }
  if (!is.null(results$ears_c1)) {
    combined$ears_c1_alert <- results$ears_c1$alert
  }
  if (!is.null(results$ears_c2)) {
    combined$ears_c2_alert <- results$ears_c2$alert
  }
  if (!is.null(results$ears_c3)) {
    combined$ears_c3_alert <- results$ears_c3$alert
  }

  # Calculate consensus score (number of algorithms detecting)
  combined <- combined |>
    mutate(
      n_algorithms_alerting = cusum_alert + ears_c1_alert +
                              ears_c2_alert + ears_c3_alert,
      confidence = case_when(
        n_algorithms_alerting >= 3 ~ "high",
        n_algorithms_alerting == 2 ~ "medium",
        n_algorithms_alerting == 1 ~ "low",
        TRUE ~ "none"
      ),
      any_alert = n_algorithms_alerting > 0
    )

  combined
}

# =============================================================================
# MAIN API FUNCTIONS
# =============================================================================

#' Get outbreak detection results for a pathogen
#' @param pathogen_code Pathogen identifier
#' @param country Optional country filter
#' @param conn Database connection (optional)
#' @return List with outbreak detection results
get_outbreak_detection <- function(pathogen_code, country = NULL, conn = NULL) {
  # Validate input
  if (!is_valid_pathogen_code(pathogen_code)) {
    return(list(
      status = "error",
      message = "Invalid pathogen code format"
    ))
  }

  # Source required modules
  if (!exists("get_db_connection")) {
    source("R/db_schema.R")
    source("R/db_operations.R")
  }
  if (!exists("prepare_incidence_data")) {
    source("R/rt_estimation.R")
  }

  # Get database connection
  close_conn <- FALSE
  if (is.null(conn)) {
    conn <- get_db_connection()
    close_conn <- TRUE
  }

  # Build query with parameterized inputs to prevent SQL injection
  if (!is.null(country) && grepl("^[A-Z]{2}$", toupper(country))) {
    query <- "
      SELECT
        sd.observation_date,
        sd.case_count,
        sd.estimated_cases,
        sd.positivity_rate,
        sd.test_volume
      FROM surveillance_data sd
      JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
      LEFT JOIN countries c ON sd.country_id = c.country_id
      WHERE p.pathogen_code = ?
      AND c.iso_code = ?
      ORDER BY sd.observation_date
    "
    surv_data <- tryCatch({
      stmt <- DBI::dbSendQuery(conn, query)
      DBI::dbBind(stmt, list(pathogen_code, toupper(country)))
      result <- DBI::dbFetch(stmt)
      DBI::dbClearResult(stmt)
      result
    }, error = function(e) {
      warning(paste("Query failed:", e$message))
      data.frame()
    })
  } else {
    query <- "
      SELECT
        sd.observation_date,
        sd.case_count,
        sd.estimated_cases,
        sd.positivity_rate,
        sd.test_volume
      FROM surveillance_data sd
      JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
      LEFT JOIN countries c ON sd.country_id = c.country_id
      WHERE p.pathogen_code = ?
      ORDER BY sd.observation_date
    "
    surv_data <- tryCatch({
      stmt <- DBI::dbSendQuery(conn, query)
      DBI::dbBind(stmt, list(pathogen_code))
      result <- DBI::dbFetch(stmt)
      DBI::dbClearResult(stmt)
      result
    }, error = function(e) {
      warning(paste("Query failed:", e$message))
      data.frame()
    })
  }

  if (close_conn) {
    DBI::dbDisconnect(conn)
  }

  if (nrow(surv_data) == 0) {
    return(list(
      status = "no_data",
      message = "No surveillance data available"
    ))
  }

  # Prepare incidence data
  incidence_data <- prepare_incidence_data(surv_data)

  # Run outbreak detection
  detection_results <- run_outbreak_detection(incidence_data)

  # Add metadata
  detection_results$pathogen <- pathogen_code
  detection_results$country <- country
  detection_results$analysis_date <- Sys.time()
  detection_results$data_range <- c(
    min(incidence_data$dates, na.rm = TRUE),
    max(incidence_data$dates, na.rm = TRUE)
  )

  detection_results
}

#' Get current outbreak status summary
#' @param detection_result Result from get_outbreak_detection()
#' @return List with current status summary
get_outbreak_status <- function(detection_result) {
  if (is.null(detection_result) || detection_result$status != "success") {
    return(list(
      status = "unknown",
      confidence = "none",
      message = "Unable to determine outbreak status"
    ))
  }

  alerts <- detection_result$combined_alerts
  if (is.null(alerts) || nrow(alerts) == 0) {
    return(list(
      status = "unknown",
      confidence = "none",
      message = "No alert data available"
    ))
  }

  # Get most recent alert status
  recent <- tail(alerts, 4)  # Last 4 time points

  # Current status
  latest <- tail(recent, 1)

  # Trend: are alerts increasing?
  recent_alert_count <- sum(recent$any_alert, na.rm = TRUE)

  status <- if (latest$any_alert) {
    if (recent_alert_count >= 3) "outbreak_ongoing" else "potential_outbreak"
  } else {
    if (recent_alert_count >= 2) "elevated_risk" else "normal"
  }

  list(
    status = status,
    confidence = latest$confidence,
    n_algorithms_alerting = latest$n_algorithms_alerting,
    recent_alerts = recent_alert_count,
    last_alert_date = if (any(alerts$any_alert)) {
      max(alerts$date[alerts$any_alert])
    } else NA,
    message = case_when(
      status == "outbreak_ongoing" ~ "Multiple algorithms detecting sustained outbreak signal",
      status == "potential_outbreak" ~ "Alert detected - monitoring closely",
      status == "elevated_risk" ~ "Recent alert activity - elevated surveillance",
      TRUE ~ "No outbreak signals detected"
    )
  )
}

#' Get outbreak detection for all pathogens
#' @param conn Database connection (optional)
#' @return List of outbreak detection results by pathogen
get_outbreak_all_pathogens <- function(conn = NULL) {
  pathogens <- c("H3N2", "RSV", "COVID19")

  results <- lapply(pathogens, function(p) {
    tryCatch({
      detection <- get_outbreak_detection(p, conn = conn)
      status <- get_outbreak_status(detection)
      list(
        detection = detection,
        status = status
      )
    }, error = function(e) {
      list(
        detection = list(status = "error", message = e$message),
        status = list(status = "unknown", confidence = "none")
      )
    })
  })

  names(results) <- pathogens
  results
}

# =============================================================================
# VISUALIZATION HELPERS
# =============================================================================

#' Prepare outbreak detection data for plotting
#' @param detection_result Result from get_outbreak_detection()
#' @return Data frame ready for ggplot/plotly
prepare_outbreak_plot_data <- function(detection_result) {
  if (is.null(detection_result) || detection_result$status != "success") {
    return(NULL)
  }

  if (is.null(detection_result$cusum)) {
    return(NULL)
  }

  cusum <- detection_result$cusum
  alerts <- detection_result$combined_alerts

  plot_data <- cusum |>
    select(date, value, cusum_pos, cusum_neg, alert) |>
    left_join(
      alerts |> select(date, n_algorithms_alerting, confidence),
      by = "date"
    ) |>
    mutate(
      pathogen = detection_result$pathogen,
      threshold_upper = detection_result$cusum$threshold[1],
      threshold_lower = -detection_result$cusum$threshold[1]
    )

  plot_data
}

#' Summary table for outbreak alerts
#' @param all_results Result from get_outbreak_all_pathogens()
#' @return Data frame formatted for display
summarize_outbreak_table <- function(all_results) {
  summaries <- lapply(names(all_results), function(p) {
    result <- all_results[[p]]
    status <- result$status

    data.frame(
      Pathogen = p,
      Status = status$status,
      Confidence = status$confidence,
      `Algorithms Alerting` = status$n_algorithms_alerting,
      `Recent Alerts (4 periods)` = status$recent_alerts,
      Message = status$message,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  do.call(rbind, summaries)
}
