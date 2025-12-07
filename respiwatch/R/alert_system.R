# ============================================================================
# Title: RespiWatch Alert System Module
# Purpose: Generate and manage in-app alert notifications
# Input: Surveillance data, Rt estimates, outbreak detection results
# Output: Alert notifications for display in Shiny app
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(lubridate)

# =============================================================================
# ALERT CONFIGURATION
# =============================================================================

#' Alert severity levels
ALERT_LEVELS <- list(
  critical = list(
    level = 4,
    color = "#DC2626",  # Red
    icon = "exclamation-triangle",
    badge = "danger"
  ),
  warning = list(
    level = 3,
    color = "#F59E0B",  # Amber
    icon = "exclamation-circle",
    badge = "warning"
  ),
  info = list(
    level = 2,
    color = "#3B82F6",  # Blue
    icon = "info-circle",
    badge = "info"
  ),
  success = list(
    level = 1,
    color = "#10B981",  # Green
    icon = "check-circle",
    badge = "success"
  )
)

#' Alert thresholds
ALERT_THRESHOLDS <- list(
  rt_critical = 1.5,      # Rt above this is critical
  rt_warning = 1.2,       # Rt above this is warning
  rt_decline = 0.8,       # Rt below this shows decline
  positivity_high = 10,   # Positivity rate above 10%
  case_surge = 1.5,       # 50% increase week-over-week
  hospitalization_high = 5  # Hospitalization rate above 5%
)

# =============================================================================
# ALERT GENERATION
# =============================================================================

#' Create an alert object
#' @param type Alert type identifier
#' @param severity Alert severity level (critical, warning, info, success)
#' @param pathogen Pathogen code
#' @param title Short title for the alert
#' @param message Detailed message
#' @param value Numeric value that triggered the alert
#' @param threshold Threshold that was crossed
#' @return Alert object (list)
create_alert <- function(type, severity, pathogen, title, message,
                         value = NULL, threshold = NULL) {
  list(
    id = paste(type, pathogen, format(Sys.time(), "%Y%m%d%H%M%S"), sep = "_"),
    type = type,
    severity = severity,
    pathogen = pathogen,
    title = title,
    message = message,
    value = value,
    threshold = threshold,
    timestamp = Sys.time(),
    dismissed = FALSE,
    config = ALERT_LEVELS[[severity]]
  )
}

#' Generate alerts based on Rt estimates
#' @param rt_result Result from get_rt_for_pathogen()
#' @return List of alert objects
generate_rt_alerts <- function(rt_result) {
  alerts <- list()

  if (is.null(rt_result) || is.null(rt_result$current_rt)) {
    return(alerts)
  }

  current_rt <- rt_result$current_rt
  pathogen <- rt_result$pathogen

  if (is.na(current_rt$value)) {
    return(alerts)
  }

  # Critical: Rt > 1.5
  if (current_rt$value > ALERT_THRESHOLDS$rt_critical) {
    alerts[[length(alerts) + 1]] <- create_alert(
      type = "rt_critical",
      severity = "critical",
      pathogen = pathogen,
      title = sprintf("%s: Critical Rt Level", pathogen),
      message = sprintf(
        "Reproduction number Rt = %.2f (95%% CI: %.2f-%.2f) indicates rapid spread. Each case generates %.1f new cases on average.",
        current_rt$value, current_rt$lower, current_rt$upper, current_rt$value
      ),
      value = current_rt$value,
      threshold = ALERT_THRESHOLDS$rt_critical
    )
  }
  # Warning: Rt > 1.2
  else if (current_rt$value > ALERT_THRESHOLDS$rt_warning) {
    alerts[[length(alerts) + 1]] <- create_alert(
      type = "rt_warning",
      severity = "warning",
      pathogen = pathogen,
      title = sprintf("%s: Elevated Rt Level", pathogen),
      message = sprintf(
        "Reproduction number Rt = %.2f indicates epidemic growth. Surveillance recommended.",
        current_rt$value
      ),
      value = current_rt$value,
      threshold = ALERT_THRESHOLDS$rt_warning
    )
  }
  # Info: Rt declining
  else if (current_rt$value < ALERT_THRESHOLDS$rt_decline) {
    alerts[[length(alerts) + 1]] <- create_alert(
      type = "rt_declining",
      severity = "success",
      pathogen = pathogen,
      title = sprintf("%s: Declining Transmission", pathogen),
      message = sprintf(
        "Reproduction number Rt = %.2f indicates declining transmission.",
        current_rt$value
      ),
      value = current_rt$value,
      threshold = ALERT_THRESHOLDS$rt_decline
    )
  }

  # Trend alerts
  if (!is.null(current_rt$trend) && current_rt$trend == "increasing") {
    alerts[[length(alerts) + 1]] <- create_alert(
      type = "rt_trend_up",
      severity = "warning",
      pathogen = pathogen,
      title = sprintf("%s: Rt Trend Increasing", pathogen),
      message = "The reproduction number shows an increasing trend over recent weeks.",
      value = current_rt$value
    )
  }

  alerts
}

#' Generate alerts based on outbreak detection
#' @param detection_result Result from get_outbreak_detection()
#' @return List of alert objects
generate_outbreak_alerts <- function(detection_result) {
  alerts <- list()

  if (is.null(detection_result) || detection_result$status != "success") {
    return(alerts)
  }

  # Source outbreak detection if needed
  if (!exists("get_outbreak_status")) {
    source("R/outbreak_detection.R")
  }

  status <- get_outbreak_status(detection_result)
  pathogen <- detection_result$pathogen

  if (status$status == "outbreak_ongoing") {
    alerts[[length(alerts) + 1]] <- create_alert(
      type = "outbreak_detected",
      severity = "critical",
      pathogen = pathogen,
      title = sprintf("%s: Outbreak Detected", pathogen),
      message = sprintf(
        "Multiple surveillance algorithms detecting sustained outbreak signal. %d of 4 algorithms alerting. Confidence: %s.",
        status$n_algorithms_alerting, status$confidence
      ),
      value = status$n_algorithms_alerting,
      threshold = 3
    )
  } else if (status$status == "potential_outbreak") {
    alerts[[length(alerts) + 1]] <- create_alert(
      type = "potential_outbreak",
      severity = "warning",
      pathogen = pathogen,
      title = sprintf("%s: Potential Outbreak", pathogen),
      message = "Alert detected by surveillance algorithms. Continue monitoring.",
      value = status$n_algorithms_alerting
    )
  } else if (status$status == "elevated_risk") {
    alerts[[length(alerts) + 1]] <- create_alert(
      type = "elevated_risk",
      severity = "info",
      pathogen = pathogen,
      title = sprintf("%s: Elevated Risk", pathogen),
      message = "Recent alert activity detected. Enhanced surveillance recommended.",
      value = status$recent_alerts
    )
  }

  alerts
}

#' Generate alerts based on surveillance data
#' @param surveillance_data Current surveillance data
#' @return List of alert objects
generate_surveillance_alerts <- function(surveillance_data) {
  alerts <- list()

  if (is.null(surveillance_data) || nrow(surveillance_data) == 0) {
    return(alerts)
  }

  # Get most recent data by pathogen
  recent <- surveillance_data |>
    group_by(pathogen_code) |>
    arrange(desc(observation_date)) |>
    slice(1:2) |>
    ungroup()

  # Check each pathogen
  for (pathogen in unique(recent$pathogen_code)) {
    pathogen_data <- recent |> filter(pathogen_code == pathogen)

    # High positivity rate
    if ("positivity_rate" %in% names(pathogen_data)) {
      latest_pos <- pathogen_data$positivity_rate[1]
      if (!is.na(latest_pos) && latest_pos > ALERT_THRESHOLDS$positivity_high) {
        alerts[[length(alerts) + 1]] <- create_alert(
          type = "high_positivity",
          severity = "warning",
          pathogen = pathogen,
          title = sprintf("%s: High Positivity Rate", pathogen),
          message = sprintf(
            "Test positivity rate of %.1f%% exceeds threshold of %.1f%%.",
            latest_pos, ALERT_THRESHOLDS$positivity_high
          ),
          value = latest_pos,
          threshold = ALERT_THRESHOLDS$positivity_high
        )
      }
    }

    # Case surge (week-over-week)
    if (nrow(pathogen_data) >= 2 && "case_count" %in% names(pathogen_data)) {
      current_cases <- pathogen_data$case_count[1]
      previous_cases <- pathogen_data$case_count[2]
      if (!is.na(current_cases) && !is.na(previous_cases) && previous_cases > 0) {
        ratio <- current_cases / previous_cases
        if (ratio > ALERT_THRESHOLDS$case_surge) {
          alerts[[length(alerts) + 1]] <- create_alert(
            type = "case_surge",
            severity = "warning",
            pathogen = pathogen,
            title = sprintf("%s: Case Surge Detected", pathogen),
            message = sprintf(
              "Cases increased by %.0f%% week-over-week (from %d to %d).",
              (ratio - 1) * 100, previous_cases, current_cases
            ),
            value = ratio,
            threshold = ALERT_THRESHOLDS$case_surge
          )
        }
      }
    }

    # High hospitalization
    if ("hospitalization_rate" %in% names(pathogen_data)) {
      latest_hosp <- pathogen_data$hospitalization_rate[1]
      if (!is.na(latest_hosp) && latest_hosp > ALERT_THRESHOLDS$hospitalization_high) {
        alerts[[length(alerts) + 1]] <- create_alert(
          type = "high_hospitalization",
          severity = "critical",
          pathogen = pathogen,
          title = sprintf("%s: High Hospitalization Rate", pathogen),
          message = sprintf(
            "Hospitalization rate of %.1f%% indicates severe disease burden.",
            latest_hosp
          ),
          value = latest_hosp,
          threshold = ALERT_THRESHOLDS$hospitalization_high
        )
      }
    }
  }

  alerts
}

# =============================================================================
# MAIN API FUNCTIONS
# =============================================================================

#' Get all current alerts for the dashboard
#' @param rt_results Optional list of Rt results by pathogen
#' @param detection_results Optional outbreak detection results
#' @param surveillance_data Optional surveillance data
#' @return List of all current alerts, sorted by severity
get_current_alerts <- function(rt_results = NULL, detection_results = NULL,
                               surveillance_data = NULL) {
  all_alerts <- list()

  # Generate Rt alerts
  if (!is.null(rt_results)) {
    for (p in names(rt_results)) {
      rt_alerts <- generate_rt_alerts(rt_results[[p]])
      all_alerts <- c(all_alerts, rt_alerts)
    }
  }

  # Generate outbreak alerts
  if (!is.null(detection_results)) {
    for (p in names(detection_results)) {
      outbreak_alerts <- generate_outbreak_alerts(detection_results[[p]]$detection)
      all_alerts <- c(all_alerts, outbreak_alerts)
    }
  }

  # Generate surveillance alerts
  if (!is.null(surveillance_data)) {
    surv_alerts <- generate_surveillance_alerts(surveillance_data)
    all_alerts <- c(all_alerts, surv_alerts)
  }

  # Sort by severity (critical first)
  if (length(all_alerts) > 0) {
    severities <- sapply(all_alerts, function(a) a$config$level)
    all_alerts <- all_alerts[order(severities, decreasing = TRUE)]
  }

  all_alerts
}

#' Get alerts filtered by pathogen
#' @param alerts List of alerts from get_current_alerts()
#' @param pathogen Pathogen code to filter by
#' @return Filtered list of alerts
filter_alerts_by_pathogen <- function(alerts, pathogen) {
  if (is.null(alerts) || length(alerts) == 0) {
    return(list())
  }

  Filter(function(a) a$pathogen == pathogen, alerts)
}

#' Get alerts filtered by severity
#' @param alerts List of alerts
#' @param min_severity Minimum severity level
#' @return Filtered list of alerts
filter_alerts_by_severity <- function(alerts, min_severity = "info") {
  if (is.null(alerts) || length(alerts) == 0) {
    return(list())
  }

  min_level <- ALERT_LEVELS[[min_severity]]$level
  Filter(function(a) a$config$level >= min_level, alerts)
}

#' Get alert count by severity
#' @param alerts List of alerts
#' @return Named vector with counts by severity
count_alerts_by_severity <- function(alerts) {
  if (is.null(alerts) || length(alerts) == 0) {
    return(c(critical = 0, warning = 0, info = 0, success = 0))
  }

  severities <- sapply(alerts, function(a) a$severity)
  c(
    critical = sum(severities == "critical"),
    warning = sum(severities == "warning"),
    info = sum(severities == "info"),
    success = sum(severities == "success")
  )
}

# =============================================================================
# SHINY UI HELPERS
# =============================================================================

#' Format alerts for display in Shiny
#' @param alerts List of alerts
#' @return HTML-formatted alert content
format_alerts_html <- function(alerts) {
  if (is.null(alerts) || length(alerts) == 0) {
    return('<div class="alert alert-success">
      <i class="fas fa-check-circle"></i>
      <strong>All Clear</strong> - No active alerts
    </div>')
  }

  # Helper function to escape HTML to prevent XSS
  html_escape <- function(text) {
    if (is.null(text)) return("")
    text <- gsub("&", "&amp;", text)
    text <- gsub("<", "&lt;", text)
    text <- gsub(">", "&gt;", text)
    text <- gsub("\"", "&quot;", text)
    text <- gsub("'", "&#39;", text)
    text
  }

  html_alerts <- sapply(alerts, function(a) {
    # Escape user-influenced content to prevent XSS
    safe_title <- html_escape(a$title)
    safe_message <- html_escape(a$message)
    safe_badge <- html_escape(a$config$badge)
    safe_color <- html_escape(a$config$color)
    safe_severity <- html_escape(toupper(a$severity))

    sprintf(
      '<div class="alert alert-%s" style="border-left: 4px solid %s; margin-bottom: 10px;">
        <strong>%s</strong>
        <span class="badge bg-%s float-end">%s</span>
        <br><small class="text-muted">%s</small>
        <p style="margin-top: 5px; margin-bottom: 0;">%s</p>
      </div>',
      safe_badge,
      safe_color,
      safe_title,
      safe_badge,
      safe_severity,
      format(a$timestamp, "%Y-%m-%d %H:%M"),
      safe_message
    )
  })

  paste(html_alerts, collapse = "\n")
}

#' Create alert summary for dashboard header
#' @param alerts List of alerts
#' @return List with summary counts and badge HTML
create_alert_summary <- function(alerts) {
  counts <- count_alerts_by_severity(alerts)
  total <- sum(counts)

  badge_class <- if (counts["critical"] > 0) {
    "bg-danger"
  } else if (counts["warning"] > 0) {
    "bg-warning"
  } else {
    "bg-success"
  }

  list(
    total = total,
    counts = counts,
    badge_html = sprintf(
      '<span class="badge %s">%d</span>',
      badge_class, total
    ),
    status = if (counts["critical"] > 0) {
      "critical"
    } else if (counts["warning"] > 0) {
      "warning"
    } else {
      "normal"
    }
  )
}

#' Summarize alerts for table display
#' @param alerts List of alerts
#' @return Data frame formatted for DT
summarize_alert_table <- function(alerts) {
  if (is.null(alerts) || length(alerts) == 0) {
    return(data.frame(
      Severity = character(),
      Pathogen = character(),
      Title = character(),
      Timestamp = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    Severity = sapply(alerts, function(a) toupper(a$severity)),
    Pathogen = sapply(alerts, function(a) a$pathogen),
    Title = sapply(alerts, function(a) a$title),
    Message = sapply(alerts, function(a) a$message),
    Timestamp = sapply(alerts, function(a) format(a$timestamp, "%Y-%m-%d %H:%M")),
    stringsAsFactors = FALSE
  )
}
