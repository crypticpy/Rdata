# ============================================================================
# Title: RespiWatch Logging Module
# Purpose: Production logging and monitoring for the Shiny application
# Input: Application events and errors
# Output: Log files and metrics
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)

# =============================================================================
# LOGGING CONFIGURATION
# =============================================================================

#' Logging configuration
LOG_CONFIG <- list(
  level = Sys.getenv("LOG_LEVEL", "INFO"),
  file_enabled = TRUE,
  console_enabled = TRUE,
  log_dir = "logs",
  max_file_size_mb = 10,
  max_files = 5,
  date_format = "%Y-%m-%d %H:%M:%S"
)

#' Log levels (higher = more severe)
LOG_LEVELS <- list(
  DEBUG = 1,
  INFO = 2,
  WARNING = 3,
  ERROR = 4,
  CRITICAL = 5
)

# =============================================================================
# LOGGER INITIALIZATION
# =============================================================================

#' Initialize logging system
#' @param config Optional custom configuration
#' @return TRUE if successful
initialize_logging <- function(config = NULL) {
  if (!is.null(config)) {
    for (key in names(config)) {
      if (key %in% names(LOG_CONFIG)) {
        LOG_CONFIG[[key]] <<- config[[key]]
      }
    }
  }

  # Create log directory if needed
  if (LOG_CONFIG$file_enabled && !dir.exists(LOG_CONFIG$log_dir)) {
    dir.create(LOG_CONFIG$log_dir, recursive = TRUE)
  }

  # Initialize session log
  log_info("Logging system initialized", category = "system")

  TRUE
}

#' Get current log file path
#' @return Path to current log file
get_log_file_path <- function() {
  file.path(LOG_CONFIG$log_dir,
            paste0("respiwatch_", format(Sys.Date(), "%Y%m%d"), ".log"))
}

# =============================================================================
# CORE LOGGING FUNCTIONS
# =============================================================================

#' Write log message
#' @param level Log level
#' @param message Log message
#' @param category Optional category for filtering
#' @param data Optional additional data (will be JSON-encoded)
write_log <- function(level, message, category = NULL, data = NULL) {
  # Check if we should log at this level
  config_level <- LOG_LEVELS[[LOG_CONFIG$level]]
  message_level <- LOG_LEVELS[[level]]

  if (is.null(message_level) || message_level < config_level) {
    return(invisible(NULL))
  }

  # Format log entry
  timestamp <- format(Sys.time(), LOG_CONFIG$date_format)
  category_str <- if (!is.null(category)) paste0("[", category, "] ") else ""
  data_str <- if (!is.null(data)) {
    paste0(" | data: ", jsonlite::toJSON(data, auto_unbox = TRUE))
  } else ""

  log_entry <- sprintf("%s [%s] %s%s%s",
                       timestamp, level, category_str, message, data_str)

  # Console output
  if (LOG_CONFIG$console_enabled) {
    color <- switch(
      level,
      "DEBUG" = "\033[36m",    # Cyan
      "INFO" = "\033[32m",     # Green
      "WARNING" = "\033[33m",  # Yellow
      "ERROR" = "\033[31m",    # Red
      "CRITICAL" = "\033[35m", # Magenta
      ""
    )
    reset <- "\033[0m"
    cat(paste0(color, log_entry, reset, "\n"))
  }

  # File output
  if (LOG_CONFIG$file_enabled) {
    tryCatch({
      log_file <- get_log_file_path()
      cat(log_entry, "\n", file = log_file, append = TRUE)

      # Rotate if needed
      rotate_logs_if_needed(log_file)
    }, error = function(e) {
      # Fallback to console if file write fails
      cat(sprintf("LOG WRITE ERROR: %s\n", e$message))
    })
  }

  invisible(log_entry)
}

#' Log debug message
#' @param message Message to log
#' @param ... Additional arguments passed to write_log
log_debug <- function(message, ...) {
  write_log("DEBUG", message, ...)
}

#' Log info message
#' @param message Message to log
#' @param ... Additional arguments passed to write_log
log_info <- function(message, ...) {
  write_log("INFO", message, ...)
}

#' Log warning message
#' @param message Message to log
#' @param ... Additional arguments passed to write_log
log_warning <- function(message, ...) {
  write_log("WARNING", message, ...)
}

#' Log error message
#' @param message Message to log
#' @param ... Additional arguments passed to write_log
log_error <- function(message, ...) {
  write_log("ERROR", message, ...)
}

#' Log critical message
#' @param message Message to log
#' @param ... Additional arguments passed to write_log
log_critical <- function(message, ...) {
  write_log("CRITICAL", message, ...)
}

# =============================================================================
# LOG ROTATION
# =============================================================================

#' Rotate logs if current file exceeds size limit
#' @param log_file Path to current log file
rotate_logs_if_needed <- function(log_file) {
  if (!file.exists(log_file)) {
    return(invisible(NULL))
  }

  # Check file size
  file_size_mb <- file.info(log_file)$size / (1024 * 1024)

  if (file_size_mb >= LOG_CONFIG$max_file_size_mb) {
    # Rotate: rename current to .1, .1 to .2, etc.
    for (i in LOG_CONFIG$max_files:1) {
      old_file <- paste0(log_file, ".", i - 1)
      new_file <- paste0(log_file, ".", i)

      if (i == 1) {
        old_file <- log_file
      }

      if (file.exists(old_file)) {
        if (i == LOG_CONFIG$max_files) {
          file.remove(old_file)
        } else {
          file.rename(old_file, new_file)
        }
      }
    }

    # Create fresh log file
    cat("", file = log_file)
  }

  invisible(NULL)
}

# =============================================================================
# APPLICATION EVENT LOGGING
# =============================================================================

#' Log application startup
#' @param version Application version
log_app_startup <- function(version = "1.0.0") {
  log_info(
    "Application started",
    category = "app",
    data = list(
      version = version,
      r_version = R.version.string,
      platform = Sys.info()["sysname"]
    )
  )
}

#' Log application shutdown
log_app_shutdown <- function() {
  log_info("Application shutdown", category = "app")
}

#' Log user session start
#' @param session_id Shiny session ID
log_session_start <- function(session_id) {
  log_info(
    sprintf("Session started: %s", substr(session_id, 1, 8)),
    category = "session"
  )
}

#' Log user session end
#' @param session_id Shiny session ID
log_session_end <- function(session_id) {
  log_info(
    sprintf("Session ended: %s", substr(session_id, 1, 8)),
    category = "session"
  )
}

#' Log page navigation
#' @param session_id Session ID
#' @param page Page name
log_page_view <- function(session_id, page) {
  log_debug(
    sprintf("Page view: %s", page),
    category = "navigation",
    data = list(session = substr(session_id, 1, 8))
  )
}

#' Log data refresh
#' @param source Data source name
#' @param records Number of records loaded
#' @param duration Duration in seconds
log_data_refresh <- function(source, records, duration = NULL) {
  log_info(
    sprintf("Data refresh: %s (%d records)", source, records),
    category = "data",
    data = list(
      source = source,
      records = records,
      duration_sec = duration
    )
  )
}

#' Log API call
#' @param endpoint API endpoint
#' @param status HTTP status code
#' @param duration Duration in seconds
log_api_call <- function(endpoint, status, duration = NULL) {
  level <- if (status >= 400) "ERROR" else "DEBUG"
  write_log(
    level,
    sprintf("API call: %s (status: %d)", endpoint, status),
    category = "api",
    data = list(
      endpoint = endpoint,
      status = status,
      duration_sec = duration
    )
  )
}

#' Log error with stack trace
#' @param error Error object
#' @param context Context description
log_error_with_trace <- function(error, context = NULL) {
  trace <- if (!is.null(error$call)) {
    deparse(error$call)
  } else {
    "No stack trace available"
  }

  log_error(
    sprintf("Error%s: %s",
            if (!is.null(context)) paste0(" in ", context) else "",
            error$message),
    category = "error",
    data = list(
      message = error$message,
      trace = trace
    )
  )
}

# =============================================================================
# METRICS COLLECTION
# =============================================================================

#' Performance metrics storage (in-memory for session)
.metrics <- new.env(parent = emptyenv())

#' Record a metric
#' @param name Metric name
#' @param value Metric value
#' @param type Metric type (counter, gauge, timing)
record_metric <- function(name, value, type = "gauge") {
  key <- paste(type, name, sep = "_")
  timestamp <- Sys.time()

  if (!exists(key, envir = .metrics)) {
    .metrics[[key]] <- list()
  }

  .metrics[[key]] <- c(.metrics[[key]], list(
    list(value = value, timestamp = timestamp)
  ))

  # Keep only last 1000 data points
  if (length(.metrics[[key]]) > 1000) {
    .metrics[[key]] <- tail(.metrics[[key]], 1000)
  }

  invisible(NULL)
}

#' Get metric summary
#' @param name Metric name
#' @param type Metric type
#' @return Summary statistics for the metric
get_metric_summary <- function(name, type = "gauge") {
  key <- paste(type, name, sep = "_")

  if (!exists(key, envir = .metrics)) {
    return(NULL)
  }

  values <- sapply(.metrics[[key]], function(x) x$value)

  list(
    name = name,
    type = type,
    count = length(values),
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    mean = mean(values, na.rm = TRUE),
    median = median(values, na.rm = TRUE),
    latest = tail(values, 1)
  )
}

#' Time a function execution
#' @param fn Function to time
#' @param metric_name Metric name for recording
#' @return Function result
time_execution <- function(fn, metric_name) {
  start <- Sys.time()
  result <- tryCatch(fn(), error = function(e) {
    log_error_with_trace(e, metric_name)
    stop(e)
  })
  duration <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  record_metric(metric_name, duration, type = "timing")
  result
}

# =============================================================================
# HEALTH CHECK
# =============================================================================

#' Get application health status
#' @return List with health status information
get_health_status <- function() {
  # Get memory usage using base R gc() instead of pryr (archived package)
  gc_info <- gc(reset = FALSE, verbose = FALSE)
  memory_mb <- sum(gc_info[, 2])  # Mb used column

  list(
    status = "healthy",
    timestamp = Sys.time(),
    uptime = if (exists("APP_START_TIME", envir = globalenv())) {
      as.numeric(difftime(Sys.time(), get("APP_START_TIME", envir = globalenv()), units = "hours"))
    } else NA,
    memory_mb = memory_mb,
    log_level = LOG_CONFIG$level,
    metrics = list(
      data_refresh = get_metric_summary("data_refresh", "timing"),
      api_calls = get_metric_summary("api_calls", "counter")
    )
  )
}

#' Log health status periodically
log_health_check <- function() {
  health <- get_health_status()
  log_info(
    sprintf("Health check: %s (uptime: %.1f hrs, memory: %.0f MB)",
            health$status, health$uptime, health$memory_mb),
    category = "health",
    data = health
  )
}
