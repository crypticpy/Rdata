#!/usr/bin/env Rscript
# ============================================================================
# Title: RespiWatch Nightly Data Refresh
# Purpose: Cron-compatible script for automated database updates
# Usage: Add to crontab: 0 2 * * * Rscript /path/to/scripts/scheduled_refresh.R
# ============================================================================

# Get script directory and set working directory
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  # Fallback for interactive use
  script_dir <- getwd()
}
project_root <- normalizePath(file.path(script_dir, ".."))
setwd(project_root)

# Setup logging
log_dir <- file.path(project_root, "data", "cache")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

log_file <- file.path(log_dir, sprintf("refresh_%s.log", format(Sys.Date(), "%Y%m%d")))
error_log <- file.path(log_dir, "refresh_errors.log")

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entry <- sprintf("[%s] [%s] %s\n", timestamp, level, msg)
  cat(entry, file = log_file, append = TRUE)
  if (level == "ERROR") {
    cat(entry, file = error_log, append = TRUE)
  }
  cat(entry)  # Also print to console
}

# Start refresh
log_message("========== Starting RespiWatch Data Refresh ==========")
log_message(sprintf("Working directory: %s", getwd()))

# Load required modules
tryCatch({
  log_message("Loading R modules...")
  source("R/data_scheduler.R")
  source("R/data_validation.R")
  log_message("Modules loaded successfully")
}, error = function(e) {
  log_message(sprintf("Failed to load modules: %s", e$message), "ERROR")
  quit(status = 1)
})

# Run the scheduled refresh
refresh_result <- tryCatch({
  log_message("Starting data refresh from APIs...")

  # Run the main refresh function
  result <- run_scheduled_refresh()

  log_message("Data refresh completed successfully")
  log_message(sprintf("Influenza records: %d", result$influenza_records %||% 0))
  log_message(sprintf("RSV records: %d", result$rsv_records %||% 0))
  log_message(sprintf("COVID records: %d", result$covid_records %||% 0))

  result
}, error = function(e) {
  log_message(sprintf("Data refresh failed: %s", e$message), "ERROR")
  log_message(sprintf("Traceback: %s", paste(capture.output(traceback()), collapse = "\n")), "ERROR")
  NULL
})

# Run validation
validation_result <- tryCatch({
  log_message("Running data validation...")

  conn <- get_db_connection()
  on.exit(close_db_connection(conn), add = TRUE)

  result <- validate_all_pathogens(conn)

  log_message(sprintf("Validation complete: %d/%d pathogens valid",
    result$overall$pathogens_valid,
    result$overall$pathogens_checked
  ))

  if (result$overall$total_issues > 0) {
    log_message(sprintf("Found %d validation issues", result$overall$total_issues), "WARNING")
  }

  result
}, error = function(e) {
  log_message(sprintf("Validation failed: %s", e$message), "ERROR")
  NULL
})

# Write validation report
if (!is.null(validation_result)) {
  report_file <- file.path(log_dir, sprintf("validation_report_%s.txt", format(Sys.Date(), "%Y%m%d")))
  report_text <- generate_validation_report(validation_result)
  writeLines(report_text, report_file)
  log_message(sprintf("Validation report written to: %s", report_file))
}

# Summary
log_message("========== Refresh Summary ==========")
if (!is.null(refresh_result)) {
  log_message("Refresh Status: SUCCESS")
} else {
  log_message("Refresh Status: FAILED", "ERROR")
}

if (!is.null(validation_result) && validation_result$overall$all_valid) {
  log_message("Validation Status: ALL VALID")
} else if (!is.null(validation_result)) {
  log_message(sprintf("Validation Status: %d ISSUES FOUND", validation_result$overall$total_issues), "WARNING")
} else {
  log_message("Validation Status: FAILED", "ERROR")
}

log_message("========== Refresh Complete ==========")

# Exit with appropriate status
if (is.null(refresh_result)) {
  quit(status = 1)
} else {
  quit(status = 0)
}
