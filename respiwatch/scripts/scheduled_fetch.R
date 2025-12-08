# ============================================================================
# Title: Scheduled Fetch Orchestrator
# Purpose: Coordinate all data fetchers and manage scheduled updates
# Input: All individual fetch scripts
# Output: Fully populated database with fresh data from all sources
# ============================================================================

# Set working directory to project root
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# Load required packages
library(DBI)
library(RSQLite)
library(dplyr)

# Source dependencies
source("R/db_schema.R")
source("R/logging.R")

# =============================================================================
# ORCHESTRATOR CONFIGURATION
# =============================================================================

FETCH_CONFIG <- list(
  # Enable/disable individual fetchers
  fetch_healthcare = TRUE,
  fetch_rsv = TRUE,
  fetch_vaccination = TRUE,
  fetch_flu = TRUE,
  fetch_ecdc = TRUE,
  fetch_vaccine_effectiveness = TRUE,

  # Use synthetic data fallback if APIs fail
  use_synthetic_fallback = TRUE,

  # Historical data depth in weeks
  weeks_back = 52,

  # Logging
  log_file = "data/cache/fetch_log.txt",

  # Parallel fetch (not implemented yet, sequential)
  parallel = FALSE
)

# =============================================================================
# FETCH EXECUTION FUNCTIONS
# =============================================================================

#' Run a single fetch script safely
#' @param script_name Name of the script (without path)
#' @param use_synthetic Use synthetic data flag
#' @return List with status and record count
run_fetch_script <- function(script_name, use_synthetic = FALSE) {
  script_path <- file.path("scripts", script_name)

  if (!file.exists(script_path)) {
    return(list(
      script = script_name,
      status = "error",
      records = 0,
      message = "Script not found"
    ))
  }

  message(sprintf("\n>>> Executing: %s", script_name))

  start_time <- Sys.time()

  result <- tryCatch({
    # Source the script functions
    source(script_path, local = TRUE)

    # Each script has a main function named fetch_all_*
    main_func <- switch(script_name,
      "fetch_healthcare_capacity.R" = fetch_all_healthcare_capacity,
      "fetch_rsv_data.R" = fetch_all_rsv_data,
      "fetch_vaccination_data.R" = fetch_all_vaccination_data,
      "fetch_flu_enhanced.R" = fetch_all_flu_data,
      "fetch_ecdc_data.R" = fetch_all_ecdc_data,
      "import_vaccine_effectiveness.R" = import_all_vaccine_effectiveness,
      NULL
    )

    if (is.null(main_func)) {
      return(list(
        script = script_name,
        status = "error",
        records = 0,
        message = "Main function not found"
      ))
    }

    # Execute the fetch
    records <- main_func(use_synthetic = use_synthetic, weeks_back = FETCH_CONFIG$weeks_back)

    list(
      script = script_name,
      status = "success",
      records = records,
      message = NULL
    )

  }, error = function(e) {
    list(
      script = script_name,
      status = "error",
      records = 0,
      message = e$message
    )
  })

  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
  result$elapsed_seconds <- elapsed

  message(sprintf("<<< %s: %s (%d records in %.1fs)",
                  script_name, result$status, result$records, elapsed))

  result
}

# =============================================================================
# DATABASE VERIFICATION
# =============================================================================

#' Verify database has required tables
#' @return TRUE if database is ready
verify_database <- function() {
  if (!file.exists(DB_PATH)) {
    message("Database not found, initializing...")
    initialize_database(force = TRUE)
    return(TRUE)
  }

  conn <- get_db_connection()

  required_tables <- c(
    "pathogens", "countries", "regions", "data_sources",
    "surveillance_data", "healthcare_capacity", "vaccine_coverage"
  )

  existing_tables <- dbListTables(conn)
  close_db_connection(conn)

  missing <- setdiff(required_tables, existing_tables)

  if (length(missing) > 0) {
    warning(sprintf("Missing tables: %s", paste(missing, collapse = ", ")))
    message("Reinitializing database...")
    initialize_database(force = TRUE)
  }

  TRUE
}

#' Get current database statistics
#' @return Data frame with table row counts
get_database_stats <- function() {
  conn <- get_db_connection()

  tables <- dbListTables(conn)

  stats <- lapply(tables, function(tbl) {
    count <- dbGetQuery(conn, sprintf("SELECT COUNT(*) as n FROM %s", tbl))$n
    data.frame(table = tbl, records = count)
  })

  close_db_connection(conn)

  bind_rows(stats) |>
    arrange(desc(records))
}

# =============================================================================
# MAIN ORCHESTRATOR
# =============================================================================

#' Main function to run all data fetches
#' @param use_synthetic Force synthetic data for all fetchers
#' @param skip_existing Skip fetchers if data already exists
#' @return Summary data frame
run_all_fetches <- function(use_synthetic = FALSE, skip_existing = FALSE) {
  message("=" |> rep(70) |> paste(collapse = ""))
  message("RespiWatch Data Fetch Orchestrator")
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  message("=" |> rep(70) |> paste(collapse = ""))

  # Verify database is ready
  verify_database()

  # Show initial stats
  message("\nInitial Database State:")
  initial_stats <- get_database_stats()
  print(initial_stats)

  results <- list()
  use_synthetic_flag <- use_synthetic || FETCH_CONFIG$use_synthetic_fallback

  # Run each fetcher
  if (FETCH_CONFIG$fetch_healthcare) {
    results$healthcare <- run_fetch_script(
      "fetch_healthcare_capacity.R",
      use_synthetic = use_synthetic_flag
    )
  }

  if (FETCH_CONFIG$fetch_rsv) {
    results$rsv <- run_fetch_script(
      "fetch_rsv_data.R",
      use_synthetic = use_synthetic_flag
    )
  }

  if (FETCH_CONFIG$fetch_vaccination) {
    results$vaccination <- run_fetch_script(
      "fetch_vaccination_data.R",
      use_synthetic = use_synthetic_flag
    )
  }

  if (FETCH_CONFIG$fetch_flu) {
    results$flu <- run_fetch_script(
      "fetch_flu_enhanced.R",
      use_synthetic = use_synthetic_flag
    )
  }

  if (FETCH_CONFIG$fetch_ecdc) {
    results$ecdc <- run_fetch_script(
      "fetch_ecdc_data.R",
      use_synthetic = use_synthetic_flag
    )
  }

  if (FETCH_CONFIG$fetch_vaccine_effectiveness) {
    results$vaccine_effectiveness <- run_fetch_script(
      "import_vaccine_effectiveness.R",
      use_synthetic = FALSE  # VE importer doesn't use synthetic flag
    )
  }

  # Compile summary
  summary_df <- bind_rows(lapply(results, function(r) {
    data.frame(
      fetcher = r$script,
      status = r$status,
      records = r$records,
      elapsed_secs = r$elapsed_seconds %||% 0,
      error = r$message %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))

  message("\n" |> paste0(rep("=", 70) |> paste(collapse = "")))
  message("Fetch Summary:")
  print(summary_df |> select(-error))

  # Show final stats
  message("\nFinal Database State:")
  final_stats <- get_database_stats()
  print(final_stats)

  # Calculate totals
  total_records <- sum(summary_df$records, na.rm = TRUE)
  total_time <- sum(summary_df$elapsed_secs, na.rm = TRUE)
  success_count <- sum(summary_df$status == "success")

  message("\n" |> paste0(rep("=", 70) |> paste(collapse = "")))
  message(sprintf("TOTAL: %d records from %d/%d fetchers in %.1f seconds",
                  total_records, success_count, nrow(summary_df), total_time))
  message("=" |> rep(70) |> paste(collapse = ""))

  # Log results
  log_fetch_results(summary_df)

  invisible(summary_df)
}

#' Log fetch results to file
#' @param summary Summary data frame
log_fetch_results <- function(summary) {
  log_file <- FETCH_CONFIG$log_file

  # Ensure directory exists
  dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  log_lines <- c(
    sprintf("=== Fetch Run: %s ===", timestamp),
    apply(summary, 1, function(row) {
      sprintf("  %s: %s (%d records)", row["fetcher"], row["status"], as.numeric(row["records"]))
    }),
    sprintf("  Total: %d records", sum(as.numeric(summary$records))),
    ""
  )

  cat(log_lines, file = log_file, sep = "\n", append = TRUE)
}

# =============================================================================
# QUICK REFRESH FUNCTIONS
# =============================================================================

#' Quick refresh - only fetch if data is stale
#' @param max_age_hours Maximum age before refresh
#' @return Invisible NULL
quick_refresh <- function(max_age_hours = 4) {
  message("Checking data freshness...")

  conn <- get_db_connection()

  freshness <- tryCatch({
    dbGetQuery(conn, "
      SELECT source_code, last_fetch_timestamp
      FROM data_sources ds
      LEFT JOIN data_freshness df ON ds.source_id = df.source_id
      WHERE ds.is_active = 1
    ")
  }, error = function(e) {
    data.frame()
  })

  close_db_connection(conn)

  if (nrow(freshness) == 0) {
    message("No freshness data found, running full fetch")
    return(run_all_fetches(use_synthetic = TRUE))
  }

  stale_sources <- freshness |>
    mutate(
      age_hours = as.numeric(difftime(
        Sys.time(),
        as.POSIXct(last_fetch_timestamp),
        units = "hours"
      ))
    ) |>
    filter(is.na(age_hours) | age_hours > max_age_hours)

  if (nrow(stale_sources) > 0) {
    message(sprintf("Found %d stale sources, refreshing...", nrow(stale_sources)))
    run_all_fetches(use_synthetic = TRUE)
  } else {
    message("All data sources are fresh")
  }

  invisible(NULL)
}

#' Populate database with synthetic data for demo
#' @return Summary data frame
populate_demo_data <- function() {
  message("Populating database with demo data...")
  run_all_fetches(use_synthetic = TRUE)
}

# =============================================================================
# COMMAND LINE INTERFACE
# =============================================================================

# Run if executed directly
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)

  if ("--help" %in% args || "-h" %in% args) {
    cat("
RespiWatch Data Fetch Orchestrator

Usage:
  Rscript scripts/scheduled_fetch.R [options]

Options:
  --synthetic   Use synthetic data for all fetchers
  --quick       Only fetch if data is stale (>4 hours)
  --demo        Populate with demo data (same as --synthetic)
  --stats       Show database statistics only
  --help, -h    Show this help message

Examples:

  # Full fetch with API calls + synthetic fallback
  Rscript scripts/scheduled_fetch.R

  # Demo mode with synthetic data only
  Rscript scripts/scheduled_fetch.R --demo

  # Quick refresh (skip if recent)
  Rscript scripts/scheduled_fetch.R --quick

")
    quit(save = "no")
  }

  if ("--stats" %in% args) {
    stats <- get_database_stats()
    print(stats)
    quit(save = "no")
  }

  if ("--quick" %in% args) {
    quick_refresh()
    quit(save = "no")
  }

  use_synthetic <- "--synthetic" %in% args || "--demo" %in% args

  run_all_fetches(use_synthetic = use_synthetic)
}
