# ============================================================================
# Title: RespiWatch Data Scheduler
# Purpose: Periodic data refresh from APIs with database storage
# Input: Live CDC, NREVSS, RSV-NET APIs
# Output: Updated SQLite database records
# ============================================================================

# Load required packages -------------------------------------------------------
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(lubridate)
library(httr2)

# Source dependencies
source("R/db_schema.R")
source("R/db_operations.R")
source("R/data_fetch.R")

# Configuration ----------------------------------------------------------------
REFRESH_INTERVAL_HOURS <- 1
LAST_REFRESH_FILE <- "data/cache/.last_refresh"

# =============================================================================
# SCHEDULER FUNCTIONS
# =============================================================================

#' Check if data refresh is needed
#' @param interval_hours Hours between refreshes
#' @return TRUE if refresh is needed
needs_refresh <- function(interval_hours = REFRESH_INTERVAL_HOURS) {
  if (!file.exists(LAST_REFRESH_FILE)) {
    return(TRUE)
  }

  last_refresh <- tryCatch({
    as.POSIXct(readLines(LAST_REFRESH_FILE, n = 1))
  }, error = function(e) {
    return(as.POSIXct("1970-01-01"))
  })

  hours_since <- as.numeric(difftime(Sys.time(), last_refresh, units = "hours"))
  return(hours_since >= interval_hours)
}

#' Record successful refresh
record_refresh <- function() {
  dir.create(dirname(LAST_REFRESH_FILE), recursive = TRUE, showWarnings = FALSE)
  writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), LAST_REFRESH_FILE)
}

#' Get last refresh time
#' @return POSIXct timestamp of last refresh
get_last_refresh <- function() {
  if (!file.exists(LAST_REFRESH_FILE)) {
    return(NULL)
  }
  tryCatch({
    as.POSIXct(readLines(LAST_REFRESH_FILE, n = 1))
  }, error = function(e) {
    NULL
  })
}

# =============================================================================
# API TO DATABASE FUNCTIONS
# =============================================================================

#' Fetch influenza data from APIs and store in database
#' @param conn Database connection
#' @return Number of records stored
refresh_influenza_data <- function(conn) {
  message("Refreshing influenza data from CDC APIs...")
  records_stored <- 0

  tryCatch({
    # Fetch ILINet national data
    ilinet_data <- fetch_ilinet_national()

    if (!is.null(ilinet_data) && nrow(ilinet_data) > 0) {
      # Transform to database format
      flu_records <- ilinet_data |>
        mutate(
          pathogen_code = "H3N2",  # Primary influenza strain
          iso_code = "USA",
          observation_date = as.character(as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u")),
          positivity_rate = as.numeric(unweighted_ili),
          data_confidence = "high"
        ) |>
        select(pathogen_code, iso_code, observation_date, positivity_rate, data_confidence) |>
        filter(!is.na(observation_date))

      if (nrow(flu_records) > 0) {
        records_stored <- upsert_surveillance_data(conn, flu_records)
        message(sprintf("  Stored %d influenza records", records_stored))
      }
    }

    # Also fetch state-level data if available
    state_data <- fetch_ilinet_state()
    if (!is.null(state_data) && nrow(state_data) > 0) {
      message(sprintf("  Also fetched %d state-level records (not stored in DB yet)", nrow(state_data)))
    }

  }, error = function(e) {
    warning(paste("Influenza refresh failed:", e$message))
  })

  records_stored
}

#' Fetch RSV data from APIs and store in database
#' @param conn Database connection
#' @return Number of records stored
refresh_rsv_data <- function(conn) {
  message("Refreshing RSV data from CDC APIs...")
  records_stored <- 0

  tryCatch({
    # Fetch respiratory hospital capacity (includes RSV proxy)
    resp_data <- fetch_rsv_direct()

    if (!is.null(resp_data) && nrow(resp_data) > 0) {
      # Transform to database format - aggregate by week
      rsv_records <- resp_data |>
        mutate(
          pathogen_code = "RSV",
          iso_code = "USA",
          observation_date = as.character(as.Date(weekendingdate)),
          hospitalization_rate = occupancy_rate,
          data_confidence = "medium"
        ) |>
        group_by(pathogen_code, iso_code, observation_date) |>
        summarize(
          hospitalization_rate = mean(hospitalization_rate, na.rm = TRUE),
          data_confidence = first(data_confidence),
          .groups = "drop"
        ) |>
        filter(!is.na(observation_date))

      if (nrow(rsv_records) > 0) {
        records_stored <- upsert_surveillance_data(conn, rsv_records)
        message(sprintf("  Stored %d RSV records", records_stored))
      }
    }

    # Try RSV-NET data as well
    rsvnet_data <- fetch_rsv_net()
    if (!is.null(rsvnet_data) && nrow(rsvnet_data) > 0) {
      message(sprintf("  Also fetched %d RSV-NET records", nrow(rsvnet_data)))
    }

  }, error = function(e) {
    warning(paste("RSV refresh failed:", e$message))
  })

  records_stored
}

#' Fetch COVID data from APIs and store in database
#' @param conn Database connection
#' @return Number of records stored
refresh_covid_data <- function(conn) {
  message("Refreshing COVID-19 data from CDC APIs...")
  records_stored <- 0

  tryCatch({
    # Fetch COVID surveillance data
    covid_data <- fetch_covid_cdc_direct()

    if (!is.null(covid_data) && nrow(covid_data) > 0) {
      # Transform to database format - aggregate by state
      covid_records <- covid_data |>
        mutate(
          pathogen_code = "COVID19",
          iso_code = "USA",
          observation_date = as.character(as.Date(date_updated)),
          case_count = total_cases,
          deaths = total_deaths,
          data_confidence = "high"
        ) |>
        group_by(pathogen_code, iso_code, observation_date) |>
        summarize(
          case_count = sum(case_count, na.rm = TRUE),
          deaths = sum(deaths, na.rm = TRUE),
          data_confidence = first(data_confidence),
          .groups = "drop"
        ) |>
        filter(!is.na(observation_date))

      if (nrow(covid_records) > 0) {
        records_stored <- upsert_surveillance_data(conn, covid_records)
        message(sprintf("  Stored %d COVID records", records_stored))
      }
    }

    # Fetch variant data
    variant_data <- fetch_covid_variants()
    if (!is.null(variant_data) && nrow(variant_data) > 0) {
      message(sprintf("  Also fetched %d variant surveillance records", nrow(variant_data)))
    }

  }, error = function(e) {
    warning(paste("COVID refresh failed:", e$message))
  })

  records_stored
}

#' Fetch global COVID data from OWID and store in database
#' @param conn Database connection
#' @return Number of records stored
refresh_global_covid_data <- function(conn) {
  message("Refreshing global COVID-19 data from Our World in Data...")
  records_stored <- 0

  tryCatch({
    # Fetch global COVID data (already implemented in data_fetch.R)
    global_data <- fetch_who_data()

    if (!is.null(global_data) && nrow(global_data) > 0) {
      # Transform to database format
      # OWID uses 3-letter iso_code which matches our DB
      global_records <- global_data |>
        mutate(
          pathogen_code = "COVID19",  # Match DB pathogen code
          observation_date = as.character(observation_date),
          case_count = as.integer(new_cases),
          deaths = as.integer(new_deaths),
          # Calculate positivity rate from vaccinations if available
          positivity_rate = NA_real_,  # OWID latest doesn't have test positivity
          data_confidence = "high"
        ) |>
        select(
          pathogen_code, iso_code, observation_date,
          case_count, deaths, positivity_rate, data_confidence
        ) |>
        filter(
          !is.na(iso_code),
          !is.na(observation_date),
          # Exclude aggregate regions (OWID includes OWID_* codes)
          !grepl("^OWID_", iso_code)
        )

      if (nrow(global_records) > 0) {
        records_stored <- upsert_surveillance_data(conn, global_records)
        message(sprintf("  Stored %d global COVID records from %d countries",
                        records_stored, length(unique(global_records$iso_code))))
      }
    }

  }, error = function(e) {
    warning(paste("Global COVID refresh failed:", e$message))
  })

  records_stored
}

#' Fetch global influenza data from WHO FluNet and store in database
#' @param conn Database connection
#' @return Number of records stored
refresh_global_influenza_data <- function(conn) {
  message("Refreshing global influenza data from WHO FluNet...")
  records_stored <- 0

  tryCatch({
    # Fetch global influenza data from WHO FluNet
    flunet_data <- fetch_who_flumart()

    if (!is.null(flunet_data) && nrow(flunet_data) > 0) {
      # Get the most recent observation per country (aggregate weekly data)
      # FluNet provides weekly data, so we take the latest week per country
      latest_data <- flunet_data |>
        group_by(iso_code) |>
        slice_max(observation_date, n = 1, with_ties = FALSE) |>
        ungroup()

      # Transform to database format
      global_records <- latest_data |>
        mutate(
          pathogen_code = "H3N2",  # Primary influenza subtype tracked
          observation_date = as.character(observation_date),
          case_count = as.integer(influenza_total),
          deaths = NA_integer_,  # FluNet doesn't provide death data directly
          positivity_rate = positivity_rate,
          data_confidence = "high"
        ) |>
        select(
          pathogen_code, iso_code, observation_date,
          case_count, deaths, positivity_rate, data_confidence
        ) |>
        filter(!is.na(iso_code), !is.na(observation_date))

      if (nrow(global_records) > 0) {
        records_stored <- upsert_surveillance_data(conn, global_records)
        message(sprintf("  Stored %d global influenza records from %d countries",
                        records_stored, length(unique(global_records$iso_code))))
      }
    }

  }, error = function(e) {
    warning(paste("Global influenza refresh failed:", e$message))
  })

  records_stored
}

#' Update data freshness tracking
#' @param conn Database connection
#' @param source_code Source identifier
#' @param status Fetch status
#' @param records Number of records fetched
#' @param error_msg Error message if any
update_freshness_tracking <- function(conn, source_code, status, records = 0, error_msg = NULL) {
  # Validate source_code to prevent SQL injection (alphanumeric and underscore only)
  if (!grepl("^[A-Za-z0-9_]+$", source_code)) {
    warning("Invalid source_code format")
    return()
  }

  # Get source_id using parameterized query
  source <- dbGetQuery(conn, sprintf(
    "SELECT source_id FROM data_sources WHERE source_code = '%s'",
    gsub("'", "''", source_code)  # Escape single quotes
  ))

  if (nrow(source) == 0) return()

  source_id <- as.integer(source$source_id)
  records <- as.integer(records)

  # Delete existing record

  dbExecute(conn, sprintf(
    "DELETE FROM data_freshness WHERE source_id = %d",
    source_id
  ))

  # Sanitize status (only allow expected values)
  valid_statuses <- c("success", "failed", "pending", "not_run")
  if (!status %in% valid_statuses) {
    status <- "failed"
  }

  # Escape error message if present
  error_sql <- if (is.null(error_msg)) {
    "NULL"
  } else {
    sprintf("'%s'", gsub("'", "''", substr(as.character(error_msg), 1, 500)))
  }

  # Insert new record with properly escaped values
  dbExecute(conn, sprintf("
    INSERT INTO data_freshness (source_id, last_fetch_timestamp, records_fetched, fetch_status, error_message, next_scheduled_fetch)
    VALUES (%d, '%s', %d, '%s', %s, '%s')
  ",
    source_id,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    records,
    status,
    error_sql,
    format(Sys.time() + hours(REFRESH_INTERVAL_HOURS), "%Y-%m-%d %H:%M:%S")
  ))
}

# =============================================================================
# MAIN REFRESH FUNCTION
# =============================================================================

#' Run full data refresh from all APIs
#' @param force Force refresh even if not due
#' @param db_path Path to database
#' @return List with refresh statistics
run_data_refresh <- function(force = FALSE, db_path = DB_PATH) {
  if (!force && !needs_refresh()) {
    last_refresh <- get_last_refresh()
    message(sprintf("Data is fresh (last refreshed: %s). Use force=TRUE to refresh anyway.",
                    format(last_refresh, "%Y-%m-%d %H:%M")))
    return(list(skipped = TRUE, last_refresh = last_refresh))
  }

  message("============================================")
  message("Starting RespiWatch Data Refresh")
  message(sprintf("Time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  message("============================================\n")

  conn <- get_db_connection(db_path)

  stats <- list(
    start_time = Sys.time(),
    influenza = list(records = 0, status = "not_run"),
    rsv = list(records = 0, status = "not_run"),
    covid = list(records = 0, status = "not_run"),
    global_covid = list(records = 0, status = "not_run"),
    global_influenza = list(records = 0, status = "not_run")
  )

  tryCatch({
    # Refresh influenza data
    stats$influenza$records <- tryCatch({
      n <- refresh_influenza_data(conn)
      update_freshness_tracking(conn, "CDC_FLUVIEW", "success", n)
      stats$influenza$status <- "success"
      n
    }, error = function(e) {
      update_freshness_tracking(conn, "CDC_FLUVIEW", "failed", 0, e$message)
      stats$influenza$status <- "failed"
      0
    })

    # Refresh RSV data
    stats$rsv$records <- tryCatch({
      n <- refresh_rsv_data(conn)
      update_freshness_tracking(conn, "CDC_NREVSS", "success", n)
      stats$rsv$status <- "success"
      n
    }, error = function(e) {
      update_freshness_tracking(conn, "CDC_NREVSS", "failed", 0, e$message)
      stats$rsv$status <- "failed"
      0
    })

    # Refresh COVID data
    stats$covid$records <- tryCatch({
      n <- refresh_covid_data(conn)
      update_freshness_tracking(conn, "CDC_COVID", "success", n)
      stats$covid$status <- "success"
      n
    }, error = function(e) {
      update_freshness_tracking(conn, "CDC_COVID", "failed", 0, e$message)
      stats$covid$status <- "failed"
      0
    })

    # Refresh global COVID data from OWID (international coverage)
    stats$global_covid$records <- tryCatch({
      n <- refresh_global_covid_data(conn)
      update_freshness_tracking(conn, "OWID", "success", n)
      stats$global_covid$status <- "success"
      n
    }, error = function(e) {
      update_freshness_tracking(conn, "OWID", "failed", 0, e$message)
      stats$global_covid$status <- "failed"
      0
    })

    # Refresh global influenza data from WHO FluNet (international coverage)
    stats$global_influenza$records <- tryCatch({
      n <- refresh_global_influenza_data(conn)
      update_freshness_tracking(conn, "WHO_FLUMART", "success", n)
      stats$global_influenza$status <- "success"
      n
    }, error = function(e) {
      update_freshness_tracking(conn, "WHO_FLUMART", "failed", 0, e$message)
      stats$global_influenza$status <- "failed"
      0
    })

    # Record successful refresh
    record_refresh()

    stats$end_time <- Sys.time()
    stats$duration_seconds <- as.numeric(difftime(stats$end_time, stats$start_time, units = "secs"))
    stats$total_records <- stats$influenza$records + stats$rsv$records + stats$covid$records +
                           stats$global_covid$records + stats$global_influenza$records

    message("\n============================================")
    message("Refresh Complete!")
    message(sprintf("Duration: %.1f seconds", stats$duration_seconds))
    message(sprintf("Total records: %d", stats$total_records))
    message(sprintf("  - Influenza (US): %d (%s)", stats$influenza$records, stats$influenza$status))
    message(sprintf("  - RSV (US): %d (%s)", stats$rsv$records, stats$rsv$status))
    message(sprintf("  - COVID (US): %d (%s)", stats$covid$records, stats$covid$status))
    message(sprintf("  - COVID (Global): %d (%s)", stats$global_covid$records, stats$global_covid$status))
    message(sprintf("  - Influenza (Global): %d (%s)", stats$global_influenza$records, stats$global_influenza$status))
    message("============================================\n")

    close_db_connection(conn)
    stats

  }, error = function(e) {
    close_db_connection(conn)
    stop("Data refresh failed: ", e$message)
  })
}

#' Get refresh status for display
#' @param db_path Path to database
#' @return Data frame with freshness status
get_refresh_status <- function(db_path = DB_PATH) {
  conn <- get_db_connection(db_path)

  status <- tryCatch({
    dbGetQuery(conn, "
      SELECT
        ds.source_code,
        ds.source_name,
        df.last_fetch_timestamp,
        df.records_fetched,
        df.fetch_status,
        df.error_message,
        df.next_scheduled_fetch
      FROM data_sources ds
      LEFT JOIN data_freshness df ON ds.source_id = df.source_id
      WHERE ds.is_active = 1
      ORDER BY ds.source_name
    ")
  }, finally = {
    close_db_connection(conn)
  })

  status
}

# =============================================================================
# SHINY REACTIVE FILE READER
# =============================================================================

#' Create a reactive file reader for Shiny that triggers on data refresh
#' @param session Shiny session
#' @param interval_ms Check interval in milliseconds
#' @return Reactive value that updates when data refreshes
create_refresh_watcher <- function(session, interval_ms = 60000) {
  shiny::reactiveFileReader(
    intervalMillis = interval_ms,
    session = session,
    filePath = LAST_REFRESH_FILE,
    readFunc = function(path) {
      if (file.exists(path)) {
        as.POSIXct(readLines(path, n = 1))
      } else {
        Sys.time()
      }
    }
  )
}

# =============================================================================
# SCHEDULED REFRESH (for background process)
# =============================================================================

#' Run as background scheduled task
#' Call this from a cron job or scheduled task
#' @param project_root Optional project root directory (auto-detected if not provided)
run_scheduled_refresh <- function(project_root = NULL) {
  # Auto-detect project root if not provided
  if (is.null(project_root)) {
    # Try to find project root by looking for app.R
    possible_roots <- c(getwd(), file.path(getwd(), ".."))
    for (root in possible_roots) {
      if (file.exists(file.path(root, "app.R"))) {
        project_root <- normalizePath(root)
        break
      }
    }
  }
  if (!is.null(project_root)) setwd(project_root)

  log_file <- sprintf("data/cache/refresh_log_%s.txt", format(Sys.Date(), "%Y%m%d"))
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)

  sink(log_file, append = TRUE)
  on.exit(sink(), add = TRUE)  # Fix: add = TRUE for proper cleanup

  cat(sprintf("\n[%s] Starting scheduled refresh\n", Sys.time()))

  tryCatch({
    stats <- run_data_refresh()
    cat(sprintf("[%s] Refresh completed: %d records\n", Sys.time(), stats$total_records))

    # Also refresh model cache after data refresh
    cat(sprintf("[%s] Refreshing Bayesian model cache...\n", Sys.time()))
    model_stats <- refresh_model_cache()
    cat(sprintf("[%s] Model cache refresh completed: %d models updated\n",
                Sys.time(), model_stats$models_updated))

  }, error = function(e) {
    cat(sprintf("[%s] ERROR: %s\n", Sys.time(), e$message))
  })
}

# =============================================================================
# BAYESIAN MODEL CACHING
# =============================================================================

MODEL_CACHE_DIR <- "data/cache/models"
MODEL_CACHE_HOURS <- 24  # Models are valid for 24 hours

#' Check if model cache needs refresh
#' @param pathogen_code Pathogen identifier
#' @return TRUE if refresh is needed
needs_model_refresh <- function(pathogen_code) {
  cache_file <- file.path(MODEL_CACHE_DIR, paste0(pathogen_code, "_model.rds"))

  if (!file.exists(cache_file)) {
    return(TRUE)
  }

  # Check file age
  file_age_hours <- as.numeric(difftime(Sys.time(),
                                         file.info(cache_file)$mtime,
                                         units = "hours"))
  return(file_age_hours >= MODEL_CACHE_HOURS)
}

#' Refresh Bayesian model cache for all pathogens
#' @param force Force refresh even if cache is valid
#' @return List with refresh statistics
refresh_model_cache <- function(force = FALSE) {
  # Ensure cache directory exists
  dir.create(MODEL_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

  pathogens <- c("H3N2", "RSV", "COVID19")
  stats <- list(
    start_time = Sys.time(),
    models_updated = 0,
    models_skipped = 0,
    errors = list()
  )

  # Check if bayesian_forecast module is available
  if (!exists("fit_simple_model")) {
    tryCatch({
      source("R/bayesian_forecast.R")
    }, error = function(e) {
      warning("Could not load bayesian_forecast.R: ", e$message)
      return(stats)
    })
  }

  for (pathogen in pathogens) {
    if (!force && !needs_model_refresh(pathogen)) {
      message(sprintf("  Model cache for %s is fresh, skipping", pathogen))
      stats$models_skipped <- stats$models_skipped + 1
      next
    }

    message(sprintf("  Refreshing model cache for %s...", pathogen))

    tryCatch({
      # Get surveillance data
      source("R/db_schema.R")
      source("R/db_operations.R")
      conn <- get_db_connection()
      on.exit(close_db_connection(conn), add = TRUE)  # Ensure connection closes on error

      # Validate pathogen code (alphanumeric only)
      if (!grepl("^[A-Za-z0-9]+$", pathogen)) {
        warning(sprintf("Invalid pathogen code: %s", pathogen))
        next
      }

      # Use escaped query (pathogen is validated above)
      query <- sprintf("
        SELECT
          sd.observation_date,
          sd.case_count,
          sd.estimated_cases,
          sd.positivity_rate
        FROM surveillance_data sd
        JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
        WHERE p.pathogen_code = '%s'
        ORDER BY sd.observation_date
      ", gsub("'", "''", pathogen))

      surv_data <- DBI::dbGetQuery(conn, query)

      if (nrow(surv_data) < 10) {
        warning(sprintf("Insufficient data for %s model (%d records)", pathogen, nrow(surv_data)))
        stats$errors[[pathogen]] <- "Insufficient data"
        next
      }

      # Prepare model data and fit model
      model_data <- prepare_model_data(surv_data, pathogen)
      fit <- fit_simple_model(model_data)

      if (!is.null(fit)) {
        # Save to cache
        cache_file <- file.path(MODEL_CACHE_DIR, paste0(pathogen, "_model.rds"))
        saveRDS(list(
          model = fit,
          pathogen = pathogen,
          fitted_at = Sys.time(),
          n_observations = nrow(model_data)
        ), cache_file)

        message(sprintf("    Saved model cache for %s", pathogen))
        stats$models_updated <- stats$models_updated + 1
      }

    }, error = function(e) {
      warning(sprintf("Model cache refresh failed for %s: %s", pathogen, e$message))
      stats$errors[[pathogen]] <- e$message
    })
  }

  stats$end_time <- Sys.time()
  stats$duration_seconds <- as.numeric(difftime(stats$end_time, stats$start_time, units = "secs"))

  message(sprintf("Model cache refresh complete: %d updated, %d skipped, %d errors",
                  stats$models_updated, stats$models_skipped, length(stats$errors)))

  stats
}

#' Load cached model for a pathogen
#' @param pathogen_code Pathogen identifier
#' @return Cached model object or NULL
load_cached_model <- function(pathogen_code) {
  cache_file <- file.path(MODEL_CACHE_DIR, paste0(pathogen_code, "_model.rds"))

  if (!file.exists(cache_file)) {
    return(NULL)
  }

  tryCatch({
    cached <- readRDS(cache_file)

    # Check if cache is still valid
    cache_age_hours <- as.numeric(difftime(Sys.time(), cached$fitted_at, units = "hours"))
    if (cache_age_hours > MODEL_CACHE_HOURS) {
      message(sprintf("Model cache for %s is stale (%.1f hours old)", pathogen_code, cache_age_hours))
      return(NULL)
    }

    cached$model
  }, error = function(e) {
    warning(sprintf("Could not load cached model for %s: %s", pathogen_code, e$message))
    NULL
  })
}

#' Get model cache status for display
#' @return Data frame with cache status
get_model_cache_status <- function() {
  pathogens <- c("H3N2", "RSV", "COVID19")

  status_list <- lapply(pathogens, function(p) {
    cache_file <- file.path(MODEL_CACHE_DIR, paste0(p, "_model.rds"))

    if (!file.exists(cache_file)) {
      return(data.frame(
        Pathogen = p,
        Status = "Not Cached",
        `Last Updated` = NA,
        `Age (hours)` = NA,
        Valid = FALSE,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    file_time <- file.info(cache_file)$mtime
    age_hours <- as.numeric(difftime(Sys.time(), file_time, units = "hours"))

    data.frame(
      Pathogen = p,
      Status = if (age_hours < MODEL_CACHE_HOURS) "Valid" else "Stale",
      `Last Updated` = format(file_time, "%Y-%m-%d %H:%M"),
      `Age (hours)` = round(age_hours, 1),
      Valid = age_hours < MODEL_CACHE_HOURS,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  do.call(rbind, status_list)
}

# =============================================================================
# DATA PIPELINE VERIFICATION
# =============================================================================

#' Verify data pipeline integrity
#' Checks surveillance data records, freshness status, and data source health
#' @param db_path Path to database (default: DB_PATH)
#' @return List with verification results
verify_data_pipeline <- function(db_path = DB_PATH) {
  if (!file.exists(db_path)) {
    return(list(
      status = "no_database",
      message = "Database file not found",
      checks = list()
    ))
  }

  conn <- get_db_connection(db_path)

  tryCatch({
    # Check 1: Surveillance data by pathogen
    surveillance_summary <- dbGetQuery(conn, "
      SELECT
        p.pathogen_code,
        COUNT(*) as record_count,
        MIN(sd.observation_date) as min_date,
        MAX(sd.observation_date) as max_date,
        COUNT(DISTINCT sd.country_id) as countries_reporting
      FROM surveillance_data sd
      JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
      GROUP BY p.pathogen_code
    ")

    # Check 2: Data freshness status
    freshness_status <- dbGetQuery(conn, "
      SELECT
        ds.source_code,
        ds.source_name,
        df.last_fetch_timestamp,
        df.records_fetched,
        df.fetch_status,
        df.error_message
      FROM data_sources ds
      LEFT JOIN data_freshness df ON ds.source_id = df.source_id
      WHERE ds.is_active = 1
    ")

    # Check 3: Active data sources count
    active_sources <- dbGetQuery(conn,
      "SELECT COUNT(*) as n FROM data_sources WHERE is_active = 1")$n

    # Check 4: Total records
    total_records <- dbGetQuery(conn,
      "SELECT COUNT(*) as n FROM surveillance_data")$n

    # Check 5: Recent data (last 7 days)
    recent_cutoff <- format(Sys.Date() - 7, "%Y-%m-%d")
    recent_data <- dbGetQuery(conn, sprintf("
      SELECT COUNT(*) as n
      FROM surveillance_data
      WHERE observation_date >= '%s'
    ", recent_cutoff))$n

    close_db_connection(conn)

    # Determine overall status
    has_data <- total_records > 0
    has_recent <- recent_data > 0
    sources_ok <- any(freshness_status$fetch_status == "success", na.rm = TRUE)

    overall_status <- if (has_data && has_recent && sources_ok) {
      "healthy"
    } else if (has_data && !has_recent) {
      "stale"
    } else if (!has_data) {
      "empty"
    } else {
      "degraded"
    }

    list(
      status = overall_status,
      message = switch(overall_status,
        "healthy" = "Data pipeline is operating normally",
        "stale" = "No recent data - consider running data refresh",
        "empty" = "Database has no surveillance data",
        "degraded" = "Some data sources are failing"
      ),
      checks = list(
        surveillance = surveillance_summary,
        freshness = freshness_status,
        active_sources = active_sources,
        total_records = total_records,
        recent_records = recent_data,
        last_checked = Sys.time()
      )
    )

  }, error = function(e) {
    close_db_connection(conn)
    list(
      status = "error",
      message = paste("Pipeline verification failed:", e$message),
      checks = list()
    )
  })
}

#' Print formatted pipeline status
#' @param verification Result from verify_data_pipeline()
print_pipeline_status <- function(verification = NULL) {
  if (is.null(verification)) {
    verification <- verify_data_pipeline()
  }

  message("============================================")
  message("RespiWatch Data Pipeline Status")
  message(sprintf("Status: %s", toupper(verification$status)))
  message(sprintf("Message: %s", verification$message))
  message("============================================")

  if (length(verification$checks) > 0) {
    message("\nSurveillance Data Summary:")
    if (!is.null(verification$checks$surveillance) && nrow(verification$checks$surveillance) > 0) {
      print(verification$checks$surveillance)
    } else {
      message("  No surveillance data found")
    }

    message(sprintf("\nTotal Records: %d", verification$checks$total_records))
    message(sprintf("Recent Records (last 7 days): %d", verification$checks$recent_records))
    message(sprintf("Active Data Sources: %d", verification$checks$active_sources))

    message("\nData Freshness:")
    if (!is.null(verification$checks$freshness) && nrow(verification$checks$freshness) > 0) {
      print(verification$checks$freshness[, c("source_code", "fetch_status", "last_fetch_timestamp")])
    }
  }

  invisible(verification)
}
