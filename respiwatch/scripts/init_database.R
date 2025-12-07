# ============================================================================
# Title: Initialize RespiWatch Database
# Purpose: Create database, seed reference data, and import JSON files
# Usage: Rscript scripts/init_database.R
# ============================================================================

# Set working directory to project root (relative from script location)
script_dir <- dirname(sys.frame(1)$ofile)
if (!is.null(script_dir) && script_dir != "") {
  setwd(file.path(script_dir, ".."))
} else {
  # Fallback: try to detect from common patterns
  if (file.exists("R/db_schema.R")) {
    # Already in project root
  } else if (file.exists("../R/db_schema.R")) {
    setwd("..")
  }
}

# Source database modules
source("R/db_schema.R")
source("R/db_operations.R")

cat("============================================\n")
cat("RespiWatch Database Initialization\n")
cat("============================================\n\n")

# Check current database status
cat("1. Checking current database status...\n")
status <- get_database_status()
if (status$exists) {
  cat("   Database exists:\n")
  cat("   - Size:", status$size_mb, "MB\n")
  cat("   - Tables:", length(status$tables), "\n")
  cat("   - Last modified:", format(status$last_modified), "\n\n")
} else {
  cat("   No database found. Will create new.\n\n")
}

# Initialize database (create tables and seed reference data)
cat("2. Initializing database...\n")
init_result <- tryCatch({
  initialize_database(force = TRUE)
}, error = function(e) {
  cat("   ERROR:", e$message, "\n")
  FALSE
})

if (init_result) {
  cat("   Database initialized successfully!\n\n")
} else {
  cat("   Database initialization skipped or failed.\n\n")
}

# Import JSON data
cat("3. Importing JSON data files...\n\n")

import_stats <- tryCatch({
  import_all_json()
}, error = function(e) {
  cat("   ERROR:", e$message, "\n")
  NULL
})

if (!is.null(import_stats)) {
  cat("\n   Import Summary:\n")
  if (!is.null(import_stats$h3n2)) {
    cat("   - H3N2: ", import_stats$h3n2$surveillance, " surveillance, ",
        import_stats$h3n2$regional, " regional, ",
        import_stats$h3n2$anomalies, " anomalies\n")
  }
  if (!is.null(import_stats$rsv)) {
    cat("   - RSV: ", import_stats$rsv$surveillance, " surveillance, ",
        import_stats$rsv$regional, " regional\n")
  }
  if (!is.null(import_stats$covid)) {
    cat("   - COVID: ", import_stats$covid$surveillance, " surveillance, ",
        import_stats$covid$regional, " regional\n")
  }
}

# Verify database contents
cat("\n4. Verifying database contents...\n")
status <- get_database_status()
cat("   Tables and row counts:\n")
for (tbl in names(status$row_counts)) {
  cat("   -", tbl, ":", status$row_counts[tbl], "rows\n")
}

# Test data retrieval
cat("\n5. Testing data retrieval...\n")

conn <- get_db_connection()

# Test surveillance data
surv_data <- get_latest_surveillance(conn)
cat("   - Latest surveillance records:", nrow(surv_data), "\n")

# Test regional overview
regional_data <- get_regional_overview(conn)
cat("   - Regional overview records:", nrow(regional_data), "\n")

# Test anomalies
anomaly_data <- get_anomalies(conn)
cat("   - Active anomalies:", nrow(anomaly_data), "\n")

close_db_connection(conn)

cat("\n============================================\n")
cat("Database initialization complete!\n")
cat("Database path: data/respiwatch.sqlite\n")
cat("============================================\n")
