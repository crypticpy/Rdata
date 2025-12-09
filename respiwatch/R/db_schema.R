# ============================================================================
# Title: RespiWatch Database Schema
# Purpose: SQLite database schema for respiratory pathogen surveillance data
# Input: None (schema definitions)
# Output: Database initialization functions
# ============================================================================

# Load required packages -------------------------------------------------------
library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)

# Configuration ----------------------------------------------------------------
DB_PATH <- "data/respiwatch.sqlite"

# =============================================================================
# DATABASE CONNECTION
# =============================================================================

#' Get database connection
#' @return SQLite database connection
get_db_connection <- function(db_path = DB_PATH) {
  if (!dir.exists(dirname(db_path))) {
    dir.create(dirname(db_path), recursive = TRUE)
  }
  dbConnect(RSQLite::SQLite(), db_path)
}

#' Close database connection safely
#' @param conn Database connection object
close_db_connection <- function(conn) {
  if (!is.null(conn) && dbIsValid(conn)) {
    dbDisconnect(conn)
  }
}

# =============================================================================
# SCHEMA DEFINITIONS
# =============================================================================

#' Create all database tables
#' @param conn Database connection
create_all_tables <- function(conn) {
  # Enable foreign keys
  dbExecute(conn, "PRAGMA foreign_keys = ON;")

  # -------------------------------------------------------------------------
  # CORE REFERENCE TABLES
  # -------------------------------------------------------------------------

  # Pathogens table
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS pathogens (
      pathogen_id INTEGER PRIMARY KEY AUTOINCREMENT,
      pathogen_code TEXT UNIQUE NOT NULL,
      pathogen_name TEXT NOT NULL,
      pathogen_family TEXT,
      description TEXT,
      is_active INTEGER DEFAULT 1,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      updated_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  # Regions table (continental/WHO regions)
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS regions (
      region_id INTEGER PRIMARY KEY AUTOINCREMENT,
      region_code TEXT UNIQUE NOT NULL,
      region_name TEXT NOT NULL,
      who_region_code TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  # Countries table
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS countries (
      country_id INTEGER PRIMARY KEY AUTOINCREMENT,
      iso_code TEXT UNIQUE NOT NULL,
      country_name TEXT NOT NULL,
      region_id INTEGER,
      population INTEGER,
      latitude REAL,
      longitude REAL,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (region_id) REFERENCES regions(region_id)
    );
  ")

  # Data sources table
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS data_sources (
      source_id INTEGER PRIMARY KEY AUTOINCREMENT,
      source_code TEXT UNIQUE NOT NULL,
      source_name TEXT NOT NULL,
      source_type TEXT,
      api_endpoint TEXT,
      update_frequency TEXT,
      is_active INTEGER DEFAULT 1,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  # -------------------------------------------------------------------------
  # SURVEILLANCE DATA TABLES
  # -------------------------------------------------------------------------

  # Main surveillance data table (time-series)
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS surveillance_data (
      data_id INTEGER PRIMARY KEY AUTOINCREMENT,
      pathogen_id INTEGER NOT NULL,
      country_id INTEGER NOT NULL,
      source_id INTEGER,
      observation_date TEXT NOT NULL,
      week_number INTEGER,
      year INTEGER,
      positivity_rate REAL,
      case_count INTEGER,
      estimated_cases INTEGER,
      hospitalizations INTEGER,
      hospitalization_rate REAL,
      icu_admissions INTEGER,
      deaths INTEGER,
      death_rate REAL,
      test_volume INTEGER,
      data_confidence TEXT CHECK(data_confidence IN ('high', 'medium', 'low')),
      fetch_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (pathogen_id) REFERENCES pathogens(pathogen_id),
      FOREIGN KEY (country_id) REFERENCES countries(country_id),
      FOREIGN KEY (source_id) REFERENCES data_sources(source_id),
      UNIQUE(pathogen_id, country_id, observation_date)
    );
  ")

  # Regional overview table (aggregated by region)
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS regional_overview (
      overview_id INTEGER PRIMARY KEY AUTOINCREMENT,
      pathogen_id INTEGER NOT NULL,
      region_id INTEGER NOT NULL,
      observation_date TEXT NOT NULL,
      status TEXT CHECK(status IN ('low', 'monitoring', 'active', 'elevated', 'peak', 'declining')),
      positivity_rate REAL,
      dominant_strain TEXT,
      trend TEXT CHECK(trend IN ('increasing', 'stable', 'decreasing')),
      week_over_week_change REAL,
      confidence_level TEXT CHECK(confidence_level IN ('high', 'medium', 'low')),
      fetch_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (pathogen_id) REFERENCES pathogens(pathogen_id),
      FOREIGN KEY (region_id) REFERENCES regions(region_id),
      UNIQUE(pathogen_id, region_id, observation_date)
    );
  ")

  # -------------------------------------------------------------------------
  # VARIANT/STRAIN TABLES
  # -------------------------------------------------------------------------

  # Variants/Strains reference table
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS variants (
      variant_id INTEGER PRIMARY KEY AUTOINCREMENT,
      pathogen_id INTEGER NOT NULL,
      variant_code TEXT NOT NULL,
      variant_name TEXT,
      lineage TEXT,
      parent_variant_id INTEGER,
      first_detected_date TEXT,
      is_voc INTEGER DEFAULT 0,
      is_voi INTEGER DEFAULT 0,
      severity_assessment TEXT,
      immune_escape TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (pathogen_id) REFERENCES pathogens(pathogen_id),
      FOREIGN KEY (parent_variant_id) REFERENCES variants(variant_id),
      UNIQUE(pathogen_id, variant_code)
    );
  ")

  # Variant prevalence over time
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS variant_prevalence (
      prevalence_id INTEGER PRIMARY KEY AUTOINCREMENT,
      variant_id INTEGER NOT NULL,
      country_id INTEGER,
      region_id INTEGER,
      observation_date TEXT NOT NULL,
      prevalence_pct REAL,
      sample_size INTEGER,
      growth_advantage REAL,
      source_id INTEGER,
      fetch_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (variant_id) REFERENCES variants(variant_id),
      FOREIGN KEY (country_id) REFERENCES countries(country_id),
      FOREIGN KEY (region_id) REFERENCES regions(region_id),
      FOREIGN KEY (source_id) REFERENCES data_sources(source_id)
    );
  ")

  # -------------------------------------------------------------------------
  # VACCINE TABLES
  # -------------------------------------------------------------------------

  # Vaccines reference table
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS vaccines (
      vaccine_id INTEGER PRIMARY KEY AUTOINCREMENT,
      pathogen_id INTEGER NOT NULL,
      vaccine_code TEXT NOT NULL,
      vaccine_name TEXT NOT NULL,
      manufacturer TEXT,
      target_variant TEXT,
      approval_date TEXT,
      vaccine_type TEXT,
      is_active INTEGER DEFAULT 1,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (pathogen_id) REFERENCES pathogens(pathogen_id),
      UNIQUE(pathogen_id, vaccine_code)
    );
  ")

  # Vaccine effectiveness data
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS vaccine_effectiveness (
      ve_id INTEGER PRIMARY KEY AUTOINCREMENT,
      vaccine_id INTEGER NOT NULL,
      country_id INTEGER,
      observation_date TEXT NOT NULL,
      effectiveness_pct REAL,
      confidence_interval_low REAL,
      confidence_interval_high REAL,
      target_outcome TEXT,
      age_group TEXT,
      study_type TEXT,
      sample_size INTEGER,
      source_id INTEGER,
      notes TEXT,
      fetch_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (vaccine_id) REFERENCES vaccines(vaccine_id),
      FOREIGN KEY (country_id) REFERENCES countries(country_id),
      FOREIGN KEY (source_id) REFERENCES data_sources(source_id)
    );
  ")

  # Vaccine coverage data
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS vaccine_coverage (
      coverage_id INTEGER PRIMARY KEY AUTOINCREMENT,
      vaccine_id INTEGER NOT NULL,
      country_id INTEGER NOT NULL,
      observation_date TEXT NOT NULL,
      coverage_pct REAL,
      age_group TEXT,
      doses_administered INTEGER,
      target_population INTEGER,
      source_id INTEGER,
      fetch_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (vaccine_id) REFERENCES vaccines(vaccine_id),
      FOREIGN KEY (country_id) REFERENCES countries(country_id),
      FOREIGN KEY (source_id) REFERENCES data_sources(source_id)
    );
  ")

  # -------------------------------------------------------------------------
  # HEALTHCARE CAPACITY TABLES
  # -------------------------------------------------------------------------

  # Healthcare capacity data
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS healthcare_capacity (
      capacity_id INTEGER PRIMARY KEY AUTOINCREMENT,
      country_id INTEGER NOT NULL,
      observation_date TEXT NOT NULL,
      total_hospital_beds INTEGER,
      occupied_hospital_beds INTEGER,
      hospital_occupancy_pct REAL,
      total_icu_beds INTEGER,
      occupied_icu_beds INTEGER,
      icu_occupancy_pct REAL,
      respiratory_patients INTEGER,
      capacity_stress TEXT CHECK(capacity_stress IN ('low', 'moderate', 'high', 'critical')),
      healthcare_worker_status TEXT,
      source_id INTEGER,
      fetch_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (country_id) REFERENCES countries(country_id),
      FOREIGN KEY (source_id) REFERENCES data_sources(source_id)
    );
  ")

  # -------------------------------------------------------------------------
  # ANOMALY TABLES
  # -------------------------------------------------------------------------

  # Anomaly types reference
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS anomaly_types (
      type_id INTEGER PRIMARY KEY AUTOINCREMENT,
      type_code TEXT UNIQUE NOT NULL,
      type_name TEXT NOT NULL,
      description TEXT,
      severity_weight REAL DEFAULT 1.0
    );
  ")

  # Detected anomalies
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS anomalies (
      anomaly_id INTEGER PRIMARY KEY AUTOINCREMENT,
      type_id INTEGER NOT NULL,
      pathogen_id INTEGER,
      country_id INTEGER,
      region_id INTEGER,
      detected_date TEXT NOT NULL,
      description TEXT NOT NULL,
      severity TEXT CHECK(severity IN ('low', 'medium', 'high', 'critical')),
      geographic_scope TEXT,
      verification_status TEXT CHECK(verification_status IN ('unverified', 'investigating', 'verified', 'dismissed')),
      resolved_date TEXT,
      source_id INTEGER,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (type_id) REFERENCES anomaly_types(type_id),
      FOREIGN KEY (pathogen_id) REFERENCES pathogens(pathogen_id),
      FOREIGN KEY (country_id) REFERENCES countries(country_id),
      FOREIGN KEY (region_id) REFERENCES regions(region_id),
      FOREIGN KEY (source_id) REFERENCES data_sources(source_id)
    );
  ")

  # -------------------------------------------------------------------------
  # POLICY RESPONSE TABLES
  # -------------------------------------------------------------------------

  # Policy responses
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS policy_responses (
      policy_id INTEGER PRIMARY KEY AUTOINCREMENT,
      country_id INTEGER NOT NULL,
      pathogen_id INTEGER,
      policy_type TEXT NOT NULL,
      policy_name TEXT,
      implementation_date TEXT,
      end_date TEXT,
      current_status TEXT CHECK(current_status IN ('planned', 'active', 'suspended', 'ended')),
      effectiveness TEXT CHECK(effectiveness IN ('unknown', 'ineffective', 'partially_effective', 'effective', 'highly_effective')),
      description TEXT,
      source_id INTEGER,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (country_id) REFERENCES countries(country_id),
      FOREIGN KEY (pathogen_id) REFERENCES pathogens(pathogen_id),
      FOREIGN KEY (source_id) REFERENCES data_sources(source_id)
    );
  ")

  # -------------------------------------------------------------------------
  # RT ESTIMATION RESULTS (Pre-computed)
  # -------------------------------------------------------------------------

  # Pre-computed Rt estimates for fast loading
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS rt_estimates (
      estimate_id INTEGER PRIMARY KEY AUTOINCREMENT,
      pathogen_id INTEGER NOT NULL,
      country_id INTEGER NOT NULL,
      estimation_date TEXT NOT NULL,
      rt_mean REAL NOT NULL,
      rt_median REAL,
      rt_lower REAL,
      rt_upper REAL,
      rt_sd REAL,
      data_points INTEGER,
      source_description TEXT,
      has_fallback INTEGER DEFAULT 0,
      computed_at TEXT DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (pathogen_id) REFERENCES pathogens(pathogen_id),
      FOREIGN KEY (country_id) REFERENCES countries(country_id),
      UNIQUE(pathogen_id, country_id, estimation_date)
    );
  ")

  # -------------------------------------------------------------------------
  # METADATA TABLES
  # -------------------------------------------------------------------------

  # Data freshness tracking
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS data_freshness (
      freshness_id INTEGER PRIMARY KEY AUTOINCREMENT,
      source_id INTEGER NOT NULL,
      last_fetch_timestamp TEXT NOT NULL,
      last_data_date TEXT,
      records_fetched INTEGER,
      fetch_status TEXT CHECK(fetch_status IN ('success', 'partial', 'failed')),
      error_message TEXT,
      next_scheduled_fetch TEXT,
      FOREIGN KEY (source_id) REFERENCES data_sources(source_id)
    );
  ")

  # Audit log for tracking changes
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS audit_log (
      log_id INTEGER PRIMARY KEY AUTOINCREMENT,
      table_name TEXT NOT NULL,
      operation TEXT CHECK(operation IN ('INSERT', 'UPDATE', 'DELETE')),
      record_id INTEGER,
      old_values TEXT,
      new_values TEXT,
      changed_by TEXT,
      changed_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")

  # Create indexes for performance
  create_indexes(conn)

  message("All database tables created successfully")
}

#' Create indexes for performance optimization
#' @param conn Database connection
create_indexes <- function(conn) {
  # Surveillance data indexes
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_surveillance_date ON surveillance_data(observation_date);")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_surveillance_pathogen ON surveillance_data(pathogen_id);")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_surveillance_country ON surveillance_data(country_id);")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_surveillance_composite ON surveillance_data(pathogen_id, country_id, observation_date);")

  # Variant prevalence indexes
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_variant_prevalence_date ON variant_prevalence(observation_date);")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_variant_prevalence_variant ON variant_prevalence(variant_id);")

  # Regional overview indexes
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_regional_date ON regional_overview(observation_date);")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_regional_pathogen ON regional_overview(pathogen_id);")

  # Healthcare capacity indexes
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_healthcare_date ON healthcare_capacity(observation_date);")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_healthcare_country ON healthcare_capacity(country_id);")

  # Rt estimates indexes
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_rt_estimates_date ON rt_estimates(estimation_date);")
  dbExecute(conn, "CREATE INDEX IF NOT EXISTS idx_rt_estimates_composite ON rt_estimates(pathogen_id, country_id, estimation_date);")

  message("Database indexes created successfully")
}

# =============================================================================
# SEED DATA
# =============================================================================

#' Insert seed data for reference tables
#' @param conn Database connection
seed_reference_data <- function(conn) {
  # Seed pathogens
  pathogens <- data.frame(
    pathogen_code = c("H3N2", "RSV", "COVID19", "H5N1", "FLU_A", "FLU_B"),
    pathogen_name = c(
      "Influenza A H3N2",
      "Respiratory Syncytial Virus",
      "COVID-19 (SARS-CoV-2)",
      "Avian Influenza H5N1",
      "Influenza A (Other)",
      "Influenza B"
    ),
    pathogen_family = c(
      "Orthomyxoviridae",
      "Paramyxoviridae",
      "Coronaviridae",
      "Orthomyxoviridae",
      "Orthomyxoviridae",
      "Orthomyxoviridae"
    ),
    is_active = c(1, 1, 1, 1, 1, 1)
  )
  dbWriteTable(conn, "pathogens", pathogens, append = TRUE, row.names = FALSE)

  # Seed regions
  regions <- data.frame(
    region_code = c("NA", "EU", "ASIA", "OCEANIA", "LATAM", "AFRICA", "ME"),
    region_name = c(
      "North America",
      "Europe",
      "Asia",
      "Oceania",
      "Latin America",
      "Africa",
      "Middle East"
    ),
    who_region_code = c("AMRO", "EURO", "WPRO", "WPRO", "AMRO", "AFRO", "EMRO")
  )
  dbWriteTable(conn, "regions", regions, append = TRUE, row.names = FALSE)

  # Seed countries (key surveillance countries)
  # Seed countries (G20 + Key Regional Hubs)
  countries <- data.frame(
    iso_code = c(
      "USA", "GBR", "JPN", "AUS", "CAN", "DEU", "FRA", "ITA", "ESP", "BRA",
      "IND", "CHN", "ZAF", "MEX", "IDN", "KOR", "TUR", "SAU", "ARG", "RUS",
      "NGA", "EGY", "KEN", "SGP", "NZL"
    ),
    country_name = c(
      "United States", "United Kingdom", "Japan", "Australia", "Canada",
      "Germany", "France", "Italy", "Spain", "Brazil",
      "India", "China", "South Africa", "Mexico", "Indonesia", "South Korea", "Turkey", "Saudi Arabia", "Argentina", "Russia",
      "Nigeria", "Egypt", "Kenya", "Singapore", "New Zealand"
    ),
    region_id = c(
      1, 2, 3, 4, 1, 2, 2, 2, 2, 5,
      3, 3, 6, 1, 3, 3, 7, 7, 5, 2,
      6, 6, 6, 3, 4
    ),
    population = c(
      331900000, 67330000, 125800000, 26000000, 38250000,
      83240000, 67390000, 59550000, 47420000, 214300000,
      1390000000, 1412000000, 60000000, 126700000, 273800000, 51740000, 85000000, 35000000, 45800000, 143400000,
      213000000, 109000000, 53000000, 5450000, 5100000
    ),
    latitude = c(
      37.09, 55.38, 36.20, -25.27, 56.13, 51.17, 46.23, 41.87, 40.46, -14.24,
      20.59, 35.86, -30.56, 23.63, -0.79, 35.90, 38.96, 23.88, -38.42, 61.52,
      9.08, 26.82, -1.29, 1.35, -40.90
    ),
    longitude = c(
      -95.71, -3.44, 138.25, 133.78, -106.35, 10.45, 2.21, 12.57, -3.75, -51.93,
      78.96, 104.19, 22.94, -102.55, 113.92, 127.77, 35.24, 45.08, -63.62, 105.32,
      8.67, 30.80, 36.82, 103.82, 174.88
    )
  )
  dbWriteTable(conn, "countries", countries, append = TRUE, row.names = FALSE)

  # Seed data sources
  data_sources <- data.frame(
    source_code = c("CDC_FLUVIEW", "CDC_NREVSS", "CDC_COVID", "RSV_NET", "WHO_FLUMART", "ECDC", "UKHSA", "DELPHI"),
    source_name = c(
      "CDC FluView", "CDC NREVSS", "CDC COVID Data Tracker", "RSV-NET",
      "WHO FluMart", "ECDC Surveillance", "UK Health Security Agency", "CMU Delphi COVIDcast"
    ),
    source_type = c("api", "api", "api", "api", "api", "api", "api", "api"),
    api_endpoint = c(
      "https://gis.cdc.gov/grasp/flu2/",
      "https://data.cdc.gov/resource/ua7e-t2fy.json",
      "https://data.cdc.gov/resource/pwn4-m3yp.json",
      "https://data.cdc.gov/resource/29hc-w46k.json",
      "https://apps.who.int/flumart/",
      "https://www.ecdc.europa.eu/en/surveillance-and-disease-data",
      "https://www.gov.uk/government/statistics/national-flu-and-covid-19-surveillance-reports",
      "https://api.delphi.cmu.edu/epidata/"
    ),
    update_frequency = c("weekly", "weekly", "daily", "weekly", "weekly", "weekly", "weekly", "daily"),
    is_active = c(1, 1, 1, 1, 1, 1, 1, 1)
  )
  dbWriteTable(conn, "data_sources", data_sources, append = TRUE, row.names = FALSE)

  # Seed anomaly types
  anomaly_types <- data.frame(
    type_code = c("TIMING", "MISMATCH", "SURVEILLANCE_GAP", "SEVERITY", "RESISTANCE", "SPREAD"),
    type_name = c(
      "Timing Anomaly",
      "Vaccine Mismatch",
      "Surveillance Gap",
      "Severity Anomaly",
      "Drug Resistance",
      "Unusual Spread Pattern"
    ),
    description = c(
      "Outbreak timing differs from expected seasonality",
      "Circulating strain differs significantly from vaccine strain",
      "Missing or incomplete surveillance data",
      "Unexpectedly high severity or mortality",
      "Detection of drug-resistant mutations",
      "Atypical geographic or demographic spread"
    ),
    severity_weight = c(1.0, 1.5, 1.2, 2.0, 1.5, 1.3)
  )
  dbWriteTable(conn, "anomaly_types", anomaly_types, append = TRUE, row.names = FALSE)
  
  # Seed vaccines
  vaccines <- data.frame(
    pathogen_id = c(1, 2, 3), # H3N2, RSV, COVID19 (IDs based on insertion order in pathogens df)
    vaccine_code = c("FLU_GENERIC", "RSV_GENERIC", "COVID_MRNA"),
    vaccine_name = c("Seasonal Influenza Vaccine", "RSV Vaccine", "COVID-19 mRNA Vaccine"),
    vaccine_type = c("Inactivated", "Protein Subunit", "mRNA"),
    is_active = c(1, 1, 1)
  )
  dbWriteTable(conn, "vaccines", vaccines, append = TRUE, row.names = FALSE)

  message("Reference data seeded successfully")
}

# =============================================================================
# DATABASE INITIALIZATION
# =============================================================================

#' Initialize the database (create tables and seed data)
#' @param db_path Path to SQLite database file
#' @param force If TRUE, drop and recreate all tables
#' @return Invisible TRUE on success
initialize_database <- function(db_path = DB_PATH, force = FALSE) {
  conn <- get_db_connection(db_path)

  tryCatch({
    if (force) {
      # Drop all tables (in reverse order due to foreign keys)
      tables <- dbListTables(conn)
      # Validate table names - only allow alphanumeric and underscore
      valid_tables <- tables[grepl("^[A-Za-z_][A-Za-z0-9_]*$", tables)]
      if (length(valid_tables) < length(tables)) {
        warning("Some table names were skipped due to invalid characters")
      }
      for (tbl in rev(valid_tables)) {
        dbExecute(conn, paste0("DROP TABLE IF EXISTS \"", tbl, "\";"))
      }
      message("Dropped all existing tables")
    }

    # Check if database already initialized
    existing_tables <- dbListTables(conn)
    if (length(existing_tables) > 0 && !force) {
      message("Database already initialized. Use force=TRUE to reinitialize.")
      close_db_connection(conn)
      return(invisible(FALSE))
    }

    # Create tables
    create_all_tables(conn)

    # Seed reference data
    seed_reference_data(conn)

    message(paste("Database initialized successfully at:", db_path))
    close_db_connection(conn)
    invisible(TRUE)

  }, error = function(e) {
    close_db_connection(conn)
    stop(paste("Database initialization failed:", e$message))
  })
}

#' Get database status
#' @param db_path Path to SQLite database file
#' @return List with database status information
get_database_status <- function(db_path = DB_PATH) {
  if (!file.exists(db_path)) {
    return(list(
      exists = FALSE,
      tables = character(),
      size_mb = 0,
      last_modified = NA
    ))
  }

  conn <- get_db_connection(db_path)
  tables <- dbListTables(conn)

  # Get row counts for each table (with validation)
  # Only process tables with valid names (alphanumeric and underscore)
  valid_tables <- tables[grepl("^[A-Za-z_][A-Za-z0-9_]*$", tables)]
  row_counts <- sapply(valid_tables, function(tbl) {
    result <- dbGetQuery(conn, paste0("SELECT COUNT(*) as n FROM \"", tbl, "\""))
    result$n
  })

  close_db_connection(conn)

  list(
    exists = TRUE,
    tables = tables,
    row_counts = row_counts,
    size_mb = round(file.size(db_path) / 1024 / 1024, 2),
    last_modified = file.mtime(db_path)
  )
}
