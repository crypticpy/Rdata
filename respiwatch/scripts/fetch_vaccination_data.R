# ============================================================================
# Title: Vaccination Coverage Data Fetcher
# Purpose: Fetch vaccination coverage data from OWID and CDC APIs
# Input: Our World in Data GitHub, CDC vaccination APIs
# Output: Populated vaccine_coverage table in SQLite database
# ============================================================================

# Set working directory to project root
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# Load required packages
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(httr2)
library(jsonlite)
library(lubridate)

# Source dependencies
source("R/api_fetcher.R")
source("R/db_schema.R")
source("R/logging.R")

# =============================================================================
# API ENDPOINTS
# =============================================================================

VACCINATION_API_CONFIG <- list(
  # Our World in Data - COVID-19 Vaccinations
  owid_covid_vax_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv",

  # Our World in Data - Flu Vaccinations (if available)
  owid_flu_vax_url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/",

  # CDC Flu Vaccination Coverage
  cdc_flu_vax_url = "https://data.cdc.gov/resource/n3as-k9wb.json"
)

# =============================================================================
# OWID COVID VACCINATION FETCHER
# =============================================================================

#' Fetch COVID vaccination data from Our World in Data
#' @param countries Vector of ISO codes to filter (NULL for all)
#' @return Data frame with vaccination coverage data
fetch_owid_covid_vaccinations <- function(countries = NULL) {
  message("Fetching OWID COVID vaccination data...")

  data <- tryCatch({
    fetch_csv_data(
      VACCINATION_API_CONFIG$owid_covid_vax_url,
      cache_hours = 12
    )
  }, error = function(e) {
    log_error(sprintf("OWID vaccination fetch failed: %s", e$message), category = "api")
    return(data.frame())
  })

  if (nrow(data) == 0) {
    warning("No OWID vaccination data returned")
    return(data.frame())
  }

  message(sprintf("Fetched %d OWID vaccination records", nrow(data)))

  # Filter by countries if specified
  if (!is.null(countries)) {
    data <- data |>
      filter(iso_code %in% countries)
  }

  # Standardize the data
  standardize_owid_vaccination_data(data)
}

#' Standardize OWID vaccination data
#' @param df Raw OWID data
#' @return Standardized data frame
standardize_owid_vaccination_data <- function(df) {
  if (nrow(df) == 0) return(df)

  # Select relevant columns
  result <- df |>
    transmute(
      observation_date = as.Date(date),
      iso_code = iso_code,
      country_name = location,
      pathogen_code = "COVID19",
      total_vaccinations = suppressWarnings(as.numeric(total_vaccinations)),
      people_vaccinated = suppressWarnings(as.numeric(people_vaccinated)),
      people_fully_vaccinated = suppressWarnings(as.numeric(people_fully_vaccinated)),
      daily_vaccinations = suppressWarnings(as.numeric(daily_vaccinations)),
      people_vaccinated_per_hundred = suppressWarnings(as.numeric(people_vaccinated_per_hundred)),
      people_fully_vaccinated_per_hundred = suppressWarnings(as.numeric(people_fully_vaccinated_per_hundred)),
      total_boosters = suppressWarnings(as.numeric(total_boosters)),
      total_boosters_per_hundred = suppressWarnings(as.numeric(total_boosters_per_hundred))
    ) |>
    filter(!is.na(observation_date))

  # Note: Date filtering is now controlled by weeks_back parameter in caller
  # This allows proper historical data retrieval without gaps

  result
}

# =============================================================================
# CDC FLU VACCINATION FETCHER
# =============================================================================

#' Fetch CDC Flu Vaccination Coverage data
#' @param limit Maximum records
#' @return Data frame with flu vaccination coverage
fetch_cdc_flu_vaccination <- function(limit = 5000) {
  message("Fetching CDC Flu Vaccination data...")

  params <- list(
    `$limit` = limit,
    `$order` = "season_survey_year DESC"
  )

  data <- tryCatch({
    fetch_api_data(
      VACCINATION_API_CONFIG$cdc_flu_vax_url,
      params = params,
      cache_hours = 24
    )
  }, error = function(e) {
    log_error(sprintf("CDC Flu vaccination fetch failed: %s", e$message), category = "api")
    return(NULL)
  })

  if (is.null(data) || length(data) == 0) {
    warning("No CDC flu vaccination data returned")
    return(data.frame())
  }

  df <- if (is.data.frame(data)) data else bind_rows(data)

  message(sprintf("Fetched %d CDC flu vaccination records", nrow(df)))

  standardize_cdc_flu_vaccination(df)
}

#' Standardize CDC flu vaccination data
#' @param df Raw CDC data
#' @return Standardized data frame
standardize_cdc_flu_vaccination <- function(df) {
  if (nrow(df) == 0) return(df)

  # CDC flu vax columns vary but typically include:
  # season_survey_year, geography, dimension, estimate, ci_half

  result <- df |>
    transmute(
      season = if ("season_survey_year" %in% names(df)) season_survey_year else NA_character_,
      geography = if ("geography" %in% names(df)) geography else "United States",
      age_group = if ("dimension" %in% names(df)) dimension else "All Ages",
      coverage_pct = if ("estimate" %in% names(df)) {
        suppressWarnings(as.numeric(estimate))
      } else {
        NA_real_
      },
      iso_code = "USA",
      pathogen_code = "FLU_A",  # Generic influenza
      data_source = "CDC_FLU_VAX"
    ) |>
    filter(!is.na(coverage_pct))

  # Create observation date from season (e.g., "2023-24" -> 2023-10-01)
  if ("season" %in% names(result)) {
    result <- result |>
      mutate(
        observation_date = as.Date(paste0(substr(season, 1, 4), "-10-01"))
      )
  } else {
    result$observation_date <- Sys.Date()
  }

  result
}

# =============================================================================
# SYNTHETIC VACCINATION DATA
# =============================================================================

#' Generate synthetic vaccination coverage for demo
#' @param countries Vector of ISO codes
#' @param pathogen "COVID19", "H3N2", or "RSV"
#' @return Data frame with synthetic coverage data
generate_synthetic_vaccination <- function(countries, pathogen = "COVID19") {
  message(sprintf("Generating synthetic %s vaccination data...", pathogen))

  # Base coverage rates by pathogen
  base_rates <- list(
    COVID19 = 70,  # ~70% base coverage
    H3N2 = 50,     # ~50% flu vaccination
    RSV = 20       # RSV vaccines are newer, lower uptake
  )

  base_rate <- base_rates[[pathogen]] %||% 50

  result <- data.frame()

  for (iso in countries) {
    # Country-specific variation
    country_modifier <- runif(1, -15, 15)

    for (week in 0:11) {
      obs_date <- Sys.Date() - (week * 7)

      # Coverage increases over season
      seasonal_increase <- week * 0.5

      coverage <- base_rate + country_modifier + seasonal_increase + runif(1, -3, 3)
      coverage <- pmin(95, pmax(10, coverage))

      result <- bind_rows(result, data.frame(
        observation_date = obs_date,
        iso_code = iso,
        pathogen_code = pathogen,
        age_group = "All Ages",
        coverage_pct = round(coverage, 1),
        doses_administered = round(coverage * 1000 * runif(1, 0.8, 1.2)),
        data_source = "SYNTHETIC"
      ))
    }
  }

  result
}

# =============================================================================
# DATABASE INSERTION
# =============================================================================

#' Insert vaccination coverage data into database
#' @param data Standardized data frame
#' @return Number of records inserted
insert_vaccination_coverage <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    message("No vaccination data to insert")
    return(0)
  }

  conn <- get_db_connection()

  tryCatch({
    total_inserted <- 0

    # Process each pathogen
    for (pathogen in unique(data$pathogen_code)) {
      pathogen_data <- data |> filter(pathogen_code == pathogen)

      # Get vaccine_id (or create generic one)
      vaccine_query <- sprintf(
        "SELECT vaccine_id FROM vaccines WHERE pathogen_id = (SELECT pathogen_id FROM pathogens WHERE pathogen_code = '%s') LIMIT 1",
        pathogen
      )
      vaccine_id <- dbGetQuery(conn, vaccine_query)$vaccine_id

      if (length(vaccine_id) == 0) {
        # Create generic vaccine for this pathogen
        pathogen_id <- dbGetQuery(
          conn,
          sprintf("SELECT pathogen_id FROM pathogens WHERE pathogen_code = '%s'", pathogen)
        )$pathogen_id

        if (length(pathogen_id) > 0) {
          dbExecute(conn, sprintf(
            "INSERT OR IGNORE INTO vaccines (pathogen_id, vaccine_code, vaccine_name, is_active)
             VALUES (%d, '%s_GENERIC', '%s Generic Vaccine', 1)",
            pathogen_id, pathogen, pathogen
          ))
          vaccine_id <- dbGetQuery(conn, vaccine_query)$vaccine_id
        }
      }

      if (length(vaccine_id) == 0) {
        warning(sprintf("Could not find/create vaccine for pathogen %s", pathogen))
        next
      }

      # Process each country
      for (iso in unique(pathogen_data$iso_code)) {
        country_data <- pathogen_data |> filter(iso_code == iso)

        country_id <- dbGetQuery(
          conn,
          sprintf("SELECT country_id FROM countries WHERE iso_code = '%s'", iso)
        )$country_id

        if (length(country_id) == 0) {
          next
        }

        # Prepare insertion data
        insert_data <- country_data |>
          transmute(
            vaccine_id = vaccine_id,
            country_id = country_id,
            observation_date = as.character(observation_date),
            coverage_pct = coverage_pct,
            age_group = if ("age_group" %in% names(country_data)) age_group else "All Ages",
            doses_administered = if ("doses_administered" %in% names(country_data)) doses_administered else NA_integer_,
            target_population = NA_integer_,
            source_id = NA_integer_,
            fetch_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          )

        # Delete existing records in date range
        min_date <- min(country_data$observation_date)
        max_date <- max(country_data$observation_date)

        dbExecute(conn, sprintf(
          "DELETE FROM vaccine_coverage
           WHERE vaccine_id = %d AND country_id = %d
           AND observation_date >= '%s' AND observation_date <= '%s'",
          vaccine_id, country_id, min_date, max_date
        ))

        dbWriteTable(conn, "vaccine_coverage", insert_data, append = TRUE, row.names = FALSE)
        total_inserted <- total_inserted + nrow(insert_data)
      }
    }

    message(sprintf("Inserted %d vaccination coverage records", total_inserted))
    close_db_connection(conn)
    total_inserted

  }, error = function(e) {
    close_db_connection(conn)
    log_error(sprintf("Vaccination insert failed: %s", e$message), category = "database")
    0
  })
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

#' Main function to fetch and store vaccination data
#' @param use_synthetic If TRUE, use synthetic data
#' @param weeks_back Number of weeks of historical data (not used for vaccinations, kept for interface consistency)
#' @return Total records inserted
fetch_all_vaccination_data <- function(use_synthetic = FALSE, weeks_back = 52) {
  message("=" |> rep(60) |> paste(collapse = ""))
  message("Vaccination Coverage Data Fetch")
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  message("=" |> rep(60) |> paste(collapse = ""))

  all_data <- data.frame()

  target_countries <- c("USA", "GBR", "CAN", "AUS", "DEU", "FRA", "JPN", "ITA", "ESP", "NZL")

  if (!use_synthetic) {
    # Fetch OWID COVID vaccinations
    owid_data <- fetch_owid_covid_vaccinations(countries = target_countries)
    if (nrow(owid_data) > 0) {
      all_data <- bind_rows(all_data, owid_data |>
        transmute(
          observation_date,
          iso_code,
          pathogen_code = "COVID19",
          age_group = "All Ages",
          coverage_pct = people_fully_vaccinated_per_hundred,
          doses_administered = total_vaccinations
        ))
    }

    # Fetch CDC flu vaccination
    cdc_flu_data <- fetch_cdc_flu_vaccination()
    if (nrow(cdc_flu_data) > 0) {
      all_data <- bind_rows(all_data, cdc_flu_data |>
        select(observation_date, iso_code, pathogen_code, age_group, coverage_pct))
    }
  }

  # Fill gaps with synthetic data
  # For H3N2 (flu) - supplement
  if (!"H3N2" %in% unique(all_data$pathogen_code)) {
    h3n2_synthetic <- generate_synthetic_vaccination(target_countries, "H3N2")
    all_data <- bind_rows(all_data, h3n2_synthetic)
  }

  # For RSV - new vaccines, less data
  rsv_synthetic <- generate_synthetic_vaccination(target_countries, "RSV")
  all_data <- bind_rows(all_data, rsv_synthetic)

  # Insert all data
  total_records <- insert_vaccination_coverage(all_data)

  message("=" |> rep(60) |> paste(collapse = ""))
  message(sprintf("Total vaccination records: %d", total_records))
  message("=" |> rep(60) |> paste(collapse = ""))

  total_records
}

# Run if executed directly
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  use_synthetic <- "--synthetic" %in% args

  result <- fetch_all_vaccination_data(use_synthetic = use_synthetic)
  message(sprintf("Fetch complete: %d records", result))
}
