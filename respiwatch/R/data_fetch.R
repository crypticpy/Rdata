# ============================================================================
# Title: RespiWatch Data Fetching Module
# Purpose: Connect to live CDC, WHO, and CMU Delphi APIs for respiratory data
# Input: API endpoints (CDC FluView, data.cdc.gov, epidatr)
# Output: Standardized data frames for influenza, RSV, and COVID
# ============================================================================

# Load required packages -------------------------------------------------------
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)

# Check and load optional packages
if (requireNamespace("cdcfluview", quietly = TRUE)) {
  library(cdcfluview)
  HAS_CDCFLUVIEW <- TRUE
} else {
  HAS_CDCFLUVIEW <- FALSE
  message("cdcfluview not available - using httr2 for flu data")
}

if (requireNamespace("epidatr", quietly = TRUE)) {
  library(epidatr)
  HAS_EPIDATR <- TRUE
} else {
  HAS_EPIDATR <- FALSE
  message("epidatr not available - COVID signals limited")
}

# Configuration ----------------------------------------------------------------
CACHE_DIR <- "data/cache"
CACHE_DURATION_HOURS <- 1  # How long to cache API responses

# Create cache directory if it doesn't exist
if (!dir.exists(CACHE_DIR)) {
  dir.create(CACHE_DIR, recursive = TRUE)
}

# Helper: Check if cache is still valid ----------------------------------------
cache_is_valid <- function(cache_file, max_age_hours = CACHE_DURATION_HOURS) {
  if (!file.exists(cache_file)) return(FALSE)
  file_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
  return(as.numeric(file_age) < max_age_hours)
}

# Helper: Read from cache or fetch fresh data ----------------------------------
cached_fetch <- function(cache_key, fetch_fn, max_age_hours = CACHE_DURATION_HOURS) {
  cache_file <- file.path(CACHE_DIR, paste0(cache_key, ".rds"))

  if (cache_is_valid(cache_file, max_age_hours)) {
    message(paste("Loading", cache_key, "from cache"))
    return(readRDS(cache_file))
  }

  message(paste("Fetching fresh", cache_key, "from API"))
  tryCatch({
    data <- fetch_fn()
    saveRDS(data, cache_file)
    return(data)
  }, error = function(e) {
    warning(paste("Failed to fetch", cache_key, ":", e$message))
    # Return cached data if available, even if stale
    if (file.exists(cache_file)) {
      warning("Returning stale cached data")
      return(readRDS(cache_file))
    }
    return(NULL)
  })
}

# =============================================================================
# INFLUENZA DATA FETCHING
# =============================================================================

#' Fetch CDC ILINet national influenza data
#' @param years Vector of years to fetch (default: current and previous season)
#' @return Data frame with national ILI surveillance data
fetch_ilinet_national <- function(years = NULL) {
  if (is.null(years)) {
    current_year <- year(Sys.Date())
    years <- c(current_year - 1, current_year)
  }

  cached_fetch("ilinet_national", function() {
    if (HAS_CDCFLUVIEW) {
      # Use cdcfluview package
      ilinet(region = "national", years = years) |>
        mutate(
          pathogen = "Influenza",
          data_source = "CDC ILINet",
          fetch_time = Sys.time()
        )
    } else {
      # Fallback: direct API call
      fetch_ilinet_direct(years)
    }
  })
}

#' Fetch CDC ILINet state-level influenza data
#' @param years Vector of years to fetch
#' @return Data frame with state-level ILI surveillance data
fetch_ilinet_state <- function(years = NULL) {
  if (is.null(years)) {
    current_year <- year(Sys.Date())
    years <- c(current_year - 1, current_year)
  }

  cached_fetch("ilinet_state", function() {
    if (HAS_CDCFLUVIEW) {
      ilinet(region = "state", years = years) |>
        mutate(
          pathogen = "Influenza",
          data_source = "CDC ILINet",
          fetch_time = Sys.time()
        )
    } else {
      NULL
    }
  })
}

#' Fetch WHO/NREVSS clinical laboratory data
#' @param years Vector of years to fetch
#' @return Data frame with virus subtype testing data
fetch_nrevss_clinical <- function(years = NULL) {
  if (is.null(years)) {
    current_year <- year(Sys.Date())
    years <- c(current_year - 1, current_year)
  }

  cached_fetch("nrevss_clinical", function() {
    if (HAS_CDCFLUVIEW) {
      who_nrevss(region = "national", years = years) |>
        mutate(
          pathogen = "Influenza",
          data_source = "WHO/NREVSS",
          fetch_time = Sys.time()
        )
    } else {
      NULL
    }
  })
}

#' Direct API call to CDC FluView (fallback if cdcfluview not available)
fetch_ilinet_direct <- function(years) {
  # CDC FluView uses a complex API - simplified version here
  base_url <- "https://gis.cdc.gov/grasp/flu2/PostPhase02DataDownload"

  tryCatch({
    req <- request(base_url) |>
      req_body_json(list(
        AppVersion = "Public",
        DatasourceDT = list(list(ID = 1, Name = "ILINet")),
        RegionTypeId = 3,  # National
        SeasonsDT = lapply(years, function(y) list(ID = y, Name = paste0(y, "-", y + 1)))
      ))

    resp <- req_perform(req)
    data <- resp_body_json(resp)

    # Parse and return as data frame
    as.data.frame(data)
  }, error = function(e) {
    warning(paste("Direct CDC API failed:", e$message))
    NULL
  })
}

# =============================================================================
# RSV DATA FETCHING
# =============================================================================

#' Fetch CDC RSV laboratory data from NREVSS via cdcfluview
#' @param years Vector of years to fetch
#' @return Data frame with RSV positivity and testing data
fetch_rsv_nrevss <- function(years = NULL) {
  if (is.null(years)) {
    current_year <- year(Sys.Date())
    years <- c(current_year - 1, current_year)
  }

  cached_fetch("rsv_nrevss", function() {
    if (HAS_CDCFLUVIEW) {
      # Use cdcfluview package - RSV data is in NREVSS clinical labs
      tryCatch({
        nrevss_data <- who_nrevss(region = "national", years = years)

        # Extract clinical labs data which includes RSV
        if ("icl_nrevss_clinical_labs" %in% names(nrevss_data)) {
          clinical_labs <- nrevss_data$icl_nrevss_clinical_labs |>
            mutate(
              pathogen = "RSV",
              data_source = "CDC NREVSS Clinical Labs",
              fetch_time = Sys.time(),
              # Calculate RSV-specific metrics if available
              rsv_positive = as.numeric(total_rsv),
              total_tests = as.numeric(total_specimens),
              rsv_percent_positive = if_else(
                total_tests > 0,
                (rsv_positive / total_tests) * 100,
                NA_real_
              )
            )
          return(clinical_labs)
        } else {
          warning("RSV data not found in NREVSS response")
          return(NULL)
        }
      }, error = function(e) {
        warning(paste("cdcfluview RSV fetch failed:", e$message))
        NULL
      })
    } else {
      # Fallback: direct API call to CDC Respiratory Virus Surveillance
      fetch_rsv_direct()
    }
  })
}

#' Fetch respiratory hospital capacity data from CDC (includes RSV burden proxy)
#' @return Data frame with hospital capacity data by jurisdiction
fetch_rsv_direct <- function() {
  # CDC Weekly Respiratory Surveillance - Hospital Capacity (working endpoint)
  base_url <- "https://data.cdc.gov/resource/ua7e-t2fy.json"

  tryCatch({
    req <- request(base_url) |>
      req_url_query(
        `$limit` = 5000,
        `$order` = "weekendingdate DESC"
      )

    resp <- req_perform(req)
    data <- resp_body_json(resp, simplifyVector = TRUE)

    if (length(data) == 0) return(NULL)

    data |>
      as_tibble() |>
      mutate(
        pathogen = "Respiratory",
        data_source = "CDC Respiratory Hospital Capacity",
        fetch_time = Sys.time(),
        # Calculate occupancy rate
        occupancy_rate = if_else(
          as.numeric(numinptbeds) > 0,
          as.numeric(numinptbedsocc) / as.numeric(numinptbeds) * 100,
          NA_real_
        )
      )
  }, error = function(e) {
    warning(paste("Direct Respiratory Hospital API failed:", e$message))
    NULL
  })
}

#' Fetch RSV hospitalization data from RSV-NET
#' @return Data frame with RSV hospitalization surveillance data
fetch_rsv_net <- function() {
  cached_fetch("rsv_net", function() {
    # RSV-NET hospitalization data endpoint
    base_url <- "https://data.cdc.gov/resource/29hc-w46k.json"

    tryCatch({
      req <- request(base_url) |>
        req_url_query(
          `$limit` = 5000,
          `$order` = "week_end DESC"
        )

      resp <- req_perform(req)
      data <- resp_body_json(resp, simplifyVector = TRUE)

      if (length(data) == 0) {
        return(NULL)
      }

      data |>
        as_tibble() |>
        mutate(
          pathogen = "RSV",
          data_source = "RSV-NET",
          fetch_time = Sys.time()
        )
    }, error = function(e) {
      warning(paste("RSV-NET API failed:", e$message))
      NULL
    })
  })
}

# =============================================================================
# COVID-19 DATA FETCHING
# =============================================================================

#' Fetch COVID-19 data from CMU Delphi COVIDcast
#' @param signal The signal to fetch (e.g., "confirmed_cumulative_num")
#' @param geo_type Geographic level ("nation", "state", "county")
#' @param start_date Start date for data
#' @param end_date End date for data
#' @return Data frame with COVID-19 surveillance signals
fetch_covid_covidcast <- function(
    signal = "confirmed_7dav_incidence_prop",
    geo_type = "nation",
    start_date = NULL,
    end_date = NULL
) {
  if (is.null(start_date)) {
    start_date <- Sys.Date() - 90  # Last 90 days
  }
  if (is.null(end_date)) {
    end_date <- Sys.Date() - 1
  }

  cache_key <- paste("covid_covidcast", signal, geo_type, sep = "_")

  cached_fetch(cache_key, function() {
    if (HAS_EPIDATR) {
      tryCatch({
        epidatr::pub_covidcast(
          source = "jhu-csse",
          signals = signal,
          geo_type = geo_type,
          time_type = "day",
          geo_values = "*",
          time_values = epidatr::epirange(start_date, end_date)
        ) |>
          mutate(
            pathogen = "COVID-19",
            data_source = "CMU Delphi COVIDcast",
            fetch_time = Sys.time()
          )
      }, error = function(e) {
        warning(paste("COVIDcast API failed:", e$message))
        NULL
      })
    } else {
      # Fallback: use CDC COVID Data Tracker API
      fetch_covid_cdc_direct()
    }
  })
}

#' Fetch COVID-19 data directly from CDC Data Tracker
#' @return Data frame with COVID-19 metrics
fetch_covid_cdc_direct <- function() {
  cached_fetch("covid_cdc_direct", function() {
    # CDC COVID Surveillance - State level (working endpoint)
    base_url <- "https://data.cdc.gov/resource/pwn4-m3yp.json"

    tryCatch({
      req <- request(base_url) |>
        req_url_query(
          `$limit` = 5000,
          `$order` = "date_updated DESC"
        )

      resp <- req_perform(req)
      data <- resp_body_json(resp, simplifyVector = TRUE)

      if (length(data) == 0) {
        return(NULL)
      }

      data |>
        as_tibble() |>
        mutate(
          pathogen = "COVID-19",
          data_source = "CDC COVID Data Tracker",
          fetch_time = Sys.time(),
          # Standardize column names
          total_cases = as.numeric(tot_cases),
          new_cases = as.numeric(new_cases),
          total_deaths = as.numeric(tot_deaths),
          new_deaths = as.numeric(new_deaths)
        )
    }, error = function(e) {
      warning(paste("CDC COVID API failed:", e$message))
      NULL
    })
  })
}

#' Fetch COVID-19 variant proportions
#' @return Data frame with variant surveillance data
fetch_covid_variants <- function() {
  cached_fetch("covid_variants", function() {
    # NCBI Nowcast variant proportions via data.cdc.gov
    base_url <- "https://data.cdc.gov/resource/jr58-6ysp.json"

    tryCatch({
      req <- request(base_url) |>
        req_url_query(
          `$limit` = 1000,
          `$order` = "week_ending DESC"
        )

      resp <- req_perform(req)
      data <- resp_body_json(resp, simplifyVector = TRUE)

      if (length(data) == 0) {
        return(NULL)
      }

      data |>
        as_tibble() |>
        mutate(
          pathogen = "COVID-19",
          data_source = "CDC Variant Surveillance",
          fetch_time = Sys.time()
        )
    }, error = function(e) {
      warning(paste("COVID variants API failed:", e$message))
      NULL
    })
  })
}

# =============================================================================
# COMBINED DATA FETCHING
# =============================================================================

#' Fetch all respiratory pathogen surveillance data
#' @param pathogens Vector of pathogens to fetch ("influenza", "rsv", "covid")
#' @param force_refresh If TRUE, bypass cache
#' @return List of data frames for each pathogen
fetch_all_surveillance_data <- function(
    pathogens = c("influenza", "rsv", "covid"),
    force_refresh = FALSE
) {
  if (force_refresh) {
    # Clear cache
    cache_files <- list.files(CACHE_DIR, full.names = TRUE)
    file.remove(cache_files)
  }

  results <- list()

  if ("influenza" %in% pathogens) {
    message("Fetching influenza data...")
    results$influenza <- list(
      ilinet_national = fetch_ilinet_national(),
      ilinet_state = fetch_ilinet_state(),
      nrevss = fetch_nrevss_clinical()
    )
  }

  if ("rsv" %in% pathogens) {
    message("Fetching RSV data...")
    results$rsv <- list(
      nrevss = fetch_rsv_nrevss(),
      rsv_net = fetch_rsv_net()
    )
  }

  if ("covid" %in% pathogens) {
    message("Fetching COVID-19 data...")
    results$covid <- list(
      national = fetch_covid_cdc_direct(),
      variants = fetch_covid_variants()
    )
  }

  return(results)
}

#' Convert surveillance data to standardized format for dashboard
#' @param data Raw API data
#' @param pathogen Pathogen name
#' @return Standardized data frame
standardize_surveillance_data <- function(data, pathogen) {
  if (is.null(data)) return(NULL)

  # Common fields across all pathogens
  data |>
    mutate(
      pathogen = pathogen,
      update_timestamp = Sys.time()
    ) |>
    select(
      any_of(c(
        "date", "week", "year", "week_date",
        "region", "state", "location",
        "positivity_rate", "percent_positive",
        "cases", "hospitalizations", "deaths",
        "pathogen", "data_source", "update_timestamp"
      ))
    )
}

#' Get data freshness report
#' @return Data frame with cache status for all data sources
get_data_freshness <- function() {
  cache_files <- list.files(CACHE_DIR, pattern = "\\.rds$", full.names = TRUE)

  if (length(cache_files) == 0) {
    return(tibble(
      source = character(),
      last_updated = as.POSIXct(character()),
      age_hours = numeric(),
      is_fresh = logical()
    ))
  }

  tibble(
    file = basename(cache_files),
    source = gsub("\\.rds$", "", basename(cache_files)),
    last_updated = file.mtime(cache_files),
    age_hours = as.numeric(difftime(Sys.time(), file.mtime(cache_files), units = "hours")),
    is_fresh = age_hours < CACHE_DURATION_HOURS
  ) |>
    select(-file)
}

#' Force refresh all data from APIs
#' @return Invisibly returns the freshness report after refresh
force_refresh_all <- function() {
  message("Force refreshing all surveillance data...")
  fetch_all_surveillance_data(force_refresh = TRUE)
  invisible(get_data_freshness())
}

# =============================================================================
# EXPORT JSON FOR DASHBOARD
# =============================================================================

#' Export surveillance data to JSON format matching existing dashboard structure
#' @param data List of surveillance data
#' @param output_file Path to output JSON file
export_to_json <- function(data, output_file) {
  json_data <- list(
    metadata = list(
      last_updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      data_sources = c("CDC FluView", "CDC NREVSS", "RSV-NET", "CMU Delphi"),
      version = "2.0.0"
    ),
    influenza = data$influenza,
    rsv = data$rsv,
    covid = data$covid
  )

  write_json(json_data, output_file, pretty = TRUE, auto_unbox = TRUE)
  message(paste("Exported data to", output_file))
}
