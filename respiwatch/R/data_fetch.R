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

  if ("vaccination" %in% pathogens) {
    message("Fetching vaccination data...")
    results$vaccination <- fetch_cdc_vaccination()
  }

  if ("global" %in% pathogens) {
    message("Fetching WHO global data...")
    results$who <- fetch_who_data()
  }

  return(results)
}

#' Fetch CDC Vaccination Data (Flu and COVID-19)
#' @return Data frame with vaccination coverage
fetch_cdc_vaccination <- function() {
  cached_fetch("cdc_vaccination", function() {
    results <- list()
    
    # 1. Flu Vaccination Coverage (General Population) using specific SODA endpoint
    # Note: Using valid endpoint for Flu Vax coverage
    flu_url <- "https://data.cdc.gov/resource/p39v-828n.json" 
    
    tryCatch({
      req <- request(flu_url) |>
        req_url_query(
          `$limit` = 5000,
          `$order` = "week_ending DESC"
        )
      
      resp <- req_perform(req)
      data <- resp_body_json(resp, simplifyVector = TRUE)
      
      if (length(data) > 0) {
         results$flu <- data |>
          as_tibble() |>
          mutate(
            pathogen = "Influenza",
            data_source = "CDC Flu Vax View",
            fetch_time = Sys.time(),
            # Map columns - adjust based on actual API response structure
            observation_date = as.Date(week_ending),
            # Extract coverage estimate
            coverage_pct = as.numeric(coverage_estimate),
            age_group = geography_type # or specific age column if available
          )
      }
    }, error = function(e) {
      warning(paste("CDC Flu Vax API failed:", e$message))
    })

    # 2. COVID-19 Vaccination Trends
    covid_url <- "https://data.cdc.gov/resource/rh2h-3yt2.json"
    
    tryCatch({
      req <- request(covid_url) |>
        req_url_query(
          `$limit` = 5000,
          `$order` = "date DESC"
        )
      
      resp <- req_perform(req)
      data <- resp_body_json(resp, simplifyVector = TRUE)
      
      if (length(data) > 0) {
         results$covid <- data |>
          as_tibble() |>
          filter(location == "US") |> # Filter for National data only
          mutate(
            pathogen = "COVID-19",
            data_source = "CDC COVID Vax Trends",
            fetch_time = Sys.time(),
            observation_date = as.Date(date),
            # Extract coverage (e.g. administered_dose1_pop_pct or similar)
            coverage_pct = as.numeric(administered_dose1_pop_pct),
            age_group = "Overall" # Simplifying for now
          )
      }
    }, error = function(e) {
      warning(paste("CDC COVID Vax API failed:", e$message))
    })
    
    # Combine results
    bind_rows(results)
  })
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

#' Fetch Global COVID-19 Data (Source: Our World in Data)
#' @return Data frame with global surveillance data
fetch_who_data <- function() {
  cached_fetch("who_global_covid", function() {
    # Using OWID Latest snapshot (much faster/smaller than full history)
    # This provides current totals and recent daily changes
    base_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/latest/owid-covid-latest.csv"
    
    tryCatch({
      # Use read.csv for simple CSV fetch
      data <- read.csv(base_url, stringsAsFactors = FALSE) |>
        as_tibble() |>
        mutate(
          Date_reported = as.Date(last_updated_date),
          pathogen = "COVID-19", 
          data_source = "Our World in Data (OWID)",
          fetch_time = Sys.time()
        ) |>
        # OWID uses 'iso_code' which matches our DB schema (3-letter)
        filter(!is.na(iso_code)) |>
        select(
           observation_date = Date_reported,
           iso_code = iso_code,
           country_name = location,
           new_cases = new_cases,
           cumulative_cases = total_cases,
           new_deaths = new_deaths,
           cumulative_deaths = total_deaths,
           # Add vaccination data if available in this snapshot
           # OWID often includes 'total_vaccinations', 'people_vaccinated' etc.
           total_vaccinations = total_vaccinations,
           fully_vaccinated = people_fully_vaccinated,
           pathogen,
           data_source,
           fetch_time
        )
      
      data
    }, error = function(e) {
      warning(paste("Global Data API (OWID) failed:", e$message))
      NULL
    })
  })
}

#' Fetch Global Influenza Data from WHO FluNet/FluMart
#' @param weeks_back Number of weeks of historical data to fetch (default 52)
#' @return Data frame with global influenza surveillance data
fetch_who_flumart <- function(weeks_back = 52) {
  cached_fetch("who_flumart", function() {
    # WHO FluNet xMart API - public access to global influenza surveillance
    # Filter to recent weeks for efficiency
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    min_year <- current_year - 1

    base_url <- sprintf(
      "https://xmart-api-public.who.int/FLUMART/VIW_FNT?$format=csv&$filter=ISO_YEAR%%20ge%%20%d",
      min_year
    )

    tryCatch({
      data <- read.csv(base_url, stringsAsFactors = FALSE) |>
        as_tibble() |>
        filter(!is.na(SPEC_PROCESSED_NB) & SPEC_PROCESSED_NB > 0) |>
        mutate(
          observation_date = as.Date(ISO_WEEKSTARTDATE),
          # WHO uses 3-letter codes in COUNTRY_CODE (matches our DB iso_code)
          iso_code = COUNTRY_CODE,
          country_name = COUNTRY_AREA_TERRITORY,
          who_region = WHOREGION,
          specimens_processed = as.integer(SPEC_PROCESSED_NB),
          influenza_a = as.integer(ifelse(is.na(INF_A), 0, INF_A)),
          influenza_b = as.integer(ifelse(is.na(INF_B), 0, INF_B)),
          influenza_total = as.integer(ifelse(is.na(INF_ALL), 0, INF_ALL)),
          # Calculate positivity rate
          positivity_rate = ifelse(
            specimens_processed > 0,
            round(influenza_total / specimens_processed * 100, 2),
            NA_real_
          ),
          pathogen = "Influenza",
          data_source = "WHO FluNet",
          fetch_time = Sys.time()
        ) |>
        filter(
          !is.na(observation_date),
          !is.na(iso_code),
          nchar(iso_code) == 3  # Ensure valid ISO3 codes
        ) |>
        select(
          observation_date, iso_code, country_name, who_region,
          specimens_processed, influenza_a, influenza_b, influenza_total,
          positivity_rate, pathogen, data_source, fetch_time
        )

      message(sprintf("  Fetched %d FluNet records from %d countries",
                      nrow(data), length(unique(data$iso_code))))
      data
    }, error = function(e) {
      warning(paste("WHO FluNet API failed:", e$message))
      NULL
    })
  })
}

#' Export surveillance data to JSON format matching existing dashboard structure
#' @param data List of surveillance data
#' @param output_file Path to output JSON file
export_to_json <- function(data, output_file) {
  json_data <- list(
    metadata = list(
      last_updated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      data_sources = c("CDC FluView", "CDC NREVSS", "RSV-NET", "CMU Delphi", "WHO Global"),
      version = "2.1.0"
    ),
    influenza = data$influenza,
    rsv = data$rsv,
    covid = data$covid,
    who_global = data$who
  )

  write_json(json_data, output_file, pretty = TRUE, auto_unbox = TRUE)
  message(paste("Exported data to", output_file))
}
