# ============================================================================
# Title: RespiWatch Universal API Fetcher
# Purpose: Generic API framework with rate limiting, caching, and error handling
# Output: Standardized data frames ready for database insertion
# ============================================================================

# Load required packages
library(httr2)
library(jsonlite)
library(dplyr)
library(digest)

# =============================================================================
# CONFIGURATION
# =============================================================================

#' API Configuration Constants
API_CONFIG <- list(
  cache_dir = "data/cache",
  default_cache_hours = 4,
  max_retries = 3,
  retry_delay_seconds = 2,
  request_timeout_seconds = 30,
  rate_limit_requests_per_minute = 30,
  user_agent = "RespiWatch/1.0 (Respiratory Surveillance Dashboard; contact@example.com)"
)

# Create cache directory if it doesn't exist
if (!dir.exists(API_CONFIG$cache_dir)) {

  dir.create(API_CONFIG$cache_dir, recursive = TRUE)
}

# =============================================================================
# CACHING FUNCTIONS
# =============================================================================

#' Generate cache key from URL and parameters
#' @param url API endpoint URL
#' @param params List of query parameters
#' @return MD5 hash string for cache filename
generate_cache_key <- function(url, params = list()) {

  key_string <- paste0(url, "_", paste(names(params), params, sep = "=", collapse = "&"))
  digest::digest(key_string, algo = "md5")
}

#' Get cached response if valid
#' @param cache_key Cache key from generate_cache_key()
#' @param cache_hours Maximum age of cache in hours
#' @return Cached data or NULL if expired/missing
get_cached_response <- function(cache_key, cache_hours = API_CONFIG$default_cache_hours) {
  cache_file <- file.path(API_CONFIG$cache_dir, paste0(cache_key, ".rds"))

  if (!file.exists(cache_file)) {
    return(NULL)
  }

  file_age_hours <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "hours"))

  if (file_age_hours > cache_hours) {
    return(NULL)
  }

  tryCatch({
    readRDS(cache_file)
  }, error = function(e) {
    NULL
  })
}

#' Save response to cache
#' @param cache_key Cache key from generate_cache_key()
#' @param data Data to cache
cache_response <- function(cache_key, data) {
  cache_file <- file.path(API_CONFIG$cache_dir, paste0(cache_key, ".rds"))

  tryCatch({
    saveRDS(data, cache_file)
    TRUE
  }, error = function(e) {
    warning(sprintf("Failed to cache response: %s", e$message))
    FALSE
  })
}

#' Clear all cached responses
clear_cache <- function() {
  cache_files <- list.files(API_CONFIG$cache_dir, pattern = "\\.rds$", full.names = TRUE)
  file.remove(cache_files)
  message(sprintf("Cleared %d cached files", length(cache_files)))
}

#' Clear expired cache entries
#' @param max_age_hours Maximum age for cache entries
clear_expired_cache <- function(max_age_hours = 24) {
  cache_files <- list.files(API_CONFIG$cache_dir, pattern = "\\.rds$", full.names = TRUE)

  expired <- sapply(cache_files, function(f) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(f), units = "hours"))
    age > max_age_hours
  })

  if (any(expired)) {
    file.remove(cache_files[expired])
    message(sprintf("Removed %d expired cache entries", sum(expired)))
  }
}

# =============================================================================
# RATE LIMITING
# =============================================================================

# Track last request time per domain
.rate_limit_state <- new.env()

#' Apply rate limiting before making request
#' @param domain API domain for rate limiting
rate_limit_wait <- function(domain) {
  last_request <- .rate_limit_state[[domain]]
  min_interval <- 60 / API_CONFIG$rate_limit_requests_per_minute

  if (!is.null(last_request)) {
    elapsed <- as.numeric(difftime(Sys.time(), last_request, units = "secs"))
    if (elapsed < min_interval) {
      Sys.sleep(min_interval - elapsed)
    }
  }

  .rate_limit_state[[domain]] <- Sys.time()
}

#' Extract domain from URL
#' @param url Full URL
#' @return Domain string
get_domain <- function(url) {
  parsed <- httr2::url_parse(url)
  parsed$hostname
}

# =============================================================================
# RETRY WITH BACKOFF
# =============================================================================

#' Execute function with exponential backoff retry
#' @param fn Function to execute
#' @param max_retries Maximum number of retry attempts
#' @param initial_delay Initial delay in seconds
#' @return Result of fn() or error
retry_with_backoff <- function(fn, max_retries = API_CONFIG$max_retries,
                                initial_delay = API_CONFIG$retry_delay_seconds) {
  attempt <- 1
  delay <- initial_delay

  while (attempt <= max_retries) {
    result <- tryCatch({
      list(success = TRUE, data = fn())
    }, error = function(e) {
      list(success = FALSE, error = e)
    })

    if (result$success) {
      return(result$data)
    }

    if (attempt == max_retries) {
      stop(sprintf("Failed after %d attempts: %s", max_retries, result$error$message))
    }

    message(sprintf("Attempt %d failed, retrying in %.1f seconds...", attempt, delay))
    Sys.sleep(delay)
    delay <- delay * 2  # Exponential backoff
    attempt <- attempt + 1
  }
}

# =============================================================================
# CORE API FETCH FUNCTIONS
# =============================================================================

#' Fetch data from API endpoint
#' @param url API endpoint URL
#' @param params Query parameters as named list
#' @param cache_hours Hours to cache response (0 to disable)
#' @param method HTTP method (GET or POST)
#' @param body Request body for POST
#' @param headers Additional headers
#' @return Parsed JSON response
fetch_api_data <- function(url, params = list(), cache_hours = API_CONFIG$default_cache_hours,
                           method = "GET", body = NULL, headers = list()) {

  # Check cache first
  if (cache_hours > 0) {
    cache_key <- generate_cache_key(url, params)
    cached <- get_cached_response(cache_key, cache_hours)
    if (!is.null(cached)) {
      message(sprintf("Using cached response for %s", url))
      return(cached)
    }
  }

  # Apply rate limiting
  domain <- get_domain(url)
  rate_limit_wait(domain)

  # Build request
  req <- httr2::request(url) |>
    httr2::req_user_agent(API_CONFIG$user_agent) |>
    httr2::req_timeout(API_CONFIG$request_timeout_seconds)

  # Add query parameters
  if (length(params) > 0) {
    req <- httr2::req_url_query(req, !!!params)
  }

  # Add custom headers
  if (length(headers) > 0) {
    for (name in names(headers)) {
      req <- httr2::req_headers(req, !!name := headers[[name]])
    }
  }

  # Set method and body
  if (method == "POST" && !is.null(body)) {
    req <- httr2::req_body_json(req, body)
  }

  # Execute with retry
  response <- retry_with_backoff(function() {
    resp <- httr2::req_perform(req)

    # Check for HTTP errors
    if (httr2::resp_status(resp) >= 400) {
      stop(sprintf("HTTP %d: %s", httr2::resp_status(resp), httr2::resp_status_desc(resp)))
    }

    # Parse response
    content_type <- httr2::resp_content_type(resp)

    if (grepl("json", content_type, ignore.case = TRUE)) {
      httr2::resp_body_json(resp)
    } else if (grepl("csv", content_type, ignore.case = TRUE)) {
      text <- httr2::resp_body_string(resp)
      read.csv(text = text, stringsAsFactors = FALSE)
    } else {
      httr2::resp_body_string(resp)
    }
  })

  # Cache response
  if (cache_hours > 0) {
    cache_response(cache_key, response)
  }

  response
}

#' Fetch CSV data from URL
#' @param url CSV endpoint URL
#' @param cache_hours Hours to cache
#' @return Data frame
fetch_csv_data <- function(url, cache_hours = API_CONFIG$default_cache_hours) {
  # Check cache
  if (cache_hours > 0) {
    cache_key <- generate_cache_key(url, list())
    cached <- get_cached_response(cache_key, cache_hours)
    if (!is.null(cached)) {
      message(sprintf("Using cached CSV for %s", url))
      return(cached)
    }
  }

  # Apply rate limiting
  domain <- get_domain(url)
  rate_limit_wait(domain)

  # Fetch and parse
  data <- retry_with_backoff(function() {
    read.csv(url, stringsAsFactors = FALSE)
  })

  # Cache
  if (cache_hours > 0) {
    cache_response(cache_key, data)
  }

  data
}

# =============================================================================
# API-SPECIFIC FETCHERS
# =============================================================================

#' Fetch from CDC Socrata API (data.cdc.gov)
#' @param dataset_id Socrata dataset ID (e.g., "29hc-w46k")
#' @param params Query parameters
#' @param limit Maximum records to fetch
#' @return Data frame
fetch_cdc_socrata <- function(dataset_id, params = list(), limit = 10000) {
  base_url <- sprintf("https://data.cdc.gov/resource/%s.json", dataset_id)

  # Add limit
  params[["$limit"]] <- limit

  data <- fetch_api_data(base_url, params)

  # Convert to data frame
  if (is.list(data) && length(data) > 0) {
    bind_rows(data)
  } else {
    data.frame()
  }
}

#' Fetch from HHS HealthData.gov API
#' @param view_id View ID for the dataset
#' @param limit Maximum records
#' @return Data frame
fetch_hhs_healthdata <- function(view_id, limit = 10000) {
  url <- sprintf("https://healthdata.gov/api/views/%s/rows.json", view_id)

  data <- fetch_api_data(url, list())

  # HealthData.gov returns nested structure
  if (!is.null(data$data) && !is.null(data$meta$view$columns)) {
    columns <- sapply(data$meta$view$columns, function(x) x$fieldName)
    df <- as.data.frame(do.call(rbind, data$data), stringsAsFactors = FALSE)

    if (ncol(df) == length(columns)) {
      names(df) <- columns
    }

    # Take first limit rows
    if (nrow(df) > limit) {
      df <- df[1:limit, ]
    }

    df
  } else {
    data.frame()
  }
}

#' Fetch from WHO FluMart
#' @param report_no Report number
#' @param params Additional parameters
#' @return Data frame
fetch_who_flumart <- function(report_no = 12, params = list()) {
  base_url <- "https://apps.who.int/flumart/Default"

  params[["ReportNo"]] <- report_no

  # WHO FluMart may return CSV or HTML
  tryCatch({
    fetch_api_data(base_url, params, cache_hours = 6)
  }, error = function(e) {
    warning(sprintf("WHO FluMart fetch failed: %s", e$message))
    data.frame()
  })
}

#' Fetch from CMU Delphi Epidata API
#' @param endpoint Epidata endpoint (e.g., "covidcast")
#' @param params Query parameters
#' @return Data frame
fetch_delphi_epidata <- function(endpoint, params) {
  base_url <- sprintf("https://api.covidcast.cmu.edu/epidata/%s", endpoint)

  data <- fetch_api_data(base_url, params)

  if (!is.null(data$epidata) && is.list(data$epidata)) {
    bind_rows(data$epidata)
  } else {
    data.frame()
  }
}

#' Fetch from Our World in Data GitHub
#' @param dataset Dataset name (e.g., "vaccinations")
#' @return Data frame
fetch_owid_data <- function(dataset = "vaccinations") {
  url <- sprintf(
    "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/%s/%s.csv",
    dataset, dataset
  )

  fetch_csv_data(url, cache_hours = 6)
}

# =============================================================================
# DATA STANDARDIZATION
# =============================================================================

#' Standardize date column
#' @param df Data frame
#' @param date_col Name of date column
#' @param format Date format string
#' @return Data frame with standardized date
standardize_date <- function(df, date_col, format = "%Y-%m-%d") {
  if (date_col %in% names(df)) {
    df[[date_col]] <- as.Date(df[[date_col]], format = format)
  }
  df
}

#' Standardize country codes to ISO 3166-1 alpha-3
#' @param df Data frame
#' @param code_col Column with country codes
#' @return Data frame with standardized codes
standardize_country_codes <- function(df, code_col) {
  # Common mappings
  code_map <- c(
    "US" = "USA", "United States" = "USA",
    "UK" = "GBR", "United Kingdom" = "GBR", "GB" = "GBR",
    "AU" = "AUS", "Australia" = "AUS",
    "CA" = "CAN", "Canada" = "CAN",
    "JP" = "JPN", "Japan" = "JPN",
    "DE" = "DEU", "Germany" = "DEU",
    "FR" = "FRA", "France" = "FRA",
    "ES" = "ESP", "Spain" = "ESP",
    "IT" = "ITA", "Italy" = "ITA",
    "NL" = "NLD", "Netherlands" = "NLD",
    "NZ" = "NZL", "New Zealand" = "NZL"
  )

  if (code_col %in% names(df)) {
    df[[code_col]] <- ifelse(
      df[[code_col]] %in% names(code_map),
      code_map[df[[code_col]]],
      df[[code_col]]
    )
  }

  df
}

#' Standardize numeric columns
#' @param df Data frame
#' @param cols Vector of column names to convert
#' @return Data frame with numeric columns
standardize_numeric <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  df
}

# =============================================================================
# VALIDATION
# =============================================================================
#' Validate API response structure
#' @param data Response data
#' @param required_cols Required column names
#' @return TRUE if valid, FALSE otherwise
validate_response <- function(data, required_cols = NULL) {
  if (is.null(data) || length(data) == 0) {
    return(FALSE)
  }

  if (is.data.frame(data) && nrow(data) == 0) {
    return(FALSE)
  }

  if (!is.null(required_cols) && is.data.frame(data)) {
    missing <- setdiff(required_cols, names(data))
    if (length(missing) > 0) {
      warning(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
      return(FALSE)
    }
  }

  TRUE
}

# =============================================================================
# LOGGING
# =============================================================================

#' Log API fetch activity
#' @param source Data source name
#' @param status success/partial/failed
#' @param records Number of records fetched
#' @param message Optional message
log_fetch <- function(source, status, records = 0, message = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s (%d records)", timestamp, source, status, records)

  if (!is.null(message)) {
    log_entry <- paste0(log_entry, " - ", message)
  }

  message(log_entry)

  # Also write to log file
  log_file <- file.path(API_CONFIG$cache_dir, "fetch_log.txt")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

message("API Fetcher framework loaded successfully")
