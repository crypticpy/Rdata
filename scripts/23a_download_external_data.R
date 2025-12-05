# ============================================================================
# Title: Download External Datasets for Multi-Dataset Fusion Analysis
# Author: Data Science Team
# Date: 2025-12-05
# Purpose: Download and prepare CDC PLACES and USDA Food Environment Atlas data
# Input: External APIs and web downloads
# Output: data/processed/cdc_places_county.rds
#         data/processed/usda_food_atlas.rds
#         data/processed/county_fips_crosswalk.rds
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(httr)
library(readxl)
library(janitor)

# Configuration ---------------------------------------------------------------
RAW_DIR <- "data/raw/external"
PROCESSED_DIR <- "data/processed"

# Ensure directories exist
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

# CDC PLACES API configuration
CDC_PLACES_API <- "https://data.cdc.gov/resource/cwsq-ngmh.csv"

# USDA Food Environment Atlas configuration
# Multiple URL patterns to try (URLs change frequently)
USDA_ATLAS_URLS <- c(
  "https://www.ers.usda.gov/webdocs/DataFiles/80526/FoodEnvironmentAtlas.xls",
  "https://www.ers.usda.gov/media/fg5d0fxj/foodenvironmentatlas.xls",
  "https://www.ers.usda.gov/media/foodenvironmentatlas.xls",
  paste0("https://www.ers.usda.gov/webdocs/DataFiles/80526/",
         "FoodEnvironmentAtlas.xls?v=", format(Sys.Date(), "%Y%m%d"))
)

# ============================================================================
# Helper Functions
# ============================================================================

#' Print progress message with timestamp
print_progress <- function(msg) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
}

#' Print summary statistics for a dataframe
print_summary <- function(df, name) {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Summary:", name, "\n")
  cat(strrep("=", 60), "\n")
  cat("Rows:", format(nrow(df), big.mark = ","), "\n")
  cat("Columns:", ncol(df), "\n")
  cat("Column names:\n")
  cat(paste(" -", names(df)[1:min(10, ncol(df))], collapse = "\n"), "\n")
  if (ncol(df) > 10) {
    cat(sprintf("  ... and %d more columns\n", ncol(df) - 10))
  }
  cat("\n")
}

#' Safe download with error handling
safe_download <- function(url, destfile, description = "file") {
  print_progress(sprintf("Downloading %s...", description))

  tryCatch({
    response <- GET(
      url,
      write_disk(destfile, overwrite = TRUE),
      progress(),
      timeout(300)  # 5 minute timeout
    )

    if (http_error(response)) {
      stop(sprintf(
        "HTTP error %d: %s",
        status_code(response),
        http_status(response)$message
      ))
    }

    print_progress(sprintf(
      "Downloaded successfully: %s (%.1f MB)",
      basename(destfile),
      file.size(destfile) / 1024^2
    ))

    return(TRUE)

  }, error = function(e) {
    warning(sprintf("Download failed: %s", e$message))
    return(FALSE)
  })
}

# ============================================================================
# CDC PLACES Download and Processing
# ============================================================================

download_cdc_places <- function() {
  print_progress("Starting CDC PLACES data download...")

  # Define measures of interest for health outcomes
  measures_of_interest <- c(
    "DIABETES",           # Diagnosed diabetes
    "OBESITY",            # Obesity
    "LPA",                # Physical inactivity
    "ACCESS2",            # Lack of health insurance
    "CHECKUP",            # Annual checkup
    "COLON_SCREEN",       # Colorectal cancer screening
    "MAMMOUSE",           # Mammography use
    "BPHIGH",             # High blood pressure
    "HIGHCHOL",           # High cholesterol
    "STROKE",             # Stroke
    "CHD",                # Coronary heart disease
    "COPD",               # COPD
    "KIDNEY",             # Chronic kidney disease
    "CSMOKING",           # Current smoking
    "BINGE"               # Binge drinking
  )

  # Query CDC PLACES API with filters
  # Using $limit and $offset for pagination
  all_data <- tibble()
  offset <- 0
  limit <- 50000  # Max records per request

  print_progress("Querying CDC PLACES API (this may take several minutes)...")

  repeat {
    # Build query URL
    query_url <- sprintf(
      "%s?$limit=%d&$offset=%d&$order=locationid",
      CDC_PLACES_API,
      limit,
      offset
    )

    print_progress(sprintf("Fetching records %d to %d...", offset, offset + limit))

    response <- tryCatch({
      GET(
        query_url,
        add_headers(
          "Accept" = "text/csv",
          "X-App-Token" = Sys.getenv("CDC_APP_TOKEN", "")  # Optional app token
        ),
        timeout(120)
      )
    }, error = function(e) {
      warning(sprintf("API request failed: %s", e$message))
      return(NULL)
    })

    if (is.null(response) || http_error(response)) {
      if (!is.null(response)) {
        warning(sprintf("HTTP error: %d", status_code(response)))
      }
      break
    }

    # Parse response
    content_text <- content(response, as = "text", encoding = "UTF-8")

    if (nchar(content_text) < 100) {
      print_progress("No more records to fetch.")
      break
    }

    batch_data <- read_csv(content_text, show_col_types = FALSE) |>
      # Ensure consistent types for binding
      mutate(across(everything(), as.character))

    if (nrow(batch_data) == 0) {
      break
    }

    all_data <- bind_rows(all_data, batch_data)
    print_progress(sprintf("Total records fetched: %d", nrow(all_data)))

    if (nrow(batch_data) < limit) {
      break  # Last page
    }

    offset <- offset + limit
    Sys.sleep(1)  # Rate limiting courtesy
  }

  if (nrow(all_data) == 0) {
    print_progress("API download failed. Attempting fallback method...")
    return(download_cdc_places_fallback())
  }

  # Save raw data
  raw_file <- file.path(RAW_DIR, "cdc_places_raw.csv")
  write_csv(all_data, raw_file)
  print_progress(sprintf("Saved raw CDC PLACES data: %s", raw_file))

  return(all_data)
}

#' Fallback download method for CDC PLACES
download_cdc_places_fallback <- function() {
  print_progress("Using fallback download method for CDC PLACES...")

  # Alternative: download full dataset export
  fallback_url <- paste0(
    "https://data.cdc.gov/api/views/cwsq-ngmh/rows.csv?",
    "accessType=DOWNLOAD"
  )

  raw_file <- file.path(RAW_DIR, "cdc_places_raw.csv")

  success <- safe_download(fallback_url, raw_file, "CDC PLACES full dataset")

  if (!success) {
    cat("\n")
    cat(strrep("!", 60), "\n")
    cat("MANUAL DOWNLOAD REQUIRED\n")
    cat(strrep("!", 60), "\n")
    cat("The CDC PLACES API is unavailable. Please download manually:\n\n")
    cat("1. Visit: https://data.cdc.gov/500-Cities-Places/\n")
    cat("   PLACES-Local-Data-for-Better-Health-County-Data-/cwsq-ngmh\n\n")
    cat("2. Click 'Export' > 'CSV'\n\n")
    cat("3. Save to:", normalizePath(raw_file, mustWork = FALSE), "\n\n")
    cat("4. Re-run this script\n")
    cat(strrep("!", 60), "\n\n")

    return(NULL)
  }

  # Read downloaded file
  all_data <- read_csv(raw_file, show_col_types = FALSE)
  return(all_data)
}

#' Process CDC PLACES data
process_cdc_places <- function(raw_data) {
  print_progress("Processing CDC PLACES data...")

  if (is.null(raw_data) || nrow(raw_data) == 0) {
    stop("No CDC PLACES data to process")
  }

  # Clean column names
  df <- raw_data |>
    clean_names()

  # Print available columns for debugging
  print_progress(sprintf("Available columns: %d", ncol(df)))
  print_progress(sprintf("Column names: %s", paste(names(df)[1:10], collapse = ", ")))

  # This dataset is at census tract level but contains countyfips
  # We need to aggregate to county level using countyfips
  print_progress("Aggregating tract-level data to county level...")

  # Use countyfips for county-level FIPS code (5-digit)
  if ("countyfips" %in% names(df)) {
    print_progress("Using countyfips column for county identification")
    df <- df |>
      mutate(county_fips = as.character(countyfips)) |>
      filter(!is.na(county_fips), nchar(county_fips) == 5)
  } else if ("locationid" %in% names(df)) {
    # Extract county FIPS from tract ID (first 5 digits)
    print_progress("Extracting county FIPS from locationid")
    df <- df |>
      mutate(county_fips = substr(as.character(locationid), 1, 5)) |>
      filter(!is.na(county_fips))
  }

  print_progress(sprintf("Records with valid county FIPS: %d", nrow(df)))

  processed <- df |>
    # Select relevant columns
    select(
      any_of(c(
        "year",
        "stateabbr", "state_abbr",
        "statedesc", "state_desc",
        "countyname", "county_name",
        "county_fips",
        "measure", "measureid", "measure_id",
        "data_value",
        "data_value_unit",
        "low_confidence_limit",
        "high_confidence_limit",
        "totalpopulation", "total_population",
        "category", "category_id"
      ))
    ) |>
    # Standardize column names
    rename_with(
      ~ case_when(
        . %in% c("stateabbr", "state_abbr") ~ "state_abbr",
        . %in% c("statedesc", "state_desc") ~ "state_name",
        . %in% c("countyname", "county_name") ~ "county_name",
        . == "county_fips" ~ "fips",
        . %in% c("measureid", "measure_id") ~ "measure_id",
        . %in% c("totalpopulation", "total_population") ~ "population",
        TRUE ~ .
      )
    )

  # Create wide format with one row per county-year
  # Aggregate tract-level data to county level, then pivot measures to columns
  if ("measure_id" %in% names(processed) || "measure" %in% names(processed)) {
    measure_col <- if ("measure_id" %in% names(processed)) "measure_id" else "measure"

    # Keep only percentage-based measures (crude prevalence)
    if ("data_value_unit" %in% names(processed)) {
      processed <- processed |>
        filter(data_value_unit == "%" | is.na(data_value_unit))
    }

    # First aggregate tract data to county level by averaging
    # This gives us county-level estimates from tract data
    print_progress("Aggregating tract data to county level...")

    county_aggregated <- processed |>
      # Convert data_value to numeric if character
      mutate(data_value = as.numeric(data_value)) |>
      # Group by county, year, and measure - aggregate across tracts
      group_by(fips, county_name, state_abbr, state_name, year,
               !!sym(measure_col)) |>
      summarise(
        data_value = mean(data_value, na.rm = TRUE),
        population = sum(as.numeric(population), na.rm = TRUE),
        .groups = "drop"
      )

    print_progress(sprintf("County-measure combinations: %d", nrow(county_aggregated)))

    # Now pivot measures to wide format
    processed_wide <- county_aggregated |>
      select(fips, county_name, state_abbr, state_name, year, population,
             all_of(measure_col), data_value) |>
      distinct() |>
      pivot_wider(
        names_from = all_of(measure_col),
        values_from = data_value,
        values_fn = mean  # Handle any remaining duplicates
      ) |>
      clean_names()

    print_progress(sprintf("Final county-year rows: %d", nrow(processed_wide)))
  } else {
    processed_wide <- processed
  }

  # Remove rows with no FIPS code
  if ("fips" %in% names(processed_wide)) {
    processed_wide <- processed_wide |>
      filter(!is.na(fips), fips != "")
  }

  print_summary(processed_wide, "CDC PLACES (processed)")

  return(processed_wide)
}

# ============================================================================
# USDA Food Environment Atlas Download and Processing
# ============================================================================

download_usda_atlas <- function() {
  print_progress("Starting USDA Food Environment Atlas download...")

  raw_file <- file.path(RAW_DIR, "FoodEnvironmentAtlas.xls")

  # Check if file already exists and is valid
  if (file.exists(raw_file) && file.size(raw_file) > 100000) {
    print_progress(sprintf("Using cached file: %s", raw_file))
    return(raw_file)
  }

  # Try all URL patterns
  success <- FALSE
  for (url in USDA_ATLAS_URLS) {
    success <- safe_download(url, raw_file, "USDA Food Environment Atlas")
    if (success && file.size(raw_file) > 100000) break
    success <- FALSE
  }

  if (!success) {
    cat("\n")
    cat(strrep("!", 60), "\n")
    cat("MANUAL DOWNLOAD REQUIRED\n")
    cat(strrep("!", 60), "\n")
    cat("The USDA Food Environment Atlas download failed.\n\n")
    cat("Please download manually:\n\n")
    cat("1. Visit: https://www.ers.usda.gov/data-products/\n")
    cat("   food-environment-atlas/data-access-and-documentation-downloads/\n\n")
    cat("2. Download 'Food Environment Atlas Data Download'\n\n")
    cat("3. Save to:", normalizePath(raw_file, mustWork = FALSE), "\n\n")
    cat("4. Re-run this script\n")
    cat(strrep("!", 60), "\n\n")

    return(NULL)
  }

  return(raw_file)
}

#' Process USDA Food Environment Atlas
process_usda_atlas <- function(excel_file) {
  print_progress("Processing USDA Food Environment Atlas...")

  if (is.null(excel_file) || !file.exists(excel_file)) {
    stop("USDA Food Atlas file not found")
  }

  # List available sheets
  sheets <- excel_sheets(excel_file)
  print_progress(sprintf("Available sheets: %s", paste(sheets, collapse = ", ")))

  # Key sheets typically include:
  # - "Supplemental Data - County" or similar for identifiers
  # - "ACCESS" - Food access indicators
  # - "STORES" - Food store counts
  # - "RESTAURANTS" - Restaurant counts
  # - "ASSISTANCE" - Food assistance programs
  # - "INSECURITY" - Food insecurity measures
  # - "LOCAL" - Local food systems
  # - "HEALTH" - Health outcomes
  # - "SOCIOECONOMIC" - Demographic data

  # Read each relevant sheet and combine
  all_sheets_data <- list()

  # Read variable descriptions first (usually first sheet)
  var_sheet <- sheets[grepl("variable|description|read", sheets, ignore.case = TRUE)]
  if (length(var_sheet) > 0) {
    print_progress(sprintf("Reading variable descriptions from: %s", var_sheet[1]))
  }

  # Key sheets to process
  target_sheets <- c(
    "ACCESS", "STORES", "RESTAURANTS", "ASSISTANCE",
    "INSECURITY", "LOCAL", "HEALTH", "SOCIOECONOMIC",
    "Supplemental Data - County"
  )

  # Find matching sheets (case-insensitive)
  matched_sheets <- sheets[
    sapply(sheets, function(s) {
      any(sapply(target_sheets, function(t) grepl(t, s, ignore.case = TRUE)))
    })
  ]

  # If no matches, try to read all non-documentation sheets
  if (length(matched_sheets) == 0) {
    matched_sheets <- sheets[
      !grepl("read|variable|description|note", sheets, ignore.case = TRUE)
    ]
  }

  print_progress(sprintf("Processing %d sheets...", length(matched_sheets)))

  for (sheet in matched_sheets) {
    print_progress(sprintf("  Reading sheet: %s", sheet))

    tryCatch({
      sheet_data <- read_excel(
        excel_file,
        sheet = sheet,
        .name_repair = "unique"
      ) |>
        clean_names()

      # Identify FIPS column
      fips_col <- names(sheet_data)[
        grepl("fips|county.*code|geo.*id", names(sheet_data), ignore.case = TRUE)
      ]

      if (length(fips_col) > 0) {
        # Rename first FIPS column to standard name
        sheet_data <- sheet_data |>
          rename(fips = all_of(fips_col[1]))

        # Convert FIPS to character and pad
        sheet_data <- sheet_data |>
          mutate(
            fips = as.character(fips),
            fips = str_pad(fips, width = 5, side = "left", pad = "0")
          )

        all_sheets_data[[sheet]] <- sheet_data
        print_progress(sprintf(
          "    Loaded %d rows, %d columns",
          nrow(sheet_data),
          ncol(sheet_data)
        ))
      } else {
        print_progress(sprintf("    Skipped (no FIPS column found)"))
      }

    }, error = function(e) {
      warning(sprintf("Error reading sheet '%s': %s", sheet, e$message))
    })
  }

  if (length(all_sheets_data) == 0) {
    stop("No valid sheets found in USDA Food Atlas")
  }

  # Merge all sheets by FIPS code
  print_progress("Merging sheets by FIPS code...")

  merged_data <- all_sheets_data[[1]]

  for (i in seq_along(all_sheets_data)[-1]) {
    sheet_data <- all_sheets_data[[i]]

    # Remove duplicate columns (except FIPS)
    common_cols <- intersect(names(merged_data), names(sheet_data))
    common_cols <- setdiff(common_cols, "fips")

    if (length(common_cols) > 0) {
      sheet_data <- sheet_data |>
        select(-all_of(common_cols))
    }

    merged_data <- merged_data |>
      left_join(sheet_data, by = "fips")
  }

  # Select key variables for food environment analysis
  # Common variable patterns in USDA Food Atlas:
  key_patterns <- c(
    "fips",
    "state", "county",
    "ffr",            # Fast food restaurants
    "fsr",            # Full service restaurants
    "grocery",        # Grocery stores
    "superc",         # Supercenters
    "convs",          # Convenience stores
    "snap",           # SNAP participation
    "wic",            # WIC participation
    "nslp",           # National School Lunch Program
    "food.*insec",    # Food insecurity
    "laccess",        # Low access to food
    "pct.*",          # Percentage variables
    "pc_.*",          # Per capita variables
    "povrate",        # Poverty rate
    "medhhinc",       # Median household income
    "pop"             # Population
  )

  # Select columns matching key patterns
  selected_cols <- names(merged_data)[
    sapply(names(merged_data), function(col) {
      any(sapply(key_patterns, function(pat) grepl(pat, col, ignore.case = TRUE)))
    })
  ]

  # Always include FIPS
  selected_cols <- unique(c("fips", selected_cols))

  # Filter to selected columns if we found enough
  if (length(selected_cols) > 10) {
    merged_data <- merged_data |>
      select(any_of(selected_cols))
  }

  # Remove rows with missing FIPS
  merged_data <- merged_data |>
    filter(!is.na(fips), fips != "", fips != "00000")

  print_summary(merged_data, "USDA Food Atlas (processed)")

  return(merged_data)
}

# ============================================================================
# County FIPS Crosswalk Creation
# ============================================================================

create_fips_crosswalk <- function(cdc_data, usda_data) {
  print_progress("Creating county FIPS crosswalk...")

  # Start with CDC data if available
  crosswalk <- tibble()

  if (!is.null(cdc_data) && "fips" %in% names(cdc_data)) {
    cdc_counties <- cdc_data |>
      select(any_of(c("fips", "county_name", "state_abbr", "state_name"))) |>
      distinct() |>
      mutate(source_cdc = TRUE)

    crosswalk <- cdc_counties
  }

  # Add USDA data if available
  if (!is.null(usda_data) && "fips" %in% names(usda_data)) {
    # Find state and county columns
    state_col <- names(usda_data)[grepl("^state$", names(usda_data), ignore.case = TRUE)]
    county_col <- names(usda_data)[grepl("^county$", names(usda_data), ignore.case = TRUE)]

    if (length(state_col) > 0 || length(county_col) > 0) {
      usda_counties <- usda_data |>
        select(
          fips,
          any_of(c(state_col, county_col))
        ) |>
        distinct()

      # Standardize column names
      if (length(state_col) > 0) {
        usda_counties <- usda_counties |>
          rename(state_name_usda = all_of(state_col[1]))
      }
      if (length(county_col) > 0) {
        usda_counties <- usda_counties |>
          rename(county_name_usda = all_of(county_col[1]))
      }

      usda_counties <- usda_counties |>
        mutate(source_usda = TRUE)

      if (nrow(crosswalk) > 0) {
        crosswalk <- crosswalk |>
          full_join(usda_counties, by = "fips")
      } else {
        crosswalk <- usda_counties
      }
    }
  }

  # If crosswalk is empty, create from scratch using Census Bureau data
  if (nrow(crosswalk) == 0) {
    print_progress("Creating crosswalk from Census Bureau FIPS codes...")

    # Download Census Bureau FIPS codes
    census_url <- paste0(
      "https://www2.census.gov/geo/docs/reference/codes2020/",
      "national_county2020.txt"
    )

    crosswalk_file <- file.path(RAW_DIR, "census_county_fips.txt")

    success <- safe_download(census_url, crosswalk_file, "Census FIPS codes")

    if (success && file.exists(crosswalk_file)) {
      crosswalk <- read_delim(
        crosswalk_file,
        delim = "|",
        col_types = cols(.default = "c"),
        show_col_types = FALSE
      ) |>
        clean_names() |>
        mutate(
          fips = paste0(statefp, countyfp),
          state_fips = statefp,
          county_fips = countyfp
        ) |>
        select(
          fips,
          state_fips,
          county_fips,
          state_name = state,
          state_abbr = stusab,
          county_name = countyname
        )
    }
  }

  # Return early if still empty
  if (nrow(crosswalk) == 0) {
    warning("Could not create FIPS crosswalk - no data available")
    return(tibble())
  }

  # Clean up crosswalk - only if we have data
  crosswalk <- crosswalk |>
    # Ensure FIPS is 5-digit character
    mutate(
      fips = as.character(fips),
      fips = str_pad(fips, width = 5, side = "left", pad = "0"),
      # Extract state FIPS (first 2 digits)
      state_fips = substr(fips, 1, 2),
      # Extract county FIPS (last 3 digits)
      county_fips = substr(fips, 3, 5)
    )

  # Consolidate name columns if both versions exist
  crosswalk_names <- names(crosswalk)

  if ("county_name_usda" %in% crosswalk_names) {
    if ("county_name" %in% crosswalk_names) {
      crosswalk <- crosswalk |>
        mutate(county_name = coalesce(county_name, county_name_usda))
    } else {
      crosswalk <- crosswalk |>
        rename(county_name = county_name_usda)
    }
  }

  if ("state_name_usda" %in% crosswalk_names) {
    if ("state_name" %in% crosswalk_names) {
      crosswalk <- crosswalk |>
        mutate(state_name = coalesce(state_name, state_name_usda))
    } else {
      crosswalk <- crosswalk |>
        rename(state_name = state_name_usda)
    }
  }

  crosswalk <- crosswalk |>
    # Remove intermediate columns
    select(-any_of(c("county_name_usda", "state_name_usda"))) |>
    distinct() |>
    arrange(fips)

  print_summary(crosswalk, "County FIPS Crosswalk")

  return(crosswalk)
}

# ============================================================================
# Main Execution
# ============================================================================

main <- function() {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("External Dataset Download and Preparation\n")
  cat("CDC PLACES + USDA Food Environment Atlas\n")
  cat(strrep("=", 60), "\n\n")

  # Track success
  results <- list(
    cdc_places = NULL,
    usda_atlas = NULL,
    crosswalk = NULL
  )

  # -------------------------------------------------------------------------
  # 1. Download and Process CDC PLACES
  # -------------------------------------------------------------------------
  cat("\n", strrep("-", 40), "\n", sep = "")
  cat("STEP 1: CDC PLACES Data\n")
  cat(strrep("-", 40), "\n")

  tryCatch({
    cdc_raw <- download_cdc_places()

    if (!is.null(cdc_raw) && nrow(cdc_raw) > 0) {
      cdc_processed <- process_cdc_places(cdc_raw)

      # Save processed data
      cdc_file <- file.path(PROCESSED_DIR, "cdc_places_county.rds")
      saveRDS(cdc_processed, cdc_file)
      print_progress(sprintf("Saved: %s", cdc_file))

      results$cdc_places <- cdc_processed
    }
  }, error = function(e) {
    warning(sprintf("CDC PLACES processing failed: %s", e$message))
  })

  # -------------------------------------------------------------------------
  # 2. Download and Process USDA Food Atlas
  # -------------------------------------------------------------------------
  cat("\n", strrep("-", 40), "\n", sep = "")
  cat("STEP 2: USDA Food Environment Atlas\n")
  cat(strrep("-", 40), "\n")

  tryCatch({
    usda_file <- download_usda_atlas()

    if (!is.null(usda_file) && file.exists(usda_file)) {
      usda_processed <- process_usda_atlas(usda_file)

      # Save processed data
      usda_output <- file.path(PROCESSED_DIR, "usda_food_atlas.rds")
      saveRDS(usda_processed, usda_output)
      print_progress(sprintf("Saved: %s", usda_output))

      results$usda_atlas <- usda_processed
    }
  }, error = function(e) {
    warning(sprintf("USDA Food Atlas processing failed: %s", e$message))
  })

  # -------------------------------------------------------------------------
  # 3. Create County FIPS Crosswalk
  # -------------------------------------------------------------------------
  cat("\n", strrep("-", 40), "\n", sep = "")
  cat("STEP 3: County FIPS Crosswalk\n")
  cat(strrep("-", 40), "\n")

  tryCatch({
    crosswalk <- create_fips_crosswalk(
      results$cdc_places,
      results$usda_atlas
    )

    if (!is.null(crosswalk) && nrow(crosswalk) > 0) {
      # Save crosswalk
      crosswalk_file <- file.path(PROCESSED_DIR, "county_fips_crosswalk.rds")
      saveRDS(crosswalk, crosswalk_file)
      print_progress(sprintf("Saved: %s", crosswalk_file))

      results$crosswalk <- crosswalk
    }
  }, error = function(e) {
    warning(sprintf("Crosswalk creation failed: %s", e$message))
  })

  # -------------------------------------------------------------------------
  # Final Summary
  # -------------------------------------------------------------------------
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("DOWNLOAD AND PROCESSING COMPLETE\n")
  cat(strrep("=", 60), "\n\n")

  cat("Output Files:\n")
  cat(strrep("-", 40), "\n")

  output_files <- c(
    "data/processed/cdc_places_county.rds",
    "data/processed/usda_food_atlas.rds",
    "data/processed/county_fips_crosswalk.rds"
  )

  for (f in output_files) {
    if (file.exists(f)) {
      size_mb <- file.size(f) / 1024^2
      cat(sprintf("  [OK] %s (%.2f MB)\n", f, size_mb))
    } else {
      cat(sprintf("  [MISSING] %s\n", f))
    }
  }

  cat("\n")
  cat("Summary Statistics:\n")
  cat(strrep("-", 40), "\n")

  if (!is.null(results$cdc_places)) {
    cat(sprintf(
      "  CDC PLACES: %s counties, %d variables\n",
      format(nrow(results$cdc_places), big.mark = ","),
      ncol(results$cdc_places)
    ))
  }

  if (!is.null(results$usda_atlas)) {
    cat(sprintf(
      "  USDA Food Atlas: %s counties, %d variables\n",
      format(nrow(results$usda_atlas), big.mark = ","),
      ncol(results$usda_atlas)
    ))
  }

  if (!is.null(results$crosswalk)) {
    cat(sprintf(
      "  FIPS Crosswalk: %s counties\n",
      format(nrow(results$crosswalk), big.mark = ",")
    ))
  }

  cat("\n")
  cat("Next Steps:\n")
  cat("  1. Review processed datasets for completeness\n")
  cat("  2. Join datasets by FIPS code for fusion analysis\n")
  cat("  3. Create regional aggregations as needed\n")
  cat("\n")

  return(results)
}

# Run if executed directly
if (!interactive()) {
  results <- main()
}
