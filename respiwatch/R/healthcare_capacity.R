# ============================================================================
# Title: RespiWatch Healthcare Capacity Module
# Purpose: Hospital/ICU forecasting with surge alerts
# Input: Surveillance data, Rt forecasts, capacity data
# Output: Healthcare demand forecasts and surge alerts
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

# =============================================================================
# CONFIGURATION
# =============================================================================

#' Healthcare capacity configuration
HEALTHCARE_CONFIG <- list(
  # Hospitalization rates by pathogen (proportion of cases hospitalized)
  hospitalization_rates = list(
    H3N2 = 0.012,      # ~1.2% of influenza cases hospitalized
    RSV = 0.025,       # ~2.5% RSV cases (higher in elderly/infants)
    COVID19 = 0.035    # ~3.5% COVID cases (varies by variant/vaccination)
  ),

  # ICU admission rates (proportion of hospitalized needing ICU)
  icu_rates = list(
    H3N2 = 0.15,       # 15% of hospitalized flu patients need ICU
    RSV = 0.20,        # 20% of hospitalized RSV patients need ICU
    COVID19 = 0.25     # 25% of hospitalized COVID patients need ICU
  ),

  # Average length of stay (days)
  los_hospital = list(
    H3N2 = 5,
    RSV = 6,
    COVID19 = 8
  ),

  los_icu = list(
    H3N2 = 7,
    RSV = 10,
    COVID19 = 14
  ),

  # Time from symptom onset to hospitalization (days)
  onset_to_hospital = list(
    H3N2 = 4,
    RSV = 3,
    COVID19 = 7
  ),

  # Capacity thresholds for alerts
  capacity_thresholds = list(
    critical = 0.90,   # >90% capacity = critical
    warning = 0.80,    # >80% capacity = warning
    elevated = 0.70    # >70% capacity = elevated
  ),

  # Default bed counts (can be overridden with real data)
  default_capacity = list(
    hospital_beds = 100000,  # National estimate
    icu_beds = 15000
  )
)

# =============================================================================
# HOSPITALIZATION FORECASTING
# =============================================================================

#' Forecast hospitalizations from case forecasts
#' @param case_forecast Data frame with date, predicted_cases columns
#' @param pathogen_code Pathogen identifier
#' @param los Length of stay in days (default: from config)
#' @return Data frame with hospitalization forecasts
forecast_hospitalizations <- function(case_forecast, pathogen_code,
                                        los = NULL) {
  if (is.null(case_forecast) || nrow(case_forecast) == 0) {
    return(NULL)
  }

  # Get pathogen-specific rates
  hosp_rate <- HEALTHCARE_CONFIG$hospitalization_rates[[pathogen_code]]
  if (is.null(hosp_rate)) hosp_rate <- 0.02  # Default 2%

  if (is.null(los)) {
    los <- HEALTHCARE_CONFIG$los_hospital[[pathogen_code]]
    if (is.null(los)) los <- 6
  }

  onset_delay <- HEALTHCARE_CONFIG$onset_to_hospital[[pathogen_code]]
  if (is.null(onset_delay)) onset_delay <- 5

  # Calculate new admissions (lagged from cases)
  result <- case_forecast |>
    mutate(
      # New hospital admissions from cases (with delay)
      new_admissions = round(predicted_cases * hosp_rate),

      # Estimate current occupancy using convolution with LOS
      # Simplified: assume steady state where occupancy = admissions * LOS / 7
      estimated_occupancy = round(new_admissions * los / 7),

      pathogen = pathogen_code,
      hospitalization_rate = hosp_rate,
      length_of_stay = los
    )

  # Add uncertainty bounds if present in input
  if ("lower_95" %in% names(case_forecast)) {
    result <- result |>
      mutate(
        admissions_lower = round(lower_95 * hosp_rate),
        admissions_upper = round(upper_95 * hosp_rate),
        occupancy_lower = round(admissions_lower * los / 7),
        occupancy_upper = round(admissions_upper * los / 7)
      )
  }

  result
}

#' Forecast ICU demand from hospitalization forecasts
#' @param hosp_forecast Data frame from forecast_hospitalizations()
#' @param pathogen_code Pathogen identifier
#' @param icu_los ICU length of stay (default: from config)
#' @return Data frame with ICU demand forecasts
forecast_icu_demand <- function(hosp_forecast, pathogen_code, icu_los = NULL) {
  if (is.null(hosp_forecast) || nrow(hosp_forecast) == 0) {
    return(NULL)
  }

  # Get pathogen-specific ICU rate

icu_rate <- HEALTHCARE_CONFIG$icu_rates[[pathogen_code]]
  if (is.null(icu_rate)) icu_rate <- 0.20  # Default 20%

  if (is.null(icu_los)) {
    icu_los <- HEALTHCARE_CONFIG$los_icu[[pathogen_code]]
    if (is.null(icu_los)) icu_los <- 10
  }

  # Calculate ICU demand
  result <- hosp_forecast |>
    mutate(
      new_icu_admissions = round(new_admissions * icu_rate),
      estimated_icu_occupancy = round(new_icu_admissions * icu_los / 7),
      icu_rate = icu_rate,
      icu_length_of_stay = icu_los
    )

  # Add uncertainty bounds if present
  if ("admissions_lower" %in% names(hosp_forecast)) {
    result <- result |>
      mutate(
        icu_admissions_lower = round(admissions_lower * icu_rate),
        icu_admissions_upper = round(admissions_upper * icu_rate),
        icu_occupancy_lower = round(icu_admissions_lower * icu_los / 7),
        icu_occupancy_upper = round(icu_admissions_upper * icu_los / 7)
      )
  }

  result
}

# =============================================================================
# CAPACITY CALCULATIONS
# =============================================================================

#' Calculate capacity headroom
#' @param current_occupancy Current hospital/ICU occupancy
#' @param total_capacity Total available beds
#' @return List with capacity metrics
calculate_capacity_headroom <- function(current_occupancy, total_capacity) {
  if (is.na(current_occupancy) || is.na(total_capacity) || total_capacity <= 0) {
    return(list(
      utilization = NA,
      available_beds = NA,
      headroom_pct = NA,
      status = "unknown"
    ))
  }

  utilization <- current_occupancy / total_capacity
  available_beds <- total_capacity - current_occupancy
  headroom_pct <- (1 - utilization) * 100

  # Determine status
  thresholds <- HEALTHCARE_CONFIG$capacity_thresholds
  status <- if (utilization >= thresholds$critical) {
    "critical"
  } else if (utilization >= thresholds$warning) {
    "warning"
  } else if (utilization >= thresholds$elevated) {
    "elevated"
  } else {
    "normal"
  }

  list(
    utilization = round(utilization, 3),
    utilization_pct = round(utilization * 100, 1),
    available_beds = available_beds,
    headroom_pct = round(headroom_pct, 1),
    status = status,
    current_occupancy = current_occupancy,
    total_capacity = total_capacity
  )
}

#' Project when capacity will be exceeded
#' @param occupancy_forecast Vector of projected occupancy values
#' @param dates Vector of corresponding dates
#' @param total_capacity Total available beds
#' @param threshold Capacity threshold (default: 0.90)
#' @return List with projection details
project_capacity_breach <- function(occupancy_forecast, dates, total_capacity,
                                      threshold = 0.90) {
  if (length(occupancy_forecast) == 0 || length(dates) == 0) {
    return(list(
      breach_predicted = FALSE,
      breach_date = NA,
      days_until_breach = NA
    ))
  }

  threshold_beds <- total_capacity * threshold

  # Find first date where occupancy exceeds threshold
  breach_idx <- which(occupancy_forecast >= threshold_beds)[1]

  if (is.na(breach_idx)) {
    return(list(
      breach_predicted = FALSE,
      breach_date = NA,
      days_until_breach = NA,
      max_occupancy = max(occupancy_forecast, na.rm = TRUE),
      max_occupancy_date = dates[which.max(occupancy_forecast)],
      peak_utilization = max(occupancy_forecast, na.rm = TRUE) / total_capacity
    ))
  }

  list(
    breach_predicted = TRUE,
    breach_date = dates[breach_idx],
    days_until_breach = as.numeric(difftime(dates[breach_idx], Sys.Date(), units = "days")),
    threshold_used = threshold,
    max_occupancy = max(occupancy_forecast, na.rm = TRUE),
    max_occupancy_date = dates[which.max(occupancy_forecast)],
    peak_utilization = max(occupancy_forecast, na.rm = TRUE) / total_capacity
  )
}

# =============================================================================
# SURGE ALERT SYSTEM
# =============================================================================

#' Generate surge alerts for healthcare capacity
#' @param capacity_result Result from get_capacity_forecast()
#' @return List of alert objects
generate_surge_alerts <- function(capacity_result) {
  alerts <- list()

  if (is.null(capacity_result) || capacity_result$status != "success") {
    return(alerts)
  }

  # Check current capacity status
  hospital <- capacity_result$hospital_capacity
  icu <- capacity_result$icu_capacity

  # Hospital capacity alerts
  if (!is.null(hospital) && hospital$status == "critical") {
    alerts[[length(alerts) + 1]] <- list(
      type = "hospital_critical",
      severity = "critical",
      title = "Hospital Capacity Critical",
      message = sprintf(
        "Hospital capacity at %.1f%% utilization. Only %d beds available.",
        hospital$utilization_pct, hospital$available_beds
      ),
      value = hospital$utilization_pct,
      threshold = HEALTHCARE_CONFIG$capacity_thresholds$critical * 100,
      timestamp = Sys.time()
    )
  } else if (!is.null(hospital) && hospital$status == "warning") {
    alerts[[length(alerts) + 1]] <- list(
      type = "hospital_warning",
      severity = "warning",
      title = "Hospital Capacity Warning",
      message = sprintf(
        "Hospital capacity at %.1f%% utilization. Monitor closely.",
        hospital$utilization_pct
      ),
      value = hospital$utilization_pct,
      threshold = HEALTHCARE_CONFIG$capacity_thresholds$warning * 100,
      timestamp = Sys.time()
    )
  }

  # ICU capacity alerts
  if (!is.null(icu) && icu$status == "critical") {
    alerts[[length(alerts) + 1]] <- list(
      type = "icu_critical",
      severity = "critical",
      title = "ICU Capacity Critical",
      message = sprintf(
        "ICU capacity at %.1f%% utilization. Only %d ICU beds available.",
        icu$utilization_pct, icu$available_beds
      ),
      value = icu$utilization_pct,
      threshold = HEALTHCARE_CONFIG$capacity_thresholds$critical * 100,
      timestamp = Sys.time()
    )
  } else if (!is.null(icu) && icu$status == "warning") {
    alerts[[length(alerts) + 1]] <- list(
      type = "icu_warning",
      severity = "warning",
      title = "ICU Capacity Warning",
      message = sprintf(
        "ICU capacity at %.1f%% utilization. Monitor closely.",
        icu$utilization_pct
      ),
      value = icu$utilization_pct,
      threshold = HEALTHCARE_CONFIG$capacity_thresholds$warning * 100,
      timestamp = Sys.time()
    )
  }

  # Future breach alerts
  if (!is.null(capacity_result$hospital_breach) &&
      capacity_result$hospital_breach$breach_predicted) {
    days <- capacity_result$hospital_breach$days_until_breach
    if (days <= 14) {
      alerts[[length(alerts) + 1]] <- list(
        type = "hospital_surge_imminent",
        severity = if (days <= 7) "critical" else "warning",
        title = sprintf("Hospital Surge Expected in %d Days", round(days)),
        message = sprintf(
          "Projected hospital capacity breach on %s based on current trajectory.",
          format(capacity_result$hospital_breach$breach_date, "%B %d")
        ),
        value = days,
        timestamp = Sys.time()
      )
    }
  }

  if (!is.null(capacity_result$icu_breach) &&
      capacity_result$icu_breach$breach_predicted) {
    days <- capacity_result$icu_breach$days_until_breach
    if (days <= 14) {
      alerts[[length(alerts) + 1]] <- list(
        type = "icu_surge_imminent",
        severity = if (days <= 7) "critical" else "warning",
        title = sprintf("ICU Surge Expected in %d Days", round(days)),
        message = sprintf(
          "Projected ICU capacity breach on %s. Consider activating surge protocols.",
          format(capacity_result$icu_breach$breach_date, "%B %d")
        ),
        value = days,
        timestamp = Sys.time()
      )
    }
  }

  alerts
}

# =============================================================================
# CAPACITY TIMELINE
# =============================================================================

#' Get capacity timeline with projections
#' @param hosp_forecast Hospitalization forecast data
#' @param icu_forecast ICU forecast data
#' @param hospital_capacity Total hospital capacity
#' @param icu_capacity Total ICU capacity
#' @return Data frame with timeline
get_capacity_timeline <- function(hosp_forecast, icu_forecast,
                                   hospital_capacity, icu_capacity) {
  if (is.null(hosp_forecast)) {
    return(NULL)
  }

  timeline <- hosp_forecast |>
    select(date, estimated_occupancy, pathogen)

  if (!is.null(icu_forecast)) {
    timeline <- timeline |>
      left_join(
        icu_forecast |> select(date, estimated_icu_occupancy),
        by = "date"
      )
  }

  timeline <- timeline |>
    mutate(
      hospital_utilization = estimated_occupancy / hospital_capacity * 100,
      icu_utilization = if ("estimated_icu_occupancy" %in% names(.)) {
        estimated_icu_occupancy / icu_capacity * 100
      } else NA,
      hospital_available = hospital_capacity - estimated_occupancy,
      icu_available = if ("estimated_icu_occupancy" %in% names(.)) {
        icu_capacity - estimated_icu_occupancy
      } else NA
    )

  # Add status
  thresholds <- HEALTHCARE_CONFIG$capacity_thresholds
  timeline <- timeline |>
    mutate(
      hospital_status = case_when(
        hospital_utilization >= thresholds$critical * 100 ~ "critical",
        hospital_utilization >= thresholds$warning * 100 ~ "warning",
        hospital_utilization >= thresholds$elevated * 100 ~ "elevated",
        TRUE ~ "normal"
      ),
      icu_status = case_when(
        icu_utilization >= thresholds$critical * 100 ~ "critical",
        icu_utilization >= thresholds$warning * 100 ~ "warning",
        icu_utilization >= thresholds$elevated * 100 ~ "elevated",
        TRUE ~ "normal"
      )
    )

  timeline
}

# =============================================================================
# DATABASE FUNCTIONS - Get Real Capacity Data
# =============================================================================

#' Get current healthcare capacity from database
#' @param country_code ISO country code (default: "USA")
#' @return List with current capacity data or NULL if not available
get_current_capacity_from_db <- function(country_code = "USA") {
  conn <- tryCatch({
    get_db_connection()
  }, error = function(e) NULL)

  if (is.null(conn)) return(NULL)

 result <- tryCatch({
    # Get the most recent record with complete data (hospital + ICU)
    query <- sprintf("
      SELECT
        hc.observation_date,
        hc.total_hospital_beds,
        hc.occupied_hospital_beds,
        hc.hospital_occupancy_pct,
        hc.total_icu_beds,
        hc.occupied_icu_beds,
        hc.icu_occupancy_pct,
        hc.capacity_stress
      FROM healthcare_capacity hc
      JOIN countries c ON hc.country_id = c.country_id
      WHERE c.iso_code = '%s'
        AND hc.total_hospital_beds IS NOT NULL
      ORDER BY hc.observation_date DESC
      LIMIT 1
    ", country_code)

    data <- dbGetQuery(conn, query)

    if (nrow(data) == 0) {
      # Fallback: try to get at least ICU data
      query_icu <- sprintf("
        SELECT
          hc.observation_date,
          hc.total_hospital_beds,
          hc.occupied_hospital_beds,
          hc.hospital_occupancy_pct,
          hc.total_icu_beds,
          hc.occupied_icu_beds,
          hc.icu_occupancy_pct,
          hc.capacity_stress
        FROM healthcare_capacity hc
        JOIN countries c ON hc.country_id = c.country_id
        WHERE c.iso_code = '%s'
          AND hc.total_icu_beds IS NOT NULL
        ORDER BY hc.observation_date DESC
        LIMIT 1
      ", country_code)

      data <- dbGetQuery(conn, query_icu)
    }

    data
  }, error = function(e) {
    warning(sprintf("Error querying healthcare capacity: %s", e$message))
    data.frame()
  })

  close_db_connection(conn)

  if (nrow(result) == 0) return(NULL)

  # Return structured data
  list(
    observation_date = as.Date(result$observation_date),
    hospital = list(
      total_beds = result$total_hospital_beds,
      occupied_beds = result$occupied_hospital_beds,
      occupancy_pct = result$hospital_occupancy_pct,
      available_beds = if (!is.na(result$total_hospital_beds))
        result$total_hospital_beds - result$occupied_hospital_beds else NA
    ),
    icu = list(
      total_beds = result$total_icu_beds,
      occupied_beds = result$occupied_icu_beds,
      occupancy_pct = result$icu_occupancy_pct,
      available_beds = if (!is.na(result$total_icu_beds))
        result$total_icu_beds - result$occupied_icu_beds else NA
    ),
    capacity_stress = result$capacity_stress,
    data_age_days = as.numeric(Sys.Date() - as.Date(result$observation_date))
  )
}

# =============================================================================
# MAIN API FUNCTIONS
# =============================================================================

#' Get comprehensive healthcare capacity forecast
#' @param pathogen_code Pathogen identifier (or "all" for combined)
#' @param horizon Weeks ahead to forecast
#' @param hospital_capacity Total hospital beds (optional)
#' @param icu_capacity Total ICU beds (optional)
#' @return List with all capacity metrics and forecasts
get_capacity_forecast <- function(pathogen_code = "all", horizon = 4,
                                   hospital_capacity = NULL,
                                   icu_capacity = NULL) {
  # Source forecasting module if needed
  if (!exists("get_ensemble_forecast")) {
    source("R/ensemble_forecast.R")
  }

  # Set default capacities
  if (is.null(hospital_capacity)) {
    hospital_capacity <- HEALTHCARE_CONFIG$default_capacity$hospital_beds
  }
  if (is.null(icu_capacity)) {
    icu_capacity <- HEALTHCARE_CONFIG$default_capacity$icu_beds
  }

  # Get case forecasts
  if (pathogen_code == "all") {
    pathogens <- c("H3N2", "RSV", "COVID19")
  } else {
    pathogens <- pathogen_code
  }

  all_hosp_forecasts <- list()
  all_icu_forecasts <- list()

  for (p in pathogens) {
    # Get case forecast
    forecast_result <- tryCatch({
      get_ensemble_forecast(p, horizon = horizon)
    }, error = function(e) {
      list(status = "error", message = e$message)
    })

    if (!is.null(forecast_result) && forecast_result$status == "success") {
      # Use ensemble forecast
      case_forecast <- forecast_result$ensemble_forecast |>
        select(date, predicted_cases, lower_95, upper_95)

      # Forecast hospitalizations
      hosp <- forecast_hospitalizations(case_forecast, p)
      if (!is.null(hosp)) {
        all_hosp_forecasts[[p]] <- hosp
      }

      # Forecast ICU
      icu <- forecast_icu_demand(hosp, p)
      if (!is.null(icu)) {
        all_icu_forecasts[[p]] <- icu
      }
    }
  }

  # Combine forecasts if multiple pathogens
  if (length(all_hosp_forecasts) == 0) {
    return(list(
      status = "error",
      message = "No forecasts available for capacity calculation"
    ))
  }

  combined_hosp <- bind_rows(all_hosp_forecasts) |>
    group_by(date) |>
    summarise(
      total_admissions = sum(new_admissions, na.rm = TRUE),
      total_occupancy = sum(estimated_occupancy, na.rm = TRUE),
      pathogens_included = paste(unique(pathogen), collapse = ", "),
      .groups = "drop"
    )

  combined_icu <- bind_rows(all_icu_forecasts) |>
    group_by(date) |>
    summarise(
      total_icu_admissions = sum(new_icu_admissions, na.rm = TRUE),
      total_icu_occupancy = sum(estimated_icu_occupancy, na.rm = TRUE),
      .groups = "drop"
    )

  # Get REAL current capacity from database (not forecasted values)
  db_capacity <- get_current_capacity_from_db("USA")

  if (!is.null(db_capacity) && !is.na(db_capacity$hospital$total_beds)) {
    # Use real database values for current status
    hospital_capacity <- db_capacity$hospital$total_beds
    icu_capacity <- db_capacity$icu$total_beds

    hospital_status <- calculate_capacity_headroom(
      db_capacity$hospital$occupied_beds,
      db_capacity$hospital$total_beds
    )
    icu_status <- calculate_capacity_headroom(
      db_capacity$icu$occupied_beds,
      db_capacity$icu$total_beds
    )

    # Add data freshness info
    hospital_status$data_date <- db_capacity$observation_date
    hospital_status$data_age_days <- db_capacity$data_age_days
    icu_status$data_date <- db_capacity$observation_date
    icu_status$data_age_days <- db_capacity$data_age_days
  } else {
    # Fallback to forecast-based estimates if no database data
    current_hosp_occ <- tail(combined_hosp$total_occupancy, 1)[1]
    current_icu_occ <- tail(combined_icu$total_icu_occupancy, 1)[1]

    hospital_status <- calculate_capacity_headroom(current_hosp_occ, hospital_capacity)
    icu_status <- calculate_capacity_headroom(current_icu_occ, icu_capacity)

    # Mark as estimated
    hospital_status$data_source <- "forecast_estimate"
    icu_status$data_source <- "forecast_estimate"
  }

  # Project breaches
  hospital_breach <- project_capacity_breach(
    combined_hosp$total_occupancy,
    combined_hosp$date,
    hospital_capacity
  )

  icu_breach <- project_capacity_breach(
    combined_icu$total_icu_occupancy,
    combined_icu$date,
    icu_capacity
  )

  # Get timeline - blend actual database values with forecast projections
  timeline <- combined_hosp |>
    left_join(combined_icu, by = "date")

  # If we have actual database values, create a proper timeline
  if (!is.null(db_capacity) && !is.na(db_capacity$hospital$occupancy_pct)) {
    # Start with actual current utilization from database
    actual_hosp_util <- db_capacity$hospital$occupancy_pct
    actual_icu_util <- db_capacity$icu$occupancy_pct
    actual_date <- db_capacity$observation_date

    # Calculate the trend from forecast (change per day)
    if (nrow(timeline) >= 2) {
      forecast_hosp_util <- timeline$total_occupancy / hospital_capacity * 100
      forecast_icu_util <- timeline$total_icu_occupancy / icu_capacity * 100
      hosp_trend <- (tail(forecast_hosp_util, 1) - head(forecast_hosp_util, 1)) / nrow(timeline)
      icu_trend <- (tail(forecast_icu_util, 1) - head(forecast_icu_util, 1)) / nrow(timeline)
    } else {
      hosp_trend <- 0
      icu_trend <- 0
    }

    # Apply trend to actual baseline
    days_ahead <- as.numeric(timeline$date - actual_date)
    timeline <- timeline |>
      mutate(
        hospital_utilization = pmin(100, pmax(0, actual_hosp_util + hosp_trend * row_number())),
        icu_utilization = pmin(100, pmax(0, actual_icu_util + icu_trend * row_number())),
        is_forecast = TRUE
      )

    # Add current actual point as first row
    current_row <- data.frame(
      date = actual_date,
      total_admissions = NA_integer_,
      total_occupancy = db_capacity$hospital$occupied_beds,
      pathogens_included = "Actual",
      total_icu_admissions = NA_integer_,
      total_icu_occupancy = db_capacity$icu$occupied_beds,
      hospital_utilization = actual_hosp_util,
      icu_utilization = actual_icu_util,
      is_forecast = FALSE
    )
    timeline <- bind_rows(current_row, timeline) |>
      arrange(date)
  } else {
    # Fallback: just use forecast-derived utilization
    timeline <- timeline |>
      mutate(
        hospital_utilization = total_occupancy / hospital_capacity * 100,
        icu_utilization = total_icu_occupancy / icu_capacity * 100,
        is_forecast = TRUE
      )
  }

  result <- list(
    status = "success",
    pathogen = pathogen_code,
    horizon = horizon,
    # Capacity status (LIST with utilization, status, available_beds, etc)
    hospital_capacity = hospital_status,
    icu_capacity = icu_status,
    # Raw numeric capacity values for calculations
    hospital_beds = hospital_capacity,
    icu_beds = icu_capacity,
    hospital_breach = hospital_breach,
    icu_breach = icu_breach,
    hospitalization_forecast = combined_hosp,
    icu_forecast = combined_icu,
    timeline = timeline,
    by_pathogen = list(
      hospitalizations = all_hosp_forecasts,
      icu = all_icu_forecasts
    ),
    config = HEALTHCARE_CONFIG,
    generated = Sys.time()
  )

  # Generate alerts
  result$alerts <- generate_surge_alerts(result)

  result
}

#' Get capacity summary for dashboard display
#' @param capacity_result Result from get_capacity_forecast()
#' @return List with summary metrics for UI
get_capacity_summary <- function(capacity_result) {
  if (is.null(capacity_result) || capacity_result$status != "success") {
    return(list(
      hospital = list(
        utilization = 0,
        status = "unknown",
        available = 0,
        occupied = 0,
        capacity = 0
      ),
      icu = list(
        utilization = 0,
        status = "unknown",
        available = 0,
        occupied = 0,
        capacity = 0
      ),
      days_to_hospital_surge = NA,
      days_to_icu_surge = NA,
      alert_count = 0
    ))
  }

  list(
    hospital = list(
      utilization = capacity_result$hospital_capacity$utilization_pct,
      status = capacity_result$hospital_capacity$status,
      available = capacity_result$hospital_capacity$available_beds,
      occupied = capacity_result$hospital_capacity$current_occupancy,
      capacity = capacity_result$hospital_capacity$total_capacity
    ),
    icu = list(
      utilization = capacity_result$icu_capacity$utilization_pct,
      status = capacity_result$icu_capacity$status,
      available = capacity_result$icu_capacity$available_beds,
      occupied = capacity_result$icu_capacity$current_occupancy,
      capacity = capacity_result$icu_capacity$total_capacity
    ),
    days_to_hospital_surge = if (capacity_result$hospital_breach$breach_predicted) {
      capacity_result$hospital_breach$days_until_breach
    } else NA,
    days_to_icu_surge = if (capacity_result$icu_breach$breach_predicted) {
      capacity_result$icu_breach$days_until_breach
    } else NA,
    alert_count = length(capacity_result$alerts),
    peak_hospital_utilization = capacity_result$hospital_breach$peak_utilization * 100,
    peak_icu_utilization = capacity_result$icu_breach$peak_utilization * 100
  )
}

# =============================================================================
# VISUALIZATION HELPERS
# =============================================================================

#' Prepare capacity data for plotting
#' @param capacity_result Result from get_capacity_forecast()
#' @return Data frame ready for ggplot/plotly
prepare_capacity_plot_data <- function(capacity_result) {
  if (is.null(capacity_result) || capacity_result$status != "success") {
    return(NULL)
  }

  # Combine timeline data for plotting
  timeline <- capacity_result$timeline |>
    pivot_longer(
      cols = c(hospital_utilization, icu_utilization),
      names_to = "capacity_type",
      values_to = "utilization"
    ) |>
    mutate(
      capacity_type = case_when(
        capacity_type == "hospital_utilization" ~ "Hospital",
        capacity_type == "icu_utilization" ~ "ICU",
        TRUE ~ capacity_type
      )
    )

  timeline
}

#' Get capacity gauge data for display
#' @param capacity_result Result from get_capacity_forecast()
#' @return List with gauge configuration data
get_capacity_gauges <- function(capacity_result) {
  if (is.null(capacity_result) || capacity_result$status != "success") {
    return(list(
      hospital = list(value = 0, color = "#10B981", label = "N/A"),
      icu = list(value = 0, color = "#10B981", label = "N/A")
    ))
  }

  # Color based on status
  get_color <- function(status) {
    switch(status,
           "critical" = "#DC2626",
           "warning" = "#F59E0B",
           "elevated" = "#3B82F6",
           "#10B981")
  }

  list(
    hospital = list(
      value = capacity_result$hospital_capacity$utilization_pct,
      color = get_color(capacity_result$hospital_capacity$status),
      label = sprintf("%.0f%%", capacity_result$hospital_capacity$utilization_pct),
      status = capacity_result$hospital_capacity$status,
      available = capacity_result$hospital_capacity$available_beds
    ),
    icu = list(
      value = capacity_result$icu_capacity$utilization_pct,
      color = get_color(capacity_result$icu_capacity$status),
      label = sprintf("%.0f%%", capacity_result$icu_capacity$utilization_pct),
      status = capacity_result$icu_capacity$status,
      available = capacity_result$icu_capacity$available_beds
    )
  )
}

#' Format alerts for display
#' @param alerts List of alerts from generate_surge_alerts()
#' @return HTML formatted alert content
format_capacity_alerts_html <- function(alerts) {
  if (is.null(alerts) || length(alerts) == 0) {
    return('<div class="alert alert-success">
      <i class="fas fa-check-circle"></i>
      <strong>All Clear</strong> - Healthcare capacity within normal limits
    </div>')
  }

  html_alerts <- sapply(alerts, function(a) {
    badge_class <- switch(a$severity,
                          "critical" = "danger",
                          "warning" = "warning",
                          "info")
    icon <- switch(a$severity,
                   "critical" = "exclamation-triangle",
                   "warning" = "exclamation-circle",
                   "info-circle")

    sprintf(
      '<div class="alert alert-%s mb-2">
        <i class="fas fa-%s"></i>
        <strong>%s</strong>
        <br><small>%s</small>
      </div>',
      badge_class, icon, a$title, a$message
    )
  })

  paste(html_alerts, collapse = "\n")
}
