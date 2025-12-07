# ============================================================================
# Title: RespiWatch Forecasting Module
# Purpose: Generate epidemic curve forecasts using Rt-based projections
# Input: Surveillance data and Rt estimates
# Output: Case forecasts with prediction intervals
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)

# Source Rt estimation module
source("R/rt_estimation.R")

# =============================================================================
# FORECAST CONFIGURATION
# =============================================================================

#' Default forecast parameters
FORECAST_CONFIG <- list(
  horizon_weeks = 4,           # Weeks ahead to forecast
  confidence_levels = c(0.50, 0.80, 0.95),  # Prediction interval levels
  min_history_weeks = 8,       # Minimum historical data required
  decay_factor = 0.1           # Rt regression to 1.0 over forecast horizon
)

# =============================================================================
# BASIC FORECASTING FUNCTIONS
# =============================================================================

#' Project future cases using current Rt and renewal equation
#' @param incidence_data Data frame with dates and I columns
#' @param rt_current Current reproduction number
#' @param si_params Serial interval parameters (mean, sd)
#' @param horizon Number of time steps to forecast
#' @return Data frame with projected cases
project_cases <- function(incidence_data, rt_current, si_params, horizon = 4) {
  if (is.null(incidence_data) || nrow(incidence_data) < 3) {
    return(NULL)
  }

  # Get last observed data
  last_date <- max(incidence_data$dates)
  last_cases <- tail(incidence_data$I, 4)  # Use last 4 time points

  # Create serial interval distribution weights
  # Discretized gamma distribution
  si_shape <- (si_params$mean / si_params$sd)^2
  si_rate <- si_params$mean / si_params$sd^2
  si_weights <- dgamma(1:length(last_cases), shape = si_shape, rate = si_rate)
  si_weights <- si_weights / sum(si_weights)

  # Project forward using renewal equation
  projected <- data.frame(
    date = seq(last_date + 7, by = 7, length.out = horizon),
    forecast_week = 1:horizon
  )

  # Calculate expected cases for each forecast week
  cases_history <- c(last_cases)
  projected_cases <- numeric(horizon)

  for (h in 1:horizon) {
    # Apply Rt decay toward 1.0 for longer horizons (uncertainty)
    rt_h <- 1 + (rt_current - 1) * exp(-FORECAST_CONFIG$decay_factor * (h - 1))

    # Renewal equation: I_t = Rt * sum(w_s * I_{t-s})
    weighted_sum <- sum(tail(cases_history, length(si_weights)) * rev(si_weights))
    expected_cases <- rt_h * weighted_sum

    projected_cases[h] <- max(0, round(expected_cases))
    cases_history <- c(cases_history, projected_cases[h])
  }

  projected$predicted_cases <- projected_cases
  projected$rt_used <- sapply(1:horizon, function(h) {
    1 + (rt_current - 1) * exp(-FORECAST_CONFIG$decay_factor * (h - 1))
  })

  projected
}

#' Calculate prediction intervals for forecast
#' @param projected Data frame from project_cases()
#' @param levels Confidence levels for intervals
#' @param n_sim Number of simulations for uncertainty
#' @return Data frame with prediction intervals
calculate_prediction_intervals <- function(projected, levels = c(0.50, 0.80, 0.95), n_sim = 1000) {
  if (is.null(projected) || nrow(projected) == 0) {
    return(NULL)
  }

  result_list <- lapply(1:nrow(projected), function(i) {
    mean_cases <- projected$predicted_cases[i]
    forecast_week <- projected$forecast_week[i]

    # Uncertainty grows with forecast horizon
    # Use negative binomial for overdispersed count data
    overdispersion <- 1 + 0.5 * forecast_week  # Increases with horizon

    # Simulate from negative binomial
    if (mean_cases > 0) {
      size <- mean_cases / overdispersion
      size <- max(size, 0.1)  # Ensure size > 0
      sims <- rnbinom(n_sim, size = size, mu = mean_cases)
    } else {
      sims <- rep(0, n_sim)
    }

    # Calculate quantiles for each level
    interval_df <- data.frame(
      date = projected$date[i],
      forecast_week = forecast_week,
      predicted_cases = mean_cases,
      rt_used = projected$rt_used[i]
    )

    for (level in levels) {
      lower_q <- (1 - level) / 2
      upper_q <- 1 - lower_q
      interval_df[[paste0("lower_", level * 100)]] <- quantile(sims, lower_q)
      interval_df[[paste0("upper_", level * 100)]] <- quantile(sims, upper_q)
    }

    interval_df
  })

  do.call(rbind, result_list)
}

#' Full forecast pipeline for a pathogen
#' @param historical_data Incidence data frame
#' @param rt_result Result from get_rt_for_pathogen()
#' @param horizon Weeks to forecast
#' @return Data frame with forecast and intervals
forecast_cases <- function(historical_data, rt_result, horizon = FORECAST_CONFIG$horizon_weeks) {
  if (is.null(rt_result) || is.null(rt_result$current_rt)) {
    return(NULL)
  }

  rt_current <- rt_result$current_rt$value
  if (is.na(rt_current)) {
    warning("No current Rt available for forecasting")
    return(NULL)
  }

  si_params <- rt_result$serial_interval

  # Project cases
  projected <- project_cases(
    incidence_data = historical_data,
    rt_current = rt_current,
    si_params = si_params,
    horizon = horizon
  )

  if (is.null(projected)) {
    return(NULL)
  }

  # Add prediction intervals
  forecast_df <- calculate_prediction_intervals(
    projected,
    levels = FORECAST_CONFIG$confidence_levels
  )

  # Add metadata
  forecast_df$pathogen <- rt_result$pathogen
  forecast_df$forecast_date <- Sys.Date()
  forecast_df$method <- "Rt-renewal"

  forecast_df
}

# =============================================================================
# MAIN API FUNCTIONS
# =============================================================================

#' Get forecast for a specific pathogen
#' @param pathogen_code Pathogen identifier
#' @param country Optional country filter
#' @param horizon Weeks ahead
#' @return List with forecast data and metadata
get_forecast_for_pathogen <- function(pathogen_code, country = NULL, horizon = 4) {
  # Get Rt estimates (also loads incidence data)
  rt_result <- get_rt_for_pathogen(pathogen_code, country = country)

  if (is.null(rt_result$rt_estimates)) {
    return(list(
      pathogen = pathogen_code,
      country = country,
      forecast = NULL,
      error = "Insufficient data for Rt estimation"
    ))
  }

  # Prepare incidence data
  source("R/db_schema.R")
  source("R/db_operations.R")

  conn <- get_db_connection()
  query <- sprintf("
    SELECT
      sd.observation_date,
      sd.case_count,
      sd.estimated_cases
    FROM surveillance_data sd
    JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
    WHERE p.pathogen_code = '%s'
    ORDER BY sd.observation_date
  ", pathogen_code)

  surv_data <- DBI::dbGetQuery(conn, query)
  close_db_connection(conn)

  incidence_data <- prepare_incidence_data(surv_data)

  # Generate forecast
  forecast_df <- forecast_cases(incidence_data, rt_result, horizon = horizon)

  list(
    pathogen = pathogen_code,
    country = country,
    current_rt = rt_result$current_rt,
    forecast = forecast_df,
    historical_cases = incidence_data,
    forecast_generated = Sys.time()
  )
}

#' Get forecasts for all pathogens
#' @param horizon Weeks ahead
#' @return List of forecast results by pathogen
get_forecasts_all_pathogens <- function(horizon = 4) {
  pathogens <- c("H3N2", "RSV", "COVID19")

  results <- lapply(pathogens, function(p) {
    tryCatch({
      get_forecast_for_pathogen(p, horizon = horizon)
    }, error = function(e) {
      list(
        pathogen = p,
        forecast = NULL,
        error = e$message
      )
    })
  })

  names(results) <- pathogens
  results
}

# =============================================================================
# VISUALIZATION HELPERS
# =============================================================================

#' Prepare forecast data for plotting with historical data
#' @param forecast_result Result from get_forecast_for_pathogen()
#' @return Data frame ready for ggplot/plotly
prepare_forecast_plot_data <- function(forecast_result) {
  if (is.null(forecast_result$forecast)) {
    return(NULL)
  }

  # Historical data
  historical <- forecast_result$historical_cases |>
    rename(date = dates, cases = I) |>
    mutate(
      type = "observed",
      pathogen = forecast_result$pathogen
    )

  # Forecast data
  forecast <- forecast_result$forecast |>
    select(date, predicted_cases, lower_50, upper_50, lower_80, upper_80, lower_95, upper_95) |>
    rename(cases = predicted_cases) |>
    mutate(
      type = "forecast",
      pathogen = forecast_result$pathogen
    )

  # Combine
  bind_rows(historical, forecast) |>
    arrange(date)
}

#' Summarize forecast for table display
#' @param forecast_results List from get_forecasts_all_pathogens()
#' @return Data frame with forecast summaries
summarize_forecast_table <- function(forecast_results) {
  summaries <- lapply(names(forecast_results), function(p) {
    result <- forecast_results[[p]]

    if (is.null(result$forecast)) {
      return(data.frame(
        Pathogen = p,
        `Current Rt` = "N/A",
        `1-Week Forecast` = "N/A",
        `4-Week Forecast` = "N/A",
        stringsAsFactors = FALSE
      ))
    }

    current_rt <- result$current_rt
    forecast <- result$forecast

    week1 <- forecast |> filter(forecast_week == 1)
    week4 <- forecast |> filter(forecast_week == 4)

    data.frame(
      Pathogen = p,
      `Current Rt` = sprintf("%.2f", current_rt$value),
      `1-Week Forecast` = sprintf("%d (%d-%d)",
                                   week1$predicted_cases,
                                   week1$lower_80,
                                   week1$upper_80),
      `4-Week Forecast` = if (nrow(week4) > 0) {
        sprintf("%d (%d-%d)",
                week4$predicted_cases,
                week4$lower_80,
                week4$upper_80)
      } else "N/A",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summaries)
}

# =============================================================================
# NOWCASTING (Adjusting for Reporting Delays)
# =============================================================================

#' Estimate reporting delay distribution
#' @param historical_data Data with reporting timestamps
#' @return List with delay distribution parameters
estimate_reporting_delay <- function(historical_data) {
  # Default delay distribution (lognormal)
  # Based on typical respiratory illness reporting patterns
  list(
    distribution = "lognormal",
    meanlog = 1.5,    # ~4.5 days median delay
    sdlog = 0.8,
    max_delay = 14    # Maximum delay in days
  )
}

#' Nowcast: Adjust recent counts for incomplete reporting
#' @param incidence_data Recent incidence data
#' @param delay_params Delay distribution parameters
#' @return Data frame with nowcasted case counts
nowcast_cases <- function(incidence_data, delay_params = NULL) {
  if (is.null(delay_params)) {
    delay_params <- estimate_reporting_delay(NULL)
  }

  if (is.null(incidence_data) || nrow(incidence_data) == 0) {
    return(NULL)
  }

  # Calculate adjustment factors based on days since observation
  today <- Sys.Date()
  incidence_data <- incidence_data |>
    mutate(
      days_ago = as.numeric(today - dates),
      # Proportion of cases reported by now (CDF of delay distribution)
      prop_reported = plnorm(days_ago,
                             meanlog = delay_params$meanlog,
                             sdlog = delay_params$sdlog),
      # Adjustment factor (inverse of proportion reported)
      adjustment_factor = ifelse(prop_reported > 0.1, 1 / prop_reported, 1),
      # Cap adjustment factor to avoid extreme corrections
      adjustment_factor = pmin(adjustment_factor, 5),
      # Nowcasted cases
      nowcast_cases = round(I * adjustment_factor),
      nowcast_lower = round(I * adjustment_factor * 0.8),
      nowcast_upper = round(I * adjustment_factor * 1.2)
    )

  incidence_data
}
