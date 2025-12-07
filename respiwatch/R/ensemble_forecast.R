# ============================================================================
# Title: RespiWatch Ensemble Forecasting Module
# Purpose: Combine Rt-renewal and Bayesian forecasts for robust predictions
# Input: Results from forecasting.R and bayesian_forecast.R
# Output: Ensemble forecasts with uncertainty quantification
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(tidyr)

# =============================================================================
# ENSEMBLE CONFIGURATION
# =============================================================================

#' Default ensemble configuration
ENSEMBLE_CONFIG <- list(
  methods = c("rt_renewal", "bayesian"),
  weights = c(rt_renewal = 0.4, bayesian = 0.6),  # Bayesian weighted higher

  combination_method = "weighted_average",  # or "median", "bma"
  uncertainty_method = "pooled"  # or "wider", "intersection"
)

# =============================================================================
# ENSEMBLE COMBINATION METHODS
# =============================================================================

#' Combine forecasts using weighted average
#' @param forecasts List of forecast data frames
#' @param weights Named vector of weights for each method
#' @return Data frame with combined forecast
weighted_average_ensemble <- function(forecasts, weights = NULL) {
  if (length(forecasts) == 0) {
    return(NULL)
  }

  # Filter out NULL forecasts
  valid_forecasts <- forecasts[!sapply(forecasts, is.null)]
  if (length(valid_forecasts) == 0) {
    return(NULL)
  }

  # Default equal weights if not provided
  if (is.null(weights)) {
    weights <- rep(1 / length(valid_forecasts), length(valid_forecasts))
    names(weights) <- names(valid_forecasts)
  }

  # Normalize weights for available methods
  available_methods <- names(valid_forecasts)
  method_weights <- weights[available_methods]
  method_weights <- method_weights / sum(method_weights, na.rm = TRUE)

  # Get common dates across all forecasts
  all_dates <- Reduce(intersect,
                      lapply(valid_forecasts, function(f) as.character(f$date)))

  if (length(all_dates) == 0) {
    warning("No common dates across forecasts")
    return(NULL)
  }

  # Combine forecasts
  combined <- data.frame(
    date = as.Date(all_dates),
    predicted_cases = 0,
    lower_50 = 0,
    upper_50 = 0,
    lower_80 = 0,
    upper_80 = 0,
    lower_95 = 0,
    upper_95 = 0
  )

  for (method in names(valid_forecasts)) {
    fc <- valid_forecasts[[method]]
    fc <- fc[as.character(fc$date) %in% all_dates, ]
    w <- method_weights[method]

    combined$predicted_cases <- combined$predicted_cases +
      w * fc$predicted_cases

    # Weighted combination of intervals
    if ("lower_50" %in% names(fc)) {
      combined$lower_50 <- combined$lower_50 + w * fc$lower_50
      combined$upper_50 <- combined$upper_50 + w * fc$upper_50
    }
    if ("lower_80" %in% names(fc)) {
      combined$lower_80 <- combined$lower_80 + w * fc$lower_80
      combined$upper_80 <- combined$upper_80 + w * fc$upper_80
    }
    if ("lower_95" %in% names(fc)) {
      combined$lower_95 <- combined$lower_95 + w * fc$lower_95
      combined$upper_95 <- combined$upper_95 + w * fc$upper_95
    }
  }

  combined$predicted_cases <- round(combined$predicted_cases)
  combined$lower_50 <- round(combined$lower_50)
  combined$upper_50 <- round(combined$upper_50)
  combined$lower_80 <- round(combined$lower_80)
  combined$upper_80 <- round(combined$upper_80)
  combined$lower_95 <- round(combined$lower_95)
  combined$upper_95 <- round(combined$upper_95)

  combined
}

#' Combine forecasts using median
#' @param forecasts List of forecast data frames
#' @return Data frame with median ensemble
median_ensemble <- function(forecasts) {
  valid_forecasts <- forecasts[!sapply(forecasts, is.null)]
  if (length(valid_forecasts) < 2) {
    return(if (length(valid_forecasts) == 1) valid_forecasts[[1]] else NULL)
  }

  # Get common dates
  all_dates <- Reduce(intersect,
                      lapply(valid_forecasts, function(f) as.character(f$date)))

  if (length(all_dates) == 0) {
    return(NULL)
  }

  # Calculate median for each date
  combined <- lapply(all_dates, function(d) {
    values <- sapply(valid_forecasts, function(f) {
      f$predicted_cases[as.character(f$date) == d]
    })

    lower_95 <- sapply(valid_forecasts, function(f) {
      if ("lower_95" %in% names(f)) {
        f$lower_95[as.character(f$date) == d]
      } else NA
    })

    upper_95 <- sapply(valid_forecasts, function(f) {
      if ("upper_95" %in% names(f)) {
        f$upper_95[as.character(f$date) == d]
      } else NA
    })

    data.frame(
      date = as.Date(d),
      predicted_cases = round(median(values, na.rm = TRUE)),
      lower_95 = round(min(lower_95, na.rm = TRUE)),
      upper_95 = round(max(upper_95, na.rm = TRUE))
    )
  })

  do.call(rbind, combined)
}

#' Combine uncertainty intervals (pooled method)
#' Takes the outer bounds of all methods for conservative uncertainty
#' @param forecasts List of forecast data frames
#' @return Data frame with pooled uncertainty
pooled_uncertainty_ensemble <- function(forecasts) {
  valid_forecasts <- forecasts[!sapply(forecasts, is.null)]
  if (length(valid_forecasts) == 0) {
    return(NULL)
  }

  # Get common dates
  all_dates <- Reduce(intersect,
                      lapply(valid_forecasts, function(f) as.character(f$date)))

  if (length(all_dates) == 0) {
    return(NULL)
  }

  combined <- lapply(all_dates, function(d) {
    # Get all predictions for this date
    predictions <- sapply(valid_forecasts, function(f) {
      f$predicted_cases[as.character(f$date) == d]
    })

    # Take mean of predictions
    mean_pred <- round(mean(predictions, na.rm = TRUE))

    # Take widest bounds for each interval level
    get_bounds <- function(lower_col, upper_col) {
      lowers <- sapply(valid_forecasts, function(f) {
        if (lower_col %in% names(f)) {
          f[[lower_col]][as.character(f$date) == d]
        } else NA
      })
      uppers <- sapply(valid_forecasts, function(f) {
        if (upper_col %in% names(f)) {
          f[[upper_col]][as.character(f$date) == d]
        } else NA
      })
      c(min(lowers, na.rm = TRUE), max(uppers, na.rm = TRUE))
    }

    bounds_50 <- get_bounds("lower_50", "upper_50")
    bounds_80 <- get_bounds("lower_80", "upper_80")
    bounds_95 <- get_bounds("lower_95", "upper_95")

    data.frame(
      date = as.Date(d),
      predicted_cases = mean_pred,
      lower_50 = round(bounds_50[1]),
      upper_50 = round(bounds_50[2]),
      lower_80 = round(bounds_80[1]),
      upper_80 = round(bounds_80[2]),
      lower_95 = round(bounds_95[1]),
      upper_95 = round(bounds_95[2])
    )
  })

  do.call(rbind, combined)
}

# =============================================================================
# MAIN ENSEMBLE FUNCTIONS
# =============================================================================

#' Generate ensemble forecast for a pathogen
#' @param pathogen_code Pathogen identifier
#' @param country Optional country filter
#' @param horizon Weeks ahead to forecast
#' @param config Ensemble configuration list
#' @return List with ensemble forecast and component forecasts
get_ensemble_forecast <- function(pathogen_code, country = NULL, horizon = 4,
                                   config = ENSEMBLE_CONFIG) {
  # Source required modules if needed
  if (!exists("get_forecast_for_pathogen")) {
    source("R/forecasting.R")
  }
  if (!exists("get_bayesian_forecast")) {
    source("R/bayesian_forecast.R")
  }

  component_forecasts <- list()
  errors <- list()

  # Get Rt-renewal forecast
  if ("rt_renewal" %in% config$methods) {
    tryCatch({
      rt_result <- get_forecast_for_pathogen(pathogen_code,
                                              country = country,
                                              horizon = horizon)
      if (!is.null(rt_result$forecast)) {
        component_forecasts$rt_renewal <- rt_result$forecast
      }
    }, error = function(e) {
      errors$rt_renewal <- e$message
    })
  }

  # Get Bayesian forecast
  if ("bayesian" %in% config$methods) {
    tryCatch({
      bayes_result <- get_bayesian_forecast(pathogen_code,
                                            country = country,
                                            horizon = horizon)
      if (!is.null(bayes_result$forecast)) {
        component_forecasts$bayesian <- bayes_result$forecast
      }
    }, error = function(e) {
      errors$bayesian <- e$message
    })
  }

  # Check if we have any valid forecasts
  if (length(component_forecasts) == 0) {
    return(list(
      status = "failed",
      message = "No component forecasts available",
      errors = errors
    ))
  }

  # Combine forecasts based on configuration
  ensemble_forecast <- switch(
    config$combination_method,
    "weighted_average" = weighted_average_ensemble(component_forecasts,
                                                    config$weights),
    "median" = median_ensemble(component_forecasts),
    "pooled" = pooled_uncertainty_ensemble(component_forecasts),
    weighted_average_ensemble(component_forecasts, config$weights)
  )

  if (is.null(ensemble_forecast)) {
    return(list(
      status = "failed",
      message = "Could not combine forecasts",
      component_forecasts = component_forecasts
    ))
  }

  # Add metadata
  ensemble_forecast$pathogen <- pathogen_code
  ensemble_forecast$method <- "ensemble"
  ensemble_forecast$forecast_date <- Sys.Date()

  list(
    status = "success",
    pathogen = pathogen_code,
    country = country,
    ensemble_forecast = ensemble_forecast,
    component_forecasts = component_forecasts,
    config = config,
    n_methods_used = length(component_forecasts),
    methods_used = names(component_forecasts),
    errors = if (length(errors) > 0) errors else NULL,
    generated = Sys.time()
  )
}

#' Get ensemble forecasts for all pathogens
#' @param horizon Weeks ahead
#' @param config Ensemble configuration
#' @return List of ensemble results by pathogen
get_ensemble_all_pathogens <- function(horizon = 4, config = ENSEMBLE_CONFIG) {
  pathogens <- c("H3N2", "RSV", "COVID19")

  results <- lapply(pathogens, function(p) {
    tryCatch({
      get_ensemble_forecast(p, horizon = horizon, config = config)
    }, error = function(e) {
      list(
        status = "error",
        pathogen = p,
        message = e$message
      )
    })
  })

  names(results) <- pathogens
  results
}

# =============================================================================
# FORECAST EVALUATION
# =============================================================================

#' Calculate forecast skill scores
#' @param observed Vector of observed values
#' @param predicted Vector of predicted values
#' @param lower Lower bound of prediction interval
#' @param upper Upper bound of prediction interval
#' @return List with skill metrics
calculate_skill_scores <- function(observed, predicted, lower = NULL,
                                   upper = NULL) {
  # Remove NA values
  valid_idx <- !is.na(observed) & !is.na(predicted)
  obs <- observed[valid_idx]
  pred <- predicted[valid_idx]

  if (length(obs) < 2) {
    return(list(
      mae = NA,
      rmse = NA,
      mape = NA,
      coverage = NA
    ))
  }

  # Mean Absolute Error
  mae <- mean(abs(obs - pred))

  # Root Mean Square Error
  rmse <- sqrt(mean((obs - pred)^2))

  # Mean Absolute Percentage Error
  mape <- mean(abs((obs - pred) / pmax(obs, 1)) * 100)

  # Coverage (if intervals provided)
  coverage <- NA
  if (!is.null(lower) && !is.null(upper)) {
    lower_valid <- lower[valid_idx]
    upper_valid <- upper[valid_idx]
    in_interval <- obs >= lower_valid & obs <= upper_valid
    coverage <- mean(in_interval) * 100
  }

  list(
    mae = round(mae, 2),
    rmse = round(rmse, 2),
    mape = round(mape, 2),
    coverage = round(coverage, 1),
    n_obs = length(obs)
  )
}

#' Compare ensemble to component forecasts
#' @param ensemble_result Result from get_ensemble_forecast()
#' @param observed_data Data frame with observed values
#' @return Data frame with comparison metrics
compare_forecast_methods <- function(ensemble_result, observed_data) {
  if (is.null(ensemble_result) || ensemble_result$status != "success") {
    return(NULL)
  }

  if (is.null(observed_data) || nrow(observed_data) == 0) {
    return(NULL)
  }

  # Prepare observed data
  obs <- observed_data |>
    select(date = dates, observed = I)

  # Evaluate ensemble
  ensemble <- ensemble_result$ensemble_forecast |>
    left_join(obs, by = "date")

  ensemble_scores <- calculate_skill_scores(
    ensemble$observed,
    ensemble$predicted_cases,
    ensemble$lower_95,
    ensemble$upper_95
  )

  results <- data.frame(
    Method = "Ensemble",
    MAE = ensemble_scores$mae,
    RMSE = ensemble_scores$rmse,
    MAPE = ensemble_scores$mape,
    Coverage_95 = ensemble_scores$coverage,
    stringsAsFactors = FALSE
  )

  # Evaluate each component
  for (method_name in names(ensemble_result$component_forecasts)) {
    fc <- ensemble_result$component_forecasts[[method_name]] |>
      left_join(obs, by = "date")

    scores <- calculate_skill_scores(
      fc$observed,
      fc$predicted_cases,
      if ("lower_95" %in% names(fc)) fc$lower_95 else NULL,
      if ("upper_95" %in% names(fc)) fc$upper_95 else NULL
    )

    results <- rbind(results, data.frame(
      Method = method_name,
      MAE = scores$mae,
      RMSE = scores$rmse,
      MAPE = scores$mape,
      Coverage_95 = scores$coverage,
      stringsAsFactors = FALSE
    ))
  }

  results
}

# =============================================================================
# VISUALIZATION HELPERS
# =============================================================================

#' Prepare ensemble forecast for plotting
#' @param ensemble_result Result from get_ensemble_forecast()
#' @param historical_data Historical incidence data
#' @return Data frame ready for ggplot/plotly
prepare_ensemble_plot_data <- function(ensemble_result, historical_data = NULL) {
  if (is.null(ensemble_result) || ensemble_result$status != "success") {
    return(NULL)
  }

  # Ensemble forecast
  ensemble <- ensemble_result$ensemble_forecast |>
    mutate(type = "ensemble", source = "Ensemble")

  # Component forecasts
  components <- lapply(names(ensemble_result$component_forecasts), function(m) {
    fc <- ensemble_result$component_forecasts[[m]]
    fc$type <- "component"
    fc$source <- m
    fc
  })

  forecast_data <- bind_rows(ensemble, bind_rows(components))

  # Add historical if provided
  if (!is.null(historical_data)) {
    historical <- historical_data |>
      rename(date = dates, predicted_cases = I) |>
      mutate(type = "observed", source = "Observed")

    forecast_data <- bind_rows(historical, forecast_data)
  }

  forecast_data |>
    arrange(date, source)
}

#' Summary table for ensemble forecasts
#' @param ensemble_results List from get_ensemble_all_pathogens()
#' @return Data frame formatted for display
summarize_ensemble_table <- function(ensemble_results) {
  summaries <- lapply(names(ensemble_results), function(p) {
    result <- ensemble_results[[p]]

    if (is.null(result) || result$status != "success") {
      return(data.frame(
        Pathogen = p,
        `Methods Used` = "N/A",
        `1-Week Forecast` = "N/A",
        `4-Week Forecast` = "N/A",
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    fc <- result$ensemble_forecast

    week1 <- fc[1, ]
    week4 <- if (nrow(fc) >= 4) fc[4, ] else NULL

    data.frame(
      Pathogen = p,
      `Methods Used` = paste(result$methods_used, collapse = ", "),
      `1-Week Forecast` = sprintf("%d (%d-%d)",
                                   week1$predicted_cases,
                                   week1$lower_80,
                                   week1$upper_80),
      `4-Week Forecast` = if (!is.null(week4)) {
        sprintf("%d (%d-%d)",
                week4$predicted_cases,
                week4$lower_80,
                week4$upper_80)
      } else "N/A",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  do.call(rbind, summaries)
}
