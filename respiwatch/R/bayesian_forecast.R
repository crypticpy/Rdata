# ============================================================================
# Title: RespiWatch Bayesian Forecasting Module
# Purpose: Hierarchical Bayesian forecasting using brms for respiratory pathogens
# Input: Surveillance data from SQLite database
# Output: Probabilistic forecasts with full posterior distributions
# ============================================================================

# Load required packages -------------------------------------------------------
library(brms)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Source diagnostic module
source("R/model_diagnostics.R")

# =============================================================================
# INPUT VALIDATION HELPERS
# =============================================================================

#' Escape string for SQL query (prevent SQL injection)
escape_sql <- function(x) {
  if (is.null(x) || is.na(x)) return(NA)
  gsub("'", "''", as.character(x))
}

#' Validate pathogen code format (alphanumeric)
is_valid_pathogen_code <- function(code) {
  !is.null(code) && grepl("^[A-Za-z0-9_]+$", code)
}

#' Validate ISO country code format (2-letter)
is_valid_iso_code <- function(code) {
  !is.null(code) && grepl("^[A-Z]{2}$", toupper(code))
}

# =============================================================================
# MODEL CONFIGURATION
# =============================================================================

#' Default Bayesian model parameters
BAYES_CONFIG <- list(
  # Model fitting parameters
  chains = 2,
  iter = 2000,
  warmup = 1000,
  thin = 1,
  cores = 2,

  # Forecast parameters
  horizon_weeks = 4,
  credible_intervals = c(0.50, 0.80, 0.95),

  # Model caching
  cache_dir = "data/cache/models",
  model_expiry_hours = 24
)

# =============================================================================
# DATA PREPARATION
# =============================================================================

#' Prepare surveillance data for brms modeling
#' @param surv_data Raw surveillance data from database
#' @return Data frame ready for brms model fitting
prepare_model_data <- function(surv_data) {
  if (is.null(surv_data) || nrow(surv_data) == 0) {
    return(NULL)
  }

  model_data <- surv_data |>
    mutate(
      date = as.Date(observation_date),
      week = isoweek(date),
      year = isoyear(date),
      time = as.numeric(date - min(date)) / 7,  # Time in weeks
      # Seasonal component (cosine/sine for annual cycle)
      season_sin = sin(2 * pi * week / 52),
      season_cos = cos(2 * pi * week / 52),
      # Handle case counts
      cases = coalesce(case_count, estimated_cases, 0),
      cases = as.integer(pmax(0, cases))
    ) |>
    filter(!is.na(cases)) |>
    arrange(date)

  # Add pathogen as factor
  if ("pathogen_code" %in% names(model_data)) {
    model_data$pathogen <- factor(model_data$pathogen_code)
  }

  # Add country as factor
  if ("iso_code" %in% names(model_data)) {
    model_data$country <- factor(model_data$iso_code)
  }

  model_data
}

#' Generate future dates for prediction
#' @param model_data Historical data
#' @param horizon Weeks to forecast
#' @return Data frame with future dates and predictors
create_newdata <- function(model_data, horizon = BAYES_CONFIG$horizon_weeks) {
  last_date <- max(model_data$date)
  last_time <- max(model_data$time)

  future_dates <- seq(last_date + 7, by = 7, length.out = horizon)

  # Create newdata with all required predictors
  newdata <- data.frame(
    date = future_dates,
    time = last_time + 1:horizon,
    week = isoweek(future_dates),
    year = isoyear(future_dates),
    season_sin = sin(2 * pi * isoweek(future_dates) / 52),
    season_cos = cos(2 * pi * isoweek(future_dates) / 52),
    forecast_week = 1:horizon
  )

  # Add pathogen/country if in model data
  if ("pathogen" %in% names(model_data)) {
    # For single pathogen forecast, use the most recent pathogen
    newdata$pathogen <- model_data$pathogen[1]
  }

  if ("country" %in% names(model_data)) {
    newdata$country <- model_data$country[1]
  }

  newdata
}

# =============================================================================
# MODEL SPECIFICATION
# =============================================================================

#' Define priors for hierarchical model
#' @return brms prior specification
get_model_priors <- function() {
  c(
    prior(normal(0, 10), class = "Intercept"),
    prior(normal(0, 2), class = "b"),
    prior(student_t(3, 0, 2.5), class = "sd"),
    prior(gamma(0.01, 0.01), class = "shape")
  )
}

#' Fit simple negative binomial model for single pathogen
#' @param model_data Prepared data from prepare_model_data()
#' @param refresh Progress refresh rate (0 = silent)
#' @return Fitted brms model
fit_simple_model <- function(model_data, refresh = 0) {
  if (is.null(model_data) || nrow(model_data) < 15) {
    warning("Insufficient data for Bayesian model fitting (need at least 15 observations)")
    return(NULL)
  }

  # Simple model with trend and seasonality
  formula <- bf(
    cases ~ 1 + time + season_sin + season_cos,
    family = negbinomial()
  )

  priors <- c(
    prior(normal(0, 10), class = "Intercept"),
    prior(normal(0, 0.1), class = "b"),  # Tight prior to prevent exponential explosion
    prior(gamma(0.1, 0.1), class = "shape")
  )

  tryCatch({
    fit <- brm(
      formula = formula,
      data = model_data,
      prior = priors,
      chains = BAYES_CONFIG$chains,
      iter = BAYES_CONFIG$iter,
      warmup = BAYES_CONFIG$warmup,
      cores = BAYES_CONFIG$cores,
      refresh = refresh,
      silent = 2,
      backend = "rstan"
    )

    fit

  }, error = function(e) {
    warning(paste("Model fitting failed:", e$message))
    return(NULL)
  })
}

#' Fit hierarchical model for multiple pathogens
#' @param model_data Prepared data with multiple pathogens
#' @param refresh Progress refresh rate
#' @return Fitted brms model
fit_hierarchical_model <- function(model_data, refresh = 0) {
  if (is.null(model_data) || nrow(model_data) < 30) {
    warning("Insufficient data for hierarchical model (need at least 30 observations)")
    return(NULL)
  }

  # Check for multiple groups
  has_pathogen <- "pathogen" %in% names(model_data) && length(unique(model_data$pathogen)) > 1
  has_country <- "country" %in% names(model_data) && length(unique(model_data$country)) > 1

  # Build formula based on available grouping
  if (has_pathogen && has_country) {
    formula <- bf(
      cases ~ 1 + time + season_sin + season_cos + (1 + time | pathogen) + (1 | country),
      family = negbinomial()
    )
  } else if (has_pathogen) {
    formula <- bf(
      cases ~ 1 + time + season_sin + season_cos + (1 + time | pathogen),
      family = negbinomial()
    )
  } else if (has_country) {
    formula <- bf(
      cases ~ 1 + time + season_sin + season_cos + (1 | country),
      family = negbinomial()
    )
  } else {
    # Fall back to simple model
    return(fit_simple_model(model_data, refresh))
  }

  priors <- get_model_priors()

  tryCatch({
    fit <- brm(
      formula = formula,
      data = model_data,
      prior = priors,
      chains = BAYES_CONFIG$chains,
      iter = BAYES_CONFIG$iter,
      warmup = BAYES_CONFIG$warmup,
      cores = BAYES_CONFIG$cores,
      refresh = refresh,
      silent = 2,
      backend = "rstan"
    )

    fit

  }, error = function(e) {
    warning(paste("Hierarchical model failed, trying simple model:", e$message))
    return(fit_simple_model(model_data, refresh))
  })
}

# =============================================================================
# POSTERIOR PREDICTION
# =============================================================================

#' Generate posterior predictions for new data
#' @param fit Fitted brms model
#' @param newdata Data frame with future predictors
#' @param ndraws Number of posterior draws
#' @return Matrix of posterior predictions
generate_posterior_predictions <- function(fit, newdata, ndraws = 1000) {
  if (is.null(fit)) {
    return(NULL)
  }

  tryCatch({
    # Generate posterior predictive samples
    pred <- posterior_predict(fit, newdata = newdata, ndraws = ndraws)
    pred
  }, error = function(e) {
    warning(paste("Prediction failed:", e$message))
    return(NULL)
  })
}

#' Summarize forecast with credible intervals
#' @param predictions Matrix from generate_posterior_predictions()
#' @param newdata Future dates data frame
#' @param levels Credible interval levels
#' @return Data frame with forecast summary
summarize_forecast <- function(predictions, newdata, levels = BAYES_CONFIG$credible_intervals) {
  if (is.null(predictions)) {
    return(NULL)
  }

  # Calculate summary statistics
  summary_list <- lapply(1:ncol(predictions), function(i) {
    sims <- predictions[, i]

    result <- data.frame(
      date = newdata$date[i],
      forecast_week = newdata$forecast_week[i],
      mean = mean(sims),
      predicted_cases = round(mean(sims)),  # Alias for ensemble compatibility
      median = median(sims),
      sd = sd(sims)
    )

    # Add credible intervals
    for (level in levels) {
      lower_q <- (1 - level) / 2
      upper_q <- 1 - lower_q
      result[[paste0("lower_", level * 100)]] <- quantile(sims, lower_q)
      result[[paste0("upper_", level * 100)]] <- quantile(sims, upper_q)
    }

    result
  })

  do.call(rbind, summary_list)
}

# =============================================================================
# MODEL CACHING
# =============================================================================

#' Get cache path for a model
#' @param pathogen_code Pathogen identifier
#' @return File path for cached model
get_model_cache_path <- function(pathogen_code, country = NULL, start_date = NULL, end_date = NULL) {
  dir.create(BAYES_CONFIG$cache_dir, recursive = TRUE, showWarnings = FALSE)
  suffix <- if (is.null(country)) "ALL" else country

  # Add date range to cache key if provided
  if (!is.null(start_date) && !is.null(end_date)) {
    date_suffix <- sprintf("_%s_%s", format(as.Date(start_date), "%Y%m%d"), format(as.Date(end_date), "%Y%m%d"))
    suffix <- paste0(suffix, date_suffix)
  }

  file.path(BAYES_CONFIG$cache_dir, paste0("model_", pathogen_code, "_", suffix, ".rds"))
}

#' Check if cached model is still valid
#' @param cache_path Path to cached model
#' @return TRUE if model is valid and not expired
is_model_valid <- function(cache_path) {
  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check file age
  file_age <- difftime(Sys.time(), file.mtime(cache_path), units = "hours")
  as.numeric(file_age) < BAYES_CONFIG$model_expiry_hours
}

#' Save model to cache
#' @param fit Fitted brms model
#' @param pathogen_code Pathogen identifier
save_model_cache <- function(fit, pathogen_code, country = NULL, start_date = NULL, end_date = NULL) {
  cache_path <- get_model_cache_path(pathogen_code, country, start_date, end_date)

  tryCatch({
    saveRDS(fit, cache_path)
    message(sprintf("Model cached: %s", cache_path))
  }, error = function(e) {
    warning(paste("Failed to cache model:", e$message))
  })
}

#' Load model from cache
#' @param pathogen_code Pathogen identifier
#' @return Fitted model or NULL if not available
load_model_cache <- function(pathogen_code, country = NULL, start_date = NULL, end_date = NULL) {
  cache_path <- get_model_cache_path(pathogen_code, country, start_date, end_date)

  if (!is_model_valid(cache_path)) {
    return(NULL)
  }

  tryCatch({
    readRDS(cache_path)
  }, error = function(e) {
    warning(paste("Failed to load cached model:", e$message))
    return(NULL)
  })
}

# =============================================================================
# MAIN API FUNCTIONS
# =============================================================================

#' Generate Bayesian forecast for a pathogen
#' @param pathogen_code Pathogen identifier
#' @param country Optional country filter
#' @param use_cache Whether to use cached models
#' @param horizon Weeks to forecast
#' @return List with forecast data and model info
get_bayesian_forecast <- function(pathogen_code, country = NULL, use_cache = TRUE,
                                  horizon = BAYES_CONFIG$horizon_weeks,
                                  start_date = NULL, end_date = NULL) {
  # Source database modules if needed
  if (!exists("get_db_connection")) {
    source("R/db_schema.R")
    source("R/db_operations.R")
  }

  # Try to load cached model
  fit <- NULL
  if (use_cache) {
    fit <- load_model_cache(pathogen_code, country, start_date, end_date)
  }

  # Validate input parameters to prevent SQL injection
  if (!is_valid_pathogen_code(pathogen_code)) {
    warning(sprintf("Invalid pathogen code format: %s", pathogen_code))
    return(list(
      pathogen = pathogen_code,
      country = country,
      forecast = NULL,
      error = "Invalid pathogen code format"
    ))
  }

  if (!is.null(country) && !is_valid_iso_code(country)) {
    warning(sprintf("Invalid country code format: %s", country))
    return(list(
      pathogen = pathogen_code,
      country = country,
      forecast = NULL,
      error = "Invalid country code format"
    ))
  }

  # Get surveillance data
  conn <- get_db_connection()

  # Build query with validated and escaped inputs
  country_clause <- ""
  if (!is.null(country)) {
    country_clause <- sprintf("AND c.iso_code = '%s'", escape_sql(toupper(country)))
  }

  # Date filtering clause
  date_clause <- ""
  if (!is.null(start_date)) {
    date_clause <- sprintf("%s AND sd.observation_date >= '%s'", date_clause, as.character(start_date))
  }
  if (!is.null(end_date)) {
    date_clause <- sprintf("%s AND sd.observation_date <= '%s'", date_clause, as.character(end_date))
  }

  query <- sprintf("
    SELECT
      sd.observation_date,
      sd.case_count,
      sd.estimated_cases,
      p.pathogen_code,
      c.iso_code
    FROM surveillance_data sd
    JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
    JOIN countries c ON sd.country_id = c.country_id
    WHERE p.pathogen_code = '%s'
    %s
    %s
    ORDER BY sd.observation_date
  ",
    escape_sql(pathogen_code),
    country_clause,
    date_clause
  )

  surv_data <- DBI::dbGetQuery(conn, query)
  close_db_connection(conn)

  if (nrow(surv_data) == 0) {
    return(list(
      pathogen = pathogen_code,
      country = country,
      forecast = NULL,
      error = "No surveillance data available"
    ))
  }

  # Prepare model data
  model_data <- prepare_model_data(surv_data)

  # Fit model if not cached
  if (is.null(fit)) {
    message(sprintf("Fitting Bayesian model for %s...", pathogen_code))
    fit <- fit_simple_model(model_data)

    if (!is.null(fit) && use_cache) {
      save_model_cache(fit, pathogen_code, country, start_date, end_date)
    }
  }

  if (is.null(fit)) {
    return(list(
      pathogen = pathogen_code,
      country = country,
      forecast = NULL,
      error = "Model fitting failed"
    ))
  }

  # Generate predictions
  newdata <- create_newdata(model_data, horizon)
  predictions <- generate_posterior_predictions(fit, newdata)
  forecast_df <- summarize_forecast(predictions, newdata)

  if (!is.null(forecast_df)) {
    forecast_df$pathogen <- pathogen_code
    forecast_df$forecast_date <- Sys.Date()
    forecast_df$method <- "brms-negbinom"

    # Sanity cap: prevent unrealistic forecasts (max 100x historical max)
    historical_max <- max(model_data$cases, na.rm = TRUE)
    reasonable_cap <- max(historical_max * 100, 1e6)  # At least 1M cap

    forecast_cols <- c("mean", "predicted_cases", "median",
                       "lower_50", "upper_50", "lower_80", "upper_80",
                       "lower_95", "upper_95")
    for (col in forecast_cols) {
      if (col %in% names(forecast_df)) {
        forecast_df[[col]] <- pmin(forecast_df[[col]], reasonable_cap)
      }
    }
  }

  # Generate diagnostic report for model convergence checks
  diagnostics <- generate_diagnostic_report(fit, pathogen_code)

  list(
    pathogen = pathogen_code,
    country = country,
    model = fit,
    forecast = forecast_df,
    historical = model_data,
    historical_cases = model_data,  # Alias for compatibility with UI
    diagnostics = diagnostics,
    model_summary = if (!is.null(fit)) summary(fit) else NULL,
    training_start = start_date,
    training_end = end_date,
    forecast_generated = Sys.time()
  )
}

#' Get Bayesian forecasts for all pathogens
#' @param use_cache Whether to use cached models
#' @param horizon Weeks to forecast
#' @return List of forecast results by pathogen
get_bayesian_forecasts_all <- function(use_cache = TRUE, horizon = 4) {
  pathogens <- c("H3N2", "RSV", "COVID19")

  results <- lapply(pathogens, function(p) {
    tryCatch({
      message(sprintf("Processing %s...", p))
      get_bayesian_forecast(p, use_cache = use_cache, horizon = horizon)
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
# MODEL COMPARISON
# =============================================================================

#' Compare models using WAIC and LOO-CV
#' @param fit1 First fitted model
#' @param fit2 Second fitted model
#' @return Data frame with comparison metrics
compare_models <- function(fit1, fit2) {
  if (is.null(fit1) || is.null(fit2)) {
    return(NULL)
  }

  tryCatch({
    # Compute WAIC
    waic1 <- waic(fit1)
    waic2 <- waic(fit2)

    # Compute LOO
    loo1 <- loo(fit1)
    loo2 <- loo(fit2)

    # Compare
    comparison <- data.frame(
      Model = c("Model 1", "Model 2"),
      WAIC = c(waic1$estimates["waic", "Estimate"],
               waic2$estimates["waic", "Estimate"]),
      LOO_ELPD = c(loo1$estimates["elpd_loo", "Estimate"],
                   loo2$estimates["elpd_loo", "Estimate"]),
      SE_WAIC = c(waic1$estimates["waic", "SE"],
                  waic2$estimates["waic", "SE"]),
      SE_LOO = c(loo1$estimates["elpd_loo", "SE"],
                 loo2$estimates["elpd_loo", "SE"])
    )

    comparison

  }, error = function(e) {
    warning(paste("Model comparison failed:", e$message))
    return(NULL)
  })
}

# =============================================================================
# VISUALIZATION HELPERS
# =============================================================================

#' Prepare Bayesian forecast data for plotting
#' @param forecast_result Result from get_bayesian_forecast()
#' @return Data frame ready for ggplot
prepare_bayesian_plot_data <- function(forecast_result) {
  if (is.null(forecast_result$forecast)) {
    return(NULL)
  }

  # Historical data
  historical <- forecast_result$historical |>
    select(date, cases) |>
    mutate(
      type = "observed",
      pathogen = forecast_result$pathogen
    )

  # Forecast data
  forecast <- forecast_result$forecast |>
    select(date, median, lower_50, upper_50, lower_80, upper_80, lower_95, upper_95) |>
    rename(cases = median) |>
    mutate(
      type = "forecast",
      pathogen = forecast_result$pathogen
    )

  # Combine
  bind_rows(historical, forecast) |>
    arrange(date)
}

#' Summarize Bayesian forecasts for table display
#' @param forecast_results List from get_bayesian_forecasts_all()
#' @return Data frame with forecast summaries
summarize_bayesian_table <- function(forecast_results) {
  summaries <- lapply(names(forecast_results), function(p) {
    result <- forecast_results[[p]]

    if (is.null(result$forecast)) {
      return(data.frame(
        Pathogen = p,
        `1-Week Forecast` = "N/A",
        `4-Week Forecast` = "N/A",
        Method = "N/A",
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    forecast <- result$forecast

    week1 <- forecast |> filter(forecast_week == 1)
    week4 <- forecast |> filter(forecast_week == 4)

    data.frame(
      Pathogen = p,
      `1-Week Forecast` = sprintf("%d (80%% CrI: %d-%d)",
                                   round(week1$median),
                                   round(week1$lower_80),
                                   round(week1$upper_80)),
      `4-Week Forecast` = if (nrow(week4) > 0) {
        sprintf("%d (80%% CrI: %d-%d)",
                round(week4$median),
                round(week4$lower_80),
                round(week4$upper_80))
      } else "N/A",
      Method = "Bayesian NB",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  do.call(rbind, summaries)
}
