# ============================================================================
# Title: RespiWatch Scenario Modeling Module
# Purpose: What-if analysis for intervention scenarios and projections
# Input: Current Rt estimates and surveillance data
# Output: Scenario projections with intervention effects
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(tidyr)

# =============================================================================
# SCENARIO CONFIGURATION
# =============================================================================

#' Predefined intervention scenarios
INTERVENTION_SCENARIOS <- list(
  no_intervention = list(
    name = "No Intervention (Baseline)",
    rt_modifier = 1.0,
    description = "Current trajectory continues without changes"
  ),
  mask_mandate = list(
    name = "Mask Mandate",
    rt_modifier = 0.85,
    description = "Universal masking reduces Rt by ~15%"
  ),
  social_distancing = list(
    name = "Social Distancing",
    rt_modifier = 0.75,
    description = "Social distancing measures reduce Rt by ~25%"
  ),
  school_closure = list(
    name = "School Closure",
    rt_modifier = 0.80,
    description = "School closures reduce Rt by ~20%"
  ),
  lockdown_light = list(
    name = "Light Lockdown",
    rt_modifier = 0.60,
    description = "Partial lockdown reduces Rt by ~40%"
  ),
  lockdown_full = list(
    name = "Full Lockdown",
    rt_modifier = 0.40,
    description = "Strict lockdown reduces Rt by ~60%"
  ),
  vaccination_boost = list(
    name = "Vaccination Campaign",
    rt_modifier = 0.70,
    description = "Intensive vaccination reduces Rt by ~30%"
  ),
  combined_moderate = list(
    name = "Combined Moderate",
    rt_modifier = 0.55,
    description = "Masks + distancing + partial closures"
  )
)

# =============================================================================
# SCENARIO PROJECTION ENGINE
# =============================================================================

#' Project cases under a modified Rt scenario
#' @param incidence_data Data frame with dates and I columns
#' @param current_rt Current reproduction number
#' @param rt_modifier Multiplicative modifier for Rt (0-1 for reduction)
#' @param si_params Serial interval parameters
#' @param horizon Weeks to project
#' @return Data frame with projected cases
project_scenario <- function(incidence_data, current_rt, rt_modifier,
                              si_params, horizon = 8) {
  if (is.null(incidence_data) || nrow(incidence_data) < 3) {
    return(NULL)
  }

  # Modified Rt
  scenario_rt <- current_rt * rt_modifier

  # Get last observed data
  last_date <- max(incidence_data$dates)
  last_cases <- tail(incidence_data$I, 4)

  # Serial interval weights
  si_shape <- (si_params$mean / si_params$sd)^2
  si_rate <- si_params$mean / si_params$sd^2
  si_weights <- dgamma(1:length(last_cases), shape = si_shape, rate = si_rate)
  si_weights <- si_weights / sum(si_weights)

  # Project forward
  projected <- data.frame(
    date = seq(last_date + 7, by = 7, length.out = horizon),
    forecast_week = 1:horizon
  )

  cases_history <- c(last_cases)
  projected_cases <- numeric(horizon)

  for (h in 1:horizon) {
    weighted_sum <- sum(tail(cases_history, length(si_weights)) * rev(si_weights))
    expected_cases <- scenario_rt * weighted_sum
    projected_cases[h] <- max(0, round(expected_cases))
    cases_history <- c(cases_history, projected_cases[h])
  }

  projected$predicted_cases <- projected_cases
  projected$rt_used <- scenario_rt

  # Add uncertainty (wider for modified scenarios)
  uncertainty_factor <- 1 + abs(1 - rt_modifier) * 0.5
  for (i in 1:nrow(projected)) {
    mean_cases <- projected$predicted_cases[i]
    week <- projected$forecast_week[i]
    cv <- 0.3 * (1 + 0.1 * week) * uncertainty_factor

    projected$lower_80[i] <- round(max(0, mean_cases * (1 - 1.28 * cv)))
    projected$upper_80[i] <- round(mean_cases * (1 + 1.28 * cv))
    projected$lower_95[i] <- round(max(0, mean_cases * (1 - 1.96 * cv)))
    projected$upper_95[i] <- round(mean_cases * (1 + 1.96 * cv))
  }

  projected
}

#' Run multiple scenarios in parallel
#' @param incidence_data Data frame with dates and I columns
#' @param current_rt Current reproduction number
#' @param si_params Serial interval parameters
#' @param scenarios List of scenario names or custom scenarios
#' @param horizon Weeks to project
#' @return List with projections for each scenario
run_scenarios <- function(incidence_data, current_rt, si_params,
                           scenarios = names(INTERVENTION_SCENARIOS),
                           horizon = 8) {
  results <- list()

  for (scenario_name in scenarios) {
    # Get scenario config
    if (scenario_name %in% names(INTERVENTION_SCENARIOS)) {
      scenario <- INTERVENTION_SCENARIOS[[scenario_name]]
    } else {
      warning(sprintf("Unknown scenario: %s, skipping", scenario_name))
      next
    }

    # Project
    projection <- project_scenario(
      incidence_data = incidence_data,
      current_rt = current_rt,
      rt_modifier = scenario$rt_modifier,
      si_params = si_params,
      horizon = horizon
    )

    if (!is.null(projection)) {
      projection$scenario <- scenario_name
      projection$scenario_name <- scenario$name
      projection$rt_modifier <- scenario$rt_modifier
      projection$description <- scenario$description
      results[[scenario_name]] <- projection
    }
  }

  results
}

# =============================================================================
# CUSTOM SCENARIO BUILDER
# =============================================================================

#' Create a custom intervention scenario
#' @param name Scenario display name
#' @param rt_reduction Percentage reduction in Rt (0-100)
#' @param description Description of the intervention
#' @return Scenario configuration list
create_custom_scenario <- function(name, rt_reduction, description = NULL) {
  # Validate rt_reduction
  rt_reduction <- max(0, min(100, rt_reduction))

  list(
    name = name,
    rt_modifier = 1 - (rt_reduction / 100),
    description = description %||% sprintf("Custom scenario: %.0f%% Rt reduction", rt_reduction)
  )
}

#' Run projection with custom Rt value
#' @param incidence_data Incidence data
#' @param target_rt Target reproduction number
#' @param si_params Serial interval parameters
#' @param horizon Weeks ahead
#' @return Projection data frame
project_with_target_rt <- function(incidence_data, target_rt, si_params, horizon = 8) {
  if (is.null(incidence_data) || nrow(incidence_data) < 3) {
    return(NULL)
  }

  last_date <- max(incidence_data$dates)
  last_cases <- tail(incidence_data$I, 4)

  # Serial interval
  si_shape <- (si_params$mean / si_params$sd)^2
  si_rate <- si_params$mean / si_params$sd^2
  si_weights <- dgamma(1:length(last_cases), shape = si_shape, rate = si_rate)
  si_weights <- si_weights / sum(si_weights)

  projected <- data.frame(
    date = seq(last_date + 7, by = 7, length.out = horizon),
    forecast_week = 1:horizon
  )

  cases_history <- c(last_cases)
  projected_cases <- numeric(horizon)

  for (h in 1:horizon) {
    weighted_sum <- sum(tail(cases_history, length(si_weights)) * rev(si_weights))
    expected_cases <- target_rt * weighted_sum
    projected_cases[h] <- max(0, round(expected_cases))
    cases_history <- c(cases_history, projected_cases[h])
  }

  projected$predicted_cases <- projected_cases
  projected$rt_used <- target_rt
  projected$scenario <- "custom"
  projected$scenario_name <- sprintf("Target Rt = %.2f", target_rt)

  projected
}

# =============================================================================
# MAIN API FUNCTIONS
# =============================================================================

#' Get scenario projections for a pathogen
#' @param pathogen_code Pathogen identifier
#' @param scenarios Vector of scenario names (or NULL for all)
#' @param horizon Weeks ahead
#' @return List with scenario projections and metadata
get_scenario_projections <- function(pathogen_code, scenarios = NULL, horizon = 8) {
  # Source required modules
  if (!exists("get_rt_for_pathogen")) {
    source("R/rt_estimation.R")
  }

  # Get current Rt and data
  rt_result <- tryCatch({
    get_rt_for_pathogen(pathogen_code)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(rt_result) || is.null(rt_result$current_rt$value) ||
      is.na(rt_result$current_rt$value)) {
    return(list(
      status = "error",
      message = "Could not get Rt estimate for pathogen"
    ))
  }

  # Get incidence data
  if (!exists("prepare_incidence_data")) {
    source("R/rt_estimation.R")
  }

  # Prepare incidence from rt_result
  incidence_data <- data.frame(
    dates = rt_result$rt_estimates$date,
    I = sapply(1:nrow(rt_result$rt_estimates), function(i) {
      # Use cumulative cases from the estimation window
      sum_cases <- round(rt_result$rt_estimates$rt_mean[i] * 100)  # Approximate
      max(1, sum_cases)
    })
  )

  # If we have better incidence data from the database, use that
  if (!is.null(rt_result$data_points) && rt_result$data_points > 0) {
    # Get fresh incidence data
    if (!exists("get_db_connection")) {
      source("R/db_schema.R")
      source("R/db_operations.R")
    }

    conn <- get_db_connection()
    # Use parameterized query to prevent SQL injection
    query <- "
      SELECT observation_date, case_count, estimated_cases
      FROM surveillance_data sd
      JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
      WHERE p.pathogen_code = ?
      ORDER BY observation_date
    "

    surv_data <- tryCatch({
      stmt <- DBI::dbSendQuery(conn, query)
      DBI::dbBind(stmt, list(pathogen_code))
      result <- DBI::dbFetch(stmt)
      DBI::dbClearResult(stmt)
      result
    }, error = function(e) {
      data.frame()
    })
    DBI::dbDisconnect(conn)

    if (nrow(surv_data) > 0) {
      incidence_data <- prepare_incidence_data(surv_data)
    }
  }

  # Default to all scenarios if not specified
  if (is.null(scenarios)) {
    scenarios <- names(INTERVENTION_SCENARIOS)
  }

  # Run scenarios
  scenario_results <- run_scenarios(
    incidence_data = incidence_data,
    current_rt = rt_result$current_rt$value,
    si_params = rt_result$serial_interval,
    scenarios = scenarios,
    horizon = horizon
  )

  list(
    status = "success",
    pathogen = pathogen_code,
    current_rt = rt_result$current_rt$value,
    serial_interval = rt_result$serial_interval,
    scenarios = scenario_results,
    n_scenarios = length(scenario_results),
    horizon = horizon,
    incidence_data = incidence_data,
    generated = Sys.time()
  )
}

#' Get scenario projections for all pathogens
#' @param scenarios Vector of scenario names
#' @param horizon Weeks ahead
#' @return List of scenario results by pathogen
get_scenarios_all_pathogens <- function(scenarios = NULL, horizon = 8) {
  pathogens <- c("H3N2", "RSV", "COVID19")

  results <- lapply(pathogens, function(p) {
    tryCatch({
      get_scenario_projections(p, scenarios = scenarios, horizon = horizon)
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
# IMPACT ANALYSIS
# =============================================================================

#' Calculate cumulative impact of scenarios
#' @param scenario_results Result from get_scenario_projections()
#' @return Data frame with cumulative case counts by scenario
calculate_scenario_impact <- function(scenario_results) {
  if (is.null(scenario_results) || scenario_results$status != "success") {
    return(NULL)
  }

  impacts <- lapply(names(scenario_results$scenarios), function(s) {
    proj <- scenario_results$scenarios[[s]]
    data.frame(
      scenario = s,
      scenario_name = proj$scenario_name[1],
      rt_modifier = proj$rt_modifier[1],
      total_cases = sum(proj$predicted_cases),
      peak_cases = max(proj$predicted_cases),
      final_week_cases = tail(proj$predicted_cases, 1),
      stringsAsFactors = FALSE
    )
  })

  impact_df <- do.call(rbind, impacts)

  # Calculate relative to baseline
  baseline_total <- impact_df$total_cases[impact_df$scenario == "no_intervention"]
  if (length(baseline_total) > 0 && !is.na(baseline_total)) {
    impact_df$cases_averted <- baseline_total - impact_df$total_cases
    impact_df$percent_reduction <- round(
      (1 - impact_df$total_cases / baseline_total) * 100, 1
    )
  } else {
    impact_df$cases_averted <- NA
    impact_df$percent_reduction <- NA
  }

  impact_df
}

#' Compare two specific scenarios
#' @param scenario_results Result from get_scenario_projections()
#' @param scenario_a First scenario name
#' @param scenario_b Second scenario name
#' @return Data frame with week-by-week comparison
compare_scenarios <- function(scenario_results, scenario_a, scenario_b) {
  if (is.null(scenario_results) || scenario_results$status != "success") {
    return(NULL)
  }

  proj_a <- scenario_results$scenarios[[scenario_a]]
  proj_b <- scenario_results$scenarios[[scenario_b]]

  if (is.null(proj_a) || is.null(proj_b)) {
    return(NULL)
  }

  comparison <- data.frame(
    date = proj_a$date,
    week = proj_a$forecast_week,
    cases_a = proj_a$predicted_cases,
    cases_b = proj_b$predicted_cases
  ) |>
    mutate(
      difference = cases_a - cases_b,
      percent_diff = round((cases_a - cases_b) / pmax(cases_a, 1) * 100, 1)
    )

  attr(comparison, "scenario_a") <- scenario_a

  attr(comparison, "scenario_b") <- scenario_b

  comparison
}

# =============================================================================
# VISUALIZATION HELPERS
# =============================================================================

#' Prepare scenario data for plotting
#' @param scenario_results Result from get_scenario_projections()
#' @param include_historical Include historical data?
#' @return Data frame ready for ggplot/plotly
prepare_scenario_plot_data <- function(scenario_results, include_historical = TRUE) {
  if (is.null(scenario_results) || scenario_results$status != "success") {
    return(NULL)
  }

  # Combine all scenarios
  scenario_data <- lapply(names(scenario_results$scenarios), function(s) {
    proj <- scenario_results$scenarios[[s]]
    proj |>
      select(date, predicted_cases, lower_80, upper_80, lower_95, upper_95,
             scenario, scenario_name, rt_modifier) |>
      mutate(type = "projection")
  })

  plot_data <- bind_rows(scenario_data)

  # Add historical if requested
  if (include_historical && !is.null(scenario_results$incidence_data)) {
    historical <- scenario_results$incidence_data |>
      rename(date = dates, predicted_cases = I) |>
      mutate(
        type = "observed",
        scenario = "historical",
        scenario_name = "Observed"
      )
    plot_data <- bind_rows(historical, plot_data)
  }

  plot_data$pathogen <- scenario_results$pathogen
  plot_data
}

#' Summary table for scenario comparison
#' @param scenario_results Result from get_scenario_projections()
#' @return Data frame formatted for display
summarize_scenario_table <- function(scenario_results) {
  if (is.null(scenario_results) || scenario_results$status != "success") {
    return(NULL)
  }

  impact <- calculate_scenario_impact(scenario_results)
  if (is.null(impact)) {
    return(NULL)
  }

  impact |>
    select(
      Scenario = scenario_name,
      `Rt Modifier` = rt_modifier,
      `Total Cases` = total_cases,
      `Peak Week` = peak_cases,
      `Cases Averted` = cases_averted,
      `% Reduction` = percent_reduction
    ) |>
    arrange(desc(`Total Cases`))
}
