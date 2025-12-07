# ============================================================================
# Title: RespiWatch Model Diagnostics Module
# Purpose: Convergence checks and posterior predictive checks for brms models
# Input: Fitted brms models
# Output: Diagnostic plots and summary statistics
# ============================================================================

# Load required packages -------------------------------------------------------
library(brms)
library(bayesplot)
library(ggplot2)
library(dplyr)

# =============================================================================
# CONVERGENCE DIAGNOSTICS
# =============================================================================

#' Check model convergence
#' @param fit Fitted brms model
#' @return List with convergence diagnostics
check_convergence <- function(fit) {
  if (is.null(fit)) {
    return(list(
      converged = FALSE,
      message = "No model provided"
    ))
  }

  tryCatch({
    # Get summary
    model_summary <- summary(fit)

    # Extract Rhat and ESS
    fixed_effects <- model_summary$fixed
    rhat_values <- fixed_effects$Rhat
    ess_bulk <- fixed_effects$Bulk_ESS
    ess_tail <- fixed_effects$Tail_ESS

    # Check convergence criteria
    rhat_ok <- all(rhat_values < 1.01, na.rm = TRUE)
    ess_ok <- all(ess_bulk > 400, na.rm = TRUE) && all(ess_tail > 400, na.rm = TRUE)

    # Divergences
    divergences <- sum(nuts_params(fit)$divergent__, na.rm = TRUE)
    divergent_ok <- divergences == 0

    # Overall convergence
    converged <- rhat_ok && ess_ok && divergent_ok

    list(
      converged = converged,
      rhat_max = max(rhat_values, na.rm = TRUE),
      ess_min_bulk = min(ess_bulk, na.rm = TRUE),
      ess_min_tail = min(ess_tail, na.rm = TRUE),
      n_divergences = divergences,
      rhat_ok = rhat_ok,
      ess_ok = ess_ok,
      divergent_ok = divergent_ok,
      message = if (converged) "Model converged" else "Convergence issues detected"
    )

  }, error = function(e) {
    list(
      converged = NA,
      message = paste("Could not check convergence:", e$message)
    )
  })
}

#' Get detailed parameter summary
#' @param fit Fitted brms model
#' @return Data frame with parameter estimates and diagnostics
get_model_summary <- function(fit) {
  if (is.null(fit)) {
    return(NULL)
  }

  tryCatch({
    model_summary <- summary(fit)

    # Fixed effects
    fixed_df <- as.data.frame(model_summary$fixed)
    fixed_df$Parameter <- rownames(fixed_df)
    fixed_df$Type <- "Fixed"

    # Try to get random effects if they exist
    if (!is.null(model_summary$random) && length(model_summary$random) > 0) {
      random_list <- lapply(names(model_summary$random), function(grp) {
        random_df <- as.data.frame(model_summary$random[[grp]])
        random_df$Parameter <- rownames(random_df)
        random_df$Type <- paste0("Random (", grp, ")")
        random_df
      })
      all_params <- bind_rows(fixed_df, bind_rows(random_list))
    } else {
      all_params <- fixed_df
    }

    # Clean up column names
    all_params |>
      select(Parameter, Type, everything()) |>
      mutate(
        Converged = ifelse(Rhat < 1.01, "Yes", "No"),
        ESS_Adequate = ifelse(Bulk_ESS > 400 & Tail_ESS > 400, "Yes", "No")
      )

  }, error = function(e) {
    warning(paste("Could not get model summary:", e$message))
    return(NULL)
  })
}

# =============================================================================
# TRACE PLOTS
# =============================================================================

#' Generate trace plots for model parameters
#' @param fit Fitted brms model
#' @param pars Parameters to plot (default: all fixed effects)
#' @return ggplot object
plot_trace <- function(fit, pars = NULL) {
  if (is.null(fit)) {
    return(NULL)
  }

  tryCatch({
    if (is.null(pars)) {
      # Plot all fixed effects
      mcmc_trace(fit, regex_pars = "^b_") +
        theme_minimal() +
        labs(title = "Trace Plots for Fixed Effects")
    } else {
      mcmc_trace(fit, pars = pars) +
        theme_minimal() +
        labs(title = "Trace Plots")
    }
  }, error = function(e) {
    warning(paste("Could not create trace plots:", e$message))
    return(NULL)
  })
}

#' Generate density plots for model parameters
#' @param fit Fitted brms model
#' @param pars Parameters to plot
#' @return ggplot object
plot_density <- function(fit, pars = NULL) {
  if (is.null(fit)) {
    return(NULL)
  }

  tryCatch({
    if (is.null(pars)) {
      mcmc_dens_overlay(fit, regex_pars = "^b_") +
        theme_minimal() +
        labs(title = "Posterior Density Plots")
    } else {
      mcmc_dens_overlay(fit, pars = pars) +
        theme_minimal() +
        labs(title = "Posterior Density Plots")
    }
  }, error = function(e) {
    warning(paste("Could not create density plots:", e$message))
    return(NULL)
  })
}

# =============================================================================
# POSTERIOR PREDICTIVE CHECKS
# =============================================================================
#' Perform posterior predictive check
#' @param fit Fitted brms model
#' @param data Original data used for fitting (optional, uses fit$data if NULL)
#' @param ndraws Number of posterior draws to use
#' @return ggplot object
posterior_predictive_check <- function(fit, data = NULL, ndraws = 100) {
  if (is.null(fit)) {
    return(NULL)
  }

  tryCatch({
    # Get posterior predictions
    yrep <- posterior_predict(fit, ndraws = ndraws)

    # Get observed data - use provided data if available, otherwise from fit
    y <- if (!is.null(data) && "cases" %in% names(data)) {
      data$cases
    } else {
      fit$data$cases
    }

    # Create PPC plot
    ppc_dens_overlay(y, yrep) +
      theme_minimal() +
      labs(
        title = "Posterior Predictive Check",
        subtitle = "Observed data vs. posterior predictions",
        x = "Case Count",
        y = "Density"
      )

  }, error = function(e) {
    warning(paste("Could not perform PPC:", e$message))
    return(NULL)
  })
}

#' Plot residuals
#' @param fit Fitted brms model
#' @return ggplot object
plot_residuals <- function(fit) {
  if (is.null(fit)) {
    return(NULL)
  }

  tryCatch({
    # Get residuals
    resid <- residuals(fit, type = "ordinary")[, "Estimate"]
    fitted_vals <- fitted(fit)[, "Estimate"]

    # Create residual plot
    resid_df <- data.frame(
      fitted = fitted_vals,
      residuals = resid
    )

    ggplot(resid_df, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(method = "loess", se = TRUE, color = "blue") +
      theme_minimal() +
      labs(
        title = "Residual Plot",
        x = "Fitted Values",
        y = "Residuals"
      )

  }, error = function(e) {
    warning(paste("Could not create residual plot:", e$message))
    return(NULL)
  })
}

# =============================================================================
# MODEL COMPARISON PLOTS
# =============================================================================

#' Plot model comparison (LOO)
#' @param fit1 First model
#' @param fit2 Second model
#' @return ggplot object
plot_loo_comparison <- function(fit1, fit2) {
  if (is.null(fit1) || is.null(fit2)) {
    return(NULL)
  }

  tryCatch({
    loo1 <- loo(fit1)
    loo2 <- loo(fit2)

    comparison <- loo_compare(loo1, loo2)

    # Create comparison data frame
    comp_df <- data.frame(
      Model = rownames(comparison),
      ELPD_Diff = comparison[, "elpd_diff"],
      SE_Diff = comparison[, "se_diff"]
    )

    ggplot(comp_df, aes(x = Model, y = ELPD_Diff)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_errorbar(aes(ymin = ELPD_Diff - SE_Diff, ymax = ELPD_Diff + SE_Diff),
                    width = 0.2) +
      theme_minimal() +
      labs(
        title = "Model Comparison (LOO-CV)",
        subtitle = "Higher ELPD is better",
        x = "Model",
        y = "ELPD Difference"
      )

  }, error = function(e) {
    warning(paste("Could not compare models:", e$message))
    return(NULL)
  })
}

# =============================================================================
# DIAGNOSTIC REPORT
# =============================================================================

#' Generate comprehensive diagnostic report
#' @param fit Fitted brms model
#' @param pathogen Pathogen identifier
#' @return List with all diagnostic information
generate_diagnostic_report <- function(fit, pathogen = "Unknown") {
  if (is.null(fit)) {
    return(list(
      pathogen = pathogen,
      status = "No model available"
    ))
  }

  # Convergence check
  convergence <- check_convergence(fit)

  # Parameter summary
  params <- get_model_summary(fit)

  # Plots (store as ggplot objects)
  trace_plot <- plot_trace(fit)
  density_plot <- plot_density(fit)
  ppc_plot <- tryCatch(posterior_predictive_check(fit), error = function(e) NULL)
  residual_plot <- tryCatch(plot_residuals(fit), error = function(e) NULL)

  list(
    pathogen = pathogen,
    convergence = convergence,
    parameters = params,
    plots = list(
      trace = trace_plot,
      density = density_plot,
      ppc = ppc_plot,
      residuals = residual_plot
    ),
    model_formula = formula(fit),
    n_observations = nrow(fit$data),
    generated = Sys.time()
  )
}

#' Format diagnostic summary for display
#' @param diagnostics Result from generate_diagnostic_report()
#' @return Data frame formatted for table display
format_diagnostic_summary <- function(diagnostics) {
  if (is.null(diagnostics) || (!is.null(diagnostics$status) && diagnostics$status == "No model available")) {
    return(data.frame(
      Metric = "Status",
      Value = "No model available",
      Status = "N/A"
    ))
  }

  conv <- diagnostics$convergence

  data.frame(
    Metric = c(
      "Overall Convergence",
      "Max Rhat",
      "Min Bulk ESS",
      "Min Tail ESS",
      "Divergent Transitions",
      "Number of Observations"
    ),
    Value = c(
      if (conv$converged) "Passed" else "Failed",
      sprintf("%.3f", conv$rhat_max),
      sprintf("%.0f", conv$ess_min_bulk),
      sprintf("%.0f", conv$ess_min_tail),
      sprintf("%.0f", conv$n_divergences),
      sprintf("%.0f", diagnostics$n_observations)
    ),
    Status = c(
      if (conv$converged) "Good" else "Warning",
      if (conv$rhat_ok) "Good" else "Warning",
      if (conv$ess_ok) "Good" else "Warning",
      if (conv$ess_ok) "Good" else "Warning",
      if (conv$divergent_ok) "Good" else "Warning",
      "Info"
    ),
    stringsAsFactors = FALSE
  )
}
