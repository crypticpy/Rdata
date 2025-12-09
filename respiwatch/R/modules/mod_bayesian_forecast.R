# ============================================================================
# Module: Bayesian Forecast Tab
# Purpose: Bayesian epidemic forecasting with brms/Stan
# ============================================================================

#' Bayesian Forecast Tab UI
#' @param id Module namespace ID
#' @return nav_panel UI element
bayesianForecastUI <- function(id) {

  ns <- NS(id)

  nav_panel(
    "Bayesian Forecast",
    icon = icon("chart-area"),

    div(
      class = "container-fluid mt-4",

      # Page Header
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h2(class = "section-header", "Bayesian Epidemic Forecasting"),
          p(class = "text-muted",
            "Probabilistic forecasts using hierarchical Bayesian models (brms). ",
            "Multiple credible intervals show forecast uncertainty."
          )
        )
      ),

      # Data Source Callout
      div(
        class = "row mb-3",
        div(class = "col-12", uiOutput(ns("data_source_callout")))
      ),

      # Fallback Banner (shows when using interpolated data)
      div(
        class = "row mb-2",
        div(class = "col-12", uiOutput(ns("bayes_fallback_banner")))
      ),

      # Controls Row
      div(
        class = "row mb-4",
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Select Pathogen"),
            selectInput(ns("bayes_pathogen"), label = NULL,
              choices = c("H3N2 (Influenza)" = "H3N2", "RSV" = "RSV", "COVID-19" = "COVID19"),
              selected = "H3N2"
            ),
            actionButton(ns("run_bayesian_model"), "Generate Forecast",
                        class = "btn btn-primary w-100 mt-2", icon = icon("play"))
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Training Data Range"),
            dateRangeControlUI(ns("date_range"), label = NULL, default_days = 120)
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Model Status"),
            uiOutput(ns("bayes_model_status"))
          )
        )
      ),

      # Main Forecast Plot
      div(
        class = "row g-4",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            chart_header_with_code(
              title = "Bayesian Forecast with Credible Intervals",
              ns = ns,
              chart_id = "bayesian_forecast",
              subtitle = "Probabilistic predictions from brms/Stan model"
            ),
            plotlyOutput(ns("bayes_forecast_plot"), height = "450px")
          )
        )
      ),

      # Forecast Details and Diagnostics
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Forecast Summary"),
            uiOutput(ns("bayes_forecast_summary"))
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Model Diagnostics"),
            uiOutput(ns("bayes_diagnostics_summary"))
          )
        )
      ),

      # Advanced Diagnostics Row
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h4(class = "section-header mb-0", icon("microscope"), " Advanced Diagnostics"),
              div(
                class = "d-flex gap-2 align-items-center",
                code_transparency_buttons(ns, "bayes_diagnostics"),
                actionButton(ns("toggle_diagnostics"), "Show Diagnostic Plots",
                            class = "btn btn-sm btn-outline-secondary", icon = icon("chart-line"))
              )
            ),
            shinyjs::hidden(
              div(
                id = ns("diagnostics_panel"),
                div(class = "row",
                  div(class = "col-md-6", h5("Trace Plots", class = "text-muted"), plotOutput(ns("bayes_trace_plot"), height = "300px")),
                  div(class = "col-md-6", h5("Posterior Density", class = "text-muted"), plotOutput(ns("bayes_density_plot"), height = "300px"))
                ),
                div(class = "row mt-3",
                  div(class = "col-md-6", h5("Posterior Predictive Check", class = "text-muted"), plotOutput(ns("bayes_ppc_plot"), height = "300px")),
                  div(class = "col-md-6", h5("Parameter Summary", class = "text-muted"), tableOutput(ns("bayes_param_table")))
                )
              )
            )
          )
        )
      ),

      # Ensemble Forecast Comparison Row
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h4(class = "section-header mb-0", icon("layer-group"), " Ensemble Forecast Comparison"),
              div(
                class = "d-flex gap-2 align-items-center",
                code_transparency_buttons(ns, "ensemble_comparison"),
                actionButton(ns("generate_ensemble"), "Generate Ensemble Forecast", class = "btn btn-primary", icon = icon("sync"))
              )
            ),
            p(class = "text-muted small mb-3", "Compare Rt-renewal, Bayesian, and weighted ensemble forecasts."),
            plotlyOutput(ns("ensemble_comparison_plot"), height = "400px"),
            div(class = "mt-4", h5(class = "text-muted", "Forecast Skill Scores"), tableOutput(ns("ensemble_skill_scores")))
          )
        )
      ),

      # All Pathogens Comparison
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            chart_header_with_code(
              title = "Multi-Pathogen Bayesian Forecast Comparison",
              ns = ns,
              chart_id = "bayes_all_pathogens",
              subtitle = "Compare forecasts across respiratory pathogens"
            ),
            tableOutput(ns("bayes_all_pathogens_table"))
          )
        )
      ),

      # Download Section
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Export Data"),
            downloadButton(ns("download_bayes_forecast"), "Download Forecast CSV", class = "btn btn-secondary"),
            tags$span(class = "ms-3 text-muted", "Export forecast data with credible intervals")
          )
        )
      )
    )
  )
}

#' Bayesian Forecast Tab Server
#' @param id Module namespace ID
#' @param timeline_data Reactive timeline data
bayesianForecastServer <- function(id, timeline_data) {
  moduleServer(id, function(input, output, session) {

    # Date range filter
    date_filter <- dateRangeControlServer("date_range", timeline_data, default_days = 120)

    # Data Source Callout
    output$data_source_callout <- renderUI({
      tryCatch({
        sources <- get_data_source_metadata()
        note <- "Forecasts generated using brms/Stan Bayesian models"
        data_source_callout(sources, note = note)
      }, error = function(e) {
        div(
          class = "data-source-callout",
          tags$div(
            class = "callout-header",
            icon("database"),
            tags$span("Data Sources", class = "ms-2 fw-semibold")
          ),
          tags$small(class = "text-muted", "Surveillance data + brms/Stan Bayesian forecasting")
        )
      })
    })

    # Reactive values
    bayes_result <- reactiveVal(NULL)

    # Fallback banner for Bayesian forecast
    output$bayes_fallback_banner <- renderUI({
      data <- date_filter$data()
      if (is.null(data) || nrow(data) == 0) return(NULL)

      # Check for fallback/interpolated data
      has_fallback <- "is_fallback" %in% names(data) && any(data$is_fallback, na.rm = TRUE)
      if (!has_fallback) return(NULL)

      fallback_pct <- round(sum(data$is_fallback, na.rm = TRUE) / nrow(data) * 100, 1)
      if (fallback_pct < 1) return(NULL)

      div(
        class = "fallback-banner alert alert-info d-flex align-items-center mb-3",
        icon("info-circle", class = "me-2"),
        span(
          sprintf("%.0f%% of training data uses interpolated values", fallback_pct)
        ),
        tags$small(class = "ms-2 text-muted", "(Model trained with gap-filled surveillance data)")
      )
    })
    bayes_running <- reactiveVal(FALSE)
    ensemble_result <- reactiveVal(NULL)
    ensemble_running <- reactiveVal(FALSE)
    diagnostics_visible <- reactiveVal(FALSE)

    # Toggle Advanced Diagnostics panel
    observeEvent(input$toggle_diagnostics, {
      current <- diagnostics_visible()
      diagnostics_visible(!current)

      if (!current) {
        shinyjs::show("diagnostics_panel")
        updateActionButton(session, "toggle_diagnostics",
                           label = "Hide Diagnostic Plots",
                           icon = icon("eye-slash"))
      } else {
        shinyjs::hide("diagnostics_panel")
        updateActionButton(session, "toggle_diagnostics",
                           label = "Show Diagnostic Plots",
                           icon = icon("chart-line"))
      }
    })

    # Run Bayesian model
    observeEvent(input$run_bayesian_model, {
      bayes_running(TRUE)
      updateActionButton(session, "run_bayesian_model", label = "Generating...", icon = icon("spinner", class = "fa-spin"))
      shinyjs::disable("run_bayesian_model")

      result <- tryCatch({
        dates <- date_filter$range()
        get_bayesian_forecast(
          pathogen_code = input$bayes_pathogen,
          horizon = 4,
          use_cache = TRUE,
          start_date = dates$start,
          end_date = dates$end
        )
      }, error = function(e) {
        list(pathogen = input$bayes_pathogen, forecast = NULL, error = e$message)
      })

      bayes_result(result)
      bayes_running(FALSE)
      updateActionButton(session, "run_bayesian_model", label = "Generate Forecast", icon = icon("play"))
      shinyjs::enable("run_bayesian_model")
    })

    # Model Status Display
    output$bayes_model_status <- renderUI({
      result <- bayes_result()

      if (bayes_running()) {
        return(div(class = "text-center p-4",
          div(class = "spinner-border text-primary", role = "status"),
          tags$p(class = "mt-2", "Fitting Bayesian model... This may take a few minutes.")
        ))
      }

      if (is.null(result)) {
        return(div(class = "row",
          div(class = "col-md-6",
            div(class = "kpi-card severity-moderate",
              div(class = "kpi-label", "Model Status"),
              div(class = "kpi-value", style = "font-size: 1.5rem;", "Ready"),
              div(class = "kpi-change", "Click 'Generate Forecast' to start")
            )
          ),
          div(class = "col-md-6",
            div(class = "kpi-card",
              div(class = "kpi-label", "Method"),
              div(style = "font-size: 0.9rem;", "Bayesian negative binomial regression with brms/Stan.")
            )
          )
        ))
      }

      if (!is.null(result$error)) {
        return(div(class = "alert alert-warning",
          tags$strong("Model Error: "), result$error,
          tags$br(), tags$small("Try using the Rt-based forecast on the Rt Analysis tab instead.")
        ))
      }

      div(class = "row",
        div(class = "col-md-4",
          div(class = "kpi-card severity-low",
            div(class = "kpi-label", "Status"),
            div(class = "kpi-value", style = "font-size: 1.5rem;", "Complete"),
            div(class = "kpi-change", "Forecast generated successfully")
          )
        ),
        div(class = "col-md-4",
          div(class = "kpi-card",
            div(class = "kpi-label", "Training Range"),
            div(class = "kpi-value", style = "font-size: 1.2rem;",
              if (!is.null(result$training_start) && !is.null(result$training_end)) {
                sprintf("%d Days", as.numeric(difftime(result$training_end, result$training_start, units = "days")))
              } else "Unknown"),
            div(class = "kpi-change", if (!is.null(result$training_start)) sprintf("%s to %s", format(result$training_start, "%b %d"), format(result$training_end, "%b %d")) else "Full History")
          )
        ),
        div(class = "col-md-4",
          div(class = "kpi-card",
            div(class = "kpi-label", "Generated"),
            div(class = "kpi-value", style = "font-size: 1.2rem;", format(Sys.time(), "%H:%M")),
            div(class = "kpi-change", format(Sys.Date(), "%b %d, %Y"))
          )
        )
      )
    })

    # Bayesian Forecast Plot
    output$bayes_forecast_plot <- renderPlotly({
      result <- bayes_result()

      if (bayes_running()) {
        return(plotly_empty() |> layout(title = list(text = "Generating Forecast...", y = 0.5)))
      }

      if (is.null(result) || is.null(result$forecast)) {
        return(plotly_empty() |> layout(title = list(text = "Click 'Generate Forecast' to create a Bayesian forecast", y = 0.5)))
      }

      tryCatch({
        forecast_df <- result$forecast
        historical <- result$historical_cases

        historical_plot <- if (!is.null(historical) && nrow(historical) > 0) {
          tail(historical, 12) |> select(date, cases) |> mutate(type = "Observed")
        } else data.frame(date = as.Date(character()), cases = numeric())

        forecast_plot <- forecast_df |> mutate(type = "Forecast")

        p <- ggplot() +
          geom_ribbon(data = forecast_plot, aes(x = date, ymin = lower_95, ymax = upper_95), fill = "#7C3AED", alpha = 0.1) +
          geom_ribbon(data = forecast_plot, aes(x = date, ymin = lower_80, ymax = upper_80), fill = "#7C3AED", alpha = 0.2) +
          geom_ribbon(data = forecast_plot, aes(x = date, ymin = lower_50, ymax = upper_50), fill = "#7C3AED", alpha = 0.3)

        if (nrow(historical_plot) > 0) {
          p <- p +
            geom_line(data = historical_plot, aes(x = date, y = cases), color = "#374151", linewidth = 1) +
            geom_point(data = historical_plot, aes(x = date, y = cases), color = "#374151", size = 2)
        }

        # Add dotted connector line between historical and forecast data
        if (nrow(historical_plot) > 0 && nrow(forecast_plot) > 0) {
          last_hist <- historical_plot[nrow(historical_plot), ]
          first_forecast <- forecast_plot[1, ]

          connector_df <- data.frame(
            x = last_hist$date,
            xend = first_forecast$date,
            y = last_hist$cases,
            yend = first_forecast$mean
          )

          p <- p + geom_segment(
            data = connector_df,
            aes(x = x, xend = xend, y = y, yend = yend),
            linetype = "dotted",
            color = "#7C3AED",
            linewidth = 1
          )
        }

        p <- p +
          geom_line(data = forecast_plot, aes(x = date, y = mean), color = "#7C3AED", linewidth = 1.2, linetype = "dashed") +
          geom_point(data = forecast_plot, aes(x = date, y = mean), color = "#7C3AED", size = 3) +
          labs(x = NULL, y = "Predicted Cases") +
          theme_minimal() +
          theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_rect(fill = "transparent", color = NA))

        ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
      }, error = function(e) {
        plotly_empty() |> layout(title = list(text = paste("Visualization error:", e$message), y = 0.5))
      })
    })

    # Forecast Summary
    output$bayes_forecast_summary <- renderUI({
      result <- bayes_result()
      if (is.null(result) || is.null(result$forecast)) return(div(class = "text-center p-4 text-muted", "No forecast available"))

      forecast_df <- result$forecast
      div(class = "table-responsive",
        tags$table(class = "table table-sm",
          tags$thead(tags$tr(tags$th("Week"), tags$th("Mean"), tags$th("50% CrI"), tags$th("80% CrI"), tags$th("95% CrI"))),
          tags$tbody(
            lapply(1:nrow(forecast_df), function(i) {
              row <- forecast_df[i, ]
              tags$tr(
                tags$td(paste("Week", row$forecast_week)),
                tags$td(tags$strong(format(round(row$mean), big.mark = ","))),
                tags$td(sprintf("%s - %s", format(round(row$lower_50), big.mark = ","), format(round(row$upper_50), big.mark = ","))),
                tags$td(sprintf("%s - %s", format(round(row$lower_80), big.mark = ","), format(round(row$upper_80), big.mark = ","))),
                tags$td(sprintf("%s - %s", format(round(row$lower_95), big.mark = ","), format(round(row$upper_95), big.mark = ",")))
              )
            })
          )
        ),
        tags$hr(),
        tags$p(class = "text-muted small", tags$strong("Model: "), "Bayesian negative binomial regression (brms/Stan)")
      )
    })

    # Diagnostics Summary
    output$bayes_diagnostics_summary <- renderUI({
      result <- bayes_result()
      if (is.null(result) || is.null(result$diagnostics)) return(div(class = "text-center p-4 text-muted", "Generate a forecast to see model diagnostics"))

      diag <- result$diagnostics
      div(
        tags$table(class = "table table-sm",
          tags$tbody(
            tags$tr(tags$td(tags$strong("Convergence")), tags$td(if (isTRUE(diag$convergence$converged)) span(class = "badge bg-success", "Passed") else span(class = "badge bg-warning", "Check Needed"))),
            tags$tr(tags$td(tags$strong("Max Rhat")), tags$td(sprintf("%.3f", diag$convergence$rhat_max %||% NA))),
            tags$tr(tags$td(tags$strong("Min Bulk ESS")), tags$td(sprintf("%.0f", diag$convergence$ess_min_bulk %||% NA))),
            tags$tr(tags$td(tags$strong("Divergences")), tags$td(sprintf("%.0f", diag$convergence$n_divergences %||% 0)))
          )
        ),
        tags$hr(),
        tags$p(class = "text-muted small", "Rhat < 1.01 and ESS > 400 indicate good convergence")
      )
    })

    # Diagnostic plots
    output$bayes_trace_plot <- renderPlot({
      result <- bayes_result()
      if (is.null(result) || is.null(result$model)) return(NULL)
      tryCatch({ plot_trace(result$model) }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 4, color = "gray50") + theme_void()
      })
    })

    output$bayes_density_plot <- renderPlot({
      result <- bayes_result()
      if (is.null(result) || is.null(result$model)) return(NULL)
      tryCatch({ plot_density(result$model) }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 4, color = "gray50") + theme_void()
      })
    })

    output$bayes_ppc_plot <- renderPlot({
      result <- bayes_result()
      if (is.null(result) || is.null(result$model)) return(NULL)
      tryCatch({ posterior_predictive_check(result$model, ndraws = 50) }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 4, color = "gray50") + theme_void()
      })
    })

    output$bayes_param_table <- renderTable({
      result <- bayes_result()
      if (is.null(result) || is.null(result$model)) return(data.frame(Message = "Generate forecast to see parameters"))
      tryCatch({
        summary_df <- get_model_summary(result$model)
        if (is.null(summary_df)) return(data.frame(Message = "Could not extract parameters"))
        summary_df |> select(Parameter, Estimate, Est.Error, `l-95% CI`, `u-95% CI`, Rhat, Converged) |> head(10)
      }, error = function(e) data.frame(Message = paste("Error:", e$message)))
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

    # All Pathogens Table
    output$bayes_all_pathogens_table <- renderTable({
      data.frame(
        Pathogen = c("H3N2", "RSV", "COVID-19"),
        `1-Week Forecast` = c("Use Generate button", "Use Generate button", "Use Generate button"),
        `4-Week Forecast` = c("for each pathogen", "for each pathogen", "for each pathogen"),
        Status = c("Ready", "Ready", "Ready"),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # Ensemble forecast
    observeEvent(input$generate_ensemble, {
      req(input$bayes_pathogen)

      # Start loading state
      ensemble_running(TRUE)
      shinyjs::disable("generate_ensemble")
      updateActionButton(session, "generate_ensemble",
                         label = "Generating...",
                         icon = icon("spinner", class = "fa-spin"))

      # Ensure reset happens even on error
      on.exit({
        ensemble_running(FALSE)
        shinyjs::enable("generate_ensemble")
        updateActionButton(session, "generate_ensemble",
                           label = "Generate Ensemble Forecast",
                           icon = icon("sync"))
      })

      showNotification("Generating ensemble forecast...", type = "message", duration = 3)

      result <- tryCatch({
        get_ensemble_forecast(pathogen_code = input$bayes_pathogen, horizon = 4)
      }, error = function(e) {
        error_msg <- safe_error_message(e)
        showNotification(paste("Error:", error_msg), type = "error", duration = 5)
        list(status = "error", message = error_msg)
      })

      ensemble_result(result)

      if (!is.null(result) && result$status == "success") {
        showNotification(
          sprintf("Ensemble forecast generated for %s with %d methods",
                  result$pathogen, result$n_methods_used),
          type = "message",
          duration = 3
        )
      }
    })

    output$ensemble_comparison_plot <- renderPlotly({
      result <- ensemble_result()
      if (is.null(result) || result$status != "success") {
        p <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Click 'Generate Ensemble Forecast' to compare methods", size = 5, color = "gray50") + theme_void()
        return(ggplotly(p))
      }

      tryCatch({
        plot_data <- prepare_ensemble_plot_data(result)
        if (is.null(plot_data) || nrow(plot_data) == 0) {
          p <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No forecast data available", size = 5, color = "gray50") + theme_void()
          return(ggplotly(p))
        }

        p <- ggplot(plot_data |> filter(type != "observed"), aes(x = date, y = predicted_cases, color = source)) +
          geom_line(linewidth = 1) +
          geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = source), alpha = 0.15, color = NA) +
          scale_color_manual(values = c("Ensemble" = "#2563eb", "rt_renewal" = "#16a34a", "bayesian" = "#dc2626")) +
          scale_fill_manual(values = c("Ensemble" = "#2563eb", "rt_renewal" = "#16a34a", "bayesian" = "#dc2626")) +
          labs(title = sprintf("Forecast Method Comparison: %s", result$pathogen), x = "Date", y = "Predicted Cases") +
          theme_minimal() + theme(legend.position = "bottom")

        ggplotly(p, tooltip = c("x", "y", "color")) |> layout(legend = list(orientation = "h", y = -0.15))
      }, error = function(e) {
        plotly_empty() |> layout(title = list(text = paste("Visualization error:", e$message), y = 0.5))
      })
    })

    output$ensemble_skill_scores <- renderTable({
      result <- ensemble_result()
      if (is.null(result) || result$status != "success") {
        return(data.frame(Method = "No data", Weight = "-", Forecast = "-"))
      }
      data.frame(
        Method = c("Ensemble", result$methods_used),
        Weight = c("Combined", sapply(result$methods_used, function(m) sprintf("%.0f%%", result$config$weights[[m]] * 100))),
        Status = rep("Available", 1 + length(result$methods_used)),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # Download handler
    output$download_bayes_forecast <- downloadHandler(
      filename = function() paste0("bayesian_forecast_", input$bayes_pathogen, "_", Sys.Date(), ".csv"),
      content = function(file) {
        result <- bayes_result()
        if (!is.null(result) && !is.null(result$forecast)) write.csv(result$forecast, file, row.names = FALSE)
        else write.csv(data.frame(message = "No forecast available"), file, row.names = FALSE)
      }
    )

    # =========================================================================
    # CODE TRANSPARENCY MODAL HANDLERS
    # =========================================================================

    # Bayesian Forecast Code Modal
    observeEvent(input$show_code_bayesian_forecast, {
      snippet <- get_code_snippet("bayesian_forecast")
      show_code_modal(
        session = session,
        title = "Bayesian Forecast Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)

    # Advanced Diagnostics Code Modal
    observeEvent(input$show_code_bayes_diagnostics, {
      snippet <- get_code_snippet("bayes_diagnostics")
      show_code_modal(
        session = session,
        title = "Bayesian Diagnostics Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)

    # Ensemble Comparison Code Modal
    observeEvent(input$show_code_ensemble_comparison, {
      snippet <- get_code_snippet("ensemble_comparison")
      show_code_modal(
        session = session,
        title = "Ensemble Forecast Comparison Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)

    # Multi-Pathogen Comparison Code Modal
    observeEvent(input$show_code_bayes_all_pathogens, {
      snippet <- get_code_snippet("bayes_all_pathogens")
      show_code_modal(
        session = session,
        title = "Multi-Pathogen Forecast Comparison Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)

    return(date_filter)
  })
}
