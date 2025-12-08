# ============================================================================
# Module: Rt Analysis Tab
# Purpose: Reproduction number estimation and forecasting
# ============================================================================

#' Rt Analysis Tab UI
#' @param id Module namespace ID
#' @return nav_panel UI element
rtAnalysisUI <- function(id) {

  ns <- NS(id)

  nav_panel(
    "Rt Analysis",
    icon = icon("chart-line"),

    div(
      class = "container-fluid mt-4",

      # Page Header
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h2(class = "section-header", "Reproduction Number (Rt) Analysis"),
          p(class = "text-muted",
            "Real-time estimation of the effective reproduction number for respiratory pathogens. ",
            "Rt > 1 indicates epidemic growth; Rt < 1 indicates decline."
          )
        )
      ),

      # Controls Row
      div(
        class = "row mb-4",
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Select Pathogen"),
            selectInput(
              ns("rt_pathogen"),
              label = NULL,
              choices = c("H3N2 (Influenza)" = "H3N2",
                          "RSV" = "RSV",
                          "COVID-19" = "COVID19"),
              selected = "H3N2"
            )
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Date Range"),
            dateRangeControlUI(ns("date_range"), label = NULL)
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Current Rt Status"),
            uiOutput(ns("rt_current_status"))
          )
        )
      ),

      # Fallback Banner (shows when using alternate data sources)
      div(
        class = "row mb-2",
        div(class = "col-12", uiOutput(ns("rt_fallback_banner")))
      ),

      # Rt Time Series Plot
      div(
        class = "row g-4",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Rt Time Series with 95% Credible Interval"),
            plotlyOutput(ns("rt_timeseries_plot"), height = "400px")
          )
        )
      ),

      # Forecast Section
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "4-Week Case Forecast"),
            plotlyOutput(ns("forecast_plot"), height = "350px")
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Forecast Summary"),
            uiOutput(ns("forecast_summary_table"))
          )
        )
      ),

      # AI Insights Panel
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h4(class = "section-header mb-0",
                 icon("robot", class = "me-2"), "AI Insights"),
              div(
                class = "d-flex gap-2",
                actionButton(
                  ns("explain_rt_btn"),
                  label = "Explain Rt Value",
                  icon = icon("lightbulb"),
                  class = "btn btn-outline-primary btn-sm"
                ),
                actionButton(
                  ns("explain_forecast_btn"),
                  label = "Explain Forecast",
                  icon = icon("chart-line"),
                  class = "btn btn-outline-info btn-sm"
                )
              )
            ),
            div(
              id = ns("ai_insights_panel"),
              class = "p-3 bg-light rounded",
              uiOutput(ns("ai_insights_content"))
            )
          )
        )
      ),

      # All Pathogens Summary Table
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Multi-Pathogen Rt Summary"),
            tableOutput(ns("rt_summary_table"))
          )
        )
      )
    )
  )
}

#' Rt Analysis Tab Server
#' @param id Module namespace ID
#' @param timeline_data Reactive timeline data for date filtering
rtAnalysisServer <- function(id, timeline_data) {
  moduleServer(id, function(input, output, session) {

    # Date range filter
    date_filter <- dateRangeControlServer("date_range", timeline_data)

    # AI insights content storage
    ai_insights_content_val <- reactiveVal(NULL)

    # Reactive: Store fallback status for banner
    fallback_status <- reactiveVal(list(has_fallback = FALSE, source_description = NULL, fallback_pct = 0))

    # Reactive: Get Rt estimates for selected pathogen (WITH FALLBACK)
    rt_data <- reactive({
      req(input$rt_pathogen)
      tryCatch({
        result <- get_rt_for_pathogen_with_fallback(input$rt_pathogen)

        # Update fallback status for banner display
        fallback_status(list(
          has_fallback = result$has_fallback %||% FALSE,
          source_description = result$source_description %||% "Primary sources",
          fallback_pct = result$fallback_pct %||% 0,
          source_coverage = result$source_coverage %||% list()
        ))

        result
      }, error = function(e) {
        fallback_status(list(has_fallback = FALSE, source_description = NULL, fallback_pct = 0))
        list(
          pathogen = input$rt_pathogen,
          rt_estimates = NULL,
          current_rt = list(value = NA, lower = NA, upper = NA, trend = "unknown", phase = "unknown"),
          error = e$message
        )
      })
    })

    # Reactive: Get forecast for selected pathogen
    forecast_data <- reactive({
      req(input$rt_pathogen)
      tryCatch({
        get_forecast_for_pathogen(input$rt_pathogen, horizon = 4)
      }, error = function(e) {
        list(pathogen = input$rt_pathogen, forecast = NULL, error = e$message)
      })
    })

    # Current Rt Status Display
    output$rt_current_status <- renderUI({
      rt_result <- rt_data()
      current <- rt_result$current_rt

      if (is.na(current$value)) {
        return(div(
          class = "text-center p-4",
          tags$i(class = "fas fa-exclamation-triangle fa-2x text-warning"),
          tags$p(class = "mt-2", "Insufficient data for Rt estimation")
        ))
      }

      phase_class <- switch(current$phase, "growing" = "severity-high", "declining" = "severity-low", "severity-moderate")
      trend_icon <- switch(current$trend, "increasing" = "fa-arrow-up text-danger", "decreasing" = "fa-arrow-down text-success", "fa-minus text-secondary")

      div(
        class = "row",
        div(class = "col-md-4",
          div(class = paste("kpi-card", phase_class),
            div(class = "kpi-label", "Current Rt"),
            div(class = "kpi-value", sprintf("%.2f", current$value)),
            div(class = "kpi-change", sprintf("95%% CrI: %.2f - %.2f", current$lower, current$upper))
          )
        ),
        div(class = "col-md-4",
          div(class = "kpi-card",
            div(class = "kpi-label", "Epidemic Phase"),
            div(class = "kpi-value", tools::toTitleCase(current$phase)),
            div(class = "kpi-change", tags$i(class = paste("fas", trend_icon)), paste(" Trend:", tools::toTitleCase(current$trend)))
          )
        ),
        div(class = "col-md-4",
          div(class = "kpi-card",
            div(class = "kpi-label", "Interpretation"),
            div(style = "font-size: 0.9rem;",
              if (current$value > 1) "Epidemic growing - cases expected to increase"
              else if (current$value < 1) "Epidemic declining - cases expected to decrease"
              else "Epidemic stable - cases roughly constant"
            )
          )
        )
      )
    })

    # Rt Time Series Plot
    output$rt_timeseries_plot <- renderPlotly({
      rt_result <- rt_data()
      if (is.null(rt_result$rt_estimates)) {
        return(plotly_empty() |> layout(title = list(text = "Insufficient data for Rt estimation", y = 0.5)))
      }

      rt_df <- rt_result$rt_estimates

      p <- ggplot(rt_df, aes(x = date)) +
        geom_ribbon(aes(ymin = rt_lower, ymax = rt_upper), fill = "#E85D4C", alpha = 0.2) +
        geom_line(aes(y = rt_mean), color = "#E85D4C", linewidth = 1.2) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "#6B7280", linewidth = 0.8) +
        annotate("text", x = min(rt_df$date), y = 1.05, label = "Rt = 1 (threshold)", hjust = 0, size = 3, color = "#6B7280") +
        scale_y_continuous(limits = c(0, max(rt_df$rt_upper, 3))) +
        labs(x = NULL, y = "Reproduction Number (Rt)") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "#E5E7EB"),
          panel.grid.minor = element_blank()
        )

      ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
    })

    # Forecast Plot
    output$forecast_plot <- renderPlotly({
      forecast_result <- forecast_data()
      if (is.null(forecast_result$forecast)) {
        return(plotly_empty() |> layout(title = list(text = "Insufficient data for forecasting", y = 0.5)))
      }

      historical <- forecast_result$historical_cases
      forecast_df <- forecast_result$forecast

      if (is.null(historical) || nrow(historical) < 3) {
        return(plotly_empty() |> layout(title = list(text = "Insufficient historical data", y = 0.5)))
      }

      historical_plot <- tail(historical, 12) |> rename(date = dates, cases = I) |> mutate(type = "Observed")
      forecast_plot <- forecast_df |>
        select(date, predicted_cases, lower_50, upper_50, lower_80, upper_80, lower_95, upper_95) |>
        rename(cases = predicted_cases) |> mutate(type = "Forecast")

      p <- ggplot() +
        geom_ribbon(data = forecast_plot, aes(x = date, ymin = lower_95, ymax = upper_95), fill = "#0D9488", alpha = 0.15) +
        geom_ribbon(data = forecast_plot, aes(x = date, ymin = lower_80, ymax = upper_80), fill = "#0D9488", alpha = 0.25) +
        geom_ribbon(data = forecast_plot, aes(x = date, ymin = lower_50, ymax = upper_50), fill = "#0D9488", alpha = 0.35) +
        geom_line(data = historical_plot, aes(x = date, y = cases), color = "#374151", linewidth = 1) +
        geom_point(data = historical_plot, aes(x = date, y = cases), color = "#374151", size = 2) +
        geom_line(data = forecast_plot, aes(x = date, y = cases), color = "#0D9488", linewidth = 1.2, linetype = "dashed") +
        geom_point(data = forecast_plot, aes(x = date, y = cases), color = "#0D9488", size = 3) +
        labs(x = NULL, y = "Cases") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "transparent", color = NA), panel.background = element_rect(fill = "transparent", color = NA))

      ggplotly(p, tooltip = c("x", "y")) |> config(displayModeBar = FALSE)
    })

    # Forecast Summary Table
    output$forecast_summary_table <- renderUI({
      forecast_result <- forecast_data()
      if (is.null(forecast_result$forecast)) return(div(class = "text-center p-4", "No forecast available"))

      forecast_df <- forecast_result$forecast

      div(
        class = "table-responsive",
        tags$table(class = "table table-sm",
          tags$thead(tags$tr(tags$th("Week"), tags$th("Predicted Cases"), tags$th("80% Interval"), tags$th("Rt Used"))),
          tags$tbody(
            lapply(1:nrow(forecast_df), function(i) {
              row <- forecast_df[i, ]
              tags$tr(
                tags$td(paste("Week", row$forecast_week)),
                tags$td(tags$strong(format(row$predicted_cases, big.mark = ","))),
                tags$td(sprintf("%s - %s", format(row$lower_80, big.mark = ","), format(row$upper_80, big.mark = ","))),
                tags$td(sprintf("%.2f", row$rt_used))
              )
            })
          )
        ),
        tags$hr(),
        tags$p(class = "text-muted small", tags$strong("Method: "), "Rt-renewal equation with negative binomial uncertainty")
      )
    })

    # Multi-Pathogen Rt Summary Table (WITH FALLBACK)
    output$rt_summary_table <- renderTable({
      tryCatch({
        all_rt <- get_rt_all_pathogens_with_fallback()
        summarize_rt_table(all_rt)
      }, error = function(e) {
        data.frame(Pathogen = c("H3N2", "RSV", "COVID19"), `Current Rt` = rep("N/A", 3),
                   Phase = rep("Unknown", 3), Trend = rep("Unknown", 3), check.names = FALSE)
      })
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # Fallback banner output for Rt analysis
    output$rt_fallback_banner <- renderUI({
      status <- fallback_status()
      if (!status$has_fallback || status$fallback_pct < 1) {
        return(NULL)
      }

      div(
        class = "fallback-banner alert alert-info d-flex align-items-center mb-3",
        icon("info-circle", class = "me-2"),
        span(
          sprintf("%.0f%% of data from alternate signals: %s",
                  status$fallback_pct,
                  status$source_description %||% "Fallback sources")
        ),
        tags$small(class = "ms-2 text-muted", "(Official surveillance data temporarily unavailable)")
      )
    })

    # AI Insights - placeholder
    output$ai_insights_content <- renderUI({
      content <- ai_insights_content_val()
      if (is.null(content)) {
        div(class = "text-muted", icon("info-circle"), " Click a button above to generate AI insights about the Rt analysis.")
      } else {
        HTML(content)
      }
    })

    # Return selected pathogen and date filter for potential parent use
    return(list(
      date_filter = date_filter,
      selected_pathogen = reactive({ input$rt_pathogen }),
      rt_data = rt_data,
      forecast_data = forecast_data
    ))
  })
}
