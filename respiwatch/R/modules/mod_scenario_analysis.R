# =============================================================================
# Scenario Analysis Module
# =============================================================================

scenarioAnalysisUI <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Scenario Analysis",
    icon = icon("sliders-h"),
    
    div(
      class = "container-fluid mt-4",
      
      # Page Header
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h2(class = "section-header", "What-If Scenario Analysis"),
          p(class = "text-muted",
            "Explore how different interventions could affect epidemic trajectories. ",
            "Compare mask mandates, social distancing, vaccination campaigns, and more."
          )
        )
      ),
      
      # Date Range Control Row
      div(
        class = "row mb-3",
        div(
          class = "col-md-4",
          dateRangeControlUI(ns("date_range"), "Historical Data Range")
        )
      ),

      # Fallback Banner (shows when using interpolated data)
      div(
        class = "row mb-2",
        div(class = "col-12", uiOutput(ns("scenario_fallback_banner")))
      ),

      # Controls Row
      div(
        class = "row mb-4",
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Pathogen"),
            selectInput(
              ns("pathogen"),
              label = NULL,
              choices = c("H3N2 (Influenza)" = "H3N2",
                          "RSV" = "RSV",
                          "COVID-19" = "COVID19"),
              selected = "H3N2"
            )
          )
        ),
        div(
          class = "col-md-4",
          div(
            class = "chart-container",
            h4(class = "section-header", "Select Scenarios"),
            checkboxGroupInput(
              ns("selection"),
              label = NULL,
              choices = c(
                "No Intervention (Baseline)" = "no_intervention",
                "Mask Mandate (-15% Rt)" = "mask_mandate",
                "Social Distancing (-25% Rt)" = "social_distancing",
                "School Closure (-20% Rt)" = "school_closure",
                "Light Lockdown (-40% Rt)" = "lockdown_light",
                "Full Lockdown (-60% Rt)" = "lockdown_full",
                "Vaccination Campaign (-30% Rt)" = "vaccination_boost",
                "Combined Moderate (-45% Rt)" = "combined_moderate"
              ),
              selected = c("no_intervention", "mask_mandate", "social_distancing")
            )
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "chart-container",
            h4(class = "section-header", "Forecast Horizon"),
            sliderInput(
              ns("horizon"),
              label = NULL,
              min = 4,
              max = 12,
              value = 8,
              step = 1,
              post = " weeks"
            ),
            actionButton(
              ns("run"),
              "Run Scenarios",
              class = "btn btn-primary w-100 mt-3",
              icon = icon("play")
            )
          )
        ),
        div(
          class = "col-md-2",
          div(
            class = "chart-container",
            h4(class = "section-header", "Current Rt"),
            uiOutput(ns("current_rt"))
          )
        )
      ),
      
      # Main Scenario Plot
      div(
        class = "row g-4",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Scenario Comparison: Projected Cases"),
            plotlyOutput(ns("comparison_plot"), height = "450px")
          )
        )
      ),
      
      # Impact Summary Table
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-md-8",
          div(
            class = "chart-container",
            h4(class = "section-header", "Scenario Impact Summary"),
            p(class = "text-muted small", "Cases averted compared to no-intervention baseline"),
            DTOutput(ns("impact_table"))
          )
        ),
        div(
          class = "col-md-4",
          div(
            class = "chart-container",
            h4(class = "section-header", "Key Insights"),
            uiOutput(ns("insights"))
          )
        )
      ),
      
      # Custom Scenario Builder
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Custom Scenario Builder"),
            div(
              class = "row",
              div(
                class = "col-md-4",
                textInput(ns("custom_name"), "Scenario Name", placeholder = "My Custom Intervention"),
                sliderInput(
                  ns("custom_rt_reduction"),
                  "Rt Reduction (%)",
                  min = 0,
                  max = 80,
                  value = 30,
                  step = 5,
                  post = "%"
                )
              ),
              div(
                class = "col-md-6",
                textAreaInput(ns("custom_desc"), "Description",
                              placeholder = "Describe the intervention mix...",
                              rows = 2),
                actionButton(
                  ns("add_custom"),
                  "Add to Comparison",
                  class = "btn btn-secondary mt-2",
                  icon = icon("plus")
                )
              )
            )
          )
        )
      )
    )
  )
}

scenarioAnalysisServer <- function(id, timeline_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Date range filter
    date_filter <- dateRangeControlServer("date_range", timeline_data)
    
    # Reactive value to store scenario results
    scenario_results <- reactiveVal(NULL)

    # Fallback banner for scenario analysis
    output$scenario_fallback_banner <- renderUI({
      data <- date_filter$data()
      if (is.null(data) || nrow(data) == 0) return(NULL)

      has_fallback <- "is_fallback" %in% names(data) && any(data$is_fallback, na.rm = TRUE)
      if (!has_fallback) return(NULL)

      fallback_pct <- round(sum(data$is_fallback, na.rm = TRUE) / nrow(data) * 100, 1)
      if (fallback_pct < 1) return(NULL)

      div(
        class = "fallback-banner alert alert-info d-flex align-items-center mb-3",
        icon("info-circle", class = "me-2"),
        span(sprintf("%.0f%% of baseline data uses interpolated values", fallback_pct)),
        tags$small(class = "ms-2 text-muted", "(Scenario projections based on gap-filled data)")
      )
    })
    
    # Run scenario analysis when button clicked
    observeEvent(input$run, {
      req(input$pathogen, input$selection)
      
      # UI Loading State
      shinyjs::disable("run")
      updateActionButton(session, "run",
                         label = "Running...",
                         icon = icon("spinner", class = "fa-spin"))
      
      # Ensure reset happens even on error
      on.exit({
        shinyjs::enable("run")
        updateActionButton(session, "run",
                           label = "Run Scenarios",
                           icon = icon("play"))
      })
      
      showNotification("Running scenario analysis...", type = "message", duration = 2)
      
      result <- tryCatch({
        get_scenario_projections(
          pathogen_code = input$pathogen,
          scenarios = input$selection,
          horizon = input$horizon
        )
      }, error = function(e) {
        error_msg <- safe_error_message(e)
        showNotification(paste("Error:", error_msg), type = "error", duration = 5)
        list(status = "error", message = error_msg)
      })
      
      scenario_results(result)
      
      if (!is.null(result) && result$status == "success") {
        showNotification(
          sprintf("Generated %d scenario projections for %s",
                  result$n_scenarios, input$pathogen),
          type = "message",
          duration = 3
        )
      }
    })
    
    # Display current Rt value
    output$current_rt <- renderUI({
      result <- scenario_results()
      if (is.null(result) || result$status != "success") {
        return(div(
          class = "text-center p-4",
          icon("chart-line", class = "fa-3x text-muted mb-3"),
          p(class = "text-muted", "Run analysis to see current Rt")
        ))
      }
      
      rt_value <- result$current_rt
      rt_color <- if (rt_value > 1.5) "#DC2626" else if (rt_value > 1.0) "#F59E0B" else "#10B981"
      rt_status <- if (rt_value > 1.5) "Critical" else if (rt_value > 1.0) "Growing" else "Declining"
      
      div(
        class = "text-center",
        h1(style = sprintf("color: %s; font-size: 3rem; margin-bottom: 0;", rt_color),
           sprintf("%.2f", rt_value)),
        p(class = "text-muted mb-2", "Current Rt"),
        span(class = sprintf("badge bg-%s",
                             if (rt_value > 1.5) "danger" else if (rt_value > 1.0) "warning" else "success"),
             rt_status),
        hr(),
        p(class = "small text-muted mb-0",
          sprintf("Pathogen: %s", result$pathogen)),
        p(class = "small text-muted",
          sprintf("Serial interval: %.1f days", result$serial_interval$mean))
      )
    })
    
    # Scenario comparison plot
    output$comparison_plot <- renderPlotly({
      result <- scenario_results()
      if (is.null(result) || result$status != "success") {
        return(plotly_empty() |>
                 layout(
                   title = list(text = "Select scenarios and click 'Run Scenario Analysis'",
                                font = list(size = 14)),
                   paper_bgcolor = "#f8f9fa",
                   plot_bgcolor = "#f8f9fa"
                 ))
      }
      
      # Prepare plot data
      plot_data <- prepare_scenario_plot_data(result, include_historical = TRUE)
      
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        return(plotly_empty() |> layout(title = "No data available"))
      }
      
      # Define colors for scenarios
      scenario_colors <- c(
        "no_intervention" = "#6B7280",
        "mask_mandate" = "#3B82F6",
        "social_distancing" = "#10B981",
        "school_closure" = "#F59E0B",
        "lockdown_light" = "#8B5CF6",
        "lockdown_full" = "#DC2626",
        "vaccination_boost" = "#06B6D4",
        "combined_moderate" = "#EC4899",
        "historical" = "#1F2937"
      )
      
      # Create plot
      p <- plot_ly()
      
      # Add historical data if present
      historical <- plot_data |> filter(type == "observed")
      if (nrow(historical) > 0) {
        p <- p |> add_trace(
          data = historical,
          x = ~date,
          y = ~predicted_cases,
          type = "scatter",
          mode = "lines+markers",
          name = "Observed",
          line = list(color = "#1F2937", width = 2),
          marker = list(color = "#1F2937", size = 6)
        )
      }
      
      # Add each scenario projection
      projections <- plot_data |> filter(type == "projection")
      for (sc in unique(projections$scenario)) {
        sc_data <- projections |> filter(scenario == sc)
        color <- scenario_colors[[sc]] %||% "#6B7280"
        
        # Add confidence ribbon
        p <- p |> add_trace(
          data = sc_data,
          x = ~date,
          y = ~upper_95,
          type = "scatter",
          mode = "lines",
          line = list(width = 0),
          showlegend = FALSE,
          hoverinfo = "skip",
          name = paste(sc, "upper")
        )
        
        p <- p |> add_trace(
          data = sc_data,
          x = ~date,
          y = ~lower_95,
          type = "scatter",
          mode = "lines",
          fill = "tonexty",
          fillcolor = paste0(color, "20"),
          line = list(width = 0),
          showlegend = FALSE,
          hoverinfo = "skip",
          name = paste(sc, "lower")
        )
        
        # Add main line
        p <- p |> add_trace(
          data = sc_data,
          x = ~date,
          y = ~predicted_cases,
          type = "scatter",
          mode = "lines",
          name = sc_data$scenario_name[1],
          line = list(color = color, width = 2),
          hovertemplate = paste(
            "<b>%{x}</b><br>",
            "Scenario: ", sc_data$scenario_name[1], "<br>",
            "Cases: %{y:,.0f}<br>",
            "<extra></extra>"
          )
        )
      }
      
      p |> layout(
        title = list(
          text = sprintf("Scenario Projections: %s", result$pathogen),
          font = list(size = 16)
        ),
        xaxis = list(title = "Date", gridcolor = "#E5E7EB"),
        yaxis = list(title = "Projected Cases", gridcolor = "#E5E7EB"),
        legend = list(
          orientation = "h",
          yanchor = "bottom",
          y = -0.3,
          xanchor = "center",
          x = 0.5
        ),
        hovermode = "x unified",
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      )
    })
    
    # Scenario impact table
    output$impact_table <- DT::renderDT({
      result <- scenario_results()
      if (is.null(result) || result$status != "success") {
        return(DT::datatable(
          data.frame(Message = "Run scenario analysis to see impact comparison"),
          options = list(dom = "t", paging = FALSE),
          rownames = FALSE
        ))
      }
      
      impact_table <- summarize_scenario_table(result)
      
      if (is.null(impact_table) || nrow(impact_table) == 0) {
        return(DT::datatable(
          data.frame(Message = "No impact data available"),
          options = list(dom = "t", paging = FALSE),
          rownames = FALSE
        ))
      }
      
      DT::datatable(
        impact_table,
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          )
        ),
        rownames = FALSE,
        class = "table table-striped table-hover"
      ) |>
        DT::formatStyle(
          "Scenario",
          fontWeight = "bold"
        ) |>
        DT::formatRound(
          columns = c("Rt Modifier"),
          digits = 2
        ) |>
        DT::formatRound(
          columns = c("Total Cases", "Peak Week", "Cases Averted"),
          digits = 0
        ) |>
        DT::formatStyle(
          "% Reduction",
          color = DT::styleInterval(
            cuts = c(0, 25, 50),
            values = c("#DC2626", "#F59E0B", "#10B981", "#047857")
          ),
          fontWeight = "bold"
        )
    })
    
    # Key insights
    output$insights <- renderUI({
      result <- scenario_results()
      if (is.null(result) || result$status != "success") {
        return(NULL)
      }
      
      impact <- calculate_scenario_impact(result)
      if (is.null(impact)) {
        return(NULL)
      }
      
      # Find best intervention
      best <- impact |>
        filter(scenario != "no_intervention") |>
        arrange(desc(percent_reduction)) |>
        slice(1)
      
      # Find most balanced (good reduction, less restrictive)
      balanced <- impact |>
        filter(scenario != "no_intervention", rt_modifier >= 0.6) |>
        arrange(desc(percent_reduction)) |>
        slice(1)
      
      baseline_cases <- impact$total_cases[impact$scenario == "no_intervention"]
      
      div(
        class = "mt-4",
        h5(icon("lightbulb"), " Key Insights"),
        div(class = "row",
            div(class = "col-md-4",
                div(class = "card border-success",
                    div(class = "card-body",
                        h6(class = "card-title text-success", icon("trophy"), " Most Effective"),
                        p(class = "card-text",
                          sprintf("%s could prevent %s cases (%.0f%% reduction)",
                                  best$scenario_name,
                                  format(best$cases_averted, big.mark = ","),
                                  best$percent_reduction))
                    )
                )
            ),
            div(class = "col-md-4",
                div(class = "card border-info",
                    div(class = "card-body",
                        h6(class = "card-title text-info", icon("balance-scale"), " Balanced Approach"),
                        p(class = "card-text",
                          if (nrow(balanced) > 0) {
                            sprintf("%s offers %.0f%% reduction with moderate restrictions",
                                    balanced$scenario_name, balanced$percent_reduction)
                          } else {
                            "No moderate interventions selected"
                          })
                    )
                )
            ),
            div(class = "col-md-4",
                div(class = "card border-warning",
                    div(class = "card-body",
                        h6(class = "card-title text-warning", icon("exclamation-triangle"), " Without Action"),
                        p(class = "card-text",
                          sprintf("Baseline trajectory: %s total cases over %d weeks",
                                  format(baseline_cases, big.mark = ","),
                                  result$horizon))
                    )
                )
            )
        )
      )
    })
    
    # Custom scenario handler
    observeEvent(input$add_custom, {
      req(input$custom_name, input$custom_rt_reduction)
      
      custom <- create_custom_scenario(
        name = input$custom_name,
        rt_reduction = input$custom_rt_reduction,
        description = sprintf("Custom: %s (%.0f%% Rt reduction)",
                              input$custom_name,
                              input$custom_rt_reduction)
      )
      
      showNotification(
        sprintf("Custom scenario '%s' created. Re-run analysis to include it.",
                input$custom_name),
        type = "message",
        duration = 3
      )
    })
  })
}
