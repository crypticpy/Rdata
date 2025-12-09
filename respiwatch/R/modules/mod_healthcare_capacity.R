# =============================================================================
# Healthcare Capacity Module
# =============================================================================

healthcareCapacityUI <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Healthcare Capacity",
    icon = icon("hospital"),
    
    div(
      class = "container-fluid mt-4",
      
      # Header
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h2(class = "section-header", icon("hospital"), " Healthcare Capacity Dashboard"),
          p(class = "text-muted",
            "Monitor hospital and ICU capacity with surge forecasting. ",
            "Alerts trigger when capacity approaches critical thresholds.")
        )
      ),

      # Data Source Callout - CRITICAL for healthcare data transparency
      div(
        class = "row mb-3",
        div(class = "col-12", uiOutput(ns("data_source_callout")))
      ),

      # Controls Row
      div(
        class = "row mb-4",
        div(
          class = "col-md-3",
          selectInput(ns("pathogen"), "Pathogen",
                      choices = c("All Combined" = "all",
                                  "Influenza H3N2" = "H3N2",
                                  "RSV" = "RSV",
                                  "COVID-19" = "COVID19"),
                      selected = "all")
        ),
        div(
          class = "col-md-3",
          div(
            h5("Data Range"),
            dateRangeControlUI(ns("date_range"), label = NULL)
          )
        ),
        div(
          class = "col-md-3",
          sliderInput(ns("horizon"), "Forecast Horizon (Weeks)",
                      min = 1, max = 8, value = 4, step = 1)
        ),
        div(
          class = "col-md-3 pt-4",
          actionButton(ns("generate"), "Generate Forecast",
                       class = "btn btn-primary",
                       icon = icon("sync"))
        )
      ),
      
      # Capacity Gauges Row
      div(
        class = "row g-4",
        # Hospital Capacity Gauge
        div(
          class = "col-md-6",
          div(
            class = "chart-container text-center",
            chart_header_with_code(
              title = "Hospital Capacity",
              ns = ns,
              chart_id = "hospital_gauge",
              subtitle = "Current bed utilization"
            ),
            uiOutput(ns("hospital_gauge")),
            p(class = "text-muted mt-2", uiOutput(ns("hospital_status_text")))
          )
        ),
        # ICU Capacity Gauge
        div(
          class = "col-md-6",
          div(
            class = "chart-container text-center",
            chart_header_with_code(
              title = "ICU Capacity",
              ns = ns,
              chart_id = "icu_gauge",
              subtitle = "Critical care bed utilization"
            ),
            uiOutput(ns("icu_gauge")),
            p(class = "text-muted mt-2", uiOutput(ns("icu_status_text")))
          )
        )
      ),
      
      # Utilization Timeline Chart
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            chart_header_with_code(
              title = "Capacity Utilization Forecast",
              ns = ns,
              chart_id = "capacity_timeline",
              subtitle = "Hospital and ICU utilization trends"
            ),
            plotlyOutput(ns("timeline_plot"), height = "350px")
          )
        )
      ),
      
      # Hospitalization Forecast & Surge Alerts Row
      div(
        class = "row g-4 mt-2",
        # Hospitalization Forecast
        div(
          class = "col-md-8",
          div(
            class = "chart-container",
            chart_header_with_code(
              title = "Projected Hospitalizations",
              ns = ns,
              chart_id = "hospitalization_forecast",
              subtitle = "Daily admissions and occupancy forecast"
            ),
            plotlyOutput(ns("hospitalization_plot"), height = "300px")
          )
        ),
        # Surge Alerts
        div(
          class = "col-md-4",
          div(
            class = "chart-container",
            h4(class = "section-header", icon("exclamation-triangle"), " Surge Alerts"),
            uiOutput(ns("surge_alerts"))
          )
        )
      ),
      
      # Surge Timing Summary
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", icon("clock"), " Surge Timing Predictions"),
            tableOutput(ns("surge_timing_table"))
          )
        )
      )
    )
  )
}

healthcareCapacityServer <- function(id, timeline_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Date range filter
    date_filter <- dateRangeControlServer("date_range", timeline_data)

    # Reactive value to store capacity forecast results
    capacity_result <- reactiveVal(NULL)

    # Data Source Callout - Shows HHS data is historical
    output$data_source_callout <- renderUI({
      # Get healthcare capacity metadata from database
      sources <- get_healthcare_source_metadata()

      # Use amber/warning variant for historical data
      div(
        class = "data-source-callout historical-warning",
        div(
          class = "callout-header d-flex align-items-center mb-2",
          icon("database", class = "me-2 text-teal"),
          tags$h6(class = "mb-0 fw-bold", "Data Sources")
        ),
        tags$ul(
          class = "source-list list-unstyled mb-0",
          lapply(sources, function(src) {
            start_fmt <- if (!is.null(src$start_date) && !is.na(src$start_date)) {
              format(as.Date(src$start_date), "%b %Y")
            } else "N/A"

            end_fmt <- if (!is.null(src$end_date) && !is.na(src$end_date)) {
              format(as.Date(src$end_date), "%b %Y")
            } else "N/A"

            tags$li(
              class = "source-item status-historical",
              tags$span(class = "source-name fw-medium", src$name),
              tags$span(class = "source-dates text-muted ms-2",
                sprintf("(%s - %s)", start_fmt, end_fmt)
              ),
              tags$span(class = "badge bg-warning text-dark ms-2", "Historical")
            )
          })
        ),
        tags$div(
          class = "source-note mt-2 pt-2 border-top",
          icon("info-circle", class = "me-1 text-info"),
          tags$small(class = "text-muted fst-italic",
            "Federal hospital reporting requirements ended May 2024. ",
            "This data represents the last available reporting period."
          )
        ),
        tags$div(
          class = "last-updated mt-2 pt-2 border-top",
          icon("clock", class = "me-1 text-muted"),
          tags$small(class = "text-muted",
            sprintf("Data fetched: %s", format(Sys.time(), "%b %d, %Y"))
          )
        ),
        div(
          class = "learn-more mt-2",
          tags$a(
            href = "#",
            class = "text-decoration-none small",
            onclick = "Shiny.setInputValue('nav_to_about_data', Math.random())",
            icon("arrow-right", class = "me-1"),
            "Learn more about our data"
          )
        )
      )
    })
    
    # Generate capacity forecast when button clicked
    observeEvent(input$generate, {
      req(input$pathogen, input$horizon)
      
      # UI Loading State
      shinyjs::disable("generate")
      updateActionButton(session, "generate",
                         label = "Processing...",
                         icon = icon("spinner", class = "fa-spin"))
      
      # Ensure reset happens even on error
      on.exit({
        shinyjs::enable("generate")
        updateActionButton(session, "generate",
                           label = "Generate Forecast",
                           icon = icon("sync"))
      })
      
      showNotification("Generating capacity forecast...", type = "message", duration = 2)
      
      result <- tryCatch({
        get_capacity_forecast(
          pathogen_code = input$pathogen,
          horizon = input$horizon
        )
      }, error = function(e) {
        log_error(sprintf("Capacity forecast error: %s", e$message), category = "healthcare")
        list(status = "error", message = e$message)
      })
      
      capacity_result(result)
      
      if (!is.null(result) && result$status == "success") {
        showNotification("Capacity forecast generated successfully!", type = "message", duration = 2)
      } else if (!is.null(result) && result$status == "error") {
        error_msg <- if (!is.null(result$message)) as.character(result$message) else "Unknown error"
        showNotification(paste("Error:", error_msg), type = "error", duration = 4)
      }
    })
    
    # Hospital capacity gauge
    output$hospital_gauge <- renderUI({
      result <- capacity_result()
      if (is.null(result) || result$status != "success") {
        return(div(
          class = "d-flex justify-content-center align-items-center",
          style = "height: 200px; background: var(--bs-light); border-radius: 10px;",
          p(class = "text-muted", "Click 'Generate Forecast' to view hospital capacity")
        ))
      }
      
      summary <- get_capacity_summary(result)
      hospital <- summary$hospital
      
      # Determine color based on status
      gauge_color <- switch(
        hospital$status,
        "critical" = "#DC2626",
        "warning" = "#F59E0B",
        "elevated" = "#3B82F6",
        "#10B981"  # normal
      )
      
      div(
        class = "text-center",
        div(
          style = sprintf("
            width: 180px; height: 180px; margin: 0 auto;
            border-radius: 50%%;
            background: conic-gradient(%s 0deg %.1fdeg, #e5e7eb %.1fdeg 360deg);
            display: flex; align-items: center; justify-content: center;
          ", gauge_color, hospital$utilization * 3.6, hospital$utilization * 3.6),
          div(
            style = "width: 140px; height: 140px; border-radius: 50%; background: white;
                     display: flex; flex-direction: column; align-items: center; justify-content: center;",
            span(style = "font-size: 2rem; font-weight: bold;",
                 sprintf("%.0f%%", hospital$utilization)),
            span(class = "text-muted small", "Utilization")
          )
        ),
        div(class = "mt-2",
            span(class = sprintf("badge bg-%s",
                                 switch(hospital$status,
                                        "critical" = "danger",
                                        "warning" = "warning",
                                        "elevated" = "info",
                                        "success")),
                 toupper(hospital$status))
        )
      )
    })
    
    # ICU capacity gauge
    output$icu_gauge <- renderUI({
      result <- capacity_result()
      if (is.null(result) || result$status != "success") {
        return(div(
          class = "d-flex justify-content-center align-items-center",
          style = "height: 200px; background: var(--bs-light); border-radius: 10px;",
          p(class = "text-muted", "Click 'Generate Forecast' to view ICU capacity")
        ))
      }
      
      summary <- get_capacity_summary(result)
      icu <- summary$icu
      
      # Determine color based on status
      gauge_color <- switch(
        icu$status,
        "critical" = "#DC2626",
        "warning" = "#F59E0B",
        "elevated" = "#3B82F6",
        "#10B981"  # normal
      )
      
      div(
        class = "text-center",
        div(
          style = sprintf("
            width: 180px; height: 180px; margin: 0 auto;
            border-radius: 50%%;
            background: conic-gradient(%s 0deg %.1fdeg, #e5e7eb %.1fdeg 360deg);
            display: flex; align-items: center; justify-content: center;
          ", gauge_color, icu$utilization * 3.6, icu$utilization * 3.6),
          div(
            style = "width: 140px; height: 140px; border-radius: 50%; background: white;
                     display: flex; flex-direction: column; align-items: center; justify-content: center;",
            span(style = "font-size: 2rem; font-weight: bold;",
                 sprintf("%.0f%%", icu$utilization)),
            span(class = "text-muted small", "Utilization")
          )
        ),
        div(class = "mt-2",
            span(class = sprintf("badge bg-%s",
                                 switch(icu$status,
                                        "critical" = "danger",
                                        "warning" = "warning",
                                        "elevated" = "info",
                                        "success")),
                 toupper(icu$status))
        )
      )
    })
    
    # Hospital status text
    output$hospital_status_text <- renderUI({
      result <- capacity_result()
      if (is.null(result) || result$status != "success") {
        return(NULL)
      }
      
      summary <- get_capacity_summary(result)
      hospital <- summary$hospital
      
      div(
        p(class = "mb-1",
          sprintf("%s of %s beds occupied",
                  format(hospital$occupied, big.mark = ","),
                  format(hospital$capacity, big.mark = ","))),
        p(class = "text-muted small mb-0",
          sprintf("%s beds available", format(hospital$available, big.mark = ",")))
      )
    })
    
    # ICU status text
    output$icu_status_text <- renderUI({
      result <- capacity_result()
      if (is.null(result) || result$status != "success") {
        return(NULL)
      }
      
      summary <- get_capacity_summary(result)
      icu <- summary$icu
      
      div(
        p(class = "mb-1",
          sprintf("%s of %s beds occupied",
                  format(icu$occupied, big.mark = ","),
                  format(icu$capacity, big.mark = ","))),
        p(class = "text-muted small mb-0",
          sprintf("%s beds available", format(icu$available, big.mark = ",")))
      )
    })
    
    # Capacity timeline plot
    output$timeline_plot <- renderPlotly({
      result <- capacity_result()
      if (is.null(result) || result$status != "success") {
        return(plotly_empty() |>
                 layout(
                   title = list(text = "Generate forecast to view capacity timeline",
                                font = list(size = 14, color = "#6b7280")),
                   paper_bgcolor = "#f9fafb",
                   plot_bgcolor = "#f9fafb"
                 ))
      }
      
      plot_data <- result$timeline
      if (is.null(plot_data) || nrow(plot_data) == 0) {
        return(plotly_empty() |>
                 layout(title = "No timeline data available"))
      }
      
      # Create capacity timeline
      plot_ly(plot_data, x = ~date) |>
        add_trace(
          y = ~hospital_utilization,
          type = "scatter",
          mode = "lines+markers",
          name = "Hospital",
          line = list(color = "#3B82F6", width = 3),
          marker = list(size = 6)
        ) |>
        add_trace(
          y = ~icu_utilization,
          type = "scatter",
          mode = "lines+markers",
          name = "ICU",
          line = list(color = "#8B5CF6", width = 3),
          marker = list(size = 6)
        ) |>
        # Add threshold lines
        add_trace(
          y = rep(90, nrow(plot_data)),
          type = "scatter",
          mode = "lines",
          name = "Critical (90%)",
          line = list(color = "#DC2626", width = 2, dash = "dash")
        ) |>
        add_trace(
          y = rep(80, nrow(plot_data)),
          type = "scatter",
          mode = "lines",
          name = "Warning (80%)",
          line = list(color = "#F59E0B", width = 2, dash = "dot")
        ) |>
        layout(
          title = list(text = "Capacity Utilization Over Time", font = list(size = 16)),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Utilization (%)", range = c(0, 105)),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified",
          paper_bgcolor = "white",
          plot_bgcolor = "white"
        )
    })
    
    # Hospitalization forecast plot
    output$hospitalization_plot <- renderPlotly({
      result <- capacity_result()
      if (is.null(result) || result$status != "success") {
        return(plotly_empty() |>
                 layout(
                   title = list(text = "Generate forecast to view hospitalization projections",
                                font = list(size = 14, color = "#6b7280")),
                   paper_bgcolor = "#f9fafb",
                   plot_bgcolor = "#f9fafb"
                 ))
      }
      
      # Get hospitalization forecast data
      hosp_data <- result$hospitalization_forecast
      if (is.null(hosp_data) || nrow(hosp_data) == 0) {
        return(plotly_empty() |>
                 layout(title = "No hospitalization forecast available"))
      }
      
      plot_ly(hosp_data, x = ~date) |>
        add_trace(
          y = ~total_admissions,
          type = "bar",
          name = "Projected Admissions",
          marker = list(color = "#3B82F6")
        ) |>
        add_trace(
          y = ~total_occupancy,
          type = "scatter",
          mode = "lines",
          name = "Projected Occupancy",
          yaxis = "y2",
          line = list(color = "#DC2626", width = 3)
        ) |>
        layout(
          title = list(text = "Hospitalization Forecast", font = list(size = 16)),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Daily Admissions", side = "left"),
          yaxis2 = list(
            title = "Total Occupancy",
            overlaying = "y",
            side = "right",
            showgrid = FALSE
          ),
          legend = list(orientation = "h", y = -0.2),
          barmode = "group",
          paper_bgcolor = "white",
          plot_bgcolor = "white"
        )
    })
    
    # Surge alerts display
    output$surge_alerts <- renderUI({
      result <- capacity_result()
      if (is.null(result) || result$status != "success") {
        return(div(
          class = "alert alert-secondary",
          icon("info-circle"), " Generate forecast to view surge alerts"
        ))
      }
      
      alerts <- generate_surge_alerts(result)
      if (length(alerts) == 0) {
        return(div(
          class = "alert alert-success",
          icon("check-circle"), " ",
          strong("All Clear"), " - No surge alerts detected"
        ))
      }
      
      HTML(format_capacity_alerts_html(alerts))
    })
    
    # Surge timing table
    output$surge_timing_table <- renderTable({
      result <- capacity_result()
      if (is.null(result) || result$status != "success") {
        return(data.frame(
          Metric = character(),
          `Current Value` = character(),
          `Projected Peak` = character(),
          `Days to 90%` = character(),
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))
      }
      
      summary <- get_capacity_summary(result)
      
      # Calculate projected breach dates using raw numeric capacity values
      hospital_breach <- project_capacity_breach(
        result$hospitalization_forecast$total_occupancy,
        result$hospitalization_forecast$date,
        result$hospital_beds,  # Use numeric value, not the status list
        0.90
      )

      icu_breach <- project_capacity_breach(
        result$icu_forecast$total_icu_occupancy,
        result$icu_forecast$date,
        result$icu_beds,  # Use numeric value, not the status list
        0.90
      )
      
      data.frame(
        Metric = c("Hospital Beds", "ICU Beds"),
        `Current Utilization` = c(
          sprintf("%.1f%%", summary$hospital$utilization),
          sprintf("%.1f%%", summary$icu$utilization)
        ),
        `Available Beds` = c(
          format(summary$hospital$available, big.mark = ","),
          format(summary$icu$available, big.mark = ",")
        ),
        `Status` = c(
          toupper(summary$hospital$status),
          toupper(summary$icu$status)
        ),
        `Days to Critical` = c(
          if (is.null(hospital_breach$days_until_breach) || is.na(hospital_breach$days_until_breach))
            "N/A" else sprintf("%d days", hospital_breach$days_until_breach),
          if (is.null(icu_breach$days_until_breach) || is.na(icu_breach$days_until_breach))
            "N/A" else sprintf("%d days", icu_breach$days_until_breach)
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }, striped = TRUE, bordered = TRUE, hover = TRUE)

    # =========================================================================
    # CODE TRANSPARENCY MODAL HANDLERS
    # =========================================================================

    # Hospital Gauge Code Modal
    observeEvent(input$show_code_hospital_gauge, {
      snippet <- get_code_snippet("healthcare_gauge")
      show_code_modal(
        session = session,
        title = "Hospital Capacity Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)

    # ICU Gauge Code Modal
    observeEvent(input$show_code_icu_gauge, {
      snippet <- get_code_snippet("icu_gauge")
      show_code_modal(
        session = session,
        title = "ICU Capacity Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)

    # Capacity Timeline Code Modal
    observeEvent(input$show_code_capacity_timeline, {
      snippet <- get_code_snippet("capacity_timeline")
      show_code_modal(
        session = session,
        title = "Capacity Utilization Forecast Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)

    # Hospitalization Forecast Code Modal
    observeEvent(input$show_code_hospitalization_forecast, {
      snippet <- get_code_snippet("hospitalization_forecast")
      show_code_modal(
        session = session,
        title = "Projected Hospitalizations Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)
  })
}
