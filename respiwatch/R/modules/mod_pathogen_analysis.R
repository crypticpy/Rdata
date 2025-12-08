# ============================================================================
# Module: Pathogen Analysis Tab
# Purpose: Multi-pathogen comparison, vaccine effectiveness, vaccination impact
# ============================================================================

#' Pathogen Analysis Tab UI
#' @param id Module namespace ID
#' @param outbreak_data Static outbreak data for initial KPIs
#' @return nav_panel UI element
pathogenAnalysisUI <- function(id, outbreak_data) {

  ns <- NS(id)

  nav_panel(
    "Pathogen Analysis",
    icon = icon("virus"),

    div(
      class = "container-fluid mt-4",

      # Date Range Control Row
      div(
        class = "row mb-4",
        div(
          class = "col-md-4",
          dateRangeControlUI(ns("date_range"), "Pathogen Data Range")
        )
      ),

      # Fallback Banner (shows when using interpolated data)
      div(
        class = "row mb-2",
        div(class = "col-12", uiOutput(ns("pathogen_fallback_banner")))
      ),

      div(
        class = "row g-4",

        # Primary Pathogen Info
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Primary Pathogen: H3N2"),
            div(
              class = "row",
              div(
                class = "col-6",
                div(class = "kpi-card severity-high",
                    div(class = "kpi-label", "Dominant Strain"),
                    div(class = "kpi-value", style = "font-size: 1.5rem;",
                        outbreak_data$pathogen_analysis$primary_pathogen$dominant_strain),
                    div(class = "kpi-change", "Subclade K (J.2.4.1)")
                )
              ),
              div(
                class = "col-6",
                div(class = "kpi-card severity-high",
                    div(class = "kpi-label", "Global Prevalence"),
                    div(class = "kpi-value",
                        paste0(outbreak_data$pathogen_analysis$primary_pathogen$global_prevalence, "%")),
                    div(class = "kpi-change up", "Severity: HIGH")
                )
              )
            )
          )
        ),

        # Vaccine Effectiveness
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Vaccine Effectiveness by Strain"),
            plotlyOutput(ns("vaccine_effectiveness_chart"), height = "250px")
          )
        )
      ),

      # Comparative Charts
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Comparative Positivity Rates"),
            plotlyOutput(ns("comparative_positivity_chart"), height = "300px")
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Hospitalization Rates by Pathogen"),
            plotlyOutput(ns("comparative_hospitalization_chart"), height = "300px")
          )
        )
      ),

      # Multi-Pathogen Timeline
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Multi-Pathogen Surveillance Timeline"),
            plotlyOutput(ns("multi_pathogen_timeline"), height = "350px")
          )
        )
      ),

      # Cross-Pathogen Analysis Table
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Cross-Pathogen Analysis"),
            DT::DTOutput(ns("pathogen_comparison_table"))
          )
        )
      ),

      # Co-infection Patterns
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Co-Infection Patterns"),
            uiOutput(ns("coinfection_cards"))
          )
        )
      ),

      # Vaccination Impact Tracker
      div(
        class = "row g-4 mt-4",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h4(class = "section-header mb-0",
                 icon("syringe", class = "me-2"), "Vaccination Impact Tracker"),
              div(
                class = "d-flex gap-2",
                selectInput(
                  ns("vaccination_pathogen_select"),
                  label = NULL,
                  choices = c("Influenza A", "Influenza B", "RSV", "COVID-19"),
                  selected = "Influenza A",
                  width = "180px"
                ),
                actionButton(
                  ns("refresh_vaccination_data"),
                  label = NULL,
                  icon = icon("sync"),
                  class = "btn btn-outline-primary"
                )
              )
            ),

            # Vaccination KPIs
            div(
              class = "row g-3 mb-3",
              div(
                class = "col-md-3",
                div(class = "kpi-card",
                    div(class = "kpi-label", "Population Coverage"),
                    uiOutput(ns("vaccination_coverage_display")),
                    div(class = "kpi-change", "Weighted average")
                )
              ),
              div(
                class = "col-md-3",
                div(class = "kpi-card severity-low",
                    div(class = "kpi-label", "Current VE (Infection)"),
                    uiOutput(ns("vaccination_ve_display")),
                    div(class = "kpi-change", "With waning adjustment")
                )
              ),
              div(
                class = "col-md-3",
                div(class = "kpi-card severity-medium",
                    div(class = "kpi-label", "Cases Prevented"),
                    uiOutput(ns("prevented_cases_display")),
                    div(class = "kpi-change", "Estimated this season")
                )
              ),
              div(
                class = "col-md-3",
                div(class = "kpi-card severity-high",
                    div(class = "kpi-label", "Deaths Averted"),
                    uiOutput(ns("prevented_deaths_display")),
                    div(class = "kpi-change", "Estimated this season")
                )
              )
            ),

            # VE Details
            div(
              class = "row g-3",
              div(
                class = "col-md-6",
                div(
                  class = "p-3 bg-light rounded",
                  h5(class = "mb-3", icon("chart-pie", class = "me-2"), "VE by Outcome"),
                  uiOutput(ns("ve_by_outcome_display"))
                )
              ),
              div(
                class = "col-md-6",
                div(
                  class = "p-3 bg-light rounded",
                  h5(class = "mb-3", icon("virus-covid", class = "me-2"), "VE by Variant"),
                  uiOutput(ns("ve_by_variant_display"))
                )
              )
            ),

            div(
              class = "mt-3",
              uiOutput(ns("vaccination_impact_summary"))
            )
          )
        )
      )
    )
  )
}

#' Pathogen Analysis Tab Server
#' @param id Module namespace ID
#' @param outbreak_data Static outbreak data
#' @param timeline_data Reactive timeline data
#' @param pathogen_colors Named vector of pathogen colors
pathogenAnalysisServer <- function(id, outbreak_data, timeline_data, pathogen_colors) {
  moduleServer(id, function(input, output, session) {

    # Date range filter
    date_filter <- dateRangeControlServer("date_range", timeline_data)

    # Vaccination impact results
    vaccination_impact_result <- reactiveVal(NULL)

    # Fallback banner for pathogen analysis
    output$pathogen_fallback_banner <- renderUI({
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
          sprintf("%.0f%% of displayed data uses interpolated values", fallback_pct)
        ),
        tags$small(class = "ms-2 text-muted", "(Filling gaps in primary surveillance data)")
      )
    })

    # Calculate vaccination impact
    calculate_vaccination_impact <- function() {
      pathogen_ui <- input$vaccination_pathogen_select %||% "Influenza A"
      pathogen <- switch(
        pathogen_ui,
        "Influenza A" = "H3N2",
        "Influenza B" = "H3N2",
        "COVID-19" = "COVID-19",
        "RSV" = "RSV",
        "H3N2"
      )

      surveillance <- timeline_data()
      tryCatch({
        result <- get_vaccination_impact(surveillance_data = surveillance, pathogen = pathogen)
        vaccination_impact_result(result)
      }, error = function(e) {
        vaccination_impact_result(list(success = FALSE, error = e$message))
      })
    }

    observeEvent(input$vaccination_pathogen_select, calculate_vaccination_impact(), ignoreInit = FALSE)
    observeEvent(input$refresh_vaccination_data, calculate_vaccination_impact())

    # Vaccine Effectiveness Chart
    output$vaccine_effectiveness_chart <- renderPlotly({
      tryCatch({
        ve_data <- data.frame(
          strain = c("Overall", "H3N2 General", "Subclade K", "H1N1pdm09"),
          effectiveness = c(45, 35, 25, 65),
          lower = c(35, 25, 15, 55),
          upper = c(55, 45, 35, 75)
        )
        ve_data$strain <- factor(ve_data$strain, levels = rev(ve_data$strain))

        p <- ggplot(ve_data, aes(x = effectiveness, y = strain)) +
          geom_segment(aes(x = lower, xend = upper, y = strain, yend = strain),
                       color = "#E85D4C", linewidth = 2, alpha = 0.3) +
          geom_point(color = "#E85D4C", size = 5) +
          geom_text(aes(label = paste0(effectiveness, "%")), hjust = -0.5, color = "#1A1A2E") +
          xlim(0, 100) +
          labs(x = "Effectiveness (%)", y = NULL) +
          theme_minimal(base_family = "DM Sans") +
          theme(
            panel.grid.major.y = element_blank(),
            axis.text = element_text(color = "#1A1A2E"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA)
          )

        ggplotly(p, tooltip = c("x")) |>
          config(displayModeBar = FALSE) |>
          layout(margin = list(l = 100, r = 50, t = 20, b = 50))
      }, error = function(e) {
        plotly_empty() |> layout(title = list(text = paste("Visualization error:", e$message), y = 0.5))
      })
    })

    # Comparative Positivity Chart
    output$comparative_positivity_chart <- renderPlotly({
      filtered_data <- date_filter$data()
      if (is.null(filtered_data) || nrow(filtered_data) == 0) {
        return(plotly_empty() |> layout(title = list(text = "No data available")))
      }

      p <- ggplot(filtered_data, aes(x = date, y = positivity_rate, color = pathogen)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2.5) +
        scale_color_manual(values = pathogen_colors) +
        labs(x = NULL, y = "Positivity Rate (%)", color = "Pathogen") +
        theme_minimal(base_family = "DM Sans") +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )

      ggplotly(p, tooltip = c("x", "y", "color")) |>
        config(displayModeBar = FALSE) |>
        layout(legend = list(orientation = "h", y = -0.2))
    })

    # Comparative Hospitalization Chart
    output$comparative_hospitalization_chart <- renderPlotly({
      filtered_data <- date_filter$data()
      if (is.null(filtered_data) || nrow(filtered_data) == 0) {
        return(plotly_empty() |> layout(title = list(text = "No data available")))
      }

      # Aggregate to weekly data for cleaner grouped bar chart
      weekly_data <- filtered_data |>
        mutate(week = floor_date(date, "week")) |>
        group_by(week, pathogen) |>
        summarise(
          hospitalization_rate = mean(hospitalization_rate, na.rm = TRUE),
          .groups = "drop"
        )

      p <- ggplot(weekly_data, aes(x = week, y = hospitalization_rate, fill = pathogen)) +
        geom_col(position = position_dodge2(width = 0.9, preserve = "single", padding = 0.1), alpha = 0.8) +
        scale_fill_manual(values = pathogen_colors) +
        scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
        labs(x = NULL, y = "Hospitalization Rate (%)", fill = "Pathogen") +
        theme_minimal(base_family = "DM Sans") +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )

      ggplotly(p, tooltip = c("x", "y", "fill")) |>
        config(displayModeBar = FALSE) |>
        layout(legend = list(orientation = "h", y = -0.2))
    })

    # Multi-Pathogen Timeline
    output$multi_pathogen_timeline <- renderPlotly({
      filtered_data <- date_filter$data()
      if (is.null(filtered_data) || nrow(filtered_data) == 0) {
        return(plotly_empty() |> layout(title = list(text = "No data available")))
      }

      p <- ggplot(filtered_data, aes(x = date, y = case_numbers / 1000, fill = pathogen)) +
        geom_area(alpha = 0.7, position = "stack") +
        scale_fill_manual(values = pathogen_colors) +
        scale_y_continuous(labels = scales::comma_format(suffix = "K")) +
        labs(x = NULL, y = "Estimated Cases (thousands)", fill = "Pathogen") +
        theme_minimal(base_family = "DM Sans") +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )

      ggplotly(p, tooltip = c("x", "y", "fill")) |>
        config(displayModeBar = FALSE) |>
        layout(legend = list(orientation = "h", y = -0.15))
    })

    # Pathogen Comparison Table
    output$pathogen_comparison_table <- DT::renderDT({
      cross_analysis <- outbreak_data$pathogen_analysis$pathogen_cross_analysis
      if (is.null(cross_analysis)) {
        return(DT::datatable(data.frame(Message = "No data available"), options = list(dom = 't')))
      }

      if (is.data.frame(cross_analysis)) {
        pathogens_compared <- if (is.list(cross_analysis$pathogens_compared)) {
          sapply(cross_analysis$pathogens_compared, function(x) paste(unlist(x), collapse = " vs "))
        } else {
          as.character(cross_analysis$pathogens_compared)
        }
        comparison_df <- data.frame(
          `Pathogens Compared` = pathogens_compared,
          `Analysis Type` = gsub("_", " ", cross_analysis$analysis_type),
          `Key Findings` = cross_analysis$findings,
          `Confidence` = cross_analysis$confidence_level,
          check.names = FALSE
        )
      } else {
        comparison_df <- data.frame(
          `Pathogens Compared` = sapply(cross_analysis, function(x) paste(x$pathogens_compared, collapse = " vs ")),
          `Analysis Type` = sapply(cross_analysis, function(x) gsub("_", " ", x$analysis_type)),
          `Key Findings` = sapply(cross_analysis, function(x) x$findings),
          `Confidence` = sapply(cross_analysis, function(x) x$confidence_level),
          check.names = FALSE
        )
      }

      DT::datatable(comparison_df, options = list(dom = 't', pageLength = 10), rownames = FALSE)
    })

    # Co-infection Cards
    output$coinfection_cards <- renderUI({
      co_infection <- outbreak_data$pathogen_analysis$co_infection_patterns
      if (is.null(co_infection)) return(div(class = "text-muted", "No data available"))

      if (is.data.frame(co_infection)) {
        cards <- lapply(seq_len(nrow(co_infection)), function(i) {
          pathogens <- if (is.list(co_infection$pathogens)) {
            paste(unlist(co_infection$pathogens[[i]]), collapse = " + ")
          } else co_infection$pathogens[i]
          geo_dist <- if (is.list(co_infection$geographic_distribution)) {
            paste(unlist(co_infection$geographic_distribution[[i]]), collapse = ", ")
          } else co_infection$geographic_distribution[i]

          div(class = "col-md-4",
            div(class = "kpi-card",
              div(class = "kpi-label", pathogens),
              div(class = "kpi-value", style = "font-size: 2rem;", paste0(co_infection$frequency[i], "%")),
              div(class = "kpi-change", paste("Impact:", gsub("_", " ", co_infection$severity_impact[i]))),
              tags$small(class = "text-muted d-block mt-2", paste("Regions:", geo_dist))
            )
          )
        })
      } else {
        cards <- lapply(co_infection, function(pattern) {
          div(class = "col-md-4",
            div(class = "kpi-card",
              div(class = "kpi-label", paste(pattern$pathogens, collapse = " + ")),
              div(class = "kpi-value", style = "font-size: 2rem;", paste0(pattern$frequency, "%")),
              div(class = "kpi-change", paste("Impact:", gsub("_", " ", pattern$severity_impact))),
              tags$small(class = "text-muted d-block mt-2", paste("Regions:", paste(pattern$geographic_distribution, collapse = ", ")))
            )
          )
        })
      }
      div(class = "row g-3", cards)
    })

    # Vaccination displays
    output$vaccination_coverage_display <- renderUI({
      result <- vaccination_impact_result()
      if (is.null(result) || !isTRUE(result$success)) return(div(class = "kpi-value", "--"))
      coverage <- (result$summary$average_coverage %||% result$prevention_impact$average_coverage %||% 0) * 100
      div(class = "kpi-value", sprintf("%.1f%%", coverage))
    })

    output$vaccination_ve_display <- renderUI({
      result <- vaccination_impact_result()
      if (is.null(result) || !isTRUE(result$success)) return(div(class = "kpi-value", "--"))
      ve <- (result$summary$average_ve %||% result$prevention_impact$average_effectiveness %||% 0) * 100
      div(class = "kpi-value", sprintf("%.0f%%", ve))
    })

    output$prevented_cases_display <- renderUI({
      result <- vaccination_impact_result()
      if (is.null(result) || !isTRUE(result$success)) return(div(class = "kpi-value", "--"))
      cases <- result$summary$cases_prevented %||% result$prevention_impact$total_cases_prevented %||% 0
      div(class = "kpi-value", format(round(cases), big.mark = ","))
    })

    output$prevented_deaths_display <- renderUI({
      result <- vaccination_impact_result()
      if (is.null(result) || !isTRUE(result$success)) return(div(class = "kpi-value", "--"))
      deaths <- result$summary$deaths_prevented %||% result$prevention_impact$total_deaths_prevented %||% 0
      div(class = "kpi-value", format(round(deaths), big.mark = ","))
    })

    output$ve_by_outcome_display <- renderUI({
      result <- vaccination_impact_result()
      if (is.null(result) || !isTRUE(result$success)) return(div(class = "text-muted", "No data available"))
      base_ve <- result$summary$average_ve %||% result$prevention_impact$average_effectiveness %||% 0.45
      div(class = "row g-2",
        div(class = "col-4 text-center", div(class = "small text-muted", "Infection"), div(class = "fw-bold text-primary", sprintf("%.0f%%", base_ve * 100))),
        div(class = "col-4 text-center", div(class = "small text-muted", "Hospitalization"), div(class = "fw-bold text-warning", sprintf("%.0f%%", min(base_ve * 1.3, 0.95) * 100))),
        div(class = "col-4 text-center", div(class = "small text-muted", "Death"), div(class = "fw-bold text-danger", sprintf("%.0f%%", min(base_ve * 1.5, 0.95) * 100)))
      )
    })

    output$ve_by_variant_display <- renderUI({
      result <- vaccination_impact_result()
      if (is.null(result) || !isTRUE(result$success) || is.null(result$ve_breakdown)) {
        return(div(class = "text-muted", "No variant data available"))
      }
      variants <- result$ve_breakdown
      infection_ve <- variants[variants$outcome == "infection", ]
      if (nrow(infection_ve) == 0) infection_ve <- variants[1:min(3, nrow(variants)), ]
      div(lapply(seq_len(min(3, nrow(infection_ve))), function(i) {
        v <- infection_ve[i, ]
        div(class = "d-flex justify-content-between align-items-center mb-2",
          span(v$variant), span(class = "badge bg-primary", sprintf("%.0f%%", (v$ve %||% 0) * 100)))
      }))
    })

    output$vaccination_impact_summary <- renderUI({
      result <- vaccination_impact_result()
      if (is.null(result) || !isTRUE(result$success)) {
        return(div(class = "alert alert-info", icon("info-circle"), " Select a pathogen to view vaccination impact."))
      }
      HTML(format_vaccination_impact_html(result))
    })

    return(date_filter)
  })
}
