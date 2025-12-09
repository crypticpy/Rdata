# ============================================================================
# Module: Country Analysis Tab
# Purpose: Country-specific outbreak analysis and healthcare capacity
# ============================================================================

#' Country Analysis Tab UI
#' @param id Module namespace ID
#' @param countries_df Data frame with country information for selector
#' @return nav_panel UI element
countryAnalysisUI <- function(id, countries_df) {

  ns <- NS(id)

  nav_panel(
    "Country Analysis",
    icon = icon("flag"),

    div(
      class = "container-fluid mt-4",

      # Country Selector
      div(
        class = "row mb-4",
        div(
          class = "col-md-4",
          selectInput(
            ns("selected_country"),
            "Select Country",
            choices = setNames(countries_df$iso_code, countries_df$country_name),
            selected = "USA"
          )
        ),
        div(
          class = "col-md-4",
          dateRangeControlUI(ns("date_range"), "Analysis Date Range")
        )
      ),

      # Country Details
      div(
        class = "row g-4",

        # Country KPIs
        div(
          class = "col-md-8",
          uiOutput(ns("country_kpis")),

          div(
            class = "chart-container mt-4",
            chart_header_with_code(
              title = "Country Anomaly Flags",
              ns = ns,
              chart_id = "country_anomalies",
              subtitle = "Detected surveillance anomalies"
            ),
            uiOutput(ns("country_anomalies"))
          )
        ),

        # Country Info Sidebar
        div(
          class = "col-md-4",
          div(
            class = "sidebar",
            chart_header_with_code(
              title = "Healthcare Capacity",
              ns = ns,
              chart_id = "country_healthcare",
              subtitle = "Bed utilization and stress levels"
            ),
            uiOutput(ns("country_healthcare"))
          ),

          div(
            class = "sidebar mt-4",
            chart_header_with_code(
              title = "Policy Responses",
              ns = ns,
              chart_id = "country_policies",
              subtitle = "Active interventions"
            ),
            uiOutput(ns("country_policies"))
          )
        )
      )
    )
  )
}

#' Country Analysis Tab Server
#' @param id Module namespace ID
#' @param outbreak_data Static outbreak data with country information
#' @param timeline_data Reactive timeline data for date filtering
countryAnalysisServer <- function(id, outbreak_data, timeline_data) {
  moduleServer(id, function(input, output, session) {

    # Date range filter
    date_filter <- dateRangeControlServer("date_range", timeline_data)

    # Selected country data reactive
    selected_country_data <- reactive({
      req(input$selected_country)
      outbreak_data$countries[[input$selected_country]]
    })

    # Country KPIs
    output$country_kpis <- renderUI({
      tryCatch({
        country <- selected_country_data()
        req(country)

        div(
          class = "row g-3",
          div(
            class = "col-md-3",
            div(
              class = "kpi-card severity-high",
              div(class = "kpi-label", "Positivity Rate"),
              div(class = "kpi-value", paste0(country$h3n2_data$positivity_rate %||% "N/A", "%")),
              div(class = "kpi-change", country$outbreak_status)
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = "kpi-card severity-moderate",
              div(class = "kpi-label", "Subclade K Prevalence"),
              div(class = "kpi-value", paste0(country$h3n2_data$subclade_k_prevalence %||% "N/A", "%")),
              div(class = "kpi-change", country$subclade_data$dominant_clade %||% "")
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = "kpi-card",
              div(class = "kpi-label", "Confirmed Cases"),
              div(class = "kpi-value", style = "font-size: 1.8rem;",
                  format(country$h3n2_data$case_numbers$confirmed_cases %||%
                        country$h3n2_data$case_numbers$estimated_cases %||% 0, big.mark = ",")),
              div(class = "kpi-change", "Estimated total")
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = "kpi-card",
              div(class = "kpi-label", "Data Confidence"),
              div(class = "kpi-value", style = "font-size: 1.8rem;", toupper(country$data_confidence_level)),
              div(class = "kpi-change", country$primary_data_source %||% "")
            )
          )
        )
      }, error = function(e) {
        div(class = "alert alert-danger", "Error loading Country KPIs")
      })
    })

    # Country Anomalies
    output$country_anomalies <- renderUI({
      tryCatch({
        country <- selected_country_data()
        req(country)

        anomaly_flags <- country$anomaly_flags
        if (length(anomaly_flags) == 0 || is.null(anomaly_flags)) {
          return(tags$p("No anomaly flags for this country.", class = "text-muted"))
        }

        if (is.data.frame(anomaly_flags)) {
          cards <- lapply(seq_len(nrow(anomaly_flags)), function(i) {
            severity_class <- switch(
              anomaly_flags$severity[i],
              "critical" = "critical",
              "high" = "high",
              "high"
            )
            div(
              class = paste("alert-card", severity_class),
              div(class = "alert-title", gsub("_", " ", anomaly_flags$anomaly_type[i])),
              div(class = "alert-description", anomaly_flags$description[i]),
              div(class = "alert-meta",
                paste(country$country_name, "|", format(as.Date(anomaly_flags$first_detected[i]), "%b %d, %Y"))
              )
            )
          })
          do.call(div, cards)
        } else {
          div(
            lapply(anomaly_flags, function(alert) {
              severity_class <- switch(
                alert$severity,
                "critical" = "critical",
                "high" = "high",
                "high"
              )
              div(
                class = paste("alert-card", severity_class),
                div(class = "alert-title", gsub("_", " ", alert$pattern_type)),
                div(class = "alert-description", alert$description),
                div(
                  class = "alert-meta",
                  paste(alert$geographic_scope, "|", format(as.Date(alert$first_detected), "%b %d, %Y"))
                )
              )
            })
          )
        }
      }, error = function(e) {
        div(class = "alert alert-danger", "Error loading Country Anomalies")
      })
    })

    # Healthcare Capacity
    output$country_healthcare <- renderUI({
      tryCatch({
        country <- selected_country_data()
        req(country)

        hc <- country$healthcare_capacity

        div(
          div(
            class = "mb-3",
            div(class = "d-flex justify-content-between"),
            tags$small(class = "text-muted", "ICU Bed Utilization"),
            div(
              class = "progress",
              style = "height: 8px;",
              div(
                class = paste0("progress-bar ",
                              ifelse(hc$icu_bed_utilization > 80, "bg-danger",
                                    ifelse(hc$icu_bed_utilization > 60, "bg-warning", "bg-success"))),
                style = paste0("width: ", hc$icu_bed_utilization, "%;"),
                role = "progressbar"
              )
            ),
            tags$small(paste0(hc$icu_bed_utilization, "%"))
          ),
          div(
            class = "mb-3",
            tags$strong("Hospital Capacity Stress: "),
            span(
              class = paste("status-badge",
                          switch(hc$hospital_capacity_stress,
                                "critical" = "active",
                                "high" = "elevated",
                                "moderate" = "monitoring",
                                "monitoring")),
              hc$hospital_capacity_stress
            )
          ),
          div(
            tags$strong("Healthcare Worker Status: "),
            span(hc$healthcare_worker_status)
          )
        )
      }, error = function(e) {
        div(class = "alert alert-danger", "Error loading Healthcare Capacity")
      })
    })

    # Policy Responses
    output$country_policies <- renderUI({
      country <- selected_country_data()
      req(country)

      policy_responses <- country$policy_responses
      if (length(policy_responses) == 0 || is.null(policy_responses)) {
        return(tags$p("No policy responses recorded.", class = "text-muted"))
      }

      if (is.data.frame(policy_responses)) {
        cards <- lapply(seq_len(nrow(policy_responses)), function(i) {
          div(
            class = "mb-3 p-2",
            style = "background: var(--off-white); border-radius: 6px;",
            div(tags$strong(policy_responses$policy_type[i])),
            tags$small(
              class = "text-muted",
              paste("Since", format(as.Date(policy_responses$implementation_date[i]), "%b %Y"))
            ),
            tags$br(),
            span(
              class = paste("status-badge",
                           ifelse(policy_responses$current_status[i] == "active", "active", "monitoring")),
              policy_responses$current_status[i]
            ),
            span(
              class = "ms-2",
              style = "font-size: 0.75rem;",
              paste("Effectiveness:", policy_responses$effectiveness[i])
            )
          )
        })
      } else {
        cards <- lapply(policy_responses, function(policy) {
          div(
            class = "mb-3 p-2",
            style = "background: var(--off-white); border-radius: 6px;",
            div(tags$strong(policy$policy_type)),
            tags$small(
              class = "text-muted",
              paste("Since", format(as.Date(policy$implementation_date), "%b %Y"))
            ),
            tags$br(),
            span(
              class = paste("status-badge",
                           ifelse(policy$current_status == "active", "active", "monitoring")),
              policy$current_status
            ),
            span(
              class = "ms-2",
              style = "font-size: 0.75rem;",
              paste("Effectiveness:", policy$effectiveness)
            )
          )
        })
      }

      div(cards)
    })

    # =========================================================================
    # CODE TRANSPARENCY MODAL HANDLERS
    # =========================================================================

    # Country Anomalies Code Modal
    observeEvent(input$show_code_country_anomalies, {
      snippet <- get_code_snippet("country_anomalies")
      show_code_modal(
        session = session,
        title = "Country Anomaly Flags Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)

    # Country Healthcare Code Modal
    observeEvent(input$show_code_country_healthcare, {
      snippet <- get_code_snippet("country_healthcare")
      show_code_modal(
        session = session,
        title = "Healthcare Capacity Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)

    # Country Policies Code Modal
    observeEvent(input$show_code_country_policies, {
      snippet <- get_code_snippet("country_policies")
      show_code_modal(
        session = session,
        title = "Policy Responses Code",
        data_code = snippet$data_code,
        viz_code = snippet$viz_code,
        data_description = snippet$data_desc,
        viz_description = snippet$viz_desc
      )
    }, ignoreInit = TRUE)

    # Return date filter and selected country for potential use by parent
    return(list(
      date_filter = date_filter,
      selected_country = reactive({ input$selected_country })
    ))
  })
}
