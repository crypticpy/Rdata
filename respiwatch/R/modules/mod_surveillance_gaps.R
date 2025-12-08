# ============================================================================
# Module: Surveillance Gaps Tab
# Purpose: Display surveillance system status and identified data gaps
# ============================================================================

#' Surveillance Gaps Tab UI
#' @param id Module namespace ID
#' @param outbreak_data Static outbreak data for initial KPI rendering
#' @return nav_panel UI element
surveillanceGapsUI <- function(id, outbreak_data) {

  ns <- NS(id)

  # Get surveillance status for KPI cards
  surveillance_status <- outbreak_data$global_overview$surveillance_system_status

  nav_panel(
    "Surveillance Gaps",
    icon = icon("exclamation-triangle"),

    div(
      class = "container-fluid mt-4",

      # Surveillance System Status KPIs
      div(
        class = "row g-4 mb-4",
        div(
          class = "col-md-4",
          div(
            class = paste0("kpi-card ",
                          ifelse(surveillance_status$who_status == "operational",
                                 "severity-low", "severity-high")),
            div(class = "kpi-label", "WHO Status"),
            div(class = "kpi-value", style = "font-size: 1.5rem;",
                toupper(surveillance_status$who_status)),
            div(class = "kpi-change", "Global Monitoring")
          )
        ),
        div(
          class = "col-md-4",
          div(
            class = paste0("kpi-card ",
                          ifelse(surveillance_status$cdc_status == "operational",
                                 "severity-low", "severity-high")),
            div(class = "kpi-label", "CDC Status"),
            div(class = "kpi-value", style = "font-size: 1.5rem;",
                toupper(surveillance_status$cdc_status)),
            div(class = "kpi-change up", "US Surveillance")
          )
        ),
        div(
          class = "col-md-4",
          div(
            class = paste0("kpi-card ",
                          ifelse(surveillance_status$ecdc_status == "operational",
                                 "severity-low", "severity-high")),
            div(class = "kpi-label", "ECDC Status"),
            div(class = "kpi-value", style = "font-size: 1.5rem;",
                toupper(surveillance_status$ecdc_status)),
            div(class = "kpi-change", "European Monitoring")
          )
        )
      ),

      # Date Range Control Row
      div(
        class = "row mb-4",
        div(
          class = "col-md-4",
          dateRangeControlUI(ns("date_range"), "Surveillance Data Range")
        )
      ),

      # Identified Gaps
      div(
        class = "row g-4",
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Identified Data Gaps"),
            uiOutput(ns("data_gaps_list"))
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "chart-container",
            h4(class = "section-header", "Surveillance Gap Timeline"),
            DTOutput(ns("gaps_table"))
          )
        )
      ),

      # Information Suppression Evidence
      div(
        class = "row g-4 mt-2",
        div(
          class = "col-12",
          div(
            class = "chart-container",
            h4(class = "section-header", "Information Suppression Evidence"),
            uiOutput(ns("suppression_evidence"))
          )
        )
      )
    )
  )
}

#' Surveillance Gaps Tab Server
#' @param id Module namespace ID
#' @param outbreak_data Static outbreak data containing gap information
#' @param timeline_data Reactive timeline data for date filtering
surveillanceGapsServer <- function(id, outbreak_data, timeline_data) {
  moduleServer(id, function(input, output, session) {

    # Date range filter
    date_filter <- dateRangeControlServer("date_range", timeline_data)

    # Data Gaps List
    output$data_gaps_list <- renderUI({
      gaps <- outbreak_data$global_overview$surveillance_system_status$data_gaps_identified

      if (is.null(gaps) || length(gaps) == 0) {
        return(div(class = "text-muted", "No data gaps identified"))
      }

      div(
        lapply(gaps, function(gap) {
          div(
            class = "alert-card high",
            div(class = "alert-description", gap)
          )
        })
      )
    })

    # Surveillance Gaps Table
    output$gaps_table <- DT::renderDT({
      surveillance_gaps <- outbreak_data$anomaly_detection_flags$surveillance_gaps

      if (is.null(surveillance_gaps)) {
        return(DT::datatable(
          data.frame(Message = "No surveillance gaps data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Handle both data.frame and list formats from jsonlite
      if (is.data.frame(surveillance_gaps)) {
        gaps_df <- data.frame(
          Location = surveillance_gaps$affected_area,
          Type = gsub("_", " ", surveillance_gaps$gap_type),
          Start = surveillance_gaps$start_date,
          End = surveillance_gaps$end_date,
          Severity = surveillance_gaps$severity,
          Impact = surveillance_gaps$potential_impact,
          stringsAsFactors = FALSE
        )
      } else {
        gaps_df <- dplyr::bind_rows(
          lapply(surveillance_gaps, function(gap) {
            data.frame(
              Location = gap$affected_area,
              Type = gsub("_", " ", gap$gap_type),
              Start = gap$start_date,
              End = gap$end_date,
              Severity = gap$severity,
              Impact = gap$potential_impact,
              stringsAsFactors = FALSE
            )
          })
        )
      }

      DT::datatable(
        gaps_df,
        options = list(
          dom = 't',
          pageLength = 10,
          ordering = TRUE,
          columnDefs = list(
            list(width = '100px', targets = 0)
          )
        ),
        rownames = FALSE,
        class = "table table-striped table-hover"
      )
    })

    # Suppression Evidence
    output$suppression_evidence <- renderUI({
      evidence <- outbreak_data$anomaly_detection_flags$information_suppression_evidence

      if (is.null(evidence)) {
        return(div(class = "text-muted", "No suppression evidence data available"))
      }

      # Handle both data.frame and list formats from jsonlite
      if (is.data.frame(evidence)) {
        cards <- lapply(seq_len(nrow(evidence)), function(i) {
          sources <- if (is.list(evidence$evidence_sources)) {
            paste(unlist(evidence$evidence_sources[[i]]), collapse = ", ")
          } else {
            evidence$evidence_sources[i]
          }

          div(
            class = "col-md-6",
            div(
              class = "alert-card critical",
              div(class = "alert-title", gsub("_", " ", evidence$suppression_type[i])),
              div(class = "alert-description", evidence$description[i]),
              div(class = "alert-meta",
                  paste("Duration:", evidence$duration[i], "| Impact:", evidence$impact_assessment[i])),
              tags$small(
                class = "d-block mt-2 text-muted",
                paste("Sources:", sources)
              )
            )
          )
        })
      } else {
        cards <- lapply(evidence, function(item) {
          div(
            class = "col-md-6",
            div(
              class = "alert-card critical",
              div(class = "alert-title", gsub("_", " ", item$suppression_type)),
              div(class = "alert-description", item$description),
              div(class = "alert-meta",
                  paste("Duration:", item$duration, "| Impact:", item$impact_assessment)),
              tags$small(
                class = "d-block mt-2 text-muted",
                paste("Sources:", paste(item$evidence_sources, collapse = ", "))
              )
            )
          )
        })
      }

      div(class = "row g-3", cards)
    })

    # Return date filter for potential use by parent
    return(date_filter)
  })
}
