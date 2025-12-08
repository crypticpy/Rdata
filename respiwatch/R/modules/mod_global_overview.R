# =============================================================================
# Global Overview Module
# Most complex module - contains map, animation, wave analysis, KPIs
# =============================================================================

globalOverviewUI <- function(id, countries_df = NULL) {
  ns <- NS(id)
  
  nav_panel(
    "Global Overview",
    icon = icon("globe"),
    
    # KPI Cards Row
    div(
      class = "container-fluid mt-4",
      
      # Pathogen Selector Row
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          div(
            class = "pathogen-selector-container",
            style = "display: flex; flex-wrap: wrap; align-items: center; gap: 20px;",
            
            # Pathogen Radio Buttons
            div(
              style = "flex: 1; min-width: 400px;",
              h4(class = "section-header mb-2", "Select Pathogen"),
              radioButtons(
                ns("selected_pathogen"),
                label = NULL,
                choices = c(
                  "All Pathogens" = "all",
                  "H3N2 (Influenza A)" = "h3n2",
                  "RSV" = "rsv",
                  "COVID-19" = "covid",
                  "H5N1/H5N5 (Avian)" = "h5n1"
                ),
                selected = "all",
                inline = TRUE
              )
            ),
            
            # Metric Dropdown
            div(
              style = "min-width: 180px;",
              selectInput(ns("map_metric"), "Map Metric:",
                choices = c(
                  "Positivity Rate (%)" = "positivity_rate",
                  "Vaccination Coverage (%)" = "vaccination_rate",
                  "Confirmed Cases (Total)" = "confirmed_cases"
                ),
                selected = "positivity_rate",
                width = "180px"
              )
            ),
            
            # Toggle Checkboxes
            div(
              class = "selector-toggles",
              style = "display: flex; flex-direction: column; gap: 5px;",
              checkboxInput(ns("show_bubbles"), "Show Case Volume (Bubbles)", value = FALSE),
              checkboxInput(ns("show_anomalies"), "Show Alerts (High Activity)", value = TRUE)
            ),
            
            # Date Range Control
            div(
              style = "min-width: 280px;",
              dateRangeControlUI(ns("date_range"), "Timeline Range", default_days = 90)
            )
          )
        )
      ),
      
      # Dynamic KPI Cards
      uiOutput(ns("pathogen_kpis")),
      
      # Main Content Row
      div(
        class = "row g-4",
        
        # Left Column - Map
        div(
          class = "col-lg-8",
          div(
            class = "map-container",
            h4(class = "section-header", "Global Outbreak Distribution"),
            leafletOutput(ns("global_map"), height = "500px"),
            
            # Animation Controls Panel
            div(
              class = "animation-controls-panel mt-3 p-3",
              style = "background: var(--surface-elevated); border-radius: 8px; border: 1px solid var(--border-color);",
              
              # Layer Toggle Row
              div(
                class = "d-flex justify-content-between align-items-center mb-3",
                div(
                  class = "animation-date-display w-100 text-center",
                  tags$span(class = "text-muted me-2", "Current Date:"),
                  tags$span(id = ns("animation_current_date"), class = "fw-bold", "---")
                )
              ),
              
              # Playback Controls Row
              div(
                class = "d-flex align-items-center gap-3",
                
                # Play/Pause/Stop buttons
                div(
                  class = "playback-buttons d-flex gap-1",
                  actionButton(ns("animation_step_backward"), "",
                    icon = icon("step-backward"),
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Previous Frame"
                  ),
                  actionButton(ns("animation_play_pause"), "",
                    icon = icon("play"),
                    class = "btn btn-sm btn-primary",
                    title = "Play"
                  ),
                  actionButton(ns("animation_stop"), "",
                    icon = icon("stop"),
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Stop"
                  ),
                  actionButton(ns("animation_step_forward"), "",
                    icon = icon("step-forward"),
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Next Frame"
                  )
                ),
                
                # Timeline Slider
                div(
                  class = "timeline-slider flex-grow-1",
                  sliderInput(ns("animation_date"), NULL,
                              min = as.Date("2023-01-01"),
                              max = Sys.Date(),
                              value = Sys.Date(),
                              timeFormat = "%Y-%m-%d",
                              ticks = FALSE,
                              step = 7,
                              animate = FALSE,
                              width = "100%"
                  )
                ),
                
                # Frame Counter
                div(
                  class = "frame-counter",
                  style = "min-width: 80px; text-align: center;",
                  tags$span(id = ns("animation_frame_counter"), class = "text-muted small", "0 / 0")
                ),
                
                # Speed Selector
                div(
                  class = "speed-selector",
                  selectInput(
                    ns("animation_speed"),
                    NULL,
                    choices = c("Slow" = "slow", "Normal" = "normal", "Fast" = "fast", "Very Fast" = "veryFast"),
                    selected = "normal",
                    width = "100px"
                  )
                )
              )
            )
          ),
          
          # Timeline Chart
          div(
            class = "chart-container mt-4",
            h4(class = "section-header", "Outbreak Progression Timeline"),
            plotlyOutput(ns("timeline_chart"), height = "350px")
          ),
          
          # Wave Propagation Analysis Section
          div(
            class = "wave-propagation-section mt-4 p-3",
            style = "background: var(--surface-elevated); border-radius: 8px; border: 1px solid var(--border-color);",

            # Header with run button
            div(
              class = "d-flex justify-content-between align-items-center mb-3",
              h4(class = "section-header mb-0", icon("water"), " Wave Propagation Analysis"),
              actionButton(
                ns("run_wave_analysis"),
                "Refresh",
                icon = icon("arrows-rotate"),
                class = "btn btn-outline-primary btn-sm"
              )
            ),

            # Always-visible disease mini-cards grid
            div(
              class = "disease-cards-grid row g-3",
              div(class = "col-md-4", disease_wave_card("H3N2 (Influenza)", "H3N2", "#E85D4C", ns)),
              div(class = "col-md-4", disease_wave_card("RSV", "RSV", "#2563EB", ns)),
              div(class = "col-md-4", disease_wave_card("COVID-19", "COVID19", "#7C3AED", ns)),
              div(class = "col-md-4", disease_wave_card("H5N1 (Avian)", "H5N1", "#DC2626", ns)),
              div(class = "col-md-4", disease_wave_card("H5N5 (Avian)", "H5N5", "#EA580C", ns))
            ),

            # Wave analysis details/status
            div(
              class = "wave-analysis-status mt-3",
              uiOutput(ns("wave_analysis_output"))
            )
          )
        ),
        
        # Right Column - Status & Alerts
        div(
          class = "col-lg-4",
          
          # Continental Status
          div(
            class = "sidebar mb-4",
            h4(class = "section-header", "Continental Status"),
            uiOutput(ns("continental_status"))
          ),
          
          # Anomaly Alerts
          div(
            class = "sidebar",
            h4(class = "section-header", "Critical Anomalies"),
            uiOutput(ns("anomaly_alerts"))
          )
        )
      )
    )
  )
}

globalOverviewServer <- function(id, timeline_data, world_countries, map_snapshot_all, 
                                 country_coords, outbreak_data, anomalies_df, 
                                 db_surveillance = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Date range filter
    global_date_filter <- dateRangeControlServer("date_range", timeline_data)
    
    # Create reactive for world map data based on selected pathogen
    world_map_data_reactive <- reactive({
      req(input$selected_pathogen)
      
      pathogen_ui <- input$selected_pathogen
      selected_pathogen <- switch(
        pathogen_ui,
        "all" = NULL,
        "h3n2" = "H3N2",
        "rsv" = "RSV",
        "covid" = "COVID19",
        "h5n1" = "H5N1",
        "H3N2"
      )
      
      if (is.null(selected_pathogen)) {
        if (is.null(map_snapshot_all) || nrow(map_snapshot_all) == 0) {
          return(world_countries)
        }
        # Normalize column names: database uses case_count, some JSON uses confirmed_cases
        if (!"confirmed_cases" %in% names(map_snapshot_all) && "case_count" %in% names(map_snapshot_all)) {
          map_snapshot_all <- map_snapshot_all |>
            mutate(confirmed_cases = case_count)
        }
        # Group by iso_code only to ensure unique rows for join
        filtered_countries_df <- map_snapshot_all |>
          group_by(iso_code) |>
          summarise(
            country_name = first(country_name, na_rm = TRUE),
            positivity_rate = mean(positivity_rate, na.rm = TRUE),
            confirmed_cases = sum(confirmed_cases, na.rm = TRUE),
            vaccination_rate = mean(vaccination_rate, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        filtered_countries_df <- filter_by_pathogen(map_snapshot_all, selected_pathogen)
      }
      
      if (is.null(filtered_countries_df) || nrow(filtered_countries_df) == 0) {
        return(world_countries)
      }
      
      # Join with country_coords and ensure unique iso_codes to prevent list columns
      filtered_countries_df <- left_join(filtered_countries_df, country_coords, by = "iso_code") |>
        distinct(iso_code, .keep_all = TRUE)

      # Join with world_countries sf object
      result <- world_countries |>
        left_join(filtered_countries_df, by = "iso_code")

      # Fix ALL potential list columns before returning
      # This prevents "invalid 'type' (list) of argument" errors in leaflet
      numeric_cols <- c("positivity_rate", "vaccination_rate", "confirmed_cases",
                        "lat", "lng", "population", "hospitalization_rate",
                        "subclade_k_prevalence")
      char_cols <- c("country_name", "outbreak_status", "data_confidence", "last_updated")

      for (col in c(numeric_cols, char_cols)) {
        if (col %in% names(result)) {
          col_data <- result[[col]]
          if (is.list(col_data)) {
            # Unlist and take first element for each
            result[[col]] <- if (col %in% numeric_cols) {
              as.numeric(sapply(col_data, function(x) if (length(x) == 0 || is.null(x)) NA else x[1]))
            } else {
              as.character(sapply(col_data, function(x) if (length(x) == 0 || is.null(x)) NA else x[1]))
            }
          }
        }
      }

      result
    })
    
    # Pathogen KPIs (reactive) - DATABASE-DRIVEN
    output$pathogen_kpis <- renderUI({
      tryCatch({
        pathogen <- input$selected_pathogen
        
        kpi_result <- get_dashboard_kpis(pathogen_code = pathogen)
        
        if (!kpi_result$success || is.null(kpi_result$kpis)) {
          return(div(
            class = "row g-3",
            lapply(1:4, function(i) {
              div(class = "col-md-3",
                div(class = "kpi-card severity-low",
                  div(class = "kpi-title", "Loading..."),
                  div(class = "kpi-value", "--"),
                  div(class = "kpi-change", if (!is.null(kpi_result$staleness_warning)) kpi_result$staleness_warning else "Data loading")
                )
              )
            })
          ))
        }
        
        kpis <- kpi_result$kpis
        
        if (tolower(pathogen) == "all") {
          kpi_data <- list(
            title1 = "Active Pathogens",
            value1 = format_kpi_value(kpis$active_pathogens, "integer", "0"),
            change1 = "Multi-Pathogen View",
            severity1 = "severity-moderate",
            
            title2 = "Global Severity",
            value2 = format_kpi_value(kpis$global_severity, "status", "UNKNOWN"),
            change2 = "Combined Assessment",
            severity2 = kpis$global_severity_class,
            
            title3 = "Dominant Pathogen",
            value3 = format_kpi_value(kpis$dominant_pathogen, "text", "N/A"),
            change3 = if (!is.na(kpis$dominant_prevalence)) paste0(round(kpis$dominant_prevalence, 1), "% Prevalence") else "Calculating...",
            severity3 = "severity-high",
            
            title4 = "Data Quality",
            value4 = paste0(format_kpi_value(kpis$data_quality_score, "integer", "0"), "%"),
            change4 = if (!is.null(kpis$latest_date) && !is.na(kpis$latest_date)) paste("Updated:", format(as.Date(kpis$latest_date), "%b %d")) else "No data",
            severity4 = if (kpis$data_quality_score >= 80) "severity-low" else if (kpis$data_quality_score >= 50) "severity-moderate" else "severity-high"
          )
        } else {
          pathogen_name <- switch(tolower(pathogen),
            "h3n2" = "H3N2",
            "rsv" = "RSV",
            "covid" = "COVID-19",
            "h5n1" = "H5N1 (Avian)",
            toupper(pathogen)
          )
          
          kpi_data <- list(
            title1 = paste(pathogen_name, "Status"),
            value1 = format_kpi_value(kpis$status, "status", "UNKNOWN"),
            change1 = if (!is.null(kpis$trend) && !is.na(kpis$trend)) paste("Trend:", kpis$trend) else "Status from database",
            severity1 = kpis$status_severity,
            
            title2 = "Positivity Rate",
            value2 = format_kpi_value(kpis$positivity_rate, "percent", "N/A"),
            change2 = "14-Day Average",
            severity2 = kpis$positivity_severity,
            
            title3 = if (!is.na(kpis$dominant_variant)) "Dominant Variant" else "Hospitalization Rate",
            value3 = if (!is.na(kpis$dominant_variant)) format_kpi_value(kpis$dominant_variant, "text", "N/A") else format_kpi_value(kpis$hospitalization_rate, "percent", "N/A"),
            change3 = if (!is.na(kpis$dominant_variant)) "Current Strain" else "14-Day Average",
            severity3 = if (!is.na(kpis$dominant_variant)) "severity-moderate" else kpis$hospitalization_severity,
            
            title4 = "Vaccine Coverage",
            value4 = format_kpi_value(kpis$vaccine_coverage, "percent", "N/A"),
            change4 = if (!is.null(kpis$countries_reporting) && kpis$countries_reporting > 0) paste(kpis$countries_reporting, "countries reporting") else "Coverage data",
            severity4 = kpis$coverage_severity
          )
        }
        
        staleness_banner <- if (!is.null(kpi_result$staleness_warning)) {
          div(class = "alert alert-warning mb-3",
            icon("exclamation-triangle"),
            kpi_result$staleness_warning
          )
        } else NULL
        
        tagList(
          staleness_banner,
          div(
            class = "row g-4 mb-4",
            div(
              class = "col-md-3",
              div(
                class = paste("kpi-card", kpi_data$severity1),
                div(class = "kpi-label", kpi_data$title1),
                div(class = "kpi-value", kpi_data$value1),
                div(class = "kpi-change", kpi_data$change1)
              )
            ),
            div(
              class = "col-md-3",
              div(
                class = paste("kpi-card", kpi_data$severity2),
                div(class = "kpi-label", kpi_data$title2),
                div(class = "kpi-value", kpi_data$value2),
                div(class = "kpi-change", kpi_data$change2)
              )
            ),
            div(
              class = "col-md-3",
              div(
                class = paste("kpi-card", kpi_data$severity3),
                div(class = "kpi-label", kpi_data$title3),
                div(class = "kpi-value", kpi_data$value3),
                div(class = "kpi-change", kpi_data$change3)
              )
            ),
            div(
              class = "col-md-3",
              div(
                class = paste("kpi-card", kpi_data$severity4),
                div(class = "kpi-label", kpi_data$title4),
                div(class = "kpi-value", kpi_data$value4),
                div(class = "kpi-change", kpi_data$change4)
              )
            )
          )
        )
      }, error = function(e) {
        message("Error in pathogen_kpis renderUI: ", e$message)
        div(class = "alert alert-danger", paste("Error loading Pathogen KPIs:", e$message))
      })
    })
    
    # Global Map
    output$global_map <- renderLeaflet({
      # Wrap entire map rendering in tryCatch for robust error handling
      tryCatch({
        map_data <- world_map_data_reactive()

        if (is.null(map_data) || !inherits(map_data, "sf") || nrow(map_data) == 0) {
          return(
            leaflet() |>
              addProviderTiles(providers$CartoDB.Positron) |>
              setView(lng = 0, lat = 30, zoom = 2) |>
              addControl(
                html = "<div style='padding: 10px; background: white; border-radius: 4px;'>
                         <i class='fa fa-info-circle'></i> Map data is loading or unavailable
                       </div>",
                position = "topright"
              )
          )
        }

        # Coerce any list columns to atomic vectors to prevent "invalid type (list)" errors
        # This can happen when joins create duplicate rows that get collapsed into lists
        numeric_map_cols <- c("positivity_rate", "vaccination_rate", "confirmed_cases",
                              "population", "hospitalization_rate", "lat", "lng")
        char_map_cols <- c("country_name", "outbreak_status", "data_confidence", "iso_code")

        for (col in c(numeric_map_cols, char_map_cols)) {
          if (col %in% names(map_data) && is.list(map_data[[col]])) {
            if (col %in% numeric_map_cols) {
              map_data[[col]] <- as.numeric(sapply(map_data[[col]], function(x) {
                if (is.null(x) || length(x) == 0) NA else x[1]
              }))
            } else {
              map_data[[col]] <- as.character(sapply(map_data[[col]], function(x) {
                if (is.null(x) || length(x) == 0) NA else x[1]
              }))
            }
          }
        }

        # Ensure display values are never lists before passing to colorNumeric
        if (!is.null(map_data$positivity_rate) && is.list(map_data$positivity_rate)) {
          map_data$positivity_rate <- as.numeric(unlist(lapply(map_data$positivity_rate, `[`, 1)))
        }
        if (!is.null(map_data$vaccination_rate) && is.list(map_data$vaccination_rate)) {
          map_data$vaccination_rate <- as.numeric(unlist(lapply(map_data$vaccination_rate, `[`, 1)))
        }
        if (!is.null(map_data$confirmed_cases) && is.list(map_data$confirmed_cases)) {
          map_data$confirmed_cases <- as.numeric(unlist(lapply(map_data$confirmed_cases, `[`, 1)))
        }

      metric <- input$map_metric %||% "positivity_rate"

      # Force ALL data columns to atomic vectors before any operations
      # This is a bulletproof fix for list column issues from sf joins
      force_atomic_numeric <- function(x) {
        if (is.null(x)) return(rep(NA_real_, nrow(map_data)))
        if (is.list(x)) {
          return(as.numeric(vapply(x, function(v) if (length(v) == 0 || is.null(v[[1]])) NA_real_ else as.numeric(v[[1]]), numeric(1))))
        }
        as.numeric(x)
      }

      force_atomic_char <- function(x) {
        if (is.null(x)) return(rep(NA_character_, nrow(map_data)))
        if (is.list(x)) {
          return(vapply(x, function(v) if (length(v) == 0 || is.null(v[[1]])) NA_character_ else as.character(v[[1]]), character(1)))
        }
        as.character(x)
      }

      # Apply to all relevant columns
      map_data$positivity_rate <- force_atomic_numeric(map_data$positivity_rate)
      map_data$vaccination_rate <- force_atomic_numeric(map_data$vaccination_rate)
      map_data$confirmed_cases <- force_atomic_numeric(map_data$confirmed_cases)
      map_data$country_name <- force_atomic_char(map_data$country_name)
      map_data$outbreak_status <- force_atomic_char(map_data$outbreak_status)

      if (metric == "vaccination_rate") {
        display_val <- map_data$vaccination_rate
        pal_colors <- c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45")
        legend_title <- "Vaccination (%)"
        domain_range <- c(0, 100)
        border_color <- "#006d2c"
        val_suffix <- "%"
      } else if (metric == "confirmed_cases") {
        display_val <- map_data$confirmed_cases
        pal_colors <- c("#F3E5F5", "#E1BEE7", "#CE93D8", "#BA68C8", "#AB47BC", "#8E24AA", "#4A148C")
        legend_title <- "Total Cases"
        domain_range <- NULL
        border_color <- "#4A148C"
        val_suffix <- ""
      } else {
        display_val <- map_data$positivity_rate
        pal_colors <- c("#FEE2E2", "#FECACA", "#FCA5A5", "#F87171", "#EF4444", "#DC2626", "#B91C1C")
        legend_title <- "Positivity (%)"
        domain_range <- c(0, 50)
        border_color <- "#991B1B"
        val_suffix <- "%"
      }

      # Final guard: ensure display_val is definitely atomic numeric
      if (!is.atomic(display_val) || !is.numeric(display_val)) {
        display_val <- force_atomic_numeric(display_val)
      }
      
      fill_pal <- colorNumeric(
        palette = pal_colors,
        domain = if(is.null(domain_range)) display_val else domain_range,
        na.color = "#F3F4F6"
      )
      
      labels <- sprintf(
        "<strong style='font-size: 14px;'>%s</strong><br/>
         <span style='color: %s; font-weight: bold;'>%s: %s%s</span><br/>
         <hr style='margin: 4px 0;'/>
         Positivity Rate: %s%%<br/>
         Vaccination: %s%%<br/>
         Confirmed Cases: %s<br/>
         Status: %s",
        ifelse(is.na(map_data$country_name), map_data$country_name_geo, map_data$country_name),
        border_color,
        legend_title,
        ifelse(is.na(display_val), "N/A",
               if(metric == "confirmed_cases") format(display_val, big.mark=",")
               else round(display_val, 1)),
        val_suffix,
        ifelse(is.na(map_data$positivity_rate), "N/A", round(map_data$positivity_rate, 1)),
        ifelse(is.na(map_data$vaccination_rate), "N/A", round(map_data$vaccination_rate, 1)),
        ifelse(is.na(map_data$confirmed_cases), "N/A", format(map_data$confirmed_cases, big.mark = ",")),
        ifelse(is.na(map_data$outbreak_status), "Unknown", map_data$outbreak_status)
      ) |> lapply(htmltools::HTML)
      
      map <- leaflet(map_data, options = leafletOptions(
        worldCopyJump = FALSE,
        maxBoundsViscosity = 1.0,
        minZoom = 2
      )) |>
        addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
          noWrap = TRUE,
          bounds = list(c(-85.06, -180), c(85.06, 180))
        )) |>
        setView(lng = 0, lat = 20, zoom = 2) |>
        setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85) |>
        addPolygons(
          fillColor = ~fill_pal(display_val),
          fillOpacity = 0.7,
          color = border_color,
          weight = ~ifelse(is.na(display_val), 0.5, 2.0),
          opacity = 1,
          dashArray = ~ifelse(is.na(display_val), "3", ""),
          highlightOptions = highlightOptions(
            weight = 4,
            color = "#1A1A2E",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = labelOptions(
            style = list(
              "font-family" = "DM Sans, sans-serif",
              "font-size" = "12px",
              "padding" = "8px 12px",
              "border-radius" = "8px",
              "box-shadow" = "0 2px 8px rgba(0,0,0,0.15)"
            ),
            textsize = "12px",
            direction = "auto"
          )
        ) |>
        addLegend(
          position = "bottomright",
          pal = fill_pal,
          values = if(is.null(domain_range)) display_val else domain_range,
          title = legend_title,
          opacity = 0.8,
          na.label = "No Data",
          labFormat = labelFormat(suffix = val_suffix)
        )
      
      if (isTRUE(input$show_bubbles)) {
        max_cases <- max(map_data$confirmed_cases, na.rm = TRUE)
        if (!is.finite(max_cases) || max_cases <= 0) max_cases <- 1

        map <- map |>
          addCircleMarkers(
            lng = ~st_coordinates(st_centroid(geometry))[,1],
            lat = ~st_coordinates(st_centroid(geometry))[,2],
            radius = ~ifelse(is.na(confirmed_cases), 0, sqrt(confirmed_cases) / sqrt(max_cases) * 20),
            color = "#1A1A2E",
            weight = 1,
            opacity = 0.8,
            fillColor = "#FFFFFF",
            fillOpacity = 0.3,
            label = ~paste0(country_name, ": ", format(confirmed_cases, big.mark=","), " Cases")
          )
      }

      map
      }, error = function(e) {
        # Extract error message safely using conditionMessage
        error_msg <- if (inherits(e, "condition")) {
          conditionMessage(e)
        } else if (is.list(e) && !is.null(e$message)) {
          as.character(e$message)
        } else {
          tryCatch(as.character(e)[1], error = function(inner_e) "Unknown error")
        }

        log_error(sprintf("Map rendering failed: %s", error_msg), category = "ui")

        # Return fallback map with error message
        leaflet() |>
          addProviderTiles(providers$CartoDB.Positron) |>
          setView(lng = 0, lat = 30, zoom = 2) |>
          addControl(
            html = sprintf(
              "<div style='padding: 12px; background: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; max-width: 300px;'>
                <strong style='color: #856404;'>Error loading map</strong><br>
                <small style='color: #856404;'>%s</small>
              </div>",
              htmltools::htmlEscape(error_msg)
            ),
            position = "topright"
          )
      })
    })
    
    # Animation State
    animation_playing <- reactiveVal(FALSE)
    autoInvalidate <- reactiveTimer(1000)
    
    # Initialize Slider Range from DB
    observe({
      req(USE_DATABASE)
      conn <- get_db_connection()
      range <- dbGetQuery(conn, "SELECT MIN(observation_date) as start, MAX(observation_date) as end FROM surveillance_data")
      close_db_connection(conn)
      
      start_date <- tryCatch(as.Date(range$start), error = function(e) NA)
      end_date <- tryCatch(as.Date(range$end), error = function(e) NA)
      
      if (!is.na(start_date) && !is.na(end_date)) {
        updateSliderInput(session, "animation_date",
          min = start_date,
          max = end_date,
          value = end_date
        )
      }
    })
    
    # Toggle Play/Pause
    observeEvent(input$animation_play_pause, {
      new_state <- !animation_playing()
      animation_playing(new_state)
      
      if (new_state) {
        updateActionButton(session, "animation_play_pause", icon = icon("pause"))
      } else {
        updateActionButton(session, "animation_play_pause", icon = icon("play"))
      }
    })
    
    # Step Forward Button
    observeEvent(input$animation_step_forward, {
      current <- input$animation_date
      updateSliderInput(session, "animation_date", value = as.Date(current) + 7)
    })
    
    # Step Backward Button
    observeEvent(input$animation_step_backward, {
      current <- input$animation_date
      updateSliderInput(session, "animation_date", value = as.Date(current) - 7)
    })
    
    # Stop Button
    observeEvent(input$animation_stop, {
      animation_playing(FALSE)
      updateActionButton(session, "animation_play_pause", icon = icon("play"))
    })
    
    # Animation Loop
    observe({
      if (animation_playing()) {
        autoInvalidate()
        isolate({
          current <- input$animation_date
          if (current >= Sys.Date()) {
            animation_playing(FALSE)
            updateActionButton(session, "animation_play_pause", icon = icon("play"))
          } else {
            updateSliderInput(session, "animation_date", value = as.Date(current) + 7)
          }
        })
      }
    })
    
    # Store wave analysis results
    wave_analysis_result <- reactiveVal(NULL)
    wave_analysis_all_pathogens <- reactiveVal(NULL)
    
    # Helper function to map UI pathogen to database code
    map_ui_to_pathogen_code <- function(pathogen_ui) {
      switch(
        pathogen_ui,
        "all" = "H3N2",
        "h3n2" = "H3N2",
        "rsv" = "RSV",
        "covid" = "COVID19",
        "h5n1" = "H5N1",
        pathogen_ui
      )
    }
    
    # Shared function to run wave analysis
    run_wave_analysis_for_pathogen <- function(pathogen_code, show_loading = TRUE) {
      surveillance <- if (!is.null(db_surveillance) && nrow(db_surveillance) > 0) {
        db_surveillance |> rename(country_code = iso_code)
      } else {
        NULL
      }
      
      if (is.null(surveillance)) return(NULL)
      
      tryCatch({
        get_wave_analysis(
          surveillance_data = surveillance,
          target_pathogen = pathogen_code,
          countries_sf = NULL
        )
      }, error = function(e) {
        log_error(sprintf("Wave analysis error: %s", e$message), category = "analysis")
        NULL
      })
    }

    # Consolidated wave analysis refresh function
    # Always analyzes all 5 pathogens for the mini-cards
    refresh_wave_analysis <- function(pathogen_ui) {
      pathogen <- map_ui_to_pathogen_code(pathogen_ui)

      result <- run_wave_analysis_for_pathogen(pathogen, show_loading = FALSE)
      if (!is.null(result)) {
        wave_analysis_result(result)
      }

      # Always run analysis for all pathogens to populate mini-cards
      all_pathogens <- c("H3N2", "RSV", "COVID19", "H5N1", "H5N5")
      all_results <- lapply(all_pathogens, function(p) {
        tryCatch(
          run_wave_analysis_for_pathogen(p, show_loading = FALSE),
          error = function(e) NULL
        )
      })
      names(all_results) <- all_pathogens
      wave_analysis_all_pathogens(all_results)
    }

    # Unified wave analysis observer: triggers on session start, button click, or pathogen change
    observe({
      # React to both button and pathogen selection
      input$run_wave_analysis
      pathogen_ui <- input$selected_pathogen %||% "all"

      refresh_wave_analysis(pathogen_ui)
    })
    
    # Wave analysis output
    output$wave_analysis_output <- renderUI({
      result <- wave_analysis_result()
      
      if (is.null(result)) {
        return(div(
          class = "text-center py-3 text-muted",
          "Wave analysis loading..."
        ))
      }
      
      if (result$status %in% c("insufficient_data", "insufficient_spread")) {
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " Unable to calculate wave propagation. Insufficient data for selected pathogen."
        )
      } else {
        HTML(format_wave_analysis_html(result))
      }
    })
    
    # Wave velocity display
    output$wave_velocity_display <- renderUI({
      result <- wave_analysis_result()
      if (is.null(result) || result$status %in% c("insufficient_data", "insufficient_spread")) {
        span("--")
      } else {
        span(sprintf("%.1f", result$velocity$velocity_km_day))
      }
    })
    
    # Wave countries display
    output$wave_countries_display <- renderUI({
      result <- wave_analysis_result()
      if (is.null(result) || result$status %in% c("insufficient_data", "insufficient_spread")) {
        span("--")
      } else {
        span(sprintf("%d", result$velocity$n_countries_affected))
      }
    })
    
    # Wave confidence display
    output$wave_confidence_display <- renderUI({
      result <- wave_analysis_result()
      if (is.null(result) || result$status %in% c("insufficient_data", "insufficient_spread")) {
        span("--")
      } else {
        conf <- result$velocity$confidence
        conf_class <- switch(conf,
          "high" = "text-success",
          "medium" = "text-warning",
          "text-danger"
        )
        span(class = conf_class, toupper(conf))
      }
    })

    # Per-pathogen wave card outputs for mini-cards
    # Generate outputs for each pathogen (5 pathogens × 3 metrics = 15 outputs)
    pathogens_to_track <- c("H3N2", "RSV", "COVID19", "H5N1", "H5N5")

    lapply(pathogens_to_track, function(pathogen_code) {
      # Velocity output
      output[[paste0("wave_velocity_", pathogen_code)]] <- renderUI({
        all_results <- wave_analysis_all_pathogens()
        result <- all_results[[pathogen_code]]

        if (is.null(result) ||
            !result$status %in% c("success", "velocity_only") ||
            is.null(result$velocity) ||
            is.na(result$velocity$velocity_km_day)) {
          return(span("--", class = "text-muted"))
        }
        span(sprintf("%.0f", result$velocity$velocity_km_day))
      })

      # Countries output
      output[[paste0("wave_countries_", pathogen_code)]] <- renderUI({
        all_results <- wave_analysis_all_pathogens()
        result <- all_results[[pathogen_code]]

        if (is.null(result) ||
            !result$status %in% c("success", "velocity_only") ||
            is.null(result$velocity)) {
          return(span("--", class = "text-muted"))
        }
        n_countries <- result$velocity$n_countries_affected %||%
                       result$velocity$n_countries %||% 0
        span(sprintf("%d", n_countries))
      })

      # Confidence output
      output[[paste0("wave_confidence_", pathogen_code)]] <- renderUI({
        all_results <- wave_analysis_all_pathogens()
        result <- all_results[[pathogen_code]]

        if (is.null(result) ||
            !result$status %in% c("success", "velocity_only") ||
            is.null(result$velocity)) {
          return(span("--", class = "text-muted"))
        }
        conf <- result$velocity$confidence %||% "low"
        color <- switch(conf,
          "high" = "#059669",
          "medium" = "#D97706",
          "#DC2626"
        )
        span(toupper(conf), style = sprintf("color: %s; font-weight: 600;", color))
      })
    })

    # Wave comparison cards
    output$wave_comparison_cards <- renderUI({
      pathogen_ui <- input$selected_pathogen %||% "all"
      
      if (pathogen_ui != "all") {
        return(NULL)
      }
      
      all_results <- wave_analysis_all_pathogens()
      
      if (is.null(all_results)) {
        return(div(
          class = "wave-comparison-loading",
          style = "padding: 20px; text-align: center; color: #6B7280;",
          icon("spinner", class = "fa-spin"),
          span(" Loading wave analysis for all pathogens...")
        ))
      }
      
      max_velocity <- max(sapply(all_results, function(r) {
        if (!is.null(r) && r$status == "success" && !is.null(r$velocity)) {
          r$velocity$velocity_km_day %||% 0
        } else {
          0
        }
      }), na.rm = TRUE)
      
      if (max_velocity == 0) max_velocity <- 100
      
      pathogen_config <- list(
        H3N2 = list(name = "H3N2 (Influenza A)", color = "#E85D4C", icon = "virus"),
        RSV = list(name = "RSV", color = "#2563EB", icon = "lungs"),
        COVID19 = list(name = "COVID-19", color = "#7C3AED", icon = "virus-covid")
      )
      
      cards <- lapply(names(all_results), function(pathogen_code) {
        result <- all_results[[pathogen_code]]
        config <- pathogen_config[[pathogen_code]]
        
        if (is.null(config)) return(NULL)
        
        has_data <- !is.null(result) &&
                    result$status %in% c("success", "velocity_only") &&
                    !is.null(result$velocity) &&
                    !is.na(result$velocity$velocity_km_day)
        
        if (has_data) {
          velocity <- result$velocity$velocity_km_day %||% 0
          countries <- result$velocity$n_countries_affected %||% result$velocity$n_countries %||% 0
          velocity_text <- sprintf("%.1f", velocity)
        } else {
          velocity <- 0
          countries <- 0
          velocity_text <- "N/A"
        }
        
        bar_width <- if (has_data) min((velocity / max_velocity) * 100, 100) else 0
        
        div(
          class = "wave-comparison-card",
          style = sprintf("
            background: linear-gradient(135deg, rgba(255,255,255,0.05), rgba(255,255,255,0.02));
            border: 1px solid rgba(255,255,255,0.1);
            border-left: 4px solid %s;
            border-radius: 8px;
            padding: 12px 16px;
            margin-bottom: 12px;
          ", config$color),
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              icon(config$icon, style = sprintf("color: %s;", config$color)),
              span(style = "font-weight: 600; color: #E5E7EB;", config$name)
            ),
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              span(
                style = sprintf("font-size: 1.1rem; font-weight: 700; color: %s;",
                                if (has_data) "#F9FAFB" else "#6B7280"),
                velocity_text
              ),
              span(style = "font-size: 0.75rem; color: #9CA3AF;", "km/day")
            )
          ),
          
          div(
            style = "background: rgba(255,255,255,0.1); border-radius: 4px; height: 8px; margin-bottom: 8px; overflow: hidden;",
            div(
              style = sprintf("
                width: %.1f%%;
                height: 100%%;
                background: linear-gradient(90deg, %s, %sCC);
                border-radius: 4px;
                transition: width 0.5s ease;
              ", bar_width, config$color, config$color)
            )
          ),
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            span(
              style = "font-size: 0.75rem; color: #9CA3AF;",
              if (has_data) sprintf("%d countries tracked", countries) else "Insufficient data"
            )
          )
        )
      })
      
      div(
        class = "wave-comparison-container",
        style = "margin-top: 16px;",
        h5(
          style = "color: #9CA3AF; font-size: 0.875rem; font-weight: 500; margin-bottom: 12px; text-transform: uppercase; letter-spacing: 0.05em;",
          icon("chart-line", style = "margin-right: 8px;"),
          "Wave Velocity Comparison"
        ),
        cards
      )
    })
    
    # Timeline Chart - Smoothed visualization with confidence ribbons
    output$timeline_chart <- renderPlotly({
      filtered_data <- global_date_filter$data()
      pathogen_selection <- input$selected_pathogen %||% "all"
      
      if (is.null(filtered_data) || nrow(filtered_data) == 0) {
        return(
          plotly_empty() |>
            layout(
              title = list(text = "No timeline data available", font = list(size = 14)),
              annotations = list(
                list(
                  text = "Timeline data is loading or unavailable",
                  x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                  showarrow = FALSE, font = list(size = 12, color = "#64748B")
                )
              )
            )
        )
      }
      
      pathogen_colors <- c(
        "H3N2 (Influenza)" = "#E85D4C",
        "RSV" = "#2563EB",
        "COVID-19" = "#7C3AED",
        "H3N2" = "#E85D4C",
        "RSV" = "#2563EB",
        "COVID19" = "#7C3AED"
      )
      
      if (pathogen_selection == "all" && "pathogen" %in% names(filtered_data)) {
        # Clean pathogen names for better legend display
        plot_data <- filtered_data |>
          mutate(
            pathogen_clean = case_when(
              grepl("H3N2", pathogen, ignore.case = TRUE) ~ "H3N2 (Influenza)",
              grepl("RSV", pathogen, ignore.case = TRUE) ~ "RSV",
              grepl("COVID", pathogen, ignore.case = TRUE) ~ "COVID-19",
              TRUE ~ pathogen
            )
          )

        # Apply loess smoothing per pathogen using group_modify for proper per-group execution
        plot_data <- plot_data |>
          arrange(date) |>
          group_by(pathogen_clean) |>
          group_modify(~ {
            df <- .x
            df$date_numeric <- as.numeric(df$date)

            # Apply loess smoothing if sufficient data
            if (nrow(df) >= 10) {
              smoothed <- tryCatch({
                loess_fit <- loess(positivity_rate ~ date_numeric, data = df, span = 0.4)
                predict(loess_fit)
              }, error = function(e) {
                zoo::rollmean(df$positivity_rate, k = 5, fill = NA, align = "center")
              })
            } else {
              smoothed <- zoo::rollmean(df$positivity_rate, k = 3, fill = NA, align = "center")
            }

            df$positivity_smoothed <- smoothed

            # Fill any remaining NAs with original values
            df$positivity_smoothed[is.na(df$positivity_smoothed)] <-
              df$positivity_rate[is.na(df$positivity_smoothed)]

            # Confidence bands based on smoothed values
            df$ci_95_lower <- pmax(0, df$positivity_smoothed * 0.75)
            df$ci_95_upper <- df$positivity_smoothed * 1.25
            df$ci_80_lower <- pmax(0, df$positivity_smoothed * 0.85)
            df$ci_80_upper <- df$positivity_smoothed * 1.15
            df$ci_50_lower <- pmax(0, df$positivity_smoothed * 0.92)
            df$ci_50_upper <- df$positivity_smoothed * 1.08

            df
          }) |>
          ungroup()

        # Updated pathogen colors with clean names
        pathogen_colors_clean <- c(
          "H3N2 (Influenza)" = "#E85D4C",
          "RSV" = "#2563EB",
          "COVID-19" = "#7C3AED"
        )

        p <- ggplot(plot_data, aes(x = date)) +
          # Reference threshold lines
          geom_hline(yintercept = c(10, 25), linetype = "dashed",
                     color = "#E2E8F0", linewidth = 0.4) +
          # Gradient confidence ribbons (outer to inner)
          geom_ribbon(aes(ymin = ci_95_lower, ymax = ci_95_upper, fill = pathogen_clean),
                      alpha = 0.08, show.legend = FALSE) +
          geom_ribbon(aes(ymin = ci_80_lower, ymax = ci_80_upper, fill = pathogen_clean),
                      alpha = 0.12, show.legend = FALSE) +
          geom_ribbon(aes(ymin = ci_50_lower, ymax = ci_50_upper, fill = pathogen_clean),
                      alpha = 0.18, show.legend = FALSE) +
          # Smoothed trend line with rounded ends
          geom_line(aes(y = positivity_smoothed, color = pathogen_clean),
                    linewidth = 1.8, lineend = "round", linejoin = "round") +
          scale_fill_manual(values = pathogen_colors_clean) +
          scale_color_manual(values = pathogen_colors_clean, name = "Pathogen") +
          scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
          labs(x = NULL, y = "Positivity Rate (%)") +
          theme_minimal(base_family = "DM Sans") +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#F1F5F9", linewidth = 0.25),
            axis.text = element_text(color = "#475569", size = 10),
            axis.title = element_text(color = "#1A1A2E", size = 11),
            axis.title.y = element_text(margin = margin(r = 10)),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.margin = margin(15, 15, 15, 15),
            legend.position = "bottom",
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 9)
          )
      } else {
        fill_color <- switch(
          pathogen_selection,
          "h3n2" = "#E85D4C",
          "rsv" = "#2563EB",
          "covid" = "#7C3AED",
          "h5n1" = "#DC2626",
          "#E85D4C"
        )

        # Apply loess smoothing for single pathogen view
        plot_data <- filtered_data |>
          arrange(date) |>
          mutate(
            date_numeric = as.numeric(date),
            # Use loess for smoother curves when sufficient data points
            positivity_smoothed = if (n() >= 10) {
              tryCatch(
                predict(loess(positivity_rate ~ date_numeric, span = 0.3)),
                error = function(e) zoo::rollmean(positivity_rate, k = 3, fill = NA, align = "right")
              )
            } else {
              zoo::rollmean(positivity_rate, k = 3, fill = NA, align = "right")
            },
            # Gradient confidence bands (95%, 80%, 50%)
            ci_95_lower = pmax(0, positivity_rate * 0.75),
            ci_95_upper = positivity_rate * 1.25,
            ci_80_lower = pmax(0, positivity_rate * 0.85),
            ci_80_upper = positivity_rate * 1.15,
            ci_50_lower = pmax(0, positivity_rate * 0.92),
            ci_50_upper = positivity_rate * 1.08
          ) |>
          filter(!is.na(positivity_smoothed))

        if (nrow(plot_data) == 0) {
          plot_data <- filtered_data |>
            mutate(
              positivity_smoothed = positivity_rate,
              ci_95_lower = pmax(0, positivity_rate * 0.75),
              ci_95_upper = positivity_rate * 1.25,
              ci_80_lower = pmax(0, positivity_rate * 0.85),
              ci_80_upper = positivity_rate * 1.15,
              ci_50_lower = pmax(0, positivity_rate * 0.92),
              ci_50_upper = positivity_rate * 1.08
            )
        }

        p <- ggplot(plot_data, aes(x = date)) +
          # Reference threshold lines
          geom_hline(yintercept = c(10, 25), linetype = "dashed",
                     color = "#E2E8F0", linewidth = 0.4) +
          # Gradient confidence ribbons (outer to inner)
          geom_ribbon(aes(ymin = ci_95_lower, ymax = ci_95_upper),
                      fill = fill_color, alpha = 0.08) +
          geom_ribbon(aes(ymin = ci_80_lower, ymax = ci_80_upper),
                      fill = fill_color, alpha = 0.12) +
          geom_ribbon(aes(ymin = ci_50_lower, ymax = ci_50_upper),
                      fill = fill_color, alpha = 0.18) +
          # Smoothed trend line with rounded ends
          geom_line(aes(y = positivity_smoothed), color = fill_color,
                    linewidth = 2, lineend = "round", linejoin = "round") +
          scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
          labs(x = NULL, y = "Positivity Rate (%)") +
          theme_minimal(base_family = "DM Sans") +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#F1F5F9", linewidth = 0.25),
            axis.text = element_text(color = "#475569", size = 10),
            axis.title = element_text(color = "#1A1A2E", size = 11),
            axis.title.y = element_text(margin = margin(r = 10)),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.margin = margin(15, 15, 15, 15)
          )
      }
      
      # Convert to plotly and clean up legend
      plotly_obj <- ggplotly(p, tooltip = c("x", "y")) |>
        config(displayModeBar = FALSE)

      # Fix legend labels by removing ",1" suffix and hiding ribbon traces
      for (i in seq_along(plotly_obj$x$data)) {
        trace_name <- plotly_obj$x$data[[i]]$name
        if (!is.null(trace_name)) {
          # Remove the ",1" or ",2" suffix from legend entries
          clean_name <- gsub(",\\d+\\)$", ")", trace_name)
          clean_name <- gsub("^\\(|\\)$", "", clean_name)  # Remove parentheses
          plotly_obj$x$data[[i]]$name <- clean_name

          # Hide ribbon traces from legend (keep only line traces)
          if (!is.null(plotly_obj$x$data[[i]]$fill) &&
              plotly_obj$x$data[[i]]$fill == "toself") {
            plotly_obj$x$data[[i]]$showlegend <- FALSE
          } else if (!is.null(plotly_obj$x$data[[i]]$mode) &&
                     grepl("lines", plotly_obj$x$data[[i]]$mode)) {
            # Explicitly show line traces in legend
            plotly_obj$x$data[[i]]$showlegend <- TRUE
          }
        }
      }

      plotly_obj |>
        layout(
          showlegend = TRUE,
          hoverlabel = list(bgcolor = "white"),
          margin = list(l = 50, r = 20, t = 20, b = 80),
          legend = list(
            orientation = "h",
            y = -0.25,
            yanchor = "top",
            x = 0.5,
            xanchor = "center"
          )
        )
    })
    
    # Continental Status
    output$continental_status <- renderUI({
      tryCatch({
        if (is.null(outbreak_data) || is.null(outbreak_data$global_overview) ||
            is.null(outbreak_data$global_overview$current_status)) {
          return(div(
            class = "text-center text-muted py-4",
            icon("globe", class = "fa-2x mb-2"),
            tags$p("Continental status data unavailable")
          ))
        }
        
        continents <- outbreak_data$global_overview$current_status
        
        div(
          lapply(names(continents), function(name) {
            continent <- continents[[name]]
            status_class <- switch(
              continent$status,
              "active" = "active",
              "elevated" = "elevated",
              "peak" = "peak",
              "monitoring" = "monitoring",
              "monitoring"
            )
            
            div(
              class = "continent-card",
              div(
                class = "d-flex justify-content-between align-items-center",
                div(
                  span(class = "continent-name", gsub("_", " ", name)),
                  tags$br(),
                  span(class = paste("status-badge", status_class), continent$status)
                ),
                div(
                  class = "text-end",
                  div(class = "continent-rate", paste0(continent$positivity_rate %||% "N/A", "%")),
                  div(style = "font-size: 0.75rem; color: var(--slate);", continent$dominant_pathogen %||% "")
                )
              )
            )
          })
        )
      }, error = function(e) {
        message("Error in continental_status: ", e$message)
        div(class = "alert alert-danger", "Error loading Continental Status")
      })
    })
    
    # Anomaly Alerts
    output$anomaly_alerts <- renderUI({
      if (is.null(anomalies_df) || nrow(anomalies_df) == 0) {
        return(div(
          class = "text-center text-muted py-4",
          icon("check-circle", class = "fa-2x mb-2 text-success"),
          tags$p("No active anomalies detected")
        ))
      }
      
      div(
        lapply(1:min(5, nrow(anomalies_df)), function(i) {
          alert <- anomalies_df[i, ]
          alert_class <- ifelse(alert$confidence_level == "high", "critical", "high")
          
          div(
            class = paste("alert-card", alert_class),
            div(class = "alert-title", gsub("_", " ", alert$pattern_type)),
            div(class = "alert-description", alert$description),
            div(
              class = "alert-meta",
              paste(alert$geographic_scope, "|", format(alert$first_detected, "%b %d, %Y"))
            )
          )
        })
      )
    })
    
    # Return the selected pathogen for use by other modules
    reactive(input$selected_pathogen)
  })
}
