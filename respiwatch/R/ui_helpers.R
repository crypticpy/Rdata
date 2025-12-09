# ============================================================================
# Title: RespiWatch UI Helpers
# Purpose: Reusable UI components to reduce duplication across modules
# ============================================================================

# =============================================================================
# DATA SOURCE COLORS FOR FALLBACK VISUALIZATION
# =============================================================================

#' Color palette for data sources
#' Used for consistent styling across charts when showing fallback data
DATA_SOURCE_COLORS <- list(
  primary = "#1E40AF",      # Deep blue - CDC/WHO primary surveillance
  wastewater = "#7C3AED",   # Purple - NWSS wastewater signals
  forecast = "#EA580C",     # Orange - FluSight ensemble forecasts
  syndromic = "#059669",    # Green - Delphi syndromic signals
  state = "#0891B2",        # Cyan - State health department data
  interpolated = "#6B7280"  # Gray - Last resort interpolation
)

#' Map source codes to display colors
get_source_color <- function(source_code) {
  color_map <- list(
    CDC_FLUVIEW = DATA_SOURCE_COLORS$primary,
    CDC_NREVSS = DATA_SOURCE_COLORS$primary,
    CDC_COVID = DATA_SOURCE_COLORS$primary,
    RSV_NET = DATA_SOURCE_COLORS$primary,
    WHO_FLUMART = DATA_SOURCE_COLORS$primary,
    ECDC = DATA_SOURCE_COLORS$primary,
    NWSS_WASTEWATER = DATA_SOURCE_COLORS$wastewater,
    FLUSIGHT_FORECAST = DATA_SOURCE_COLORS$forecast,
    DELPHI = DATA_SOURCE_COLORS$syndromic,
    STATE_HEALTH = DATA_SOURCE_COLORS$state,
    INTERPOLATED = DATA_SOURCE_COLORS$interpolated
  )

  color_map[[source_code]] %||% DATA_SOURCE_COLORS$primary
}

# =============================================================================
# FALLBACK INDICATOR BANNER
# =============================================================================

#' Create clickable fallback indicator banner
#'
#' Displays when alternate signals are filling gaps in primary surveillance.
#' Clicking opens a modal with detailed source breakdown.
#'
#' @param data Data frame from get_surveillance_with_fallback()
#' @param ns Shiny namespace function
#' @param show_details If TRUE, include the detail link
#' @return UI element for banner (NULL if no fallback used)
fallback_banner <- function(data, ns, show_details = TRUE) {
  has_fallback <- attr(data, "has_fallback")
  coverage <- attr(data, "source_coverage")

  if (!has_fallback || is.null(coverage) || length(coverage) == 0) {
    return(NULL)  # All primary data, no banner needed
  }

  # Calculate fallback percentage
  total_points <- sum(unlist(coverage))
  primary_codes <- c("CDC_FLUVIEW", "CDC_NREVSS", "CDC_COVID", "RSV_NET", "WHO_FLUMART", "ECDC")
  primary_points <- sum(sapply(primary_codes, function(s) coverage[[s]] %||% 0))
  fallback_pct <- round((1 - primary_points / total_points) * 100)

  if (fallback_pct == 0) return(NULL)

  # Build source summary
  fallback_sources <- names(coverage)[!names(coverage) %in% primary_codes]
  source_names <- list(
    NWSS_WASTEWATER = "Wastewater",
    FLUSIGHT_FORECAST = "Forecasts",
    DELPHI = "Syndromic",
    STATE_HEALTH = "State Data",
    INTERPOLATED = "Interpolated"
  )
  source_text <- paste(sapply(fallback_sources, function(s) source_names[[s]] %||% s), collapse = ", ")

  # Build the banner
  detail_link <- if (show_details) {
    tags$a(
      href = "#",
      class = "alert-link ms-1",
      onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("show_fallback_details")),
      "View details",
      icon("chevron-right", class = "ms-1")
    )
  } else NULL

  div(
    class = "alert alert-info d-flex align-items-center mb-3 fallback-banner",
    style = "cursor: pointer; background: linear-gradient(90deg, #EFF6FF 0%, #DBEAFE 100%); border-left: 4px solid #3B82F6;",
    icon("info-circle", class = "me-2"),
    span(
      sprintf("%.0f%% of data from alternate signals: %s", fallback_pct, source_text)
    ),
    detail_link
  )
}

#' Render fallback details modal content
#'
#' @param data Data frame with source information
#' @return UI element with detailed source breakdown
render_fallback_details <- function(data) {
  coverage <- attr(data, "source_coverage")
  gap_periods <- attr(data, "gap_periods")

  if (is.null(coverage) || length(coverage) == 0) {
    return(div(class = "text-muted", "No data source information available"))
  }

  # Build source details table
  source_names <- list(
    CDC_FLUVIEW = "CDC FluView",
    CDC_NREVSS = "CDC NREVSS",
    CDC_COVID = "CDC COVID Tracker",
    RSV_NET = "RSV-NET",
    WHO_FLUMART = "WHO FluNet",
    ECDC = "ECDC Surveillance",
    NWSS_WASTEWATER = "Wastewater Surveillance",
    FLUSIGHT_FORECAST = "CDC FluSight Forecast",
    DELPHI = "Syndromic Signals",
    STATE_HEALTH = "State Health Depts",
    INTERPOLATED = "Interpolated"
  )

  source_reliability <- list(
    CDC_FLUVIEW = "High",
    CDC_NREVSS = "High",
    CDC_COVID = "High",
    RSV_NET = "High",
    WHO_FLUMART = "High",
    ECDC = "High",
    NWSS_WASTEWATER = "High",
    FLUSIGHT_FORECAST = "High (Ensemble)",
    DELPHI = "Medium",
    STATE_HEALTH = "High",
    INTERPOLATED = "Low"
  )

  primary_codes <- c("CDC_FLUVIEW", "CDC_NREVSS", "CDC_COVID", "RSV_NET", "WHO_FLUMART", "ECDC")

  # Build table rows
  table_rows <- lapply(names(coverage), function(src) {
    is_primary <- src %in% primary_codes
    reliability <- source_reliability[[src]] %||% "Unknown"
    reliability_class <- switch(reliability,
      "High" = "text-success",
      "High (Ensemble)" = "text-success",
      "Medium" = "text-warning",
      "Low" = "text-danger",
      ""
    )

    tags$tr(
      class = if (!is_primary) "table-info" else "",
      tags$td(
        if (!is_primary) icon("exchange-alt", class = "me-1 text-primary") else NULL,
        source_names[[src]] %||% src
      ),
      tags$td(coverage[[src]]),
      tags$td(class = reliability_class, reliability),
      tags$td(if (is_primary) "Primary" else "Alternate")
    )
  })

  # Gap periods section
  gap_section <- if (!is.null(gap_periods) && nrow(gap_periods) > 0) {
    gap_rows <- lapply(1:nrow(gap_periods), function(i) {
      row <- gap_periods[i, ]
      tags$li(
        class = "mb-1",
        sprintf("%s to %s (%d days) - %s",
                format(row$start, "%b %d"),
                format(row$end, "%b %d"),
                row$duration,
                row$fallback_sources)
      )
    })

    div(
      class = "mt-3",
      h6(class = "fw-bold", icon("calendar-times", class = "me-1"), "Primary Data Gaps"),
      tags$ul(class = "small", gap_rows)
    )
  } else NULL

  div(
    class = "fallback-details",
    h5(icon("database", class = "me-2"), "Data Sources Used"),
    p(class = "text-muted small",
      "Primary surveillance data was unavailable for some periods. ",
      "RespiWatch automatically filled these gaps with alternate signals."
    ),

    tags$table(
      class = "table table-sm table-hover mt-3",
      tags$thead(
        class = "table-light",
        tags$tr(
          tags$th("Source"),
          tags$th("Data Points"),
          tags$th("Reliability"),
          tags$th("Type")
        )
      ),
      tags$tbody(table_rows)
    ),

    gap_section,

    div(
      class = "alert alert-light mt-3 small",
      icon("sync-alt", class = "me-2 text-primary"),
      "When primary sources resume, RespiWatch will automatically switch back to official surveillance data."
    )
  )
}

#' Create small inline fallback indicator
#'
#' Compact indicator for use in KPI cards or headers
#'
#' @param has_fallback Logical, TRUE if fallback data is being used
#' @param fallback_pct Percentage of data from fallback sources
#' @return Small badge UI element
fallback_badge <- function(has_fallback, fallback_pct = NULL) {
  if (!has_fallback) return(NULL)

  label <- if (!is.null(fallback_pct) && fallback_pct > 0) {
    sprintf("~%.0f%% alternate", fallback_pct)
  } else {
    "alternate signals"
  }

  tags$span(
    class = "badge bg-info ms-2",
    title = "Some data from alternate signals (wastewater, syndromic, forecasts)",
    icon("exchange-alt", class = "me-1"),
    label
  )
}

# =============================================================================
# DATA SOURCE CALLOUT COMPONENT
# =============================================================================

#' Create Data Source Attribution Callout
#'
#' Displays a prominent callout showing data sources, date ranges, and freshness.
#' Used on each tab to provide transparent data attribution.
#'
#' @param sources_list List of source objects, each with: name, start_date, end_date, status
#' @param last_updated POSIXct timestamp of last data refresh
#' @param note Optional additional note (e.g., "Federal reporting ended May 2024")
#' @return UI element with styled data source attribution
#'
#' @examples
#' data_source_callout(
#'   sources_list = list(
#'     list(name = "CDC FluView", start_date = "2023-01-01", end_date = "2025-12-08", status = "current"),
#'     list(name = "HHS Hospital Data", start_date = "2023-04-30", end_date = "2024-04-27", status = "historical")
#'   ),
#'   last_updated = Sys.time(),
#'   note = "Hospital capacity reporting ended May 2024"
#' )
data_source_callout <- function(sources_list, last_updated = NULL, note = NULL) {

  # Build source items
  source_items <- lapply(sources_list, function(src) {
    # Status indicator styling
    status_class <- if (src$status == "current") {
      "status-current"
    } else {
      "status-historical"
    }

    status_label <- if (src$status == "current") {
      tags$span(class = "badge bg-success ms-2", "Current")
    } else {
      tags$span(class = "badge bg-warning text-dark ms-2", "Historical")
    }

    # Format date range
    start_fmt <- if (!is.null(src$start_date)) {
      format(as.Date(src$start_date), "%b %Y")
    } else "N/A"

    end_fmt <- if (!is.null(src$end_date)) {
      format(as.Date(src$end_date), "%b %Y")
    } else "Present"

    tags$li(
      class = paste("source-item", status_class),
      tags$span(class = "source-name fw-medium", src$name),
      tags$span(class = "source-dates text-muted ms-2",
        sprintf("(%s - %s)", start_fmt, end_fmt)
      ),
      status_label
    )
  })

  # Last updated timestamp
  timestamp_html <- if (!is.null(last_updated)) {
    tags$div(
      class = "last-updated mt-2 pt-2 border-top",
      icon("clock", class = "me-1 text-muted"),
      tags$small(class = "text-muted",
        sprintf("Last refreshed: %s", format(last_updated, "%b %d, %Y at %I:%M %p"))
      )
    )
  } else NULL

  # Note section (for special messages)
  note_html <- if (!is.null(note)) {
    tags$div(
      class = "source-note mt-2 pt-2 border-top",
      icon("info-circle", class = "me-1 text-info"),
      tags$small(class = "text-muted fst-italic", note)
    )
  } else NULL

  # Build the callout
  div(
    class = "data-source-callout",
    div(
      class = "callout-header d-flex align-items-center mb-2",
      icon("database", class = "me-2 text-teal"),
      tags$h6(class = "mb-0 fw-bold", "Data Sources")
    ),
    tags$ul(
      class = "source-list list-unstyled mb-0",
      source_items
    ),
    note_html,
    timestamp_html,
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
}

#' Get data source metadata for a tab
#'
#' Queries the database to get actual date ranges and freshness for data sources
#'
#' @param source_codes Vector of source codes to query (e.g., c("CDC_FLUVIEW", "ECDC"))
#' @param pathogen_code Optional pathogen filter
#' @return List suitable for data_source_callout()
get_data_source_metadata <- function(source_codes, pathogen_code = NULL) {
  conn <- tryCatch(get_db_connection(), error = function(e) NULL)
  if (is.null(conn)) {
    return(list())
  }

  on.exit(close_db_connection(conn))

  source_names <- list(
    CDC_FLUVIEW = "CDC FluView",
    CDC_NREVSS = "CDC NREVSS",
    CDC_COVID = "CDC COVID Tracker",
    RSV_NET = "RSV-NET",
    WHO_FLUMART = "WHO FluNet",
    ECDC = "ECDC Surveillance",
    HHS_HEALTHDATA = "HHS HealthData.gov",
    NWSS_WASTEWATER = "Wastewater (NWSS)",
    FLUSIGHT_FORECAST = "FluSight Forecast",
    DELPHI = "Delphi Syndromic"
  )

  # Query actual date ranges from surveillance_data
  results <- lapply(source_codes, function(src) {
    query <- sprintf("
      SELECT
        MIN(sd.observation_date) as start_date,
        MAX(sd.observation_date) as end_date,
        MAX(sd.fetch_timestamp) as last_fetch
      FROM surveillance_data sd
      JOIN data_sources ds ON sd.source_id = ds.source_id
      WHERE ds.source_code = '%s'
    ", src)

    if (!is.null(pathogen_code)) {
      query <- sprintf("
        SELECT
          MIN(sd.observation_date) as start_date,
          MAX(sd.observation_date) as end_date,
          MAX(sd.fetch_timestamp) as last_fetch
        FROM surveillance_data sd
        JOIN data_sources ds ON sd.source_id = ds.source_id
        JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
        WHERE ds.source_code = '%s' AND p.pathogen_code = '%s'
      ", src, pathogen_code)
    }

    row <- tryCatch(
      dbGetQuery(conn, query),
      error = function(e) data.frame(start_date = NA, end_date = NA, last_fetch = NA)
    )

    if (nrow(row) == 0 || is.na(row$start_date)) {
      return(NULL)
    }

    # Determine if current (within last 14 days)
    end_date <- as.Date(row$end_date)
    is_current <- !is.na(end_date) && (Sys.Date() - end_date) <= 14

    list(
      name = source_names[[src]] %||% src,
      start_date = row$start_date,
      end_date = row$end_date,
      status = if (is_current) "current" else "historical"
    )
  })

  # Filter out NULLs
  Filter(Negate(is.null), results)
}

#' Get healthcare capacity metadata
#'
#' Special function for healthcare capacity which has different table structure
#'
#' @return List suitable for data_source_callout()
get_healthcare_source_metadata <- function() {
  conn <- tryCatch(get_db_connection(), error = function(e) NULL)
  if (is.null(conn)) {
    return(list(
      list(name = "HHS HealthData.gov", start_date = NA, end_date = NA, status = "historical")
    ))
  }

  on.exit(close_db_connection(conn))

  row <- tryCatch(
    dbGetQuery(conn, "
      SELECT
        MIN(observation_date) as start_date,
        MAX(observation_date) as end_date,
        MAX(fetch_timestamp) as last_fetch
      FROM healthcare_capacity
      WHERE total_hospital_beds IS NOT NULL
    "),
    error = function(e) data.frame(start_date = NA, end_date = NA, last_fetch = NA)
  )

  if (nrow(row) == 0 || is.na(row$start_date)) {
    return(list(
      list(name = "HHS HealthData.gov", start_date = NA, end_date = NA, status = "historical")
    ))
  }

  # Healthcare data is historical (HHS stopped reporting May 2024)
  list(
    list(
      name = "HHS HealthData.gov",
      start_date = row$start_date,
      end_date = row$end_date,
      status = "historical"
    )
  )
}

# =============================================================================
# KPI CARD COMPONENT
# =============================================================================

#' Create a KPI Card
#'
#' Generates a styled KPI card for displaying metrics
#'
#' @param title Card title
#' @param value Main metric value
#' @param change Optional change indicator (e.g., "+5%")
#' @param change_direction Optional: "up", "down", or NULL for neutral
#' @param icon Optional icon name (FontAwesome)
#' @param col_width Bootstrap column width (default: 3)
#' @param severity Optional severity level: "low", "moderate", "high", "critical"
#' @return A div containing the styled KPI card
kpi_card <- function(title, value, change = NULL, change_direction = NULL,
                     icon = NULL, col_width = 3, severity = NULL) {

  # Determine severity color
  severity_class <- if (!is.null(severity)) {
    switch(severity,
      "low" = "border-success",
      "moderate" = "border-warning",
      "high" = "border-danger",
      "critical" = "border-danger border-3",
      ""
    )
  } else ""

  # Change indicator styling
  change_html <- if (!is.null(change)) {
    change_color <- switch(change_direction %||% "neutral",
      "up" = "text-danger",
      "down" = "text-success",
      "text-muted"
    )
    change_icon <- switch(change_direction %||% "neutral",
      "up" = icon("arrow-up"),
      "down" = icon("arrow-down"),
      NULL
    )
    tags$small(class = paste("mt-1", change_color), change_icon, change)
  } else NULL

  # Icon element
  icon_html <- if (!is.null(icon)) {
    tags$span(class = "me-2 text-muted", icon(icon))
  } else NULL

  div(
    class = sprintf("col-md-%d", col_width),
    div(
      class = paste("card h-100", severity_class),
      style = "background: var(--surface-card);",
      div(
        class = "card-body text-center",
        icon_html,
        h6(class = "card-subtitle mb-1 text-muted", title),
        h3(class = "card-title mb-0 fw-bold", value),
        change_html
      )
    )
  )
}

# =============================================================================
# DISEASE WAVE PROPAGATION MINI-CARD
# =============================================================================

#' Create a Disease Wave Propagation Mini-Card
#'
#' Generates a compact card for displaying wave propagation metrics per disease
#'
#' @param disease_name Display name (e.g., "H3N2 (Influenza)")
#' @param pathogen_code Database code (e.g., "H3N2")
#' @param color Hex color for the card accent
#' @param ns Shiny namespace function
#' @return A div containing the styled mini-card
disease_wave_card <- function(disease_name, pathogen_code, color, ns) {
  div(
    class = "disease-wave-card",
    style = sprintf("border-left: 4px solid %s; background: var(--bg-primary);", color),
    div(
      class = "card-header d-flex align-items-center gap-2 px-3 py-2",
      span(
        class = "disease-icon",
        style = sprintf("color: %s;", color),
        icon("virus")
      ),
      span(class = "disease-name fw-semibold", disease_name)
    ),
    div(
      class = "card-body px-3 py-2",
      div(
        class = "row g-2",
        div(
          class = "col-4 text-center",
          div(
            class = "metric-value fs-6 fw-bold",
            uiOutput(ns(paste0("wave_velocity_", pathogen_code)), inline = TRUE)
          ),
          div(class = "metric-label text-muted small", "km/day")
        ),
        div(
          class = "col-4 text-center",
          div(
            class = "metric-value fs-6 fw-bold",
            uiOutput(ns(paste0("wave_countries_", pathogen_code)), inline = TRUE)
          ),
          div(class = "metric-label text-muted small", "Countries")
        ),
        div(
          class = "col-4 text-center",
          div(
            class = "metric-value fs-6 fw-bold",
            uiOutput(ns(paste0("wave_confidence_", pathogen_code)), inline = TRUE)
          ),
          div(class = "metric-label text-muted small", "Confidence")
        )
      )
    )
  )
}

# =============================================================================
# PATHOGEN SELECTOR
# =============================================================================

#' Create a Pathogen Selector Dropdown
#'
#' Generates a standardized selectInput for pathogen selection
#'
#' @param ns Namespace function from Shiny module
#' @param id Input ID (default: "pathogen")
#' @param include_all Whether to include "All Pathogens" option
#' @param label Optional label (NULL for no label)
#' @param selected Default selected value
#' @param radio Use radio buttons instead of dropdown
#' @return A selectInput or radioButtons element
pathogen_selector <- function(ns, id = "pathogen", include_all = FALSE,
                              label = "Select Pathogen", selected = NULL,
                              radio = FALSE) {

  choices <- c(
    "H3N2 (Influenza)" = "H3N2",
    "RSV" = "RSV",
    "COVID-19" = "COVID19"
  )

  if (include_all) {
    choices <- c("All Pathogens" = "all", choices)
    if (is.null(selected)) selected <- "all"
  } else {
    if (is.null(selected)) selected <- "H3N2"
  }

  if (radio) {
    radioButtons(
      ns(id),
      label = label,
      choices = choices,
      selected = selected,
      inline = TRUE
    )
  } else {
    selectInput(
      ns(id),
      label = label,
      choices = choices,
      selected = selected
    )
  }
}

#' Map UI pathogen code to database pathogen code
#'
#' Converts UI-friendly pathogen codes to database codes
#'
#' @param ui_code UI pathogen code (e.g., "h3n2", "rsv", "covid")
#' @return Database pathogen code (e.g., "H3N2", "RSV", "COVID19")
map_pathogen_code <- function(ui_code) {
  switch(
    tolower(ui_code),
    "all" = NULL,
    "h3n2" = "H3N2",
    "rsv" = "RSV",
    "covid" = "COVID19",
    "covid19" = "COVID19",
    "h5n1" = "H5N1",
    ui_code  # Return as-is if not mapped
  )
}

# =============================================================================
# EMPTY STATE / ERROR MESSAGES
# =============================================================================

#' Empty Plot Message
#'
#' Returns a styled placeholder for empty plots
#'
#' @param message Message to display
#' @param icon_name Optional icon (default: "chart-bar")
#' @return Plotly object with centered message
empty_plot_message <- function(message = "No data available", icon_name = "chart-bar") {
  plotly::plot_ly() |>
    plotly::layout(
      annotations = list(
        list(
          text = message,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, color = "#666")
        )
      ),
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}

#' Empty State Card
#'
#' Returns a styled div for empty states in UI
#'
#' @param message Message to display
#' @param icon_name Optional icon (default: "inbox")
#' @param action_btn Optional action button to add
#' @return A tagList with styled empty state
empty_state_card <- function(message = "No data available", icon_name = "inbox",
                             action_btn = NULL) {
  div(
    class = "empty-state text-center py-5",
    style = "color: var(--text-muted);",
    icon(icon_name, class = "fa-3x mb-3"),
    h5(message),
    action_btn
  )
}

# =============================================================================
# LOADING / BUTTON STATE HELPERS
# =============================================================================

#' Loading Button Wrapper
#'
#' Executes code while showing loading state on button
#'
#' @param session Shiny session object
#' @param btn_id Button input ID (without namespace)
#' @param running_label Label to show while running
#' @param original_label Label to restore after completion
#' @param original_icon Original icon (FontAwesome name)
#' @param expr Expression to execute
#' @return Result of expression
with_loading_button <- function(session, btn_id, running_label, original_label,
                                original_icon, expr) {
  # Update to loading state
  shinyjs::disable(btn_id)
  updateActionButton(session, btn_id,
    label = running_label,
    icon = icon("spinner", class = "fa-spin")
  )

  # Ensure button is restored on exit (even on error)
  on.exit({
    shinyjs::enable(btn_id)
    updateActionButton(session, btn_id,
      label = original_label,
      icon = icon(original_icon)
    )
  })

  # Execute expression
  force(expr)
}

# =============================================================================
# SAFE RENDER WRAPPER
# =============================================================================

#' Safe Render Error Boundary
#'
#' Wraps render expressions with error handling and logging
#'
#' @param expr Expression to evaluate
#' @param fallback_message Message to show on error
#' @param context Description for logging
#' @return Result of expression or error message UI
safe_render <- function(expr, fallback_message = "Error loading content",
                        context = "render") {
  tryCatch(
    expr,
    error = function(e) {
      # Log the error if logging is available
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("%s failed: %s", context, e$message), category = "ui")
      }

      # Return error UI
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        sprintf(" %s: %s", fallback_message, e$message)
      )
    }
  )
}

#' Safe Render for Plotly
#'
#' Wraps plotly render expressions with error handling
#'
#' @param expr Expression to evaluate (should return plotly object)
#' @param fallback_message Message to show on error
#' @param context Description for logging
#' @return Plotly object or empty plot with error message
safe_render_plotly <- function(expr, fallback_message = "Error loading chart",
                               context = "plotly_render") {
  tryCatch(
    expr,
    error = function(e) {
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("%s failed: %s", context, e$message), category = "ui")
      }
      empty_plot_message(paste(fallback_message, "-", e$message))
    }
  )
}

# =============================================================================
# VALUE BOX HELPER (for bslib)
# =============================================================================

#' Create Value Box
#'
#' Wrapper for bslib::value_box with consistent styling
#'
#' @param title Box title
#' @param value Main value to display
#' @param subtitle Optional subtitle
#' @param icon_name Icon name (FontAwesome)
#' @param theme Color theme: "primary", "success", "warning", "danger"
#' @return A bslib value_box
create_value_box <- function(title, value, subtitle = NULL, icon_name = "chart-line",
                             theme = "primary") {
  bslib::value_box(
    title = title,
    value = value,
    showcase = bsicons::bs_icon(icon_name),
    p(subtitle),
    theme = theme
  )
}

# =============================================================================
# DATA VALIDATION GUARDS
# =============================================================================

#' Check if data is valid for rendering
#'
#' Validates that data exists and has rows
#'
#' @param data Data frame to check
#' @param required_cols Optional vector of required column names
#' @return TRUE if valid, FALSE otherwise
is_valid_data <- function(data, required_cols = NULL) {
  if (is.null(data)) return(FALSE)
  if (!is.data.frame(data)) return(FALSE)
  if (nrow(data) == 0) return(FALSE)

  if (!is.null(required_cols)) {
    if (!all(required_cols %in% names(data))) return(FALSE)
  }

  TRUE
}

#' Guard clause for invalid data
#'
#' Returns early with fallback if data is invalid
#'
#' @param data Data to validate
#' @param fallback Value to return if invalid
#' @param required_cols Optional required columns
#' @return NULL if valid (continue), fallback if invalid
guard_invalid_data <- function(data, fallback = NULL, required_cols = NULL) {
  if (!is_valid_data(data, required_cols)) {
    return(fallback)
  }
  NULL
}

# =============================================================================
# ERROR MESSAGE EXTRACTION
# =============================================================================

#' Safely Extract Error Message
#'
#' Extracts a human-readable error message from various error object types.
#' Handles R condition objects, simpleError, and plain strings to avoid
#' "[object Object]" style display issues.
#'
#' @param e Error object (condition, simpleError, list with $message, or character)
#' @param default_message Fallback message if extraction fails
#' @return Character string with the error message
#' @examples
#' tryCatch(stop("test error"), error = function(e) safe_error_message(e))
#' # Returns: "test error"
safe_error_message <- function(e, default_message = "Unknown error") {
  msg <- tryCatch({
    if (inherits(e, "condition")) {
      # R condition objects (error, warning, message)
      conditionMessage(e)
    } else if (is.list(e) && !is.null(e$message)) {
      # List-like objects with $message field
      as.character(e$message)
    } else if (is.character(e) && length(e) > 0) {
      # Plain character strings
      e[1]
    } else {
      # Last resort: try as.character
      as.character(e)[1]
    }
  }, error = function(inner_e) {
    default_message
  })

  # Ensure we always return a valid string

  if (is.null(msg) || is.na(msg) || nchar(trimws(msg)) == 0) {
    return(default_message)
  }

  # Truncate very long messages
  if (nchar(msg) > 500) {
    msg <- paste0(substr(msg, 1, 497), "...")
  }

  msg
}

#' Safe Render for Leaflet Maps
#'
#' Wraps leaflet render expressions with error handling
#'
#' @param expr Expression to evaluate (should return leaflet object)
#' @param fallback_message Message to show on error
#' @param context Description for logging
#' @return Leaflet object or fallback map with error message
safe_render_leaflet <- function(expr, fallback_message = "Error loading map",
                                context = "leaflet_render") {
  tryCatch(
    expr,
    error = function(e) {
      error_msg <- safe_error_message(e)

      if (exists("log_error", mode = "function")) {
        log_error(sprintf("%s failed: %s", context, error_msg), category = "ui")
      }

      # Return fallback map with error message
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::setView(lng = 0, lat = 30, zoom = 2) |>
        leaflet::addControl(
          html = sprintf(
            "<div style='padding: 12px; background: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; max-width: 300px;'>
              <strong style='color: #856404;'>%s</strong><br>
              <small style='color: #856404;'>%s</small>
            </div>",
            htmltools::htmlEscape(fallback_message),
            htmltools::htmlEscape(error_msg)
          ),
          position = "topright"
        )
    }
  )
}

# =============================================================================
# CODE TRANSPARENCY COMPONENT
# =============================================================================

#' Create Chart Header with Code Transparency Buttons
#'
#' Generates a chart header with title and optional info/code buttons.
#' The code button opens a modal showing how the data is fetched and visualized.
#'
#' @param title Chart title
#' @param ns Namespace function from Shiny module
#' @param chart_id Unique identifier for this chart (used for button IDs)
#' @param show_info Show info button linking to About data section (default TRUE)
#' @param show_code Show code transparency button (default TRUE)
#' @param subtitle Optional subtitle text
#' @return A div containing the styled chart header with buttons
chart_header_with_code <- function(title, ns, chart_id, show_info = TRUE,
                                   show_code = TRUE, subtitle = NULL) {
  buttons <- tagList()

  if (show_info) {
    buttons <- tagList(
      buttons,
      tags$button(
        class = "btn btn-sm btn-outline-secondary chart-header-btn",
        type = "button",
        title = "Learn about data sources",
        onclick = "Shiny.setInputValue('nav_to_about_data', Math.random())",
        icon("circle-info")
      )
    )
  }

  if (show_code) {
    buttons <- tagList(
      buttons,
      tags$button(
        class = "btn btn-sm btn-outline-secondary chart-header-btn",
        type = "button",
        title = "View source code",
        onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns(paste0("show_code_", chart_id))),
        icon("code")
      )
    )
  }

  div(
    class = "chart-header-with-buttons d-flex justify-content-between align-items-start mb-3",
    div(
      h4(class = "section-header mb-0", title),
      if (!is.null(subtitle)) tags$small(class = "text-muted", subtitle)
    ),
    div(class = "chart-header-actions d-flex gap-1", buttons)
  )
}

#' Code Transparency Buttons Only
#'
#' Returns just the info and code buttons for use in headers that already
#' have their own d-flex layout with other action buttons.
#'
#' @param ns Shiny namespace function
#' @param chart_id Unique identifier for this chart (used for button IDs)
#' @param show_info Show info button linking to About data section (default TRUE)
#' @param show_code Show code transparency button (default TRUE)
#' @return A tagList containing the buttons
code_transparency_buttons <- function(ns, chart_id, show_info = TRUE, show_code = TRUE) {
  buttons <- tagList()

  if (show_info) {
    buttons <- tagList(
      buttons,
      tags$button(
        class = "btn btn-sm btn-outline-secondary chart-header-btn",
        type = "button",
        title = "Learn about data sources",
        onclick = "Shiny.setInputValue('nav_to_about_data', Math.random())",
        icon("circle-info")
      )
    )
  }

  if (show_code) {
    buttons <- tagList(
      buttons,
      tags$button(
        class = "btn btn-sm btn-outline-secondary chart-header-btn",
        type = "button",
        id = ns(paste0("show_code_", chart_id)),
        title = "View chart code",
        onclick = sprintf(
          "Shiny.setInputValue('%s', Math.random(), {priority: 'event'})",
          ns(paste0("show_code_", chart_id))
        ),
        icon("code")
      )
    )
  }

  buttons
}

#' Create Code Modal Content
#'
#' Generates the content for a code transparency modal showing data fetch
#' and visualization code with copy buttons.
#'
#' @param title Modal title
#' @param data_code R code for fetching/processing data
#' @param viz_code R code for creating the visualization
#' @param data_description Brief description of the data source
#' @param viz_description Brief description of the visualization
#' @return A tagList with modal content
code_modal_content <- function(title, data_code = NULL, viz_code = NULL,
                               data_description = NULL, viz_description = NULL) {
  # Generate unique IDs for code blocks
  data_code_id <- paste0("code-data-", format(Sys.time(), "%H%M%S%OS3"))
  viz_code_id <- paste0("code-viz-", format(Sys.time(), "%H%M%S%OS3"))

  sections <- tagList()

  # Data section
  if (!is.null(data_code)) {
    sections <- tagList(
      sections,
      div(
        class = "code-section",
        div(
          class = "code-section-header",
          span(class = "code-section-title", icon("database"), "Data Acquisition"),
          tags$button(
            class = "copy-code-btn",
            type = "button",
            onclick = sprintf("copyCodeToClipboard(this, '%s')", data_code_id),
            icon("copy"), " Copy"
          )
        ),
        if (!is.null(data_description)) {
          tags$p(class = "code-section-description", data_description)
        },
        div(
          class = "code-block",
          div(
            class = "code-block-header",
            span(class = "code-block-lang", "R")
          ),
          div(
            class = "code-content",
            tags$pre(id = data_code_id, data_code)
          )
        )
      )
    )
  }

  # Visualization section
  if (!is.null(viz_code)) {
    sections <- tagList(
      sections,
      div(
        class = "code-section",
        div(
          class = "code-section-header",
          span(class = "code-section-title", icon("chart-line"), "Visualization"),
          tags$button(
            class = "copy-code-btn",
            type = "button",
            onclick = sprintf("copyCodeToClipboard(this, '%s')", viz_code_id),
            icon("copy"), " Copy"
          )
        ),
        if (!is.null(viz_description)) {
          tags$p(class = "code-section-description", viz_description)
        },
        div(
          class = "code-block",
          div(
            class = "code-block-header",
            span(class = "code-block-lang", "R")
          ),
          div(
            class = "code-content",
            tags$pre(id = viz_code_id, viz_code)
          )
        )
      )
    )
  }

  div(
    class = "code-transparency-content",
    div(
      class = "alert alert-light border mb-3",
      icon("lightbulb", class = "me-2 text-warning"),
      tags$small(
        "These code snippets show the methodology used for this analysis. ",
        "Copy and adapt them for your own R projects."
      )
    ),
    sections
  )
}

#' Show Code Transparency Modal
#'
#' Helper function to display a code transparency modal
#'
#' @param session Shiny session
#' @param title Modal title
#' @param data_code R code for data acquisition
#' @param viz_code R code for visualization
#' @param data_description Description of data source
#' @param viz_description Description of visualization
show_code_modal <- function(session, title, data_code = NULL, viz_code = NULL,
                            data_description = NULL, viz_description = NULL) {
  showModal(modalDialog(
    title = tagList(icon("code", class = "me-2"), title),
    code_modal_content(
      title = title,
      data_code = data_code,
      viz_code = viz_code,
      data_description = data_description,
      viz_description = viz_description
    ),
    footer = tagList(
      tags$a(
        href = "#",
        class = "btn btn-outline-secondary",
        onclick = "Shiny.setInputValue('nav_to_about_data', Math.random()); Shiny.modalClose();",
        icon("book", class = "me-1"),
        "View Full Data Documentation"
      ),
      modalButton("Close")
    ),
    size = "l",
    easyClose = TRUE
  ))
}

# =============================================================================
# CODE SNIPPETS REGISTRY
# =============================================================================

#' Get Code Snippet for a Chart
#'
#' Returns curated code snippets for data fetching and visualization.
#' These are clean, educational versions of the actual implementation.
#'
#' @param chart_id Identifier for the chart
#' @return List with data_code, viz_code, data_desc, viz_desc
get_code_snippet <- function(chart_id) {

  snippets <- list(

    # Rt Time Series
    rt_timeseries = list(
      data_desc = "Rt (reproduction number) estimated using EpiEstim package with CDC/WHO surveillance data",
      data_code = '# Load pre-computed Rt estimates from database
library(DBI)
library(RSQLite)

conn <- dbConnect(SQLite(), "respiwatch.db")

# Query Rt estimates for selected pathogen
rt_data <- dbGetQuery(conn, "
  SELECT observation_date as date,
         rt_mean, rt_lower, rt_upper,
         cases_input
  FROM rt_estimates
  WHERE pathogen_code = \'H3N2\'
    AND country_code = \'USA\'
  ORDER BY observation_date
")

dbDisconnect(conn)

# Alternative: Compute Rt directly using EpiEstim
library(EpiEstim)

# Prepare incidence data
incidence <- data.frame(
  dates = as.Date(surveillance$observation_date),
  I = surveillance$cases
)
incidence <- incidence[complete.cases(incidence), ]

# Estimate Rt with 7-day sliding window
rt_result <- estimate_R(
  incid = incidence,
  method = "parametric_si",
  config = make_config(
    mean_si = 4.7,  # Serial interval mean (pathogen-specific)
    std_si = 2.9,   # Serial interval SD
    t_start = seq(2, nrow(incidence) - 6),
    t_end = seq(8, nrow(incidence))
  )
)',
      viz_desc = "Time series with 95% credible interval ribbon using ggplot2 + plotly",
      viz_code = 'library(ggplot2)
library(plotly)

p <- ggplot(rt_data, aes(x = date)) +
  # 95% credible interval ribbon
geom_ribbon(
    aes(ymin = rt_lower, ymax = rt_upper),
    fill = "#E85D4C", alpha = 0.2
  ) +
  # Mean Rt line
  geom_line(
    aes(y = rt_mean),
    color = "#E85D4C", linewidth = 1.2
  ) +
  # Epidemic threshold
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "#6B7280"
  ) +
  annotate(
    "text", x = min(rt_data$date), y = 1.05,
    label = "Rt = 1 (threshold)",
    hjust = 0, size = 3, color = "#6B7280"
  ) +
  labs(x = NULL, y = "Reproduction Number (Rt)") +
  theme_minimal()

ggplotly(p, tooltip = c("x", "y")) |>
  config(displayModeBar = FALSE)'
    ),

    # Forecast plot
    forecast = list(
      data_desc = "4-week probabilistic forecast using Rt-renewal equation with negative binomial uncertainty",
      data_code = '# Generate forecast from current Rt
library(dplyr)

forecast_horizon <- 4  # weeks
current_rt <- tail(rt_data$rt_mean, 1)
last_cases <- tail(surveillance$cases, 1)

# Renewal equation: I(t+1) = Rt * sum(I(t-s) * w(s))
# where w(s) is the serial interval distribution

forecast <- tibble(
  forecast_week = 1:forecast_horizon,
  date = seq(
    from = max(surveillance$date) + 7,
    by = "week",
    length.out = forecast_horizon
  ),
  rt_used = current_rt
) |>
  mutate(
    # Point estimate using renewal equation
    predicted_cases = round(last_cases * rt_used^forecast_week),
    # Negative binomial uncertainty intervals
    lower_50 = qnbinom(0.25, mu = predicted_cases, size = 10),
    upper_50 = qnbinom(0.75, mu = predicted_cases, size = 10),
    lower_80 = qnbinom(0.10, mu = predicted_cases, size = 10),
    upper_80 = qnbinom(0.90, mu = predicted_cases, size = 10),
    lower_95 = qnbinom(0.025, mu = predicted_cases, size = 10),
    upper_95 = qnbinom(0.975, mu = predicted_cases, size = 10)
  )',
      viz_desc = "Forecast with multiple prediction intervals (50%, 80%, 95%)",
      viz_code = 'library(ggplot2)
library(plotly)

p <- ggplot() +
  # Prediction intervals (outermost to innermost)
  geom_ribbon(
    data = forecast,
    aes(x = date, ymin = lower_95, ymax = upper_95),
    fill = "#0D9488", alpha = 0.15
  ) +
  geom_ribbon(
    data = forecast,
    aes(x = date, ymin = lower_80, ymax = upper_80),
    fill = "#0D9488", alpha = 0.25
  ) +
  geom_ribbon(
    data = forecast,
    aes(x = date, ymin = lower_50, ymax = upper_50),
    fill = "#0D9488", alpha = 0.35
  ) +
  # Historical observed data
  geom_line(
    data = tail(historical, 12),
    aes(x = date, y = cases),
    color = "#374151", linewidth = 1
  ) +
  geom_point(
    data = tail(historical, 12),
    aes(x = date, y = cases),
    color = "#374151", size = 2
  ) +
  # Forecast line
  geom_line(
    data = forecast,
    aes(x = date, y = predicted_cases),
    color = "#0D9488", linewidth = 1.2, linetype = "dashed"
  ) +
  labs(x = NULL, y = "Cases") +
  theme_minimal()

ggplotly(p) |> config(displayModeBar = FALSE)'
    ),

    # Healthcare capacity gauges
    healthcare_gauge = list(
      data_desc = "Hospital capacity data from HHS HealthData.gov (historical: ended May 2024)",
      data_code = '# Fetch hospital capacity data from database
library(DBI)
library(RSQLite)

conn <- dbConnect(SQLite(), "respiwatch.db")

capacity <- dbGetQuery(conn, "
  SELECT
    state,
    observation_date,
    total_hospital_beds,
    inpatient_beds_occupied,
    icu_beds_total,
    icu_beds_used,
    inpatient_bed_utilization,
    icu_utilization
  FROM healthcare_capacity
  WHERE observation_date = (
    SELECT MAX(observation_date)
    FROM healthcare_capacity
  )
")

dbDisconnect(conn)

# Calculate national aggregates
national <- capacity |>
  summarise(
    total_beds = sum(total_hospital_beds, na.rm = TRUE),
    occupied_beds = sum(inpatient_beds_occupied, na.rm = TRUE),
    total_icu = sum(icu_beds_total, na.rm = TRUE),
    occupied_icu = sum(icu_beds_used, na.rm = TRUE)
  ) |>
  mutate(
    bed_utilization = occupied_beds / total_beds * 100,
    icu_utilization = occupied_icu / total_icu * 100
  )',
      viz_desc = "Gauge visualization using plotly with color-coded stress levels",
      viz_code = 'library(plotly)

# Create gauge chart
gauge <- plot_ly(
  type = "indicator",
  mode = "gauge+number",
  value = national$bed_utilization,
  title = list(text = "Hospital Bed Utilization"),
  gauge = list(
    axis = list(range = list(0, 100)),
    bar = list(color = "#0D9488"),
    steps = list(
      list(range = c(0, 60), color = "#D1FAE5"),    # Green: OK
      list(range = c(60, 80), color = "#FEF3C7"),   # Yellow: Elevated
      list(range = c(80, 90), color = "#FED7AA"),   # Orange: High
      list(range = c(90, 100), color = "#FECACA")   # Red: Critical
    ),
    threshold = list(
      line = list(color = "#DC2626", width = 4),
      thickness = 0.75,
      value = 85  # Critical threshold
    )
  )
) |>
  layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    font = list(color = "#374151")
  )'
    ),

    # Bayesian forecast
    bayesian_forecast = list(
      data_desc = "Bayesian hierarchical model fitted with brms/Stan for probabilistic forecasting",
      data_code = '# Fit Bayesian model using brms
library(brms)
library(tidyverse)

# Prepare data for modeling
model_data <- surveillance |>
  arrange(observation_date) |>
  mutate(
    time_index = row_number(),
    week = as.numeric(format(observation_date, "%W")),
    year = year(observation_date)
  )

# Fit negative binomial model with trend and seasonality
fit <- brm(
  cases ~ s(time_index) + s(week, bs = "cc", k = 12),
  data = model_data,
  family = negbinomial(),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 42
)

# Generate posterior predictions
future_dates <- seq(
  max(model_data$observation_date) + 7,
  by = "week",
  length.out = 8
)

new_data <- tibble(
  observation_date = future_dates,
  time_index = max(model_data$time_index) + 1:8,
  week = as.numeric(format(future_dates, "%W"))
)

# Posterior predictive samples
predictions <- posterior_predict(fit, newdata = new_data)',
      viz_desc = "Fan chart showing 50%, 80%, 95% credible intervals from posterior samples",
      viz_code = 'library(ggplot2)
library(tidybayes)

# Summarize posterior predictions
forecast_summary <- predictions |>
  as_tibble() |>
  pivot_longer(everything(), names_to = "horizon", values_to = "value") |>
  group_by(horizon) |>
  summarise(
    median = median(value),
    lower_50 = quantile(value, 0.25),
    upper_50 = quantile(value, 0.75),
    lower_80 = quantile(value, 0.10),
    upper_80 = quantile(value, 0.90),
    lower_95 = quantile(value, 0.025),
    upper_95 = quantile(value, 0.975)
  ) |>
  mutate(date = future_dates)

# Fan chart visualization
p <- ggplot() +
  # Historical data
  geom_line(
    data = tail(model_data, 26),
    aes(x = observation_date, y = cases),
    color = "#374151", linewidth = 1
  ) +
  # Prediction intervals
  geom_ribbon(
    data = forecast_summary,
    aes(x = date, ymin = lower_95, ymax = upper_95),
    fill = "#8B5CF6", alpha = 0.2
  ) +
  geom_ribbon(
    data = forecast_summary,
    aes(x = date, ymin = lower_80, ymax = upper_80),
    fill = "#8B5CF6", alpha = 0.3
  ) +
  geom_ribbon(
    data = forecast_summary,
    aes(x = date, ymin = lower_50, ymax = upper_50),
    fill = "#8B5CF6", alpha = 0.4
  ) +
  geom_line(
    data = forecast_summary,
    aes(x = date, y = median),
    color = "#7C3AED", linewidth = 1.2
  ) +
  labs(
    x = NULL, y = "Cases",
    caption = "Shaded regions: 50%, 80%, 95% credible intervals"
  ) +
  theme_minimal()

ggplotly(p)'
    ),

    # Surveillance map
    global_map = list(
      data_desc = "Multi-source surveillance data from CDC, WHO FluNet, and ECDC aggregated by country",
      data_code = '# Fetch global surveillance data
library(DBI)

conn <- dbConnect(SQLite(), "respiwatch.db")

# Query latest data per country
surveillance_map <- dbGetQuery(conn, "
  SELECT
    c.country_code,
    c.country_name,
    c.latitude,
    c.longitude,
    sd.cases,
    sd.positivity_rate,
    sd.observation_date,
    ds.source_name
  FROM surveillance_data sd
  JOIN countries c ON sd.country_id = c.country_id
  JOIN data_sources ds ON sd.source_id = ds.source_id
  WHERE sd.observation_date >= date(\'now\', \'-7 days\')
    AND sd.pathogen_id = (
      SELECT pathogen_id FROM pathogens
      WHERE pathogen_code = \'H3N2\'
    )
")

dbDisconnect(conn)

# Aggregate to country level
country_summary <- surveillance_map |>
  group_by(country_code, country_name, latitude, longitude) |>
  summarise(
    total_cases = sum(cases, na.rm = TRUE),
    avg_positivity = mean(positivity_rate, na.rm = TRUE),
    sources = paste(unique(source_name), collapse = ", "),
    .groups = "drop"
  )',
      viz_desc = "Interactive choropleth map using Leaflet with country-level coloring",
      viz_code = 'library(leaflet)
library(sf)
library(rnaturalearth)

# Get world boundaries
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(iso_a3, name, geometry)

# Join surveillance data
map_data <- world |>
  left_join(country_summary, by = c("iso_a3" = "country_code"))

# Color palette based on positivity rate
pal <- colorNumeric(
  palette = c("#D1FAE5", "#FEF3C7", "#FECACA", "#991B1B"),
  domain = c(0, 30),
  na.color = "#E5E7EB"
)

# Create leaflet map
leaflet(map_data) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillColor = ~pal(avg_positivity),
    fillOpacity = 0.7,
    weight = 1,
    color = "#FFFFFF",
    popup = ~paste0(
      "<strong>", name, "</strong><br>",
      "Cases: ", format(total_cases, big.mark = ","), "<br>",
      "Positivity: ", round(avg_positivity, 1), "%<br>",
      "Sources: ", sources
    )
  ) |>
  addLegend(
    pal = pal,
    values = ~avg_positivity,
    title = "Positivity %",
    position = "bottomright"
  )'
    ),

    # Timeline chart (Global Overview)
    timeline_chart = list(
      data_desc = "Multi-pathogen time series from CDC/WHO/ECDC surveillance data",
      data_code = '# Query outbreak progression data
library(DBI)
library(dplyr)

conn <- dbConnect(SQLite(), "respiwatch.db")

timeline <- dbGetQuery(conn, "
  SELECT
    observation_date,
    pathogen_code,
    SUM(cases) as total_cases,
    AVG(positivity_rate) as avg_positivity
  FROM surveillance_data sd
  JOIN pathogens p ON sd.pathogen_id = p.pathogen_id
  GROUP BY observation_date, pathogen_code
  ORDER BY observation_date
")

dbDisconnect(conn)',
      viz_desc = "Multi-line time series with pathogen color encoding",
      viz_code = 'library(ggplot2)
library(plotly)

p <- ggplot(timeline, aes(x = as.Date(observation_date))) +
  geom_line(aes(y = avg_positivity, color = pathogen_code), linewidth = 1) +
  scale_color_manual(values = pathogen_colors) +
  labs(x = NULL, y = "Positivity Rate (%)", color = "Pathogen") +
  theme_minimal()

ggplotly(p) |> config(displayModeBar = FALSE)'
    ),

    # Wave propagation (Global Overview)
    wave_propagation = list(
      data_desc = "Wave spread velocity calculated from first detection dates across countries",
      data_code = '# Calculate wave propagation metrics
library(dplyr)
library(geosphere)

# Get first detection date per country per wave
first_detections <- surveillance_data |>
  filter(cases > threshold) |>
  group_by(pathogen_code, country_code, wave_id) |>
  summarise(
    first_date = min(observation_date),
    latitude = first(latitude),
    longitude = first(longitude),
    .groups = "drop"
  )

# Calculate spread velocity (km/day)
wave_stats <- first_detections |>
  arrange(pathogen_code, wave_id, first_date) |>
  group_by(pathogen_code, wave_id) |>
  mutate(
    days_since_origin = as.numeric(first_date - min(first_date)),
    distance_from_origin = distHaversine(
      cbind(longitude, latitude),
      cbind(first(longitude), first(latitude))
    ) / 1000  # Convert to km
  ) |>
  summarise(
    spread_velocity = coef(lm(distance_from_origin ~ days_since_origin))[2],
    countries_affected = n_distinct(country_code),
    .groups = "drop"
  )',
      viz_desc = "KPI cards showing spread velocity, country count, and confidence",
      viz_code = '# Wave propagation cards rendered via disease_wave_card() helper
# Each card shows:
# - Pathogen name with icon
# - Spread velocity (km/day)
# - Number of countries affected
# - Confidence level based on data coverage'
    ),

    # ICU gauge (Healthcare Capacity)
    icu_gauge = list(
      data_desc = "ICU capacity data from HHS HealthData.gov (historical: ended May 2024)",
      data_code = '# Same data source as hospital_gauge
# Query ICU-specific fields
icu_data <- capacity |>
  summarise(
    total_icu = sum(icu_beds_total, na.rm = TRUE),
    occupied_icu = sum(icu_beds_used, na.rm = TRUE)
  ) |>
  mutate(icu_utilization = occupied_icu / total_icu * 100)',
      viz_desc = "Gauge visualization with ICU-specific thresholds",
      viz_code = '# Same pattern as healthcare_gauge
# Critical threshold set higher for ICU (90%)'
    ),

    # Capacity timeline (Healthcare Capacity)
    capacity_timeline = list(
      data_desc = "Historical hospital utilization trends from HHS HealthData.gov",
      data_code = '# Query time series of capacity utilization
capacity_history <- dbGetQuery(conn, "
  SELECT
    observation_date,
    AVG(inpatient_bed_utilization) as avg_bed_util,
    AVG(icu_utilization) as avg_icu_util
  FROM healthcare_capacity
  GROUP BY observation_date
  ORDER BY observation_date
")',
      viz_desc = "Dual-axis time series showing bed and ICU utilization trends",
      viz_code = 'library(ggplot2)
library(plotly)

p <- ggplot(capacity_history, aes(x = as.Date(observation_date))) +
  geom_line(aes(y = avg_bed_util, color = "Hospital Beds"), linewidth = 1) +
  geom_line(aes(y = avg_icu_util, color = "ICU"), linewidth = 1) +
  geom_hline(yintercept = 85, linetype = "dashed", color = "#DC2626") +
  scale_color_manual(values = c("Hospital Beds" = "#0D9488", "ICU" = "#8B5CF6")) +
  labs(x = NULL, y = "Utilization (%)", color = NULL) +
  theme_minimal()

ggplotly(p)'
    ),

    # Hospitalization forecast (Healthcare Capacity)
    hospitalization_forecast = list(
      data_desc = "Projected hospitalizations based on case trends and hospitalization rates",
      data_code = '# Project hospitalizations from case forecasts
hosp_rate <- 0.02  # 2% hospitalization rate (pathogen-specific)
icu_rate <- 0.25   # 25% of hospitalized need ICU

hospitalization_forecast <- case_forecast |>
  mutate(
    projected_hospitalizations = predicted_cases * hosp_rate,
    projected_icu = projected_hospitalizations * icu_rate,
    hosp_lower = lower_80 * hosp_rate,
    hosp_upper = upper_80 * hosp_rate
  )',
      viz_desc = "Area chart with projection intervals",
      viz_code = 'p <- ggplot(hospitalization_forecast, aes(x = date)) +
  geom_ribbon(aes(ymin = hosp_lower, ymax = hosp_upper),
              fill = "#F59E0B", alpha = 0.3) +
  geom_line(aes(y = projected_hospitalizations),
            color = "#D97706", linewidth = 1.2) +
  labs(x = NULL, y = "Projected Hospitalizations") +
  theme_minimal()

ggplotly(p)'
    ),

    # Bayesian diagnostics (Bayesian Forecast)
    bayes_diagnostics = list(
      data_desc = "MCMC diagnostics from brms model fit",
      data_code = '# Extract MCMC diagnostics
library(brms)
library(bayesplot)

# Rhat convergence diagnostic
rhat_vals <- rhat(fit)

# Effective sample size
neff_vals <- neff_ratio(fit)

# Extract posterior draws for trace plots
posterior_draws <- as_draws_df(fit)',
      viz_desc = "Trace plots, density plots, and posterior predictive checks",
      viz_code = '# Trace plots - check for mixing
mcmc_trace(posterior_draws, pars = c("b_Intercept", "shape"))

# Density plots - check posterior shape
mcmc_dens_overlay(posterior_draws, pars = c("b_Intercept"))

# Posterior predictive check
pp_check(fit, type = "dens_overlay", ndraws = 50)'
    ),

    # Ensemble comparison (Bayesian Forecast)
    ensemble_comparison = list(
      data_desc = "Multiple forecast models combined using skill-weighted averaging",
      data_code = '# Generate forecasts from multiple models
models <- list(
  arima = auto.arima(ts_data),
  ets = ets(ts_data),
  prophet = prophet(df),
  bayesian = fit  # brms model
)

# Calculate skill scores (CRPS, WIS)
skill_scores <- map_dfr(models, ~{
  forecast <- predict(.x, h = 4)
  tibble(
    model = class(.x)[1],
    crps = mean(crps_sample(actuals, forecast)),
    wis = mean(wis(actuals, forecast))
  )
})

# Ensemble weights (inverse skill weighting)
weights <- 1 / skill_scores$wis
weights <- weights / sum(weights)',
      viz_desc = "Overlay of individual model forecasts with ensemble mean",
      viz_code = 'p <- ggplot() +
  # Individual model forecasts
  geom_line(data = forecasts, aes(x = date, y = value, color = model),
            alpha = 0.5, linetype = "dashed") +
  # Ensemble mean
  geom_line(data = ensemble_forecast, aes(x = date, y = weighted_mean),
            color = "#1F2937", linewidth = 1.5) +
  geom_ribbon(data = ensemble_forecast,
              aes(x = date, ymin = lower_80, ymax = upper_80),
              fill = "#6366F1", alpha = 0.2) +
  labs(x = NULL, y = "Cases", color = "Model") +
  theme_minimal()

ggplotly(p)'
    ),

    # Multi-pathogen Bayesian (Bayesian Forecast)
    bayes_all_pathogens = list(
      data_desc = "Bayesian forecasts for all tracked pathogens",
      data_code = '# Run Bayesian model for each pathogen
pathogens <- c("H3N2", "RSV", "COVID19")

all_forecasts <- map_dfr(pathogens, ~{
  pathogen_data <- filter(surveillance, pathogen_code == .x)
  fit <- brm(cases ~ s(time_index), data = pathogen_data, family = negbinomial())
  pred <- posterior_predict(fit, newdata = future_data)
  tibble(
    pathogen = .x,
    date = future_data$date,
    median = apply(pred, 2, median),
    lower_80 = apply(pred, 2, quantile, 0.1),
    upper_80 = apply(pred, 2, quantile, 0.9)
  )
})',
      viz_desc = "Summary table with forecast metrics by pathogen",
      viz_code = '# Rendered as styled HTML table
# Columns: Pathogen, Current Cases, 4-Week Forecast, Trend, Confidence'
    ),

    # Scenario comparison (Scenario Analysis)
    scenario_comparison = list(
      data_desc = "Policy scenario projections using modified transmission parameters",
      data_code = '# Define scenario parameters
scenarios <- list(
  baseline = list(rt_modifier = 1.0, label = "No Intervention"),
  masks = list(rt_modifier = 0.85, label = "Mask Mandate"),
  distancing = list(rt_modifier = 0.70, label = "Social Distancing"),
  lockdown = list(rt_modifier = 0.50, label = "Full Lockdown")
)

# Project each scenario using renewal equation
scenario_projections <- map_dfr(scenarios, ~{
  modified_rt <- current_rt * .x$rt_modifier
  project_cases(baseline_cases, modified_rt, horizon = 8) |>
    mutate(scenario = .x$label)
})',
      viz_desc = "Multi-line comparison of scenario trajectories",
      viz_code = 'p <- ggplot(scenario_projections, aes(x = date, y = cases, color = scenario)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = scenario_colors) +
  labs(x = NULL, y = "Projected Cases", color = "Scenario") +
  theme_minimal()

ggplotly(p)'
    ),

    # Scenario impact (Scenario Analysis)
    scenario_impact = list(
      data_desc = "Quantified impact metrics comparing scenarios to baseline",
      data_code = '# Calculate impact metrics
impact_summary <- scenario_projections |>
  group_by(scenario) |>
  summarise(
    total_cases = sum(cases),
    peak_cases = max(cases),
    peak_date = date[which.max(cases)]
  ) |>
  mutate(
    cases_averted = total_cases[scenario == "No Intervention"] - total_cases,
    pct_reduction = cases_averted / total_cases[scenario == "No Intervention"] * 100
  )',
      viz_desc = "DataTable with scenario comparison metrics",
      viz_code = '# Rendered as DT::datatable with conditional formatting
# Highlights: green for reductions, includes sparklines for trends'
    ),

    # Scenario builder (Scenario Analysis)
    scenario_builder = list(
      data_desc = "Custom scenario parameters for user-defined policy combinations",
      data_code = '# Custom scenario equation
# Rt_effective = Rt_baseline * (1 - mask_effect) * (1 - distancing_effect) * vaccine_effect

custom_rt <- baseline_rt *
  (1 - input$mask_reduction/100) *
  (1 - input$distancing_reduction/100) *
  (1 - input$vaccination_rate/100 * vaccine_efficacy)

custom_projection <- project_cases(
  baseline_cases = current_cases,
  rt = custom_rt,
  horizon = input$projection_weeks
)',
      viz_desc = "Interactive controls for policy parameter adjustment",
      viz_code = '# Shiny inputs: sliders for each intervention parameter
# Real-time projection updates as user adjusts values'
    ),

    # Vaccine effectiveness (Pathogen Analysis)
    vaccine_effectiveness = list(
      data_desc = "Vaccine effectiveness estimates from CDC/ECDC studies",
      data_code = '# Load vaccine effectiveness data
vaccine_data <- dbGetQuery(conn, "
  SELECT pathogen_code, strain, vaccine_type,
         effectiveness_pct, confidence_lower, confidence_upper,
         study_season, population_group
  FROM vaccine_effectiveness
  WHERE study_season >= \'2023-2024\'
")',
      viz_desc = "Grouped bar chart with confidence intervals by strain",
      viz_code = 'p <- ggplot(vaccine_data, aes(x = strain, y = effectiveness_pct, fill = vaccine_type)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = confidence_lower, ymax = confidence_upper),
                position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values = c("#0D9488", "#8B5CF6", "#F59E0B")) +
  labs(x = "Strain", y = "Effectiveness (%)", fill = "Vaccine Type") +
  theme_minimal()

ggplotly(p)'
    ),

    # Comparative positivity (Pathogen Analysis)
    comparative_positivity = list(
      data_desc = "Weekly positivity rates across all tracked pathogens",
      data_code = '# Query comparative positivity data
positivity_data <- surveillance_data |>
  group_by(observation_date, pathogen_code) |>
  summarise(
    positivity = mean(positivity_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = pathogen_code, values_from = positivity)',
      viz_desc = "Faceted line chart or grouped bars by pathogen",
      viz_code = 'p <- ggplot(positivity_long, aes(x = observation_date, y = positivity, fill = pathogen)) +
  geom_area(alpha = 0.6, position = "identity") +
  scale_fill_manual(values = pathogen_colors) +
  facet_wrap(~pathogen, scales = "free_y") +
  labs(x = NULL, y = "Positivity Rate (%)") +
  theme_minimal()

ggplotly(p)'
    ),

    # Comparative hospitalization (Pathogen Analysis)
    comparative_hospitalization = list(
      data_desc = "Age-adjusted hospitalization rates by pathogen",
      data_code = '# Query hospitalization rates
hosp_rates <- dbGetQuery(conn, "
  SELECT pathogen_code, age_group,
         hospitalization_rate_per_100k,
         observation_week
  FROM hospitalization_data
  WHERE observation_week >= date(\'now\', \'-12 weeks\')
")',
      viz_desc = "Heatmap or grouped bar chart by age and pathogen",
      viz_code = 'p <- ggplot(hosp_rates, aes(x = age_group, y = pathogen_code,
                            fill = hospitalization_rate_per_100k)) +
  geom_tile() +
  scale_fill_gradient2(low = "#D1FAE5", mid = "#FEF3C7", high = "#991B1B",
                       midpoint = 50) +
  labs(x = "Age Group", y = "Pathogen", fill = "Rate/100k") +
  theme_minimal()

ggplotly(p)'
    ),

    # Multi-pathogen timeline (Pathogen Analysis)
    multi_pathogen_timeline = list(
      data_desc = "Stacked time series of all pathogen activity",
      data_code = '# Same as timeline_chart but with additional smoothing
multi_timeline <- surveillance_data |>
  group_by(observation_date, pathogen_code) |>
  summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop") |>
  group_by(pathogen_code) |>
  mutate(cases_smooth = zoo::rollmean(cases, k = 3, fill = NA))',
      viz_desc = "Stacked area chart showing relative pathogen burden",
      viz_code = 'p <- ggplot(multi_timeline, aes(x = observation_date, y = cases_smooth,
                            fill = pathogen_code)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = pathogen_colors) +
  labs(x = NULL, y = "Cases (7-day avg)", fill = "Pathogen") +
  theme_minimal()

ggplotly(p)'
    ),

    # Pathogen comparison table (Pathogen Analysis)
    pathogen_comparison_table = list(
      data_desc = "Summary statistics for cross-pathogen comparison",
      data_code = '# Generate comparison metrics
comparison <- surveillance_data |>
  group_by(pathogen_code) |>
  summarise(
    current_cases = last(cases),
    weekly_change = (last(cases) - nth(cases, -2)) / nth(cases, -2) * 100,
    peak_this_season = max(cases),
    current_positivity = last(positivity_rate),
    data_coverage = sum(!is.na(cases)) / n() * 100
  )',
      viz_desc = "Interactive DataTable with sparklines and conditional formatting",
      viz_code = '# Rendered with DT::datatable
# Includes: trend sparklines, color-coded change indicators, sortable columns'
    ),

    # Country anomalies (Country Analysis)
    country_anomalies = list(
      data_desc = "Anomaly detection using CUSUM and EARS algorithms",
      data_code = '# Detect anomalies using surveillance algorithms
library(surveillance)

# EARS C1-C3 algorithm
anomalies <- earsC(
  surveillance_ts,
  control = list(
    method = "C2",
    baseline = 7,
    alpha = 0.05
  )
)

# Flag significant deviations
flagged_countries <- anomalies |>
  filter(alarm == TRUE) |>
  select(country_code, observation_date, observed, expected, zscore)',
      viz_desc = "Alert cards showing flagged countries with severity",
      viz_code = '# Rendered as alert cards with:
# - Country name and flag
# - Observed vs expected ratio
# - Days since anomaly detected
# - Severity color coding'
    ),

    # Country healthcare (Country Analysis)
    country_healthcare = list(
      data_desc = "Country-specific healthcare capacity indicators",
      data_code = '# Query country-level capacity
country_capacity <- dbGetQuery(conn, "
  SELECT country_code, beds_per_1000, icu_beds_per_100k,
         physicians_per_1000, health_expenditure_pct_gdp
  FROM country_healthcare_indicators
  WHERE country_code = ?
", params = list(selected_country))',
      viz_desc = "KPI cards showing key healthcare metrics",
      viz_code = '# Rendered as value boxes with:
# - Beds per 1,000 population
# - ICU capacity per 100k
# - Healthcare spending % GDP
# - Comparison to regional average'
    ),

    # Country policies (Country Analysis)
    country_policies = list(
      data_desc = "Policy response tracker from Oxford COVID-19 Government Response Tracker",
      data_code = '# Note: Historical data - tracker ended 2023
policy_data <- dbGetQuery(conn, "
  SELECT country_code, policy_date,
         containment_index, stringency_index,
         school_closing, workplace_closing,
         public_events_cancelled, stay_at_home
  FROM policy_responses
  WHERE country_code = ?
  ORDER BY policy_date DESC
  LIMIT 1
", params = list(selected_country))',
      viz_desc = "Policy indicator summary with icons",
      viz_code = '# Rendered as icon grid showing:
# - Active policies (green/red indicators)
# - Stringency index gauge
# - Policy timeline'
    ),

    # Data gaps list (Surveillance Gaps)
    data_gaps_list = list(
      data_desc = "Identified reporting gaps in surveillance data streams",
      data_code = '# Detect gaps in reporting
gaps <- surveillance_data |>
  group_by(country_code, pathogen_code) |>
  arrange(observation_date) |>
  mutate(
    days_since_last = as.numeric(observation_date - lag(observation_date)),
    is_gap = days_since_last > 14  # Gap = >2 weeks without data

  ) |>
  filter(is_gap) |>
  select(country_code, pathogen_code, gap_start = lag(observation_date),
         gap_end = observation_date, gap_days = days_since_last)',
      viz_desc = "Alert cards for active and recent reporting gaps",
      viz_code = '# Rendered as warning cards with:
# - Country/pathogen affected
# - Gap duration
# - Impact on analysis
# - Suggested fallback data source'
    ),

    # Gaps timeline (Surveillance Gaps)
    gaps_timeline = list(
      data_desc = "Historical view of data availability across sources",
      data_code = '# Generate availability matrix
availability <- surveillance_data |>
  mutate(week = floor_date(observation_date, "week")) |>
  group_by(country_code, pathogen_code, source_name, week) |>
  summarise(has_data = n() > 0, .groups = "drop") |>
  complete(country_code, pathogen_code, source_name, week,
           fill = list(has_data = FALSE))',
      viz_desc = "DataTable with availability status by source and time",
      viz_code = '# Rendered as DT::datatable with:
# - Color-coded cells (green = data, red = gap)
# - Filterable by country/pathogen/source
# - Exportable for reporting'
    ),

    # Suppression evidence (Surveillance Gaps)
    suppression_evidence = list(
      data_desc = "Statistical analysis of potential data suppression patterns",
      data_code = '# Analyze reporting patterns for anomalies
# Note: This is for analytical purposes only
suppression_analysis <- surveillance_data |>
  group_by(country_code) |>
  summarise(
    sudden_drops = sum(cases / lag(cases) < 0.1 & lag(cases) > 1000, na.rm = TRUE),
    sustained_zeros = max(rle(cases == 0)$lengths[rle(cases == 0)$values]),
    weekend_reporting_ratio = mean(cases[wday(observation_date) %in% c(1,7)]) /
                              mean(cases[!wday(observation_date) %in% c(1,7)])
  )',
      viz_desc = "Evidence summary with statistical indicators",
      viz_code = '# Rendered as analysis cards showing:
# - Pattern indicators (sudden drops, sustained zeros)
# - Statistical confidence levels
# - Comparison to expected reporting patterns'
    )
  )

  # Return requested snippet or empty list
  snippets[[chart_id]] %||% list(
    data_desc = "Data source information not available",
    data_code = "# Code snippet not available for this chart",
    viz_desc = "Visualization details not available",
    viz_code = "# Code snippet not available for this chart"
  )
}
