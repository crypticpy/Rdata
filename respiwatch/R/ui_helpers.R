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
