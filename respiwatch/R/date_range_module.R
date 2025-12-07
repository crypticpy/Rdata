# ============================================================================
# Title: Date Range Control Module
# Purpose: Reusable Shiny module for date range filtering with 30-day default
# Output: Filtered data reactive based on user-selected date range
# ============================================================================

# =============================================================================
# DATE RANGE CONTROL MODULE
# =============================================================================

#' Date Range Control UI
#'
#' Creates a date range input with reset button for filtering data
#'
#' @param id Namespace ID for the module
#' @param label Label for the date range input
#' @param default_days Number of days to show by default (default: 30)
#' @param available_months Number of months of data available (default: 9)
#' @return tagList with dateRangeInput and reset button
dateRangeControlUI <- function(id, label = "Date Range",
                                default_days = 30, available_months = 9) {
  ns <- NS(id)

  # Calculate date boundaries
  end_date <- Sys.Date()
  start_date <- end_date - default_days
  min_date <- end_date - (available_months * 30)  # Approximate months

  tagList(
    div(
      class = "date-range-control",
      style = "display: flex; align-items: flex-end; gap: 10px; flex-wrap: wrap;",
      dateRangeInput(
        ns("date_range"),
        label = label,
        start = start_date,
        end = end_date,
        min = min_date,
        max = end_date,
        format = "yyyy-mm-dd",
        separator = " to "
      ),
      actionButton(
        ns("reset"),
        sprintf("Reset to %d days", default_days),
        class = "btn-sm btn-outline-secondary",
        style = "margin-bottom: 15px;"
      )
    )
  )
}

#' Date Range Control Server
#'
#' Server logic for date range filtering module
#'
#' @param id Namespace ID matching the UI
#' @param data_reactive Reactive expression returning a data frame with 'date' column
#' @param date_col Name of the date column in the data (default: "date")
#' @return Reactive expression with filtered data
dateRangeControlServer <- function(id, data_reactive, date_col = "date", default_days = 30) {
  moduleServer(id, function(input, output, session) {

    # Reset button functionality
    observeEvent(input$reset, {
      updateDateRangeInput(
        session,
        "date_range",
        start = Sys.Date() - default_days,
        end = Sys.Date()
      )
    })

    # Filter data based on date range
    filtered_data <- reactive({
      req(input$date_range)
      data <- data_reactive()

      if (is.null(data) || nrow(data) == 0) {
        return(data)
      }

      # Get column name
      col <- date_col

      # Ensure the date column exists
      if (!col %in% names(data)) {
        warning(paste("Date column", col, "not found in data. Available columns:",
                      paste(names(data), collapse = ", ")))
        return(data)
      }

      # Ensure date column is Date type
      if (!inherits(data[[col]], "Date")) {
        data[[col]] <- as.Date(data[[col]])
      }

      # Filter by date range
      data |>
        dplyr::filter(
          .data[[col]] >= input$date_range[1],
          .data[[col]] <= input$date_range[2]
        )
    })

    # Return date range values (for other uses)
    date_range <- reactive({
      req(input$date_range)
      list(
        start = input$date_range[1],
        end = input$date_range[2]
      )
    })

    # Return both filtered data and date range
    return(list(
      data = filtered_data,
      range = date_range
    ))
  })
}

#' Create Date Range Filter for Non-Reactive Data
#'
#' Helper function to create a simple date filter reactive
#' when data is not reactive (e.g., loaded at startup)
#'
#' @param id Namespace ID matching the UI
#' @param data Static data frame with date column
#' @param date_col Name of the date column
#' @return Reactive with filtered data
dateRangeFilterStatic <- function(id, data, date_col = "date") {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$reset, {
      updateDateRangeInput(
        session,
        "date_range",
        start = Sys.Date() - 30,
        end = Sys.Date()
      )
    })

    filtered_data <- reactive({
      req(input$date_range)

      if (is.null(data) || nrow(data) == 0) {
        return(data)
      }

      col <- date_col

      if (!col %in% names(data)) {
        return(data)
      }

      if (!inherits(data[[col]], "Date")) {
        data[[col]] <- as.Date(data[[col]])
      }

      data |>
        dplyr::filter(
          .data[[col]] >= input$date_range[1],
          .data[[col]] <= input$date_range[2]
        )
    })

    return(filtered_data)
  })
}

# =============================================================================
# QUICK DATE RANGE HELPERS
# =============================================================================

#' Get default date range (last 30 days)
#' @return Named list with start and end dates
get_default_date_range <- function(days = 30) {
  list(
    start = Sys.Date() - days,
    end = Sys.Date()
  )
}

#' Get available date range (last 9 months)
#' @return Named list with min and max dates
get_available_date_range <- function(months = 9) {
  list(
    min = Sys.Date() - (months * 30),
    max = Sys.Date()
  )
}
