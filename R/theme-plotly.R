#' Apply Theme-Aware Styling to Plotly Charts
#'
#' Applies consistent theming to plotly objects, matching the aesthetic of
#' [theme_worldclass()]. Handles background colors, fonts, axis styling,
#' and legend appearance for both dark and light modes.
#'
#' @param p A plotly object created with [plotly::plot_ly()] or
#'   [plotly::ggplotly()].
#' @param dark_mode Logical. If `TRUE` (default), applies dark mode styling.
#'   If `FALSE`, applies light mode styling suitable for print or light UIs.
#'
#' @return A modified plotly object with updated layout styling.
#'
#' @details
#' The function modifies the following plotly layout properties:
#' \itemize{
#'   \item `paper_bgcolor` and `plot_bgcolor` - Chart backgrounds
#'   \item `font` - Global font settings
#'   \item `legend` - Legend styling and positioning
#'   \item `xaxis` and `yaxis` - Axis colors, gridlines, and tick formatting
#' }
#'
#' @examples
#' library(plotly)
#'
#' # Create a basic plotly chart
#' p <- plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", mode = "markers")
#'
#' # Apply dark mode theme (default)
#' p |> apply_plotly_theme()
#'
#' # Apply light mode theme
#' p |> apply_plotly_theme(dark_mode = FALSE)
#'
#' # Chain with other plotly modifications
#' p |>
#'   apply_plotly_theme() |>
#'   plotly::layout(title = "Weight vs MPG")
#'
#' @seealso [theme_worldclass()] for ggplot2 theming
#' @export
apply_plotly_theme <- function(p, dark_mode = TRUE) {
  if (!inherits(p, "plotly")) {
    rlang::abort(
      message = "`p` must be a plotly object",
      class = "rdataviz_error"
    )
  }

  if (dark_mode) {
    p |> plotly::layout(
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      font = list(
        color = "#F5F0E8",
        family = "Inter, Outfit, system-ui, sans-serif"
      ),
      legend = list(
        font = list(color = "#E2E8F0", size = 12),
        bgcolor = "transparent"
      ),
      xaxis = list(
        color = "#94A3B8",
        gridcolor = "rgba(255,255,255,0.08)",
        zerolinecolor = "rgba(255,255,255,0.1)",
        tickfont = list(color = "#94A3B8")
      ),
      yaxis = list(
        color = "#94A3B8",
        gridcolor = "rgba(255,255,255,0.08)",
        zerolinecolor = "rgba(255,255,255,0.1)",
        tickfont = list(color = "#94A3B8")
      )
    )
  } else {
    p |> plotly::layout(
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF",
      font = list(
        color = "#0F172A",
        family = "Inter, Outfit, system-ui, sans-serif"
      ),
      legend = list(
        font = list(color = "#334155", size = 12),
        bgcolor = "rgba(255,255,255,0.9)"
      ),
      xaxis = list(
        color = "#64748B",
        gridcolor = "rgba(0,0,0,0.08)",
        zerolinecolor = "rgba(0,0,0,0.1)",
        tickfont = list(color = "#64748B")
      ),
      yaxis = list(
        color = "#64748B",
        gridcolor = "rgba(0,0,0,0.08)",
        zerolinecolor = "rgba(0,0,0,0.1)",
        tickfont = list(color = "#64748B")
      )
    )
  }
}

#' @rdname apply_plotly_theme
#' @export
plotly_dark_layout <- function(p) {
  apply_plotly_theme(p, dark_mode = TRUE)
}

#' @rdname apply_plotly_theme
#' @export
plotly_light_layout <- function(p) {
  apply_plotly_theme(p, dark_mode = FALSE)
}
