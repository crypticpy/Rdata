#' World-Class ggplot2 Theme with Dark/Light Mode Support
#'
#' A polished, publication-ready ggplot2 theme designed for data communication
#' excellence. Features clean typography, minimal non-data ink, and full
#' support for both dark and light color schemes.
#'
#' @param dark_mode Logical. If `TRUE` (default), uses a dark color scheme
#'   optimized for presentations and dashboards. If `
#' @param base_size Base font size in points. Default is 14.
#' @param base_family Base font family. Default uses system sans-serif.
#'
#' @return A ggplot2 theme object that can be added to any ggplot.
#'
#' @details
#' The theme follows key data visualization principles:
#' \itemize{
#'   \item **High data-ink ratio**: Minimal gridlines and borders

#'   \item **Clear typography**: Hierarchical text sizing for titles, labels
#'   \item **Accessibility**: Sufficient contrast ratios for readability
#'   \item **Flexibility**: Works on both light and dark backgrounds
#' }
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic usage with dark mode (default)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_worldclass()
#'
#' # Light mode for print/export
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_worldclass(dark_mode = FALSE)
#'
#' # Customize base size
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   theme_worldclass(base_size = 16)
#'
#' @seealso [apply_plotly_theme()] for theming plotly charts
#' @export
theme_worldclass <- function(dark_mode = TRUE,
                             base_size = 14,
                             base_family = "") {


  # Define color scheme based on mode

if (dark_mode) {
    bg_color <- "transparent"
    text_color <- "#F5F0E8"
    subtitle_color <- "#94A3B8"
    axis_color <- "#94A3B8"
    grid_color <- "#2D3748"
    strip_color <- "#F5F0E8"
  } else {
    bg_color <- "white"
    text_color <- "#0F172A"
    subtitle_color <- "#64748B"
    axis_color <- "#334155"
    grid_color <- "#E2E8F0"
    strip_color <- "#0F172A"
  }

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Title hierarchy
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.3),
        face = "bold",
        color = text_color,
        margin = ggplot2::margin(b = 12)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.95),
        color = subtitle_color,
        margin = ggplot2::margin(b = 16)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        color = subtitle_color,
        hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",

      # Grid - minimal for high data-ink ratio
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = grid_color,
        linewidth = 0.5
      ),

      # Axes
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(0.95),
        color = axis_color
      ),
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        color = axis_color
      ),
      axis.ticks = ggplot2::element_blank(),

      # Legend - positioned at bottom for wide charts
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        face = "bold",
        color = text_color
      ),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        color = subtitle_color
      ),
      legend.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      legend.key = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),

      # Facet strips
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        face = "bold",
        color = strip_color
      ),
      strip.background = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),

      # Plot margins and backgrounds
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.background = ggplot2::element_rect(
        fill = bg_color,
        color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = bg_color,
        color = NA
      )
    )
}

#' @rdname theme_worldclass
#' @export
theme_wc <- theme_worldclass

#' @rdname theme_worldclass
#' @export
theme_dashboard <- theme_worldclass
