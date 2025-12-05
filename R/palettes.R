#' Color Palettes for Data Visualization
#'
#' A collection of colorblind-safe color palettes designed for data
#' visualization. All palettes have been tested for accessibility using
#' WCAG contrast guidelines and colorblindness simulation.
#'
#' @name palettes
#' @aliases color_palettes
#'
#' @details
#' ## Available Palettes
#'
#' ### Categorical Palettes
#' \itemize{
#'   \item `chart_colors` - General purpose 10-color palette
#'   \item `diabetes_colors` - Semantic colors for health status
#'   \item `subgroup_colors` - Colors for population subgroups
#' }
#'
#' ### Sequential Palettes
#' \itemize{
#'   \item `risk_gradient` - 6-color gradient from low to high risk
#' }
#'
#' ## Accessibility
#' All palettes are designed to:
#' \itemize{
#'   \item Be distinguishable under common forms of color blindness
#'   \item Meet WCAG 2.1 contrast requirements on dark backgrounds
#'   \item Work well in both screen and print contexts
#' }
#'
#' @examples
#' library(ggplot2)
#'
#' # Use chart_colors for categorical data
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   scale_color_manual(values = chart_colors[1:3]) +
#'   theme_worldclass()
#'
#' # Use diabetes_colors for health data
#' ggplot(data.frame(status = names(diabetes_colors), n = c(100, 30, 20)),
#'        aes(status, n, fill = status)) +
#'   geom_col() +
#'   scale_fill_manual(values = diabetes_colors) +
#'   theme_worldclass()
#'
#' @seealso [scale_fill_rdataviz()], [scale_color_rdataviz()]
NULL

#' Chart Colors - General Purpose Categorical Palette
#'
#' A 10-color palette suitable for most categorical data visualizations.
#' Colors are ordered for maximum contrast between adjacent values.
#'
#' @format A character vector of 10 hex color codes.
#' @export
#' @examples
#' # View the palette
#' scales::show_col(chart_colors)
chart_colors <- c(
 "#0D9488",
 "#0EA5E9",
 "#10B981",
 "#F59E0B",
 "#F43F5E",
 "#8B5CF6",
 "#EC4899",
 "#14B8A6",
 "#6366F1",
 "#84CC16"
)

#' Diabetes Status Colors
#'
#' Semantic color palette for representing diabetes health status.
#' Uses intuitive color associations: green for healthy, amber for
#' warning/prediabetes, and rose for diabetes.
#'
#' @format A named character vector of 3 hex color codes.
#' @export
#' @examples
#' # View the palette
#' scales::show_col(diabetes_colors, labels = TRUE)
diabetes_colors <- c(
  "No Diabetes" = "#10B981",
  "Prediabetes" = "#F59E0B",
  "Diabetes" = "#F43F5E"
)

#' Risk Gradient Colors
#'
#' A 6-color sequential gradient for representing risk levels from
#' low (green) to high (rose). Suitable for continuous risk scores
#' or ordinal risk categories.
#'
#' @format A character vector of 6 hex color codes.
#' @export
#' @examples
#' # Use with scale_fill_gradientn
#' library(ggplot2)
#' ggplot(data.frame(x = 1:6, y = 1, risk = 1:6),
#'        aes(x, y, fill = risk)) +
#'   geom_tile() +
#'   scale_fill_gradientn(colors = risk_gradient) +
#'   theme_worldclass()
risk_gradient <- c(
  "#10B981",
  "#22C55E",
  "#84CC16",
  "#EAB308",
  "#F59E0B",
  "#F43F5E"
)

#' Subgroup Colors
#'
#' Semantic colors for representing population subgroups in health
#' analytics, particularly for anomaly detection results.
#'
#' @format A named character vector of 5 hex color codes.
#' @export
#' @examples
#' scales::show_col(subgroup_colors, labels = TRUE)
subgroup_colors <- c(
  "Resilient" = "#10B981",
  "Vulnerable" = "#EF4444",
  "Expected Healthy" = "#3B82F6",
  "Expected Diabetic" = "#F97316",
  "Typical" = "#6B7280"
)

#' Get Theme-Aware Color Palette
#'
#' Returns a list of semantic colors appropriate for the specified
#' color mode. Useful for programmatically selecting colors that
#' match the current theme.
#'
#' @param dark_mode Logical. If `TRUE` (default), returns colors
#'   optimized for dark backgrounds.
#'
#' @return A named list of color values for common UI elements.
#'
#' @export
#' @examples
#' # Get dark mode colors
#' colors <- get_theme_colors()
#' colors$text
#' colors$accent
#'
#' # Get light mode colors
#' colors_light <- get_theme_colors(dark_mode = FALSE)
#' colors_light$background
get_theme_colors <- function(dark_mode = TRUE) {
  if (dark_mode) {
    list(
      text = "#F5F0E8",
      text_secondary = "#94A3B8",
      background = "transparent",
      surface = "#151D2E",
      grid = "rgba(255,255,255,0.08)",
      border = "rgba(255,255,255,0.1)",
      accent = "#FF6B6B",
      primary = "#0D9488",
      success = "#10B981",
      warning = "#F59E0B",
      danger = "#F43F5E",
      info = "#0EA5E9"
    )
  } else {
    list(
      text = "#0F172A",
      text_secondary = "#64748B",
      background = "#FFFFFF",
      surface = "#F8FAFC",
      grid = "rgba(0,0,0,0.08)",
      border = "rgba(0,0,0,0.1)",
      accent = "#0D9488",
      primary = "#0D9488",
      success = "#059669",
      warning = "#D97706",
      danger = "#E11D48",
      info = "#0284C7"
    )
  }
}
