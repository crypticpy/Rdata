# ============================================================================
# Title: Theme Utilities for Diabetes Dashboard
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Centralized theme management for ggplot2 and plotly charts
# ============================================================================

#' World-class ggplot2 theme with dark/light mode support
#'
#' @param dark_mode Logical, TRUE for dark mode (default), FALSE for light mode
#' @return A ggplot2 theme object
theme_worldclass <- function(dark_mode = TRUE) {
  if (dark_mode) {
    bg_color <- "transparent"
    text_color <- "#F5F0E8"
    subtitle_color <- "#94A3B8"
    axis_color <- "#94A3B8"
    grid_color <- "rgba(255,255,255,0.08)"
    strip_color <- "#F5F0E8"
  } else {
    bg_color <- "white"
    text_color <- "#0F172A"
    subtitle_color <- "#64748B"
    axis_color <- "#334155"
    grid_color <- "#E2E8F0"
    strip_color <- "#0F172A"
  }

  ggplot2::theme_minimal(base_size = 14, base_family = "Inter") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 18,
        face = "bold",
        color = text_color,
        margin = ggplot2::margin(b = 12)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 13,
        color = subtitle_color,
        margin = ggplot2::margin(b = 16)
      ),
      plot.caption = ggplot2::element_text(
        size = 11,
        color = subtitle_color,
        hjust = 0
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = grid_color, linewidth = 0.5),
      axis.title = ggplot2::element_text(size = 13, color = axis_color),
      axis.text = ggplot2::element_text(size = 12, color = axis_color),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 12, face = "bold", color = text_color),
      legend.text = ggplot2::element_text(size = 11, color = subtitle_color),
      legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
      strip.text = ggplot2::element_text(size = 12, face = "bold", color = strip_color),
      plot.margin = ggplot2::margin(20, 20, 20, 20),
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA)
    )
}

#' Apply theme-aware styling to plotly charts
#'
#' @param p A plotly object
#' @param dark_mode Logical, TRUE for dark mode (default), FALSE for light mode
#' @return A themed plotly object
apply_plotly_theme <- function(p, dark_mode = TRUE) {
  if (dark_mode) {
    p |> plotly::layout(
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      font = list(color = "#F5F0E8", family = "Outfit"),
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
      font = list(color = "#0F172A", family = "Outfit"),
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

#' Get theme-aware color palettes
#'
#' @param dark_mode Logical, TRUE for dark mode colors
#' @return A list of color values for different elements
get_theme_colors <- function(dark_mode = TRUE) {
  if (dark_mode) {
    list(
      text = "#F5F0E8",
      text_secondary = "#94A3B8",
      background = "transparent",
      grid = "rgba(255,255,255,0.08)",
      accent = "#FF6B6B",
      success = "#10B981",
      warning = "#F59E0B",
      danger = "#F43F5E"
    )
  } else {
    list(
      text = "#0F172A",
      text_secondary = "#64748B",
      background = "#FFFFFF",
      grid = "rgba(0,0,0,0.08)",
      accent = "#0D9488",
      success = "#059669",
      warning = "#D97706",
      danger = "#E11D48"
    )
  }
}

# Standard color palettes
diabetes_colors <- c(
  "No Diabetes" = "#10B981",
  "Prediabetes" = "#F59E0B",
  "Diabetes" = "#F43F5E"
)

chart_colors <- c(
  "#0D9488", "#0EA5E9", "#10B981", "#F59E0B", "#F43F5E",
  "#8B5CF6", "#EC4899", "#14B8A6", "#6366F1", "#84CC16"
)

risk_gradient <- c("#10B981", "#22C55E", "#84CC16", "#EAB308", "#F59E0B", "#F43F5E")

subgroup_colors <- c(
  "Resilient" = "#10B981",
  "Vulnerable" = "#EF4444",
  "Expected Healthy" = "#3B82F6",
  "Expected Diabetic" = "#F97316",
  "Typical" = "#6B7280"
)
