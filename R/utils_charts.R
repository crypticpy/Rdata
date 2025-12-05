# ============================================================================
# Title: Chart Utilities for Diabetes Dashboard
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Reusable chart creation functions with theme support
# ============================================================================

#' Create a theme-aware confusion matrix heatmap
#'
#' @param cm_data A list with tn, fp, fn, tp values
#' @param title_suffix Optional title suffix
#' @param dark_mode Logical for dark/light mode
#' @return A plotly object
create_confusion_matrix_plot <- function(cm_data, title_suffix = "", dark_mode = TRUE) {
  low_color <- if (dark_mode) "#1E293B" else "#E2E8F0"
  text_color <- if (dark_mode) "#F5F0E8" else "#0F172A"
  axis_color <- if (dark_mode) "#94A3B8" else "#64748B"

  cm <- matrix(
    c(cm_data$tn, cm_data$fp, cm_data$fn, cm_data$tp),
    nrow = 2,
    dimnames = list(
      Predicted = c("Negative", "Positive"),
      Actual = c("Negative", "Positive")
    )
  )

  cm_df <- as.data.frame(as.table(cm))
  names(cm_df) <- c("Predicted", "Actual", "Count")
  cm_df$Pct <- cm_df$Count / sum(cm_df$Count) * 100

  plotly::plot_ly(
    data = cm_df,
    x = ~Actual,
    y = ~Predicted,
    z = ~Count,
    type = "heatmap",
    colorscale = list(c(0, low_color), c(1, "#0D6EFD")),
    showscale = FALSE,
    hovertemplate = "Actual: %{x}<br>Predicted: %{y}<br>Count: %{z:,}<extra></extra>"
  ) |>
    plotly::add_annotations(
      x = cm_df$Actual,
      y = cm_df$Predicted,
      text = scales::comma(cm_df$Count),
      showarrow = FALSE,
      font = list(color = ifelse(cm_df$Pct > 40, "white", text_color), size = 16)
    ) |>
    plotly::layout(
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      xaxis = list(title = "Actual", color = axis_color),
      yaxis = list(title = "Predicted", autorange = "reversed", color = axis_color),
      font = list(color = text_color)
    ) |>
    plotly::config(displayModeBar = FALSE)
}

#' Create a theme-aware donut chart
#'
#' @param data A data frame with labels, values columns
#' @param colors Named vector of colors
#' @param center_text Text to display in center
#' @param dark_mode Logical for dark/light mode
#' @return A plotly object
create_donut_chart <- function(data, colors, center_text = "", dark_mode = TRUE) {
  text_color <- if (dark_mode) "#F5F0E8" else "#0F172A"
  legend_color <- if (dark_mode) "#E2E8F0" else "#334155"

  plotly::plot_ly(
    data,
    labels = ~labels,
    values = ~values,
    type = "pie",
    hole = 0.6,
    marker = list(colors = colors),
    textinfo = "percent",
    textposition = "outside",
    hovertemplate = "<b>%{label}</b><br>Count: %{value:,}<br>Percentage: %{percent}<extra></extra>"
  ) |>
    plotly::layout(
      showlegend = TRUE,
      legend = list(orientation = "h", y = -0.1, font = list(color = legend_color)),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      annotations = list(
        list(
          text = center_text,
          showarrow = FALSE,
          font = list(size = 16, color = text_color)
        )
      ),
      margin = list(t = 20, b = 40)
    ) |>
    plotly::config(displayModeBar = FALSE)
}

#' Create a theme-aware gauge chart for risk assessment
#'
#' @param probability Risk probability (0-1)
#' @param dark_mode Logical for dark/light mode
#' @return A plotly object
create_risk_gauge <- function(probability, dark_mode = TRUE) {
  text_color <- if (dark_mode) "#F5F0E8" else "#0F172A"
  tick_color <- if (dark_mode) "#94A3B8" else "#64748B"
  bg_color <- if (dark_mode) "rgba(148, 163, 184, 0.2)" else "rgba(148, 163, 184, 0.3)"
  bar_color <- if (dark_mode) "#FF6B6B" else "#0D9488"

  plotly::plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = probability * 100,
    number = list(suffix = "%", font = list(size = 40, color = text_color)),
    gauge = list(
      axis = list(
        range = list(0, 100),
        ticksuffix = "%",
        tickcolor = tick_color,
        tickfont = list(color = tick_color)
      ),
      bar = list(color = bar_color, thickness = 0.3),
      bgcolor = bg_color,
      steps = list(
        list(range = c(0, 15), color = "rgba(16, 185, 129, 0.3)"),
        list(range = c(15, 30), color = "rgba(251, 191, 36, 0.3)"),
        list(range = c(30, 100), color = "rgba(244, 63, 94, 0.3)")
      ),
      threshold = list(
        line = list(color = bar_color, width = 4),
        thickness = 0.75,
        value = probability * 100
      )
    )
  ) |>
    plotly::layout(
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent",
      margin = list(t = 40, b = 20),
      font = list(family = "Inter", color = text_color)
    ) |>
    plotly::config(displayModeBar = FALSE)
}

#' Create a theme-aware bar chart
#'
#' @param data Data frame for plotting
#' @param x_var X axis variable name
#' @param y_var Y axis variable name
#' @param fill_var Fill variable name
#' @param colors Named vector of fill colors
#' @param dark_mode Logical for dark/light mode
#' @param x_angle Angle for x-axis labels
#' @return A ggplot object
create_bar_chart <- function(data, x_var, y_var, fill_var, colors,
                             dark_mode = TRUE, x_angle = 0) {
  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data[[x_var]],
    y = .data[[y_var]],
    fill = .data[[fill_var]]
  )) +
    ggplot2::geom_col(position = "dodge", width = 0.7) +
    ggplot2::scale_fill_manual(values = colors) +
    theme_worldclass(dark_mode = dark_mode)

  if (x_angle != 0) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = x_angle, hjust = 1)
    )
  }

  p
}

#' Create a theme-aware density plot
#'
#' @param data Data frame for plotting
#' @param x_var X axis variable name
#' @param fill_var Fill variable name
#' @param colors Named vector of fill colors
#' @param dark_mode Logical for dark/light mode
#' @return A ggplot object
create_density_plot <- function(data, x_var, fill_var, colors, dark_mode = TRUE) {
  ggplot2::ggplot(data, ggplot2::aes(
    x = .data[[x_var]],
    fill = .data[[fill_var]]
  )) +
    ggplot2::geom_density(alpha = 0.6) +
    ggplot2::scale_fill_manual(values = colors) +
    theme_worldclass(dark_mode = dark_mode)
}
