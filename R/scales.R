#' Color and Fill Scales for rdataviz
#'
#' Convenient scale functions that apply rdataviz color palettes to
#' ggplot2 plots. These wrap `scale_*_manual()` with pre-defined
#' colorblind-safe palettes.
#'
#' @param palette Character. Name of the palette to use. One of:
#'   \itemize{
#'     \item `"default"` or `"chart"` - General purpose categorical (10 colors)
#'     \item `"diabetes"` or `"health"` - Health status colors (3 colors)
#'     \item `"subgroup"` - Population subgroup colors (5 colors)
#'     \item `"risk"` - Risk gradient (6 colors, sequential)
#'   }
#' @param ... Additional arguments passed to [ggplot2::scale_fill_manual()]
#'   or [ggplot2::scale_color_manual()].
#'
#' @return A ggplot2 scale object.
#'
#' @name scale_rdataviz
#' @aliases scale_fill_rdataviz scale_color_rdataviz scale_colour_rdataviz
#'
#' @examples
#' library(ggplot2)
#'
#' # Default categorical palette
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point(size = 3) +
#'   scale_color_rdataviz() +
#'   theme_worldclass()
#'
#' # Health/diabetes palette
#' df <- data.frame(
#'   status = c("No Diabetes", "Prediabetes", "Diabetes"),
#'   count = c(200, 50, 30)
#' )
#' ggplot(df, aes(status, count, fill = status)) +
#'   geom_col() +
#'   scale_fill_rdataviz("diabetes") +
#'   theme_worldclass()
#'
#' @export
scale_fill_rdataviz <- function(palette = "default", ...) {
  pal <- .get_palette(palette)
  ggplot2::scale_fill_manual(values = pal, ...)
}

#' @rdname scale_rdataviz
#' @export
scale_color_rdataviz <- function(palette = "default", ...) {
  pal <- .get_palette(palette)
  ggplot2::scale_color_manual(values = pal, ...)
}

#' @rdname scale_rdataviz
#' @export
scale_colour_rdataviz <- scale_color_rdataviz

#' Continuous Risk Gradient Scale
#'
#' Applies the risk_gradient color palette as a continuous fill scale.
#' Useful for heatmaps, risk scores, and other continuous data where
#' low values should appear green and high values should appear red.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_fill_gradientn()].
#' @param reverse Logical. If `TRUE`, reverses the gradient direction.
#'
#' @return A ggplot2 continuous scale object.
#'
#' @examples
#' library(ggplot2)
#'
#' # Risk heatmap
#' df <- expand.grid(x = 1:5, y = 1:5)
#' df$risk <- df$x * df$y / 25
#'
#' ggplot(df, aes(x, y, fill = risk)) +
#'   geom_tile() +
#'   scale_fill_risk() +
#'   theme_worldclass()
#'
#' @export
scale_fill_risk <- function(..., reverse = FALSE) {
  colors <- if (reverse) rev(risk_gradient) else risk_gradient
  ggplot2::scale_fill_gradientn(colors = colors, ...)
}

#' @rdname scale_fill_risk
#' @export
scale_color_risk <- function(..., reverse = FALSE) {
  colors <- if (reverse) rev(risk_gradient) else risk_gradient
  ggplot2::scale_color_gradientn(colors = colors, ...)
}

# Internal function to retrieve palette by name
.get_palette <- function(name) {
  name <- tolower(name)

  switch(name,
    "default" = chart_colors,
    "chart" = chart_colors,
    "diabetes" = diabetes_colors,
    "health" = diabetes_colors,
    "subgroup" = subgroup_colors,
    "risk" = risk_gradient,
    rlang::abort(
      message = paste0(
        "Unknown palette: '", name, "'. ",
        "Available palettes: default, chart, diabetes, health, subgroup, risk"
      ),
      class = "rdataviz_error"
    )
  )
}
