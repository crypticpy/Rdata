# ============================================================================
# Title: Iris Dataset Scatter Plot
# Date: 2025-12-05
# Purpose: Visualize iris sepal/petal relationships by species
# Input: data/raw/iris.csv
# Output: output/iris_scatter.html (interactive), output/iris_scatter.png
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(plotly)

# Load data -------------------------------------------------------------------
iris_df <- read_csv("data/raw/iris.csv", show_col_types = FALSE)

cat("Dataset loaded:", nrow(iris_df), "observations\n")
cat("Variables:", paste(names(iris_df), collapse = ", "), "\n\n")

# Create scatter plot ---------------------------------------------------------
p <- ggplot(iris_df, aes(
  x = Sepal.Length,
  y = Sepal.Width,
  color = Species,
  shape = Species
)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Iris Dataset: Sepal Dimensions by Species",
    subtitle = "Fisher's classic dataset (n = 150)",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    color = "Species",
    shape = "Species"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Display static plot
print(p)

# Create interactive version --------------------------------------------------
p_interactive <- ggplotly(p) |>
  layout(
    title = list(text = "Iris Dataset: Sepal Dimensions by Species"),
    legend = list(orientation = "h", y = -0.15)
  )

print(p_interactive)

# Save outputs ----------------------------------------------------------------
dir.create("output", showWarnings = FALSE)

ggsave("output/iris_scatter.png", p, width = 8, height = 6, dpi = 150)
cat("\nSaved: output/iris_scatter.png\n")

htmlwidgets::saveWidget(p_interactive, "output/iris_scatter.html", selfcontained = TRUE)
cat("Saved: output/iris_scatter.html\n")

cat("\nDone! Open output/iris_scatter.html in browser for interactive plot.\n")
