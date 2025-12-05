# ============================================================================
# Title: Publication-Quality Iris Dataset Visualization
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Create modern, compelling visualization of iris species and features
# Input: Built-in iris dataset
# Output: output/iris_publication_viz.png
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(ggrepel)

# Modern theme ----------------------------------------------------------------
theme_modern <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
  theme(
    text = element_text(family = "sans", color = "#333333"),
    plot.title = element_text(
      size = rel(1.4),
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(1.0),
      color = "#666666",
      margin = margin(b = 20)
    ),
    plot.caption = element_text(
      size = rel(0.8),
      color = "#999999",
      hjust = 0
    ),
    plot.title.position = "plot",
    axis.title = element_text(size = rel(0.9), color = "#666666"),
    axis.text = element_text(size = rel(0.85), color = "#666666"),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "#EEEEEE", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
}

# Load and prepare data -------------------------------------------------------
iris_data <- iris |>
  as_tibble() |>
  mutate(
    Species = str_to_title(Species),
    Species = str_replace(Species, "^(\\w+)$", "\\1")
  )

# Calculate species centroids for labeling -----------------------------------
species_labels <- iris_data |>
  group_by(Species) |>
  summarise(
    Petal.Length = mean(Petal.Length),
    Petal.Width = mean(Petal.Width),
    .groups = "drop"
  )

# Modern color palette (colorblind-safe) --------------------------------------
species_colors <- c(
  "Setosa" = "#332288",
  "Versicolor" = "#44AA99",
  "Virginica" = "#CC6677"
)

# Create main visualization ---------------------------------------------------
p_main <- ggplot(
  iris_data,
  aes(x = Petal.Length, y = Petal.Width, color = Species)
) +
  # Add subtle shadow points for depth
  geom_point(
    alpha = 0.15,
    size = 3.5,
    position = position_jitter(width = 0.05, height = 0.02, seed = 42)
  ) +
  # Main data points
  geom_point(
    alpha = 0.8,
    size = 3,
    position = position_jitter(width = 0.05, height = 0.02, seed = 42)
  ) +
  # Add ellipses to show species clusters
  stat_ellipse(
    aes(fill = Species),
    geom = "polygon",
    alpha = 0.1,
    linewidth = 0.8,
    level = 0.95
  ) +
  # Direct labels instead of legend
  geom_text_repel(
    data = species_labels,
    aes(label = Species),
    size = 5,
    fontface = "bold",
    box.padding = 1.5,
    point.padding = 0.5,
    segment.color = "#CCCCCC",
    segment.size = 0.5,
    min.segment.length = 0.5,
    seed = 42
  ) +
  # Color scales
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  # Labels with clear findings-oriented title
  labs(
    title = "Iris Species Show Distinct Petal Morphology",
    subtitle = "Setosa petals are dramatically smaller, while Virginica and Versicolor overlap in size",
    x = "Petal Length (cm)",
    y = "Petal Width (cm)",
    caption = "Data: Fisher's Iris Dataset (n=150) | Three species, 50 specimens each"
  ) +
  # Apply modern theme
  theme_modern(base_size = 14)

# Create companion sepal visualization ---------------------------------------
p_sepal <- ggplot(
  iris_data,
  aes(x = Sepal.Length, y = Sepal.Width, color = Species)
) +
  geom_point(
    alpha = 0.15,
    size = 3.5,
    position = position_jitter(width = 0.05, height = 0.02, seed = 42)
  ) +
  geom_point(
    alpha = 0.8,
    size = 3,
    position = position_jitter(width = 0.05, height = 0.02, seed = 42)
  ) +
  stat_ellipse(
    aes(fill = Species),
    geom = "polygon",
    alpha = 0.1,
    linewidth = 0.8,
    level = 0.95
  ) +
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "Sepal Dimensions Show More Overlap Between Species",
    subtitle = "All three species cluster closely in sepal morphology compared to petal differences",
    x = "Sepal Length (cm)",
    y = "Sepal Width (cm)",
    caption = "Data: Fisher's Iris Dataset (n=150)"
  ) +
  theme_modern(base_size = 14)

# Save visualizations ---------------------------------------------------------
# Main publication-quality petal visualization
ggsave(
  filename = "output/iris_publication_viz.png",
  plot = p_main,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

# Companion sepal visualization
ggsave(
  filename = "output/iris_sepal_viz.png",
  plot = p_sepal,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

# Print summary statistics ----------------------------------------------------
cat("\n=== Iris Dataset Summary ===\n\n")

iris_data |>
  group_by(Species) |>
  summarise(
    n = n(),
    mean_petal_length = mean(Petal.Length),
    mean_petal_width = mean(Petal.Width),
    mean_sepal_length = mean(Sepal.Length),
    mean_sepal_width = mean(Sepal.Width),
    .groups = "drop"
  ) |>
  print()

cat("\n=== Visualizations saved to output/ directory ===\n")
cat("- output/iris_publication_viz.png (Petal morphology - primary)\n")
cat("- output/iris_sepal_viz.png (Sepal morphology - companion)\n\n")
