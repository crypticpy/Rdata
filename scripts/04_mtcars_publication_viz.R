# ============================================================================
# Title: Publication-Quality mtcars Performance Visualization
# Author: Claude Code
# Date: 2025-12-05
# Purpose: Create compelling visualization of fuel efficiency vs performance
# Input: data/raw/mtcars.csv
# Output: output/mtcars_publication_viz.png
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)
library(scales)

# Load data -------------------------------------------------------------------
mtcars_df <- read_csv("data/raw/mtcars.csv") |>
  rename(car_model = `...1`) |>
  mutate(
    cyl_factor = factor(cyl, levels = c(4, 6, 8),
                       labels = c("4 Cylinders", "6 Cylinders", "8 Cylinders")),
    weight_tons = wt  # wt is already in 1000 lbs, about 0.5 tons
  )

# Define modern theme ---------------------------------------------------------
theme_modern <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = "sans") +
  theme(
    text = element_text(color = "#333333"),
    plot.title = element_text(size = rel(1.5), face = "bold",
                             margin = margin(b = 8), hjust = 0),
    plot.subtitle = element_text(size = rel(1.05), color = "#555555",
                                 margin = margin(b = 20), hjust = 0,
                                 lineheight = 1.2),
    plot.caption = element_text(size = rel(0.8), color = "#777777",
                               hjust = 0, margin = margin(t = 15)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.title = element_text(size = rel(0.95), color = "#555555",
                             face = "bold"),
    axis.text = element_text(size = rel(0.9), color = "#666666"),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(size = rel(0.9), face = "bold"),
    legend.text = element_text(size = rel(0.85)),
    legend.key.spacing.x = unit(0.8, "lines"),
    plot.margin = margin(20, 25, 15, 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
}

# Modern color palette (colorblind-safe) --------------------------------------
palette_cylinders <- c(
  "4 Cylinders" = "#2A9D8F",  # Teal - efficient
  "6 Cylinders" = "#E9C46A",  # Gold - moderate
  "8 Cylinders" = "#E76F51"   # Coral - powerful but thirsty
)

# Calculate summary statistics for annotation ---------------------------------
mpg_stats <- mtcars_df |>
  group_by(cyl_factor) |>
  summarise(
    mean_mpg = mean(mpg, na.rm = TRUE),
    mean_hp = mean(hp, na.rm = TRUE),
    .groups = "drop"
  )

# Identify extreme examples for annotation ------------------------------------
extreme_cars <- mtcars_df |>
  group_by(cyl_factor) |>
  slice_max(order_by = mpg, n = 1) |>
  ungroup()

# Create visualization --------------------------------------------------------
p <- ggplot(mtcars_df, aes(x = hp, y = mpg)) +
  # Add subtle weight contours using point size
  geom_point(aes(color = cyl_factor, size = weight_tons),
             alpha = 0.7, stroke = 0) +

  # Add smooth trend lines per cylinder group
  geom_smooth(aes(color = cyl_factor, fill = cyl_factor),
              method = "loess", se = TRUE, alpha = 0.15,
              linewidth = 1.2, linetype = "solid") +

  # Annotate most efficient car in each group
  geom_text(data = extreme_cars,
            aes(label = car_model, color = cyl_factor),
            nudge_y = 2, size = 3.2, fontface = "italic",
            show.legend = FALSE) +

  # Scales
  scale_x_continuous(
    breaks = seq(50, 350, by = 50),
    labels = comma_format()
  ) +
  scale_y_continuous(
    breaks = seq(10, 35, by = 5),
    limits = c(9, 36)
  ) +
  scale_color_manual(values = palette_cylinders) +
  scale_fill_manual(values = palette_cylinders, guide = "none") +
  scale_size_continuous(
    name = "Weight (1000 lbs)",
    range = c(3, 12),
    breaks = c(2, 3, 4, 5),
    labels = c("2", "3", "4", "5")
  ) +

  # Labels with compelling narrative
  labs(
    title = "More Cylinders = More Power, Less Efficiency",
    subtitle = "8-cylinder engines deliver 2x the horsepower of 4-cylinder models\nbut consume 50% more fuel per mile",
    x = "Horsepower (HP)",
    y = "Fuel Efficiency (MPG)",
    color = "Engine Configuration",
    caption = "Source: 1974 Motor Trend US magazine | Bubble size represents vehicle weight\nData: 32 automobile models (1973-74)"
  ) +

  # Apply modern theme
  theme_modern() +

  # Additional refinements
  guides(
    color = guide_legend(override.aes = list(size = 5, alpha = 1),
                        order = 1),
    size = guide_legend(order = 2)
  )

# Display summary statistics --------------------------------------------------
cat("\n=== MTCARS PERFORMANCE SUMMARY ===\n\n")
print(mpg_stats, n = Inf)

cat("\n=== MOST EFFICIENT BY CYLINDER COUNT ===\n\n")
print(extreme_cars |> select(car_model, cyl, mpg, hp, wt), n = Inf)

# Save high-resolution output -------------------------------------------------
output_path <- "output/mtcars_publication_viz.png"

# Create output directory if it doesn't exist
dir.create("output", showWarnings = FALSE, recursive = TRUE)

ggsave(
  output_path,
  plot = p,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white",
  units = "in"
)

cat("\n=== OUTPUT SAVED ===\n")
cat("Location:", normalizePath(output_path), "\n")
cat("Dimensions: 12 x 8 inches @ 300 DPI\n")
cat("File size:", round(file.size(output_path) / 1024 / 1024, 2), "MB\n\n")
