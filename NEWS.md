# rdataviz 0.1.0

Initial release of rdataviz.
## Features

### Themes
- `theme_worldclass()`: Publication-quality ggplot2 theme with dark/light mode support
- `theme_wc()`, `theme_dashboard()`: Aliases for convenience

### Plotly Integration
- `apply_plotly_theme()`: Consistent theming for plotly charts
- `plotly_dark_layout()`, `plotly_light_layout()`: Quick theme application

### Color Palettes
- `chart_colors`: 10-color categorical palette
- `diabetes_colors`: Semantic health status colors
- `risk_gradient`: Sequential gradient for risk visualization
- `subgroup_colors`: Population subgroup colors
- `get_theme_colors()`: Programmatic access to theme colors

### Scale Functions
- `scale_fill_rdataviz()`, `scale_color_rdataviz()`: Categorical scales
- `scale_fill_risk()`, `scale_color_risk()`: Continuous risk gradient scales

## Design Principles

- Colorblind-safe palettes tested with color vision deficiency simulators
- WCAG 2.1 AA compliant contrast ratios
- High data-ink ratio following Tufte principles
- Consistent styling between ggplot2 and plotly outputs
