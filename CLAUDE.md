# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About This Project

R data analysis project with two main components:
1. **rdataviz package** (`R/`, `tests/`) - CRAN-ready ggplot2/plotly theming toolkit
2. **Data analysis projects** - Diabetes analysis dashboard, RespiWatch surveillance platform

Optimized for terminal-based, voice-to-code workflows.

## Commands

### Package Development (rdataviz)
```bash
R -e 'devtools::load_all()'          # Load package for development
R -e 'devtools::test()'              # Run testthat tests
R -e 'devtools::check()'             # Full R CMD check
R -e 'devtools::document()'          # Generate roxygen2 docs
```

### Dependencies
```bash
R -e 'renv::restore()'               # Install project dependencies
R -e 'renv::install("pkg")'          # Add new package
R -e 'renv::snapshot()'              # Update lockfile
```

### Running Scripts
```bash
Rscript scripts/12_eda_diabetes.R    # Run analysis script
```

### Shiny Dashboards
```bash
R -e "shiny::runApp('diabetes_dashboard.R')"   # Main diabetes dashboard
R -e "shiny::runApp('respiwatch/app.R')"       # RespiWatch surveillance
```

### Reports
```bash
quarto render reports/diabetes_analysis_report.qmd
R -e "rmarkdown::render('reports/visualization_template.Rmd')"
```

### Code Quality
```bash
R -e 'styler::style_file("script.R")'    # Auto-format
R -e 'lintr::lint("script.R")'           # Lint check
```

## Architecture

### rdataviz Package (`R/`)

Publication-quality visualization toolkit with dark/light mode:

- **`theme-worldclass.R`** - Main ggplot2 theme (`theme_worldclass()`, `theme_wc()`, `theme_dashboard()`)
- **`palettes.R`** - Colorblind-safe palettes (`chart_colors`, `diabetes_colors`, `risk_gradient`, `subgroup_colors`) and `get_theme_colors()`
- **`scales.R`** - ggplot2 scale functions (`scale_fill_rdataviz()`, `scale_color_rdataviz()`, `scale_fill_risk()`)
- **`theme-plotly.R`** - Plotly theming (`apply_plotly_theme()`)

### Analysis Pipeline (`scripts/`)

Numbered scripts form a data pipeline:
- `01-05` - Basic data loading, iris/mtcars examples, statistical analysis
- `11-15` - Diabetes analysis: cleaning → EDA → feature engineering → modeling → evaluation
- `20-23` - Advanced: causal inference, anomaly detection, fairness audit, external data fusion

### Shiny Dashboards

- **`diabetes_dashboard.R`** (~2000 lines) - Comprehensive ML dashboard with 8 tabs: Overview, EDA, Modeling, Subgroup Analysis, Causal Inference, Fairness Audit, Data Fusion, Model Comparison
- **`respiwatch/`** - Respiratory surveillance platform (separate subproject with own `R/` modules)

### Data Flow

```
data/raw/ (read-only) → scripts/ → data/processed/ → dashboards/reports
                        ↓
                      output/figures/
```

## Coding Standards

- tidyverse style guide, snake_case naming, 2-space indentation, 80-char lines
- Use native pipe `|>` (R 4.1+)
- Use `library()` at top, `pkg::fn()` for one-off calls
- Always set `na.rm = TRUE` explicitly
- Use relative paths from project root

### Script Header Template
```r
# ============================================================================
# Title: [Script purpose]
# Author: [Name]
# Date: [Date]
# Purpose: [One-sentence description]
# Input: data/processed/clean_data.csv
# Output: output/figures/plot.png
# ============================================================================
```

## Key Files

- `DESCRIPTION` - Package metadata for rdataviz
- `renv.lock` - Reproducible dependencies
- `data/raw/diabetes_012_health_indicators_BRFSS2015.csv` - Main diabetes dataset
- `data/processed/diabetes_clean.rds` - Cleaned analysis-ready data

## Do Not

- Modify files in `data/raw/`
- Use `setwd()`, `attach()`, or right-assignment `->`
- Commit large data files or `renv/library/`
