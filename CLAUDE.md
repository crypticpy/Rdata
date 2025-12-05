# Rdata - Voice-Driven Data Analysis Project

## About This Project
R project repository for voice-driven data analysis demonstrations. Focus on interactive visualizations, statistical analysis, and reproducible research workflows optimized for terminal-based, voice-to-code development.

## Tech Stack
- Language: R 4.5.1 (ARM64/aarch64-apple-darwin20)
- Package Management: renv
- IDE: Terminal-based (Claude Code), RStudio available
- Reporting: Quarto (.qmd) and R Markdown (.Rmd)
- Visualization: ggplot2, plotly (interactive)
- Dashboards: Shiny, flexdashboard

## Key Directories
- `data/raw/` - Original, unmodified data (read-only)
- `data/processed/` - Cleaned, analysis-ready datasets
- `R/` - Custom functions and utilities
- `scripts/` - Data pipeline and processing scripts
- `reports/` - Quarto/R Markdown documents for analysis
- `output/` - Generated plots, models, and tables
- `tests/` - Unit tests

## Coding Standards

### Style
- Follow tidyverse style guide (https://style.tidyverse.org/)
- Use snake_case for all names
- 2-space indentation
- 80 character line limit
- Use native pipe `|>` (R 4.1+)

### Required Practices
- Load all packages at the top of scripts
- Use `library()`, not `require()`
- Prefix function calls from non-loaded packages: `pkg::function()`
- Always set `na.rm = TRUE` explicitly when handling missing data
- Use relative paths from project root

### Code Structure
```r
# ============================================================================
# Title: [Script purpose]
# Author: [Name]
# Date: [Date]
# Purpose: [One-sentence description]
# Input: data/processed/clean_data.csv
# Output: output/figures/plot.png
# ============================================================================

# Load packages ---------------------------------------------------------------
library(tidyverse)

# Load data -------------------------------------------------------------------
df <- read_csv("data/raw/dataset.csv")

# Analysis --------------------------------------------------------------------
# [Analysis code here]

# Save outputs ----------------------------------------------------------------
ggsave("output/figures/plot.png", width = 8, height = 6)
```

## Commands

### Development
- `renv::restore()` - Install project dependencies
- `renv::snapshot()` - Update renv.lock after adding packages
- `Rscript scripts/analysis_template.R` - Run analysis script

### Reporting
- `R -e "rmarkdown::render('reports/visualization_template.Rmd')"` - Render R Markdown
- `quarto render reports/visualization_template.qmd` - Render Quarto (requires Quarto CLI)

### Shiny
- `R -e "shiny::runApp('app.R')"` - Launch Shiny dashboard

### Quality Checks
- `styler::style_file("script.R")` - Auto-format code
- `lintr::lint("script.R")` - Check for style issues

## Installed Packages

### Core
- tidyverse (dplyr, ggplot2, tidyr, readr, purrr, stringr, forcats)

### Visualization
- plotly, htmlwidgets, leaflet

### Reporting
- rmarkdown, knitr, flexdashboard

### Interactive
- shiny, DT (data tables)

## Workflow Notes

### Voice-Driven Development
- All scripts designed to run from terminal via `Rscript`
- Reports render to HTML for browser viewing
- Shiny apps launch in default browser
- Minimal IDE dependency

### When Adding Dependencies
1. Install package: `renv::install("newpkg")`
2. Update lockfile: `renv::snapshot()`
3. Document why the package was added

### Sample Datasets Available
- `data/raw/iris.csv` - Fisher's iris flower dataset
- `data/raw/mtcars.csv` - Motor Trend car road tests

## Do Not
- Modify files in `data/raw/` - these are read-only
- Use `setwd()` - use relative paths from project root
- Commit large data files - use .gitignore
- Use `attach()` - causes namespace conflicts
- Use right-assignment `->` - use `<-` only
