# Rdata - Voice-Driven Data Analysis

R project repository for data analysis demonstrations using voice-to-code workflows.

## Prerequisites

### macOS M3 Pro (ARM64)

**Install R via Homebrew:**
```bash
brew install r
```

**Or via official installer:**
Download the ARM64 version from [CRAN](https://cran.r-project.org/bin/macosx/)

**Install Quarto CLI (for .qmd reports):**
```bash
brew install quarto
```

## Quick Start

### 1. Restore Dependencies
```bash
R -e 'renv::restore()'
```

### 2. Run Analysis Scripts
```bash
Rscript scripts/01_load_data.R
Rscript scripts/analysis_template.R
```

### 3. Generate Reports

**R Markdown:**
```bash
R -e "rmarkdown::render('reports/visualization_template.Rmd')"
```

**Quarto:**
```bash
quarto render reports/visualization_template.qmd
```

### 4. Launch Shiny App
```bash
R -e "shiny::runApp('app.R')"
```

## Project Structure

```
Rdata/
├── data/
│   ├── raw/           # Original datasets
│   └── processed/     # Cleaned/transformed data
├── scripts/           # R analysis scripts
├── output/            # Generated charts, models
├── reports/           # R Markdown and Quarto reports
├── R/                 # Reusable R functions
├── tests/             # Unit tests
├── app.R              # Shiny application
├── renv.lock          # Package lockfile
└── DESCRIPTION        # Package metadata
```

## Installed Packages

| Category | Packages |
|----------|----------|
| Data Manipulation | tidyverse (dplyr, ggplot2, tidyr, readr, purrr) |
| Interactive Viz | plotly, htmlwidgets |
| Dashboards | shiny, flexdashboard |
| Reporting | rmarkdown, knitr |
| Tables & Maps | DT, leaflet |

## Voice-Driven Workflow

This project is optimized for terminal-based, voice-to-code workflows:

- All scripts run via `Rscript` or `R -e "..."` commands
- Reports generate to HTML for browser viewing
- Shiny apps launch in default browser
- Minimal IDE dependency

## Sample Datasets

- `data/raw/iris.csv` - Fisher's iris flower dataset
- `data/raw/mtcars.csv` - Motor Trend car road tests

## Adding New Dependencies

```bash
R -e 'renv::install("package_name")'
R -e 'renv::snapshot()'
```
