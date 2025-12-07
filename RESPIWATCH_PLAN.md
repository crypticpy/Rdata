# RespiWatch: Global Respiratory Surveillance Platform

## Project Vision

A **Global Respiratory Multi-Pathogen Tracking System** for the 2024-2025 season that combines:
- Real-time geospatial disease intelligence
- Bayesian modeling and forecasting
- Hyperlocal drill-down capability (global → country → state → county)
- Multi-pathogen comparison (Flu, RSV, COVID, others)

**Target Audience:** R data community, epidemiologists, public health professionals, data scientists

**Key Differentiator:** Built entirely via voice-to-text interaction with Claude Code, demonstrating agentic AI capabilities for the R community.

---

## Core Features

### 1. Geospatial Intelligence
- Interactive choropleth maps with smooth zoom (global → hyperlocal)
- Leaflet-based with high-performance rendering
- Spatial clustering detection
- Animated temporal evolution (show disease spread over time)
- Regional comparison views

### 2. Multi-Pathogen Tracking
- **Influenza** - CDC FluView, WHO FluMart
- **RSV** - RSV-NET hospitalization data
- **COVID-19** - Ongoing surveillance data
- **Other respiratory pathogens** - NREVSS lab data
- Side-by-side pathogen comparison

### 3. Predictive Modeling & Forecasting
- **Regional acceleration forecasts** - "North America +15% next 2 weeks"
- **Spatial propagation modeling** - Predict where outbreaks are heading
- **Rt estimation** - Reproduction number with credible intervals
- **Nowcasting** - Estimate current burden from lagged reporting
- **2-4 week ahead projections** with uncertainty bands

### 4. Bayesian Elements
- Full posterior distributions, not just point estimates
- Model comparison via LOO-CV, WAIC
- Prior visualization and sensitivity analysis
- Credible intervals on all forecasts

### 5. Anomaly Detection
- Flag unexpected surges across regions/pathogens
- Aberration detection algorithms (EARS, Farrington)
- Alert system for significant changes

### 6. Automated Reporting
- Weekly Quarto situation reports
- Exportable summaries
- Key metrics dashboard

---

## Data Sources (APIs)

| Source | Data | Geographic Coverage | Update Frequency |
|--------|------|---------------------|------------------|
| CDC FluView | Influenza surveillance | US (state/regional) | Weekly |
| WHO FluMart | Global influenza | International | Weekly |
| RSV-NET | RSV hospitalizations | US | Weekly |
| COVID Data Tracker | COVID metrics | US/Global | Daily/Weekly |
| NREVSS | Respiratory virus labs | US | Weekly |

### API Considerations
- Use `httr2` for R API connections
- Implement caching to reduce API calls
- Handle rate limiting gracefully
- Store raw data locally for offline development

---

## Technical Stack

### R Packages
```r
# Core
library(shiny)
library(bslib)
library(tidyverse)

# Geospatial
library(sf)
library(leaflet)
library(tmap)

# Bayesian/Modeling
library(brms)
library(EpiEstim)
library(surveillance)
library(forecast)

# API/Data
library(httr2)
library(jsonlite)

# Visualization
library(plotly)
library(ggplot2)
library(viridis)

# Reporting
library(quarto)
```

### Project Structure
```
/Users/aiml/Projects/Rdata/respiwatch/
├── app.R                    # Main Shiny app
├── R/
│   ├── data_fetch.R         # API connections
│   ├── data_process.R       # Data cleaning/transformation
│   ├── geo_viz.R            # Map functions
│   ├── models.R             # Forecasting/Bayesian models
│   ├── anomaly.R            # Aberration detection
│   └── utils.R              # Helper functions
├── data/
│   ├── raw/                 # Cached API data
│   ├── processed/           # Cleaned datasets
│   └── geo/                 # Shapefiles, boundaries
├── www/
│   ├── css/                 # Custom styles
│   └── images/              # Assets
├── reports/
│   └── weekly_template.qmd  # Quarto report template
├── tests/                   # Unit tests
├── deploy/                  # HuggingFace deployment
│   ├── Dockerfile
│   ├── README.md
│   └── app.R
└── prototype/               # Initial prototype files (user's existing code)
```

---

## UI/UX Design Goals

### Visual Identity
- Clean, professional "Clinical Premium" aesthetic (similar to diabetes dashboard)
- Coral (#E85D4C) and Teal (#0D9488) accent colors
- Light theme with high contrast
- Playfair Display for headings, DM Sans for body

### Map Experience
- Smooth, performant zooming
- Clear visual hierarchy at all zoom levels
- Tooltips with key metrics on hover
- Legend that adapts to data range

### Dashboard Layout
- Global overview as landing view
- Sidebar for pathogen/region selection
- Main panel for map + charts
- Drill-down panel for detailed analysis

---

## Development Phases

### Phase 1: Foundation
- [ ] Set up project structure
- [ ] Implement API connections (CDC FluView first)
- [ ] Basic leaflet map with US states
- [ ] Simple time series display

### Phase 2: Multi-Pathogen
- [ ] Add RSV, COVID data sources
- [ ] Pathogen selector UI
- [ ] Comparative visualizations
- [ ] Data caching layer

### Phase 3: Modeling
- [ ] Implement Rt estimation (EpiEstim)
- [ ] Basic forecasting (exponential smoothing)
- [ ] Nowcasting for reporting lag
- [ ] Uncertainty visualization

### Phase 4: Bayesian Enhancement
- [ ] brms models for hierarchical forecasting
- [ ] Model comparison tools
- [ ] Prior sensitivity analysis
- [ ] Full posterior visualization

### Phase 5: Geospatial Advanced
- [ ] Spatial clustering detection
- [ ] Propagation prediction
- [ ] Animated temporal maps
- [ ] Hyperlocal drill-down (county level)

### Phase 6: Polish & Deploy
- [ ] Automated reporting (Quarto)
- [ ] Anomaly alerts
- [ ] Performance optimization
- [ ] HuggingFace deployment

---

## Key Differentiators from Existing Tools

1. **Multi-pathogen in one view** - Most tools focus on single disease
2. **Bayesian uncertainty** - Full posteriors, not just point estimates
3. **Predictive, not just descriptive** - Forecasts and propagation modeling
4. **Hyperlocal capability** - Drill from global to county
5. **Built via voice** - Demonstrates AI-assisted development
6. **Open source on HuggingFace** - Accessible to all

---

## Demo Narrative

*"I described a respiratory surveillance system to my computer. Over the next few hours, without touching a keyboard, we built a platform that:*
- *Connects to live CDC and WHO data*
- *Shows flu, RSV, and COVID side-by-side*
- *Predicts where outbreaks are heading*
- *Lets you zoom from global view down to your county*
- *Generates weekly situation reports automatically*

*This is what AI-assisted R development looks like in 2024."*

---

## Resources & References

### Similar Tools (for inspiration)
- CDC FluView Interactive
- WHO FluMart Dashboard
- HealthMap
- ProMED-mail

### Key Papers
- EpiEstim methodology (Cori et al.)
- Nowcasting approaches (Günther et al.)
- Spatial epidemiology methods

### R Package Documentation
- `surveillance` package vignettes
- `EpiEstim` tutorials
- `brms` case studies

---

## Notes

- User has existing prototype code to incorporate
- Stick with R (not Python) - target audience is R community
- Voice-driven development is the "wow factor"
- Deploy to HuggingFace Spaces (proven path from diabetes dashboard)
- Clinical Premium styling for professional appearance

---

## Next Steps

1. User drops prototype files into `/Users/aiml/Projects/Rdata/respiwatch/prototype/`
2. Review existing code and identify reusable components
3. Begin Phase 1: Foundation
4. Iterate via voice-to-text conversation

---

*Created: 2024-12-06*
*Project: RespiWatch Global Respiratory Surveillance*
*Collaboration: Voice-driven development with Claude Code*
