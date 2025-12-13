# Rdata - Voice-Driven Data Science Demonstration

A demonstration of voice-driven agentic development with Claude Code, showcasing what's possible when building R data science applications through natural language conversation.

> **DISCLAIMER**: This project is for **demonstration purposes only** and should **not** be used for making healthcare decisions, predictions, evaluations, or forecasting. While the dashboards connect to real public health data sources, this is a learning/demonstration project.

## 🚀 R Data Science Skill for Claude Code

We've repackaged the R Data Science skill and subagents so they can be easily copied directly into your own projects! This skill provides comprehensive R programming assistance for data analysis, visualization, and statistical workflows.

### Quick Start

**Option 1: Download the ZIP (Easiest)**

1. Download `rdata-science-skills.zip` from this repository
2. Extract the contents
3. Copy the `.claude/` folder to your project root or home directory

**Option 2: Copy from Repository**

```bash
# Clone this repo
git clone https://github.com/crypticpy/Rdata.git

# Copy to your project (project-specific skill)
cp -r Rdata/.claude/skills/r-data-science /path/to/your-project/.claude/skills/

# OR copy to home directory (available in all projects)
mkdir -p ~/.claude/skills
cp -r Rdata/.claude/skills/r-data-science ~/.claude/skills/
```

**Option 3: Claude Code Plugin (Advanced)**

```
/plugin marketplace add crypticpy/Rdata
```

### What's Included

```
.claude/skills/r-data-science/
├── SKILL.md              # Main skill file (core R data science instructions)
├── CLAUDE_TEMPLATE.md    # Template for project CLAUDE.md files
├── README.md             # Detailed documentation
└── agents/               # Specialized subagent configurations
    ├── data-wrangler.md    # Data cleaning & transformation
    ├── viz-builder.md      # Modern, compelling visualizations
    ├── stats-analyst.md    # Statistical analysis & epidemiology
    ├── report-generator.md # Professional Quarto reports
    ├── dashboard-builder.md # Shiny & Quarto dashboards
    └── data-storyteller.md # Communication & narrative
```

### After Installation

1. **Restart Claude Code** to load the new skill
2. **Copy the template** (optional): `cp .claude/skills/r-data-science/CLAUDE_TEMPLATE.md ./CLAUDE.md`
3. **Start coding!** Claude will automatically use the skill when working with R files

### Example Prompts

- "Clean this CSV file and create age groups"
- "Create an epidemic curve showing weekly cases by region"
- "Follow the viz-builder agent patterns to create this chart"
- "Generate a Quarto report summarizing these surveillance data"

---

## Live Demos

| Application | Platform | Link |
|-------------|----------|------|
| **Diabetes Risk Analysis Dashboard** | Hugging Face Spaces | [Launch App](https://huggingface.co/spaces/BeyondEarth/diabetes-dashboard) |
| **RespiWatch Surveillance Platform** | Hugging Face Spaces | [Launch App](https://huggingface.co/spaces/BeyondEarth/respiwatch) |
| **Interactive Presentation** | GitHub Pages | [View Slides](https://beyondearth.github.io/Rdata/) |

## What This Demonstrates

- **~30+ hours** of voice-driven development with Claude Code
- **2 interactive dashboards** deployed to Hugging Face Spaces
- **35+ visualizations** built through natural language conversation
- **Live API connections** to CDC, WHO, ECDC public health data
- **100% voice-to-code** - no manual IDE coding required
- **First R project** for a developer with Python/React background

---

## Applications

### 1. Diabetes Risk Analysis Dashboard

A comprehensive machine learning dashboard analyzing CDC BRFSS survey data (253,680 respondents) with 8 integrated analysis tabs.

**Features:**
- **Executive Summary** - KPI cards, prevalence metrics, key findings (42% of diabetes cases preventable via BP control)
- **Data Explorer** - Interactive filtering, BMI vs age scatter plots, distribution analysis
- **Feature Analysis** - Correlation heatmaps, feature importance rankings (logistic + random forest)
- **Model Performance** - ROC curves, confusion matrices, threshold analysis, calibration metrics
- **Risk Predictor** - Personal risk assessment with 80% confidence intervals
- **Causal Analysis** - DAG-based causal inference, average treatment effects, counterfactual scenarios
- **Fairness Audit** - Demographic disparity detection across sex, age, income, education
- **Discovery Lab** - Anomaly subgroup analysis (resilient/vulnerable populations)

**Models:** Logistic Regression (AUC: 0.82) and Random Forest (AUC: 0.82)

**Run locally:**
```bash
R -e "shiny::runApp('diabetes_dashboard.R')"
```

---

### 2. RespiWatch - Global Respiratory Surveillance Platform

Real-time multi-pathogen surveillance dashboard tracking H3N2 Influenza, RSV, COVID-19, and H5N1 across 50+ countries.

**Features:**
- **Global Overview** - Interactive choropleth map with temporal animation, pathogen filtering, anomaly alerts
- **Country Analysis** - Country-specific KPIs, healthcare capacity gauges, policy tracking
- **Pathogen Analysis** - Cross-pathogen comparison, vaccine effectiveness charts, hospitalization rates
- **Surveillance Gaps** - Data quality monitoring, CUSUM/EARS anomaly detection
- **Rt Analysis** - EpiEstim-based reproduction number estimation with credible intervals
- **Bayesian Forecast** - brms/Stan hierarchical models with 50/80/95% credible bands
- **Scenario Analysis** - 8 intervention scenarios (mask mandate, lockdown, vaccination campaigns)
- **Healthcare Capacity** - Hospital/ICU surge forecasting with critical threshold alerts

**Data Sources:** CDC FluView, CDC RSV-NET, CDC COVID Data Tracker, WHO FluMart, ECDC, NWSS Wastewater

**Intelligent Fallback System:** Automatically bridges surveillance gaps with wastewater, syndromic, and forecast data when primary sources are unavailable.

**Run locally:**
```bash
R -e "shiny::runApp('respiwatch/app.R')"
```

---

## rdataviz Package

A publication-quality visualization toolkit for R with dark/light mode support.

**Key Exports:**
- `theme_worldclass()` / `theme_wc()` / `theme_dashboard()` - ggplot2 themes
- `scale_fill_rdataviz()` / `scale_color_rdataviz()` - Categorical color scales
- `scale_fill_risk()` / `scale_color_risk()` - Risk gradient scales
- `apply_plotly_theme()` - Plotly theming utility
- `chart_colors` - 10-color categorical palette
- `diabetes_colors` - Health status palette (green/amber/rose)
- `risk_gradient` - 6-color sequential risk palette
- `subgroup_colors` - Population subgroup palette

**Example:**
```r
library(ggplot2)
devtools::load_all()

ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_rdataviz() +
  theme_worldclass(dark_mode = TRUE)
```

**Development:**
```bash
R -e 'devtools::load_all()'    # Load package
R -e 'devtools::test()'        # Run tests
R -e 'devtools::check()'       # Full R CMD check
R -e 'devtools::document()'    # Generate docs
```

---

## Project Structure

```
Rdata/
├── .claude/skills/r-data-science/  # 📦 Distributable skill package
│   ├── SKILL.md                    # Main skill definition
│   ├── CLAUDE_TEMPLATE.md          # Project template
│   ├── README.md                   # Skill documentation
│   └── agents/                     # Specialized subagents
│
├── R/                          # rdataviz package source
│   ├── theme-worldclass.R      # ggplot2 themes
│   ├── palettes.R              # Color palettes
│   ├── scales.R                # ggplot2 scale functions
│   └── theme-plotly.R          # Plotly theming
│
├── scripts/                    # Analysis pipeline (numbered)
│   ├── 01-05                   # Basics: data loading, iris/mtcars examples
│   ├── 11-15                   # Diabetes: cleaning → EDA → features → modeling
│   └── 20-23                   # Advanced: causal inference, fairness, anomaly detection
│
├── data/
│   ├── raw/                    # Original datasets (read-only)
│   │   └── diabetes_012_health_indicators_BRFSS2015.csv
│   └── processed/              # Cleaned, analysis-ready data
│       ├── diabetes_clean.rds
│       └── diabetes_features.rds
│
├── respiwatch/                 # Respiratory surveillance platform
│   ├── app.R                   # Main Shiny entry point
│   ├── global.R                # Initialization and theme
│   ├── R/                      # Module and utility functions
│   │   ├── modules/            # 9 Shiny modules (UI/Server pairs)
│   │   ├── rt_estimation.R     # EpiEstim Rt calculation
│   │   ├── bayesian_forecast.R # brms forecasting
│   │   ├── scenario_modeling.R # Intervention scenarios
│   │   └── data_fallback.R     # Intelligent data cascade
│   ├── data/
│   │   └── respiwatch.sqlite   # SQLite database
│   ├── scripts/                # Data fetching pipelines
│   ├── www/                    # CSS, JS assets
│   └── Dockerfile              # HF Spaces deployment
│
├── output/                     # Generated outputs
│   └── models/                 # Saved ML models (.rds)
│
├── reports/                    # Quarto and R Markdown documents
│   ├── diabetes_analysis_report.qmd
│   └── visualization_template.qmd
│
├── presentation/               # Interactive slides
│   └── index.html              # GitHub Pages presentation
│
├── tests/testthat/             # Unit tests for rdataviz
│
├── rdata-science-skills.zip    # 📦 Downloadable skill package
├── diabetes_dashboard.R        # Main diabetes Shiny app
├── DESCRIPTION                 # Package metadata
├── renv.lock                   # Reproducible dependencies
└── CLAUDE.md                   # Development instructions
```

---

## Quick Start

### Prerequisites (macOS ARM64)

```bash
# Install R
brew install r SEQ

# Install Quarto (for reports)
brew install quarto
```

### Setup

```bash
# Clone repository
git clone https://github.com/crypticpy/Rdata.git
cd Rdata

# Restore dependencies
R -e 'renv::restore()'
```

### Run Applications

```bash
# Diabetes Dashboard
R -e "shiny::runApp('diabetes_dashboard.R')"

# RespiWatch
R -e "shiny::runApp('respiwatch/app.R')"
```

### Run Analysis Pipeline

```bash
# Diabetes analysis (sequential)
Rscript scripts/11_clean_diabetes_data.R
Rscript scripts/12_eda_diabetes.R
Rscript scripts/13_feature_engineering.R
Rscript scripts/14_model_building.R
Rscript scripts/15_model_evaluation.R

# Advanced analysis
Rscript scripts/20_causal_inference.R
Rscript scripts/21_anomaly_discovery.R
Rscript scripts/22_fairness_audit.R
```

### Generate Reports

```bash
quarto render reports/diabetes_analysis_report.qmd
```

---

## Technology Stack

| Category | Technologies |
|----------|--------------|
| **Framework** | R 4.1+, Shiny, bslib (Bootstrap 5) |
| **Visualization** | ggplot2, plotly, leaflet, visNetwork |
| **ML/Statistics** | ranger, pROC, EpiEstim, brms (Stan) |
| **Data** | tidyverse, DBI, RSQLite |
| **Causal Inference** | dagitty, ggdag, marginaleffects |
| **Fairness** | DALEX, fairmodels |
| **Reporting** | Quarto, R Markdown, knitr |
| **Deployment** | Docker, Hugging Face Spaces |
| **Environment** | renv (lockfile reproducibility) |

---

## Key Findings

### Diabetes Analysis
- **42% PAF** for high blood pressure - single most impactful modifiable risk factor
- **15.4% "Resilient"** individuals - high predicted risk but no diabetes (hypothesis generation)
- **20 disparity flags** identified across demographic groups (fairness audit)
- **Top predictors:** General health status, age, BMI, high blood pressure

### RespiWatch Capabilities
- **Multi-pathogen tracking** - H3N2, RSV, COVID-19, H5N1 in single dashboard
- **Probabilistic forecasting** - Bayesian models with full uncertainty quantification
- **Scenario modeling** - 8 policy interventions with comparable projections
- **Intelligent fallback** - Bridges CDC reporting gaps with alternate signals

---

## Development Workflow

This project demonstrates voice-to-code development with Claude Code:

- All code written through natural language conversation
- Terminal-based workflow (`Rscript`, `R -e "..."`)
- Minimal IDE dependency
- Reports generate to HTML for browser viewing
- Shiny apps launch in default browser

---

## Adding Dependencies

```bash
R -e 'renv::install("package_name")'
R -e 'renv::snapshot()'
```

---

## License

MIT License - See [LICENSE](LICENSE) for details.

---

## Acknowledgments

Built with Claude Code (Anthropic) using voice-driven development methodology. Data sources include CDC, WHO, ECDC public health surveillance systems.
