# AI-Powered Data Science Demonstration
## Voice-Driven Analysis with Claude Code & Custom R Agents

**Project:** Diabetes Risk Analysis & Interactive Dashboard  
**Platform:** Claude Code with Custom Skills  
**Date:** December 2025

---

## Executive Summary

This project demonstrates how **AI-assisted, voice-driven development** can accelerate the entire data science lifecycle—from raw data ingestion through advanced analytics to production-ready dashboards and CRAN-quality R packages. Using Claude Code with custom-built R data science agents, we transformed multiple federal health datasets into actionable insights, predictive models, and an interactive dashboard in a fraction of traditional development time.

### What We Built

| Deliverable | Description |
|-------------|-------------|
| **Comprehensive Analysis** | 253,680-record CDC BRFSS analysis with statistical tests, visualizations |
| **Predictive Models** | Logistic regression & random forest achieving 82% AUC-ROC |
| **Interactive Dashboard** | 8-tab Shiny dashboard with dark/light modes, accessibility features |
| **Advanced Analytics** | Causal inference, fairness audits, anomaly detection, multi-level modeling |
| **CRAN-Ready Package** | `rdataviz` package with 34 tests, full documentation, CI/CD |

### Key Results

| Metric | Value |
|--------|-------|
| Diabetes Prevalence Identified | 15.8% (1 in 6 adults) |
| Model Accuracy (AUC-ROC) | 0.82 |
| Cases Preventable via BP Control | 42% |
| Sensitivity at Optimal Threshold | 78% |
| R CMD Check Result | 0 errors, 0 warnings, 0 notes |

---

## The AI-Assisted Workflow

### 1. Data Ingestion & Fusion

Using voice commands and Claude Code, we orchestrated the ingestion of **four federal datasets**:

```
Datasets Integrated:
├── CDC BRFSS 2015 Health Indicators (253,680 records)
│   └── 22 health behavior and chronic condition variables
├── CDC PLACES County Health Data
│   └── County-level health metrics
├── USDA Food Environment Atlas
│   └── Food access and nutrition environment
└── Census FIPS Codes
    └── Geographic crosswalk for data fusion
```

**AI Contribution:** Automated schema detection, data type inference, missing value strategies, and cross-dataset joining logic—tasks that typically require hours of manual exploration.

### 2. Exploratory Data Analysis

The AI agent generated:
- 26+ publication-quality visualizations
- Automated statistical summaries
- Risk factor prevalence comparisons
- Distribution analyses across demographic segments

**Output:** `output/figures/` directory with diabetes-specific visualizations following data visualization best practices (Tufte principles, colorblind-safe palettes).

### 3. Statistical Analysis

Comprehensive hypothesis testing and effect size estimation:

| Analysis | Finding |
|----------|---------|
| Odds Ratios | High BP (OR=1.61), High Cholesterol (OR=1.48), Poor Health (OR=1.65) |
| Risk Ratios | Quantified relative risk across demographic groups |
| Chi-Square Tests | Validated associations between risk factors |
| Correlation Analysis | Identified multicollinearity patterns |

### 4. Predictive Modeling

Two production-ready models with full evaluation:

**Logistic Regression**
- AUC-ROC: 0.820
- Interpretable coefficients
- Clinical decision support ready

**Random Forest**
- AUC-ROC: 0.820
- Feature importance rankings
- Ensemble robustness

**Evaluation Artifacts:**
- ROC curves, PR curves, calibration plots
- Confusion matrices at multiple thresholds
- Threshold optimization analysis

### 5. Advanced Analytics Suite

Beyond basic modeling, the AI agent implemented four advanced analysis modules:

#### Causal Inference
- Propensity score matching
- Average Treatment Effect (ATE) estimation
- **Finding:** Eliminating high blood pressure could prevent 42% of diabetes cases

#### Fairness Audit
- Demographic parity analysis across 5 protected attributes
- Equalized odds evaluation
- **Finding:** 20 disparity flags identified; income/education show largest gaps

#### Anomaly Discovery
- Identified "resilient" individuals (15.4%): high risk factors, no diabetes
- Identified "vulnerable" individuals (0.09%): low risk factors, with diabetes
- Subgroup profiling for targeted research

#### Multi-Level Modeling
- Variance partition analysis
- **Finding:** 97% individual-level variance, 3% environmental context

### 6. Interactive Dashboard

A production-ready **4,500-line Shiny dashboard** featuring:

| Tab | Functionality |
|-----|---------------|
| Overview | KPIs, prevalence metrics, summary statistics |
| Data Explorer | Interactive filtering, dynamic visualizations |
| Risk Predictor | Real-time risk scoring with model interpretations |
| Model Performance | ROC curves, confusion matrices, threshold analysis |
| Causal Analysis | Treatment effect visualizations |
| Fairness Audit | Disparity metrics, demographic comparisons |
| Discovery Lab | Anomaly exploration, subgroup analysis |
| Technical Docs | Methodology, data dictionary |

**Features:**
- Dark/light mode toggle with full theme propagation
- ARIA accessibility labels
- Lazy loading for performance
- Mobile-responsive design

### 7. Package Development

Transformed the project into a **CRAN-ready R package** (`rdataviz`):

```
rdataviz/
├── R/
│   ├── theme-worldclass.R    # ggplot2 theme with dark/light modes
│   ├── theme-plotly.R        # Plotly theming functions
│   ├── palettes.R            # Colorblind-safe color palettes
│   ├── scales.R              # ggplot2 scale functions
│   └── rdataviz-package.R    # Package documentation
├── tests/testthat/           # 34 passing tests
├── vignettes/                # Getting-started guide
├── man/                      # roxygen2 documentation
└── .github/workflows/        # CI/CD pipelines
```

**Quality Metrics:**
- R CMD check: 0 errors, 0 warnings, 0 notes
- Test coverage: 34 tests passing
- Full roxygen2 documentation

---

## Technology Stack

| Layer | Technology |
|-------|------------|
| AI Assistant | Claude Code with custom R data science skills |
| Language | R 4.5.1 |
| Package Management | renv (reproducible environments) |
| Visualization | ggplot2, plotly |
| Dashboard | Shiny + bslib |
| Modeling | ranger (random forest), glm (logistic) |
| Reporting | Quarto |
| CI/CD | GitHub Actions |
| Version Control | Git |

---

## What This Demonstrates

### For Data Scientists

1. **Accelerated Development:** Complex analyses that traditionally take weeks completed in hours through AI pair programming
2. **Best Practices by Default:** Automated application of visualization principles, code style, documentation standards
3. **Reproducibility Built-In:** renv lockfiles, documented pipelines, version-controlled outputs

### For Organizations

1. **Reduced Time-to-Insight:** From raw federal data to actionable dashboard without manual data wrangling bottlenecks
2. **Quality Assurance:** Automated testing, R CMD check compliance, CI/CD integration
3. **Knowledge Transfer:** Self-documenting code, comprehensive reports, audience-specific communications

### For the R Community

1. **Modern Package Development:** Demonstrated workflow from analysis project to CRAN-ready package
2. **Accessibility Standards:** Dark/light modes, colorblind-safe palettes, ARIA labels
3. **Reusable Components:** `rdataviz` package available for community use

---

## Key Innovations

### 1. Voice-to-Code Data Science
Natural language instructions translated into complete analytical pipelines—data cleaning, feature engineering, modeling, and visualization—without manual coding for routine tasks.

### 2. Automated Best Practices
- Tidyverse style guide compliance
- roxygen2 documentation generation
- testthat test scaffolding
- GitHub Actions CI/CD setup

### 3. Multi-Modal Output
Single analysis pipeline producing:
- Static reports (Quarto HTML)
- Interactive dashboards (Shiny)
- Reusable packages (CRAN-ready)
- Audience-specific communications (executive briefs, public health messaging)

### 4. Integrated Fairness & Causal Analysis
Beyond prediction accuracy, automatic generation of:
- Algorithmic fairness audits
- Causal effect estimation
- Anomaly detection for edge cases

---

## Conclusion

This demonstration shows that **AI-assisted data science isn't about replacing data scientists—it's about amplifying their capabilities**. By handling routine tasks (data wrangling, boilerplate code, documentation), Claude Code frees practitioners to focus on:

- Research design and hypothesis generation
- Domain expertise application
- Stakeholder communication
- Ethical considerations and fairness

The diabetes analysis project—from 253,680 raw records to production dashboard and CRAN package—exemplifies what's possible when human expertise combines with AI assistance.

---

## Repository Structure

```
Rdata/
├── data/
│   ├── raw/                  # Original datasets (BRFSS, PLACES, Food Atlas)
│   └── processed/            # Cleaned, analysis-ready data
├── scripts/                  # 17 analysis scripts (numbered pipeline)
├── output/
│   ├── figures/              # 26+ publication visualizations
│   └── models/               # Trained model artifacts
├── reports/
│   ├── diabetes_analysis_report.qmd    # Comprehensive Quarto report
│   ├── diabetes_executive_brief.md     # Executive summary
│   └── diabetes_key_messages.md        # Audience-specific messaging
├── R/                        # Package functions
├── tests/                    # testthat suite
├── diabetes_dashboard.R      # 4,500-line Shiny dashboard
└── DESCRIPTION               # CRAN-ready package metadata
```

---

*Prepared for the R data science community as a demonstration of AI-assisted analytical workflows.*
