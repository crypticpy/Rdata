# ============================================================================
# Module: About Tab
# Purpose: About page with demo disclaimer, feature showcase, and development story
# ============================================================================

#' About Tab UI
#' @param id Module namespace ID
#' @return nav_panel UI element
aboutUI <- function(id) {

  ns <- NS(id)

  nav_panel(
    "About",
    icon = icon("info-circle"),

    div(
      class = "container-fluid mt-4",
      div(
        class = "row justify-content-center",
        div(
          class = "col-lg-10 col-xl-9",

          # ================================================================
          # SECTION 1: DISCLAIMER BANNER
          # ================================================================
          div(
            class = "disclaimer-banner",
            div(class = "disclaimer-title", "Demonstration Only - Not for Medical Use"),
            tags$p(
              class = "disclaimer-text",
              "This application is a technology demonstration showcasing AI-assisted development ",
              "with the R language. It is not intended for clinical decision-making, medical ",
              "diagnosis, or public health response. Data shown may be simulated or outdated."
            )
          ),

          # ================================================================
          # SECTION 2: HERO SECTION
          # ================================================================
          div(
            class = "about-hero",
            div(class = "about-hero-title", "RespiWatch"),
            div(
              class = "about-hero-subtitle",
              "Global Respiratory Multi-Pathogen Surveillance Platform"
            ),
            tags$p(
              style = "opacity: 0.85; margin-bottom: 1rem; position: relative;",
              "A comprehensive demonstration of what can be built through AI-human collaboration ",
              "using R data science tools. This platform tracks Influenza (H3N2, H1N1), RSV, and ",
              "COVID-19 with real-time data integrations, Bayesian forecasting, and scenario analysis."
            ),
            tags$span(class = "about-hero-season", "2025-2026 Season")
          ),

          # ================================================================
          # SECTION 3: STATS HIGHLIGHT
          # ================================================================
          div(
            class = "stats-highlight",
            div(
              class = "stat-item",
              div(class = "stat-value", "~20"),
              div(class = "stat-label", "Hours of Development")
            ),
            div(
              class = "stat-item",
              div(class = "stat-value", "8"),
              div(class = "stat-label", "Data Sources")
            ),
            div(
              class = "stat-item",
              div(class = "stat-value", "9"),
              div(class = "stat-label", "Data Stories")
            ),
            div(
              class = "stat-item",
              div(class = "stat-value", "35+"),
              div(class = "stat-label", "Data Visualizations")
            ),
            div(
              class = "stat-item",
              div(class = "stat-value", "100%"),
              div(class = "stat-label", "Voice-Driven")
            )
          ),

          # ================================================================
          # SECTION 4: KEY FEATURES (Hybrid: Cards + Accordion)
          # ================================================================
          div(
            class = "chart-container mb-4",

            h4(class = "about-section-header", icon("star"), "Key Features"),

            # Top 4 Feature Cards
            div(
              class = "features-grid",

              # Global Overview
              div(
                class = "feature-card",
                div(class = "feature-card-icon", icon("globe")),
                div(class = "feature-card-title", "Global Overview"),
                tags$p(
                  class = "feature-card-description",
                  "Interactive choropleth map with temporal animation, wave propagation analysis, ",
                  "pathogen selector, and real-time anomaly alerts across 50+ countries."
                )
              ),

              # Bayesian Forecast
              div(
                class = "feature-card",
                div(class = "feature-card-icon", icon("chart-area")),
                div(class = "feature-card-title", "Bayesian Forecast"),
                tags$p(
                  class = "feature-card-description",
                  "Probabilistic forecasting using brms/Stan with ensemble methods, model diagnostics, ",
                  "and 50/80/95% credible intervals for uncertainty quantification."
                )
              ),

              # Scenario Analysis
              div(
                class = "feature-card",
                div(class = "feature-card-icon", icon("sliders-h")),
                div(class = "feature-card-title", "Scenario Analysis"),
                tags$p(
                  class = "feature-card-description",
                  "What-if policy simulations with 8 intervention scenarios including mask mandates, ",
                  "social distancing, lockdowns, and vaccination campaigns."
                )
              ),

              # Healthcare Capacity
              div(
                class = "feature-card",
                div(class = "feature-card-icon", icon("hospital")),
                div(class = "feature-card-title", "Healthcare Capacity"),
                tags$p(
                  class = "feature-card-description",
                  "Hospital and ICU utilization gauges, surge forecasting, and time-to-critical ",
                  "capacity estimates with stress level indicators."
                )
              )
            ),

            # Accordion for remaining tabs
            tags$div(
              class = "accordion about-accordion mt-3",
              id = ns("moreFeatures"),

              # Country Analysis
              tags$div(
                class = "accordion-item",
                tags$h2(
                  class = "accordion-header",
                  tags$button(
                    class = "accordion-button collapsed",
                    type = "button",
                    `data-bs-toggle` = "collapse",
                    `data-bs-target` = paste0("#", ns("collapseCountry")),
                    icon("flag"), " Country Analysis"
                  )
                ),
                div(
                  id = ns("collapseCountry"),
                  class = "accordion-collapse collapse",
                  div(
                    class = "accordion-body",
                    div(
                      class = "accordion-feature-grid",
                      div(
                        class = "accordion-feature",
                        icon("tachometer-alt"),
                        tags$strong("Healthcare Capacity Gauges"),
                        " - Real-time hospital and ICU utilization monitoring"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("file-medical"),
                        tags$strong("Policy Response Tracking"),
                        " - Intervention timelines and effectiveness metrics"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("exclamation-circle"),
                        tags$strong("Severity-Coded Anomaly Flags"),
                        " - Outbreak detection with CUSUM and EARS algorithms"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("id-card"),
                        tags$strong("Country-Specific KPIs"),
                        " - Positivity rates, variant prevalence, confidence levels"
                      )
                    )
                  )
                )
              ),

              # Pathogen Analysis
              tags$div(
                class = "accordion-item",
                tags$h2(
                  class = "accordion-header",
                  tags$button(
                    class = "accordion-button collapsed",
                    type = "button",
                    `data-bs-toggle` = "collapse",
                    `data-bs-target` = paste0("#", ns("collapsePathogen")),
                    icon("virus"), " Pathogen Analysis"
                  )
                ),
                div(
                  id = ns("collapsePathogen"),
                  class = "accordion-collapse collapse",
                  div(
                    class = "accordion-body",
                    div(
                      class = "accordion-feature-grid",
                      div(
                        class = "accordion-feature",
                        icon("syringe"),
                        tags$strong("Vaccine Effectiveness Charts"),
                        " - Strain-specific VE with 95% confidence intervals"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("project-diagram"),
                        tags$strong("Co-Infection Pattern Analysis"),
                        " - Multi-pathogen interaction tracking"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("dna"),
                        tags$strong("Variant-Specific Tracking"),
                        " - Subclade prevalence and waning immunity modeling"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("chart-pie"),
                        tags$strong("Comparative Visualizations"),
                        " - Side-by-side pathogen metrics with 6 interactive charts"
                      )
                    )
                  )
                )
              ),

              # Surveillance Gaps
              tags$div(
                class = "accordion-item",
                tags$h2(
                  class = "accordion-header",
                  tags$button(
                    class = "accordion-button collapsed",
                    type = "button",
                    `data-bs-toggle` = "collapse",
                    `data-bs-target` = paste0("#", ns("collapseGaps")),
                    icon("exclamation-triangle"), " Surveillance Gaps"
                  )
                ),
                div(
                  id = ns("collapseGaps"),
                  class = "accordion-collapse collapse",
                  div(
                    class = "accordion-body",
                    div(
                      class = "accordion-feature-grid",
                      div(
                        class = "accordion-feature",
                        icon("broadcast-tower"),
                        tags$strong("Multi-Agency Monitoring"),
                        " - WHO, CDC, and ECDC surveillance status tracking"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("search"),
                        tags$strong("Gap Detection Engine"),
                        " - Automated identification of reporting delays"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("shield-alt"),
                        tags$strong("Data Quality Assessment"),
                        " - Confidence scoring and completeness metrics"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("user-secret"),
                        tags$strong("Suppression Evidence"),
                        " - Information withholding pattern detection"
                      )
                    )
                  )
                )
              ),

              # Rt Analysis
              tags$div(
                class = "accordion-item",
                tags$h2(
                  class = "accordion-header",
                  tags$button(
                    class = "accordion-button collapsed",
                    type = "button",
                    `data-bs-toggle` = "collapse",
                    `data-bs-target` = paste0("#", ns("collapseRt")),
                    icon("chart-line"), " Rt Analysis"
                  )
                ),
                div(
                  id = ns("collapseRt"),
                  class = "accordion-collapse collapse",
                  div(
                    class = "accordion-body",
                    div(
                      class = "accordion-feature-grid",
                      div(
                        class = "accordion-feature",
                        icon("wave-square"),
                        tags$strong("Renewal Equation Rt"),
                        " - EpiEstim-powered with pathogen-specific serial intervals"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("chart-area"),
                        tags$strong("95% Credible Intervals"),
                        " - Full uncertainty quantification for decision support"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("calendar-week"),
                        tags$strong("4-Week Case Forecasts"),
                        " - Projection models with confidence bands"
                      ),
                      div(
                        class = "accordion-feature",
                        icon("robot"),
                        tags$strong("AI-Powered Explanations"),
                        " - Natural language interpretation of epidemic dynamics"
                      )
                    )
                  )
                )
              )
            )
          ),

          # ================================================================
          # SECTION 5: DATA SOURCES
          # ================================================================
          div(
            class = "chart-container mb-4",

            h4(class = "about-section-header", icon("database"), "Data Sources"),

            div(
              class = "data-sources-grid",

              div(
                class = "data-source-item",
                icon("external-link-alt"),
                tags$a(href = "https://www.cdc.gov/flu/weekly/", target = "_blank", "CDC FluView"),
                tags$span(class = "source-description", "ILINet & Labs")
              ),

              div(
                class = "data-source-item",
                icon("external-link-alt"),
                tags$a(href = "https://www.who.int/tools/flunet", target = "_blank", "WHO FluNet"),
                tags$span(class = "source-description", "Global (52+ countries)")
              ),

              div(
                class = "data-source-item",
                icon("external-link-alt"),
                tags$a(href = "https://www.cdc.gov/rsv/research/rsv-net/", target = "_blank", "CDC RSV-NET"),
                tags$span(class = "source-description", "RSV Surveillance")
              ),

              div(
                class = "data-source-item",
                icon("external-link-alt"),
                tags$a(href = "https://covid.cdc.gov/covid-data-tracker/", target = "_blank", "CDC COVID Tracker"),
                tags$span(class = "source-description", "US COVID Data")
              ),

              div(
                class = "data-source-item",
                icon("external-link-alt"),
                tags$a(href = "https://ourworldindata.org/", target = "_blank", "Our World in Data"),
                tags$span(class = "source-description", "Global COVID/Vaccines")
              ),

              div(
                class = "data-source-item",
                icon("external-link-alt"),
                tags$a(href = "https://www.ecdc.europa.eu/", target = "_blank", "ECDC"),
                tags$span(class = "source-description", "European Surveillance")
              ),

              div(
                class = "data-source-item",
                icon("external-link-alt"),
                tags$a(href = "https://healthdata.gov/", target = "_blank", "HHS HealthData.gov"),
                tags$span(class = "source-description", "Healthcare Capacity")
              ),

              div(
                class = "data-source-item",
                icon("external-link-alt"),
                tags$a(href = "https://delphi.cmu.edu/", target = "_blank", "CMU Delphi Epidata"),
                tags$span(class = "source-description", "Epi Signals")
              )
            )
          ),

          # ================================================================
          # SECTION 5B: OPEN SOURCE ASSETS (For Developers)
          # ================================================================
          div(
            class = "chart-container mb-4",

            h4(class = "about-section-header", icon("code-branch"), "Open Source Assets"),

            tags$p(
              style = "color: var(--slate); margin-bottom: 1rem;",
              "This entire repository is designed for sharing. Below are production-ready components ",
              "that epidemiologists, data scientists, and data engineers can borrow as starting templates."
            ),

            # Asset Cards Grid
            div(
              class = "assets-grid",

              # API Connectors
              div(
                class = "asset-card",
                div(
                  class = "asset-card-header",
                  icon("plug"), tags$strong("API Connectors")
                ),
                tags$ul(
                  class = "asset-list",
                  tags$li("CDC Socrata (FluView, RSV-NET, COVID)"),
                  tags$li("WHO FluMart (52+ countries)"),
                  tags$li("Our World in Data (GitHub)"),
                  tags$li("CMU Delphi Epidata"),
                  tags$li("HHS HealthData.gov"),
                  tags$li("ECDC European Surveillance")
                ),
                tags$span(class = "asset-file", "R/api_fetcher.R")
              ),

              # Epidemiological Algorithms
              div(
                class = "asset-card",
                div(
                  class = "asset-card-header",
                  icon("microscope"), tags$strong("Epi Algorithms")
                ),
                tags$ul(
                  class = "asset-list",
                  tags$li("Rt estimation (EpiEstim wrapper)"),
                  tags$li("Outbreak detection (CUSUM, EARS C1-C3)"),
                  tags$li("Renewal equation forecasting"),
                  tags$li("Bayesian forecasting (brms/Stan)"),
                  tags$li("Ensemble model averaging")
                ),
                tags$span(class = "asset-file", "R/rt_estimation.R, R/outbreak_detection.R")
              ),

              # Database Schema
              div(
                class = "asset-card",
                div(
                  class = "asset-card-header",
                  icon("database"), tags$strong("Database Schema")
                ),
                tags$ul(
                  class = "asset-list",
                  tags$li("Normalized surveillance data model"),
                  tags$li("Pathogen x Country x Date structure"),
                  tags$li("Forecast & alert storage"),
                  tags$li("Full CRUD operations"),
                  tags$li("Data lineage tracking")
                ),
                tags$span(class = "asset-file", "R/db_operations.R, R/db_schema.R")
              ),

              # Production Utilities
              div(
                class = "asset-card",
                div(
                  class = "asset-card-header",
                  icon("tools"), tags$strong("Production Utilities")
                ),
                tags$ul(
                  class = "asset-list",
                  tags$li("Logging with rotation (10MB, 5 files)"),
                  tags$li("Metrics collection & health checks"),
                  tags$li("Data scheduler (auto-refresh)"),
                  tags$li("Input validation & SQL injection prevention"),
                  tags$li("Shiny module architecture")
                ),
                tags$span(class = "asset-file", "R/logging.R, R/data_scheduler.R")
              ),

              # Data Fallback System
              div(
                class = "asset-card",
                div(
                  class = "asset-card-header",
                  icon("random"), tags$strong("Data Fallback System")
                ),
                tags$ul(
                  class = "asset-list",
                  tags$li("Priority-based source cascade (6 tiers)"),
                  tags$li("Wastewater & syndromic signal integration"),
                  tags$li("Gap detection with auto-switch back"),
                  tags$li("Transparent UI banners & badges"),
                  tags$li("Metadata attributes for downstream use")
                ),
                tags$span(class = "asset-file", "R/data_fallback.R, R/ui_helpers.R")
              )
            ),

            # Code Quality Banner
            div(
              class = "code-quality-banner",
              div(class = "quality-item", icon("check-circle"), "Roxygen2 Documented"),
              div(class = "quality-item", icon("check-circle"), "Consistent Header Templates"),
              div(class = "quality-item", icon("check-circle"), "Error Resilient (fallbacks)"),
              div(class = "quality-item", icon("check-circle"), "Docker-Ready Deployment")
            ),

            # Expandable Code Example
            div(
              class = "code-example-container",
              tags$button(
                class = "code-example-toggle",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#codeExample",
                icon("code"), " Show Example: Fetch CDC Data"
              ),
              div(
                id = "codeExample",
                class = "collapse",
                tags$pre(
                  class = "code-snippet",
                  tags$code(
"# Example: Fetch CDC FluView data with caching
library(httr2)

fetch_cdc_fluview <- function(season = \"2024-25\") {
  url <- \"https://data.cdc.gov/resource/rvwb-2h8x.json\"

  response <- request(url) |>
    req_url_query(`$where` = paste0(\"season = '\", season, \"'\")) |>
    req_retry(max_tries = 3, backoff = ~ 2) |>
    req_perform()

  resp_body_json(response) |>
    tibble::as_tibble()
}

# Usage
flu_data <- fetch_cdc_fluview(\"2024-25\")"
                  )
                )
              )
            )
          ),

          # ================================================================
          # SECTION 6: BUILT WITH AI COLLABORATION
          # ================================================================
          div(
            class = "chart-container mb-4",

            h4(class = "about-section-header", icon("robot"), "Built with AI Collaboration"),

            tags$p(
              style = "color: var(--slate); margin-bottom: 1rem;",
              "This platform was built entirely via ", tags$strong("voice-driven development"),
              " in approximately ", tags$strong("20 hours"), " of collaborative work with AI systems. ",
              "The development process demonstrates what's possible when combining human creativity ",
              "with AI coding assistants."
            ),

            # Tool Badges
            div(
              class = "tool-badges-container",
              tags$span(
                class = "tool-badge claude-code",
                icon("terminal"), "Claude Code",
                tags$span(class = "tool-percentage", "90%")
              ),
              tags$span(
                class = "tool-badge factory-droid",
                icon("cogs"), "Factory Droid (Opus 4.5)",
                tags$span(class = "tool-percentage", "5%")
              ),
              tags$span(
                class = "tool-badge anti-gravity",
                icon("atom"), "Anti-Gravity (Gemini Pro 3)",
                tags$span(class = "tool-percentage", "5%")
              )
            ),

            # Development Impact Comparison Table
            h5(style = "color: var(--charcoal); margin-top: 1.5rem; margin-bottom: 1rem;", "Development Impact"),
            div(
              class = "comparison-table-container",
              tags$table(
                class = "comparison-table",
                tags$thead(
                  tags$tr(
                    tags$th("Metric"),
                    tags$th(icon("clock"), " Traditional"),
                    tags$th(icon("robot"), " AI-Assisted")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td("Development Time"),
                    tags$td("~3 months"),
                    tags$td(class = "highlight", "~20 hours")
                  ),
                  tags$tr(
                    tags$td("API Integrations"),
                    tags$td("2-3 sources"),
                    tags$td(class = "highlight", "8 sources")
                  ),
                  tags$tr(
                    tags$td("Analysis Modules"),
                    tags$td("3-4 tabs"),
                    tags$td(class = "highlight", "9 data stories")
                  ),
                  tags$tr(
                    tags$td("Visualizations"),
                    tags$td("10-15 charts"),
                    tags$td(class = "highlight", "35+ interactive")
                  ),
                  tags$tr(
                    tags$td("Documentation"),
                    tags$td("Manual effort"),
                    tags$td(class = "highlight", "Inline generated")
                  )
                )
              )
            ),

            # Development Timeline
            h5(style = "color: var(--charcoal); margin-top: 1.5rem; margin-bottom: 1rem;", "Development Process"),

            div(
              class = "development-timeline",

              div(
                class = "timeline-step",
                div(class = "timeline-step-title", "1. Setup Phase"),
                tags$p(
                  class = "timeline-step-description",
                  "Built custom R Data sub-agents and skill sets for Claude Code, primed with Shiny ",
                  "app best practices, R coding standards, and data visualization patterns."
                )
              ),

              div(
                class = "timeline-step",
                div(class = "timeline-step-title", "2. Prototype Phase"),
                tags$p(
                  class = "timeline-step-description",
                  "Created initial working prototype with hardcoded data to establish the core ",
                  "architecture and UI/UX patterns."
                )
              ),

              div(
                class = "timeline-step",
                div(class = "timeline-step-title", "3. Iteration Phase"),
                tags$p(
                  class = "timeline-step-description",
                  "Mapped out each feature area, refined the UI/UX, and established consistent ",
                  "design patterns across all tabs."
                )
              ),

              div(
                class = "timeline-step",
                div(class = "timeline-step-title", "4. Expansion Phase"),
                tags$p(
                  class = "timeline-step-description",
                  "Built out full feature sets for each tab including data integrations, ",
                  "Bayesian modeling, and interactive visualizations."
                )
              ),

              div(
                class = "timeline-step",
                div(class = "timeline-step-title", "5. Debugging Strategy"),
                tags$p(
                  class = "timeline-step-description",
                  "When encountering loops or bugs with Claude Code, switched to Factory Droid ",
                  "(Opus 4.5) or Anti-Gravity (Gemini) for fresh perspectives, then returned to Claude Code."
                )
              )
            ),

            # Quote Box
            div(
              class = "quote-box",
              tags$p(
                class = "quote-text",
                "\"We were able to build a comprehensive respiratory surveillance platform with ",
                "Bayesian forecasting, scenario analysis, and real-time data integrations in just ",
                "two work days of voice-driven collaboration with AI. This demonstrates the ",
                "transformative potential of AI-assisted development for R data science.\""
              )
            )
          ),

          # ================================================================
          # SECTION 7: TECHNICAL STACK
          # ================================================================
          div(
            class = "chart-container mb-4",

            h4(class = "about-section-header", icon("layer-group"), "Technical Stack"),

            div(
              class = "tech-stack-container",
              tags$span(class = "tech-pill", icon("r-project"), "R"),
              tags$span(class = "tech-pill", icon("desktop"), "Shiny"),
              tags$span(class = "tech-pill", icon("palette"), "bslib"),
              tags$span(class = "tech-pill", icon("map"), "Leaflet"),
              tags$span(class = "tech-pill", icon("chart-bar"), "Plotly"),
              tags$span(class = "tech-pill", icon("chart-line"), "ggplot2"),
              tags$span(class = "tech-pill", icon("calculator"), "brms/Stan"),
              tags$span(class = "tech-pill", icon("database"), "SQLite"),
              tags$span(class = "tech-pill", icon("table"), "DT"),
              tags$span(class = "tech-pill", icon("code"), "httr2"),
              tags$span(class = "tech-pill", icon("clock"), "lubridate"),
              tags$span(class = "tech-pill", icon("broom"), "tidyverse")
            )
          ),

          # ================================================================
          # SECTION 8: FOOTER
          # ================================================================
          div(
            class = "text-center mt-4 mb-4",
            uiOutput(ns("last_updated_text")),
            tags$p(
              class = "text-muted",
              style = "font-size: 0.85rem; margin-top: 0.5rem;",
              "RespiWatch v1.0 | Built with ", icon("heart", style = "color: var(--coral);"), " and AI"
            )
          ),

          # Animated Stats Counter Script
          tags$script(src = "about-animations.js")

        )
      )
    )
  )
}

#' About Tab Server
#' @param id Module namespace ID
#' @param outbreak_data Reactive or static outbreak data containing metadata
aboutServer <- function(id, outbreak_data) {
  moduleServer(id, function(input, output, session) {

    output$last_updated_text <- renderUI({
      last_updated <- tryCatch({
        if (is.reactive(outbreak_data)) {
          outbreak_data()$metadata$last_updated
        } else {
          outbreak_data$metadata$last_updated
        }
      }, error = function(e) "Unknown")

      tags$p(
        class = "text-muted",
        style = "font-size: 0.9rem;",
        icon("sync-alt"), " Data last updated: ", last_updated
      )
    })

  })
}
