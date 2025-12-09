# ============================================================================
# RespiWatch: Global Respiratory Surveillance Platform
# app.R - Main Application Entry Point
# Multi-Pathogen Tracking System for 2024-2025 Season
# Built via voice-driven development with Claude Code
# ============================================================================

# Load global configuration, packages, theme, and data
source("global.R")

# UI --------------------------------------------------------------------------
ui <- page_navbar(
  title = "RespiWatch",
  theme = clinical_premium_theme,
  header = tags$head(
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Playfair+Display:wght@400;600;700&family=IBM+Plex+Mono&display=swap",
      rel = "stylesheet"
    ),
    tags$script(src = "epidemic-animation.js"),
    tags$script(src = "code-transparency.js")
  ),

  # Global Overview Tab (Module) -----------------------------------------------
  globalOverviewUI("global_overview"),

  # Country Analysis Tab (Module) ---------------------------------------------
  countryAnalysisUI("country_analysis", countries_df),

  pathogenAnalysisUI("pathogen_analysis", outbreak_data),

  # Surveillance Gaps Tab (Module) ---------------------------------------------
  surveillanceGapsUI("surveillance_gaps", outbreak_data),

  # Rt Analysis Tab (Module) ---------------------------------------------------
  rtAnalysisUI("rt_analysis"),

  # Bayesian Forecast Tab (Module) ---------------------------------------------
  bayesianForecastUI("bayesian_forecast"),

  # Scenario Analysis Tab -------------------------------------------------------
  scenarioAnalysisUI("scenario_analysis"),

  # Healthcare Capacity Tab (Module) -------------------------------------------
  healthcareCapacityUI("healthcare_capacity"),

  # About Tab (Module) ---------------------------------------------------------
  aboutUI("about")
)

# Server ----------------------------------------------------------------------
server <- function(input, output, session) {

  # ==========================================================================
  # DATE RANGE FILTERING (Per-Tab Controls)
  # ==========================================================================

  # Create reactive for timeline data source
  timeline_data_reactive <- reactive({
    if (!is.null(combined_timeline_df) && nrow(combined_timeline_df) > 0) {
      combined_timeline_df
    } else {
      data.frame(
        date = Sys.Date(),
        pathogen = "H3N2",
        positivity_rate = 0,
        case_numbers = 0,
        hospitalization_rate = 0
      )
    }
  })


  # ==========================================================================
  # MODULE SERVER CALLS
  # ==========================================================================

  # About Tab Module
  aboutServer("about", outbreak_data)

  # Surveillance Gaps Tab Module
  surveillanceGapsServer("surveillance_gaps", outbreak_data, timeline_data_reactive)

  # Country Analysis Tab Module
  countryAnalysisServer("country_analysis", outbreak_data, timeline_data_reactive)

  # Pathogen Analysis Tab Module
  pathogenAnalysisServer("pathogen_analysis", outbreak_data, timeline_data_reactive, pathogen_colors)

  # Rt Analysis Tab Module
  rtAnalysisServer("rt_analysis", timeline_data_reactive)

  # Bayesian Forecast Tab Module
  bayesianForecastServer("bayesian_forecast", timeline_data_reactive)
  scenarioAnalysisServer("scenario_analysis", timeline_data_reactive)
  healthcareCapacityServer("healthcare_capacity", timeline_data_reactive)
  
  # Global Overview Tab Module
  globalOverviewServer(
    "global_overview", 
    timeline_data_reactive, 
    world_countries, 
    map_snapshot_all,
    country_coords, 
    outbreak_data, 
    anomalies_df,
    db_surveillance
  )

  # ==========================================================================
  # GLOBAL NAVIGATION OBSERVERS
  # ==========================================================================

  # Observer: Navigate to About tab's data section when clicking "Learn more about our data"
  observeEvent(input$nav_to_about_data, {
    # Navigate to the About tab using JavaScript (more reliable for bslib)
    shinyjs::runjs("
      // Find and click the About tab
      var aboutTab = document.querySelector('a.nav-link[data-value=\"About\"]');
      if (aboutTab) {
        aboutTab.click();
      } else {
        // Fallback: try finding by text content
        var tabs = document.querySelectorAll('.nav-link');
        tabs.forEach(function(tab) {
          if (tab.textContent.includes('About')) {
            tab.click();
          }
        });
      }
    ")
    # Scroll to the data section after a brief delay for tab transition
    shinyjs::delay(500, shinyjs::runjs(
      "var el = document.getElementById('about-data_architecture_section');
       if (el) { el.scrollIntoView({behavior: 'smooth', block: 'start'}); }"
    ))
  }, ignoreInit = TRUE)

  # ==========================================================================
  # END MODULE SERVER CALLS
  # ==========================================================================
}


# Run App ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
