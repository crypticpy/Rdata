# ============================================================================
# Title: RespiWatch Vaccination Impact Module
# Purpose: Track vaccination coverage and estimate prevented cases
# Input: Vaccination coverage data, surveillance data
# Output: Impact analysis, prevented cases estimates
# ============================================================================
# Load required packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
# Load data loader for DB access
source("R/data_loader.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

#' Vaccination impact configuration
VACCINATION_CONFIG <- list(
  # Default vaccine effectiveness by pathogen
  default_ve = list(
    "Influenza A" = 0.45,
    "Influenza B" = 0.50,
    "COVID-19" = 0.65,
    "RSV" = 0.75,
    "default" = 0.50
  ),

  # Vaccine effectiveness by outcome
  ve_outcomes = list(
    infection = 1.0,      # Base VE multiplier for infection
    symptomatic = 1.1,    # VE typically higher for symptomatic disease
    hospitalization = 1.3, # VE typically higher for severe outcomes
    death = 1.5           # VE typically highest for death prevention
  ),

  # Age group weights for coverage calculation
  age_weights = list(
    "0-17" = 0.20,
    "18-49" = 0.35,
    "50-64" = 0.20,
    "65+" = 0.25
  ),

  # Waning parameters (months)
  waning_halflife = 6,

  # Data cache duration (hours)
  cache_duration = 24
)

# =============================================================================
# VACCINATION COVERAGE DATA
# =============================================================================

#' Get vaccination coverage data for a country/pathogen
#' @param country Country name or ISO code
#' @param pathogen Pathogen name
#' @param date_range Date range for coverage data
#' @return Data frame with coverage by age group and time
get_vaccination_coverage <- function(country, pathogen, date_range = NULL) {

  if (is.null(date_range)) {
    date_range <- c(Sys.Date() - 365, Sys.Date())
  }
  
  # Try loading from database first
  db_data <- NULL
  try({
    all_vax_data <- load_vaccination_from_db()
    if (!is.null(all_vax_data) && nrow(all_vax_data) > 0) {
      db_data <- all_vax_data %>%
        filter(country_name == country, pathogen_name == pathogen) %>%
        filter(observation_date >= date_range[1], observation_date <= date_range[2])
    }
  }, silent = TRUE)
  
  if (!is.null(db_data) && nrow(db_data) > 0) {
    # If DB has data, format it to match expected output
    return(db_data %>%
      rename(date = observation_date, coverage = coverage_pct, country = country_name, pathogen = pathogen_name) %>%
      mutate(coverage = coverage / 100) %>% # Convert pct to decimal 0-1
      select(date, country, pathogen, age_group, coverage))
  }

  # FALLBACK: Generate synthetic coverage data based on pathogen characteristics
  dates <- seq.Date(date_range[1], date_range[2], by = "week")

  # Base coverage rates by pathogen
  base_coverage <- switch(
    pathogen,
    "Influenza A" = 0.45,
    "Influenza B" = 0.45,
    "COVID-19" = 0.70,
    "RSV" = 0.30,
    0.40
  )


  # Generate coverage trajectory with seasonal uptake
  coverage_data <- expand.grid(
    date = dates,
    age_group = names(VACCINATION_CONFIG$age_weights),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      week_of_year = week(date),
      # Seasonal uptake pattern (peaks in fall)
      seasonal_factor = 1 + 0.3 * sin((week_of_year - 40) * 2 * pi / 52),
      # Age-specific base coverage
      age_factor = case_when(
        age_group == "65+" ~ 1.4,
        age_group == "50-64" ~ 1.1,
        age_group == "18-49" ~ 0.8,
        age_group == "0-17" ~ 0.9,
        TRUE ~ 1.0
      ),
      coverage = pmin(0.95, base_coverage * seasonal_factor * age_factor),
      country = country,
      pathogen = pathogen
    ) %>%
    select(date, country, pathogen, age_group, coverage)

  coverage_data
}

#' Get current coverage summary
#' @param country Country name
#' @param pathogen Pathogen name
#' @return Named list with coverage metrics
get_coverage_summary <- function(country, pathogen) {

  coverage_data <- get_vaccination_coverage(
    country,
    pathogen,
    date_range = c(Sys.Date() - 90, Sys.Date())
  )

  # Get latest coverage by age group
  latest <- coverage_data %>%
    filter(date == max(date)) %>%
    select(age_group, coverage)

  # Calculate weighted overall coverage
  weights <- VACCINATION_CONFIG$age_weights
  overall <- sum(sapply(names(weights), function(ag) {
    cov <- latest$coverage[latest$age_group == ag]
    if (length(cov) == 0) 0 else cov * weights[[ag]]
  }))

  # Calculate trend
  start_coverage <- coverage_data %>%
    filter(date == min(date)) %>%
    summarise(mean_cov = mean(coverage)) %>%
    pull(mean_cov)

  end_coverage <- coverage_data %>%
    filter(date == max(date)) %>%
    summarise(mean_cov = mean(coverage)) %>%
    pull(mean_cov)

  trend <- (end_coverage - start_coverage) / start_coverage * 100

  list(
    overall_coverage = overall,
    by_age_group = setNames(latest$coverage, latest$age_group),
    trend_pct = trend,
    last_updated = max(coverage_data$date),
    country = country,
    pathogen = pathogen
  )
}

# =============================================================================
# VACCINE EFFECTIVENESS
# =============================================================================

#' Get vaccine effectiveness for pathogen/variant
#' @param pathogen Pathogen name
#' @param variant Optional variant name
#' @param outcome Outcome type (infection, symptomatic, hospitalization, death)
#' @param months_since_vaccination Months since last vaccination
#' @return Vaccine effectiveness (0-1)
get_vaccine_effectiveness <- function(pathogen,
                                       variant = NULL,
                                       outcome = "infection",
                                       months_since_vaccination = 0) {

  # Get base VE for pathogen
  base_ve <- VACCINATION_CONFIG$default_ve[[pathogen]]
  if (is.null(base_ve)) {
    base_ve <- VACCINATION_CONFIG$default_ve[["default"]]
  }

  # Adjust for outcome
  outcome_multiplier <- VACCINATION_CONFIG$ve_outcomes[[outcome]]
  if (is.null(outcome_multiplier)) outcome_multiplier <- 1.0

  # Apply waning
  waning_factor <- 0.5 ^ (months_since_vaccination / VACCINATION_CONFIG$waning_halflife)

  # Variant adjustment (if known)
  variant_factor <- if (!is.null(variant)) {
    # In production, would look up variant-specific VE
    switch(
      variant,
      "Delta" = 0.85,
      "Omicron" = 0.70,
      "BA.5" = 0.65,
      "XBB" = 0.60,
      1.0
    )
  } else {
    1.0
  }

  ve <- base_ve * outcome_multiplier * waning_factor * variant_factor
  pmin(0.95, pmax(0, ve))
}

#' Get VE breakdown by variant for a pathogen
#' @param pathogen Pathogen name
#' @return Data frame with VE by variant and outcome
get_ve_variant_breakdown <- function(pathogen) {

  variants <- switch(
    pathogen,
    "COVID-19" = c("Wild-type", "Delta", "Omicron", "BA.5", "XBB"),
    "Influenza A" = c("H1N1", "H3N2"),
    "Influenza B" = c("Victoria", "Yamagata"),
    "RSV" = c("RSV-A", "RSV-B"),
    c("Standard")
  )

  outcomes <- names(VACCINATION_CONFIG$ve_outcomes)

  expand.grid(
    variant = variants,
    outcome = outcomes,
    stringsAsFactors = FALSE
  ) %>%
    rowwise() %>%
    mutate(
      ve = get_vaccine_effectiveness(pathogen, variant, outcome, 0),
      ve_waned_6mo = get_vaccine_effectiveness(pathogen, variant, outcome, 6)
    ) %>%
    ungroup()
}

# =============================================================================
# PREVENTED CASES ESTIMATION
# =============================================================================

#' Estimate cases prevented by vaccination
#' @param country Country name
#' @param pathogen Pathogen name
#' @param observed_cases Number of observed cases
#' @param population Population size
#' @param coverage Vaccination coverage (0-1)
#' @param ve Vaccine effectiveness (0-1)
#' @return List with prevented cases estimates
estimate_prevented_cases <- function(country,
                                      pathogen,
                                      observed_cases,
                                      population,
                                      coverage = NULL,
                                      ve = NULL) {

  # Get coverage if not provided
  if (is.null(coverage)) {
    coverage_data <- get_coverage_summary(country, pathogen)
    coverage <- coverage_data$overall_coverage
  }

  # Get VE if not provided
  if (is.null(ve)) {
    ve <- get_vaccine_effectiveness(pathogen, outcome = "infection")
  }

  # Calculate attack rate in vaccinated vs unvaccinated
  # Using: AR_unvax = AR_observed / (1 - coverage * VE)

  attack_rate_observed <- observed_cases / population
  attack_rate_unvax <- attack_rate_observed / (1 - coverage * ve)
  attack_rate_vax <- attack_rate_unvax * (1 - ve)

  # Calculate counterfactual (no vaccination scenario)
  cases_if_no_vaccine <- round(attack_rate_unvax * population)

  # Cases prevented
  cases_prevented <- cases_if_no_vaccine - observed_cases

  # Calculate prevented hospitalizations and deaths
  hospitalization_rate <- switch(
    pathogen,
    "COVID-19" = 0.05,
    "Influenza A" = 0.02,
    "Influenza B" = 0.015,
    "RSV" = 0.03,
    0.02
  )

  mortality_rate <- switch(
    pathogen,
    "COVID-19" = 0.01,
    "Influenza A" = 0.001,
    "Influenza B" = 0.0008,
    "RSV" = 0.002,
    0.001
  )

  ve_hosp <- get_vaccine_effectiveness(pathogen, outcome = "hospitalization")
  ve_death <- get_vaccine_effectiveness(pathogen, outcome = "death")

  hospitalizations_prevented <- round(cases_prevented * hospitalization_rate * ve_hosp / ve)
  deaths_prevented <- round(cases_prevented * mortality_rate * ve_death / ve)

  list(
    country = country,
    pathogen = pathogen,
    observed_cases = observed_cases,
    cases_if_no_vaccine = cases_if_no_vaccine,
    cases_prevented = cases_prevented,
    hospitalizations_prevented = hospitalizations_prevented,
    deaths_prevented = deaths_prevented,
    coverage_used = coverage,
    ve_used = ve,
    attack_rate_observed = attack_rate_observed,
    attack_rate_unvax = attack_rate_unvax,
    effectiveness_ratio = cases_prevented / cases_if_no_vaccine
  )
}

#' Calculate prevented cases from surveillance data
#' @param surveillance_data Surveillance data frame
#' @param pathogen Pathogen to analyze
#' @param period Time period in days
#' @return Aggregated prevention estimates
calculate_prevention_impact <- function(surveillance_data, pathogen, period = 90) {

  # Handle different data structures (database vs JSON timeline)
  # Normalize column names
  if ("observation_date" %in% names(surveillance_data)) {
    date_col <- "observation_date"
  } else if ("date" %in% names(surveillance_data)) {
    date_col <- "date"
  } else {
    return(list(success = FALSE, message = "No date column found in data"))
  }

  if ("pathogen_name" %in% names(surveillance_data)) {
    pathogen_col <- "pathogen_name"
  } else if ("pathogen" %in% names(surveillance_data)) {
    pathogen_col <- "pathogen"
  } else {
    return(list(success = FALSE, message = "No pathogen column found in data"))
  }

  # Map pathogen names (H3N2 -> H3N2 (Influenza) or vice versa)
  pathogen_variants <- c(pathogen, paste0(pathogen, " (Influenza)"),
                         gsub(" \\(Influenza\\)", "", pathogen))

  # Filter to pathogen and period
  end_date <- max(surveillance_data[[date_col]], na.rm = TRUE)
  start_date <- end_date - days(period)

  filtered <- surveillance_data %>%
    filter(
      !!sym(pathogen_col) %in% pathogen_variants,
      !!sym(date_col) >= start_date,
      !!sym(date_col) <= end_date
    )

  if (nrow(filtered) == 0) {
    return(list(
      success = FALSE,
      message = paste("No surveillance data for", pathogen)
    ))
  }

  # Handle different case column names
  if ("positive_cases" %in% names(filtered)) {
    case_col <- "positive_cases"
  } else if ("case_numbers" %in% names(filtered)) {
    case_col <- "case_numbers"
  } else if ("estimated_cases" %in% names(filtered)) {
    case_col <- "estimated_cases"
  } else {
    # Generate synthetic case counts from positivity rate if available
    if ("positivity_rate" %in% names(filtered)) {
      filtered$positive_cases <- round(filtered$positivity_rate * 10000)
      case_col <- "positive_cases"
    } else {
      return(list(success = FALSE, message = "No case data column found"))
    }
  }

  # Handle country column - if not present, use "Global" as placeholder
  if (!"country" %in% names(filtered)) {
    filtered$country <- "Global"
  }

  # Aggregate by country
  by_country <- filtered %>%
    group_by(country) %>%
    summarise(
      total_cases = sum(!!sym(case_col), na.rm = TRUE),
      total_tests = if ("tests_conducted" %in% names(filtered))
        sum(tests_conducted, na.rm = TRUE) else total_cases * 10,
      .groups = "drop"
    )

  # Estimate prevention for each country
  
  # Load populations once
  pop_data <- NULL
  try({ pop_data <- load_country_populations() }, silent = TRUE)
  
  results <- lapply(by_country$country, function(c) {
    cases <- by_country$total_cases[by_country$country == c]
    
    # Lookup population
    pop <- 10000000 # Default fallback
    if (!is.null(pop_data)) {
      match_pop <- pop_data$population[pop_data$country_name == c]
      if (length(match_pop) > 0 && !is.na(match_pop)) {
        pop <- match_pop
      }
    }

    estimate_prevented_cases(c, pathogen, cases, pop)
  })

  # Aggregate
  total_prevented <- sum(sapply(results, function(r) r$cases_prevented))
  total_hosp_prevented <- sum(sapply(results, function(r) r$hospitalizations_prevented))
  total_deaths_prevented <- sum(sapply(results, function(r) r$deaths_prevented))
  avg_coverage <- mean(sapply(results, function(r) r$coverage_used))

  list(
    success = TRUE,
    pathogen = pathogen,
    period_days = period,
    countries_analyzed = length(results),
    total_observed_cases = sum(by_country$total_cases),
    total_cases_prevented = total_prevented,
    total_hospitalizations_prevented = total_hosp_prevented,
    total_deaths_prevented = total_deaths_prevented,
    average_coverage = avg_coverage,
    average_effectiveness = get_vaccine_effectiveness(pathogen),
    by_country = results
  )
}

# =============================================================================
# COVERAGE MAP DATA
# =============================================================================

#' Get coverage data for map visualization
#' @param pathogen Pathogen name
#' @return Data frame with country-level coverage for mapping
get_coverage_map_data <- function(pathogen) {

  # Sample countries with synthetic coverage data
  countries <- c(
    "United States", "Canada", "Mexico",
    "United Kingdom", "France", "Germany", "Italy", "Spain",
    "Japan", "South Korea", "Australia",
    "Brazil", "Argentina", "South Africa"
  )

  # Generate coverage data
  map_data <- data.frame(
    country = countries,
    stringsAsFactors = FALSE
  ) %>%
    rowwise() %>%
    mutate(
      coverage_data = list(get_coverage_summary(country, pathogen))
    ) %>%
    ungroup() %>%
    mutate(
      coverage = sapply(coverage_data, function(x) x$overall_coverage),
      trend = sapply(coverage_data, function(x) x$trend_pct)
    ) %>%
    select(country, coverage, trend)

  # Add coverage category for coloring
  map_data %>%
    mutate(
      coverage_category = case_when(
        coverage >= 0.80 ~ "High (>80%)",
        coverage >= 0.60 ~ "Moderate (60-80%)",
        coverage >= 0.40 ~ "Low (40-60%)",
        TRUE ~ "Very Low (<40%)"
      ),
      coverage_pct = round(coverage * 100, 1)
    )
}

# =============================================================================
# MAIN API FUNCTION
# =============================================================================

#' Get comprehensive vaccination impact analysis
#' @param surveillance_data Surveillance data frame
#' @param pathogen Pathogen to analyze
#' @return List with all vaccination impact metrics
get_vaccination_impact <- function(surveillance_data, pathogen) {

  tryCatch({
    # Calculate prevention impact
    prevention <- calculate_prevention_impact(surveillance_data, pathogen)

    # Get VE breakdown
    ve_breakdown <- get_ve_variant_breakdown(pathogen)

    # Get map data
    map_data <- get_coverage_map_data(pathogen)

    list(
      success = TRUE,
      pathogen = pathogen,
      timestamp = Sys.time(),
      prevention_impact = prevention,
      ve_breakdown = ve_breakdown,
      coverage_map = map_data,
      summary = list(
        cases_prevented = prevention$total_cases_prevented,
        hospitalizations_prevented = prevention$total_hospitalizations_prevented,
        deaths_prevented = prevention$total_deaths_prevented,
        average_coverage = prevention$average_coverage,
        average_ve = prevention$average_effectiveness
      )
    )
  }, error = function(e) {
    list(
      success = FALSE,
      pathogen = pathogen,
      error = e$message
    )
  })
}

#' Format vaccination impact for HTML display
#' @param impact Impact analysis result
#' @return HTML string
format_vaccination_impact_html <- function(impact) {

  if (!impact$success) {
    return(sprintf(
      '<div class="alert alert-warning">Unable to calculate vaccination impact: %s</div>',
      impact$error %||% "Unknown error"
    ))
  }

  s <- impact$summary
  p <- impact$prevention_impact

  sprintf('
    <div class="vaccination-impact-results">
      <div class="impact-summary mb-3">
        <h5 class="text-success"><i class="fas fa-shield-virus"></i> Vaccination Impact Summary</h5>
        <p class="text-muted small">%s - %d days analyzed across %d countries</p>
      </div>

      <div class="row g-2">
        <div class="col-md-4">
          <div class="impact-stat text-center p-2" style="background: #dcfce7; border-radius: 8px;">
            <div class="stat-value fw-bold text-success" style="font-size: 1.5rem;">%s</div>
            <div class="stat-label small">Cases Prevented</div>
          </div>
        </div>
        <div class="col-md-4">
          <div class="impact-stat text-center p-2" style="background: #dbeafe; border-radius: 8px;">
            <div class="stat-value fw-bold text-primary" style="font-size: 1.5rem;">%s</div>
            <div class="stat-label small">Hospitalizations Prevented</div>
          </div>
        </div>
        <div class="col-md-4">
          <div class="impact-stat text-center p-2" style="background: #fce7f3; border-radius: 8px;">
            <div class="stat-value fw-bold text-danger" style="font-size: 1.5rem;">%s</div>
            <div class="stat-label small">Deaths Prevented</div>
          </div>
        </div>
      </div>

      <div class="mt-3 p-2" style="background: #f8fafc; border-radius: 8px;">
        <div class="d-flex justify-content-between small">
          <span><strong>Avg Coverage:</strong> %.1f%%</span>
          <span><strong>Avg VE:</strong> %.1f%%</span>
          <span><strong>Observed Cases:</strong> %s</span>
        </div>
      </div>
    </div>
  ',
  impact$pathogen,
  p$period_days,
  p$countries_analyzed,
  format(s$cases_prevented, big.mark = ","),
  format(s$hospitalizations_prevented, big.mark = ","),
  format(s$deaths_prevented, big.mark = ","),
  s$average_coverage * 100,
  s$average_ve * 100,
  format(p$total_observed_cases, big.mark = ",")
  )
}
