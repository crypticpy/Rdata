# ============================================================================
# Title: RespiWatch Wave Propagation Module
# Purpose: Model epidemic wave spread across geographic regions
# Input: Surveillance data with geographic coordinates, population data
# Output: Wave velocity estimates, predicted wave fronts, animation frames
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)

# =============================================================================
# CONFIGURATION
# =============================================================================

#' Wave propagation model parameters
WAVE_CONFIG <- list(
  # Diffusion coefficient (km^2/day) - controls spread speed
  diffusion_coefficient = 50,


  # Transmission parameters
  beta_default = 0.3,       # Transmission rate
  gamma_default = 0.1,      # Recovery rate (1/infectious period)

  # Simulation settings
  time_step = 1,            # Days per simulation step
  grid_resolution = 50,     # km per grid cell
  forecast_horizon = 28,    # Days to forecast

  # Wave detection thresholds
  wave_threshold = 0.01,    # Minimum prevalence to detect wave front
  velocity_window = 7       # Days to average for velocity calculation
)

# =============================================================================
# SPATIAL SIR MODEL WITH DIFFUSION
# =============================================================================

#' Initialize spatial grid for simulation
#' @param countries_sf sf object with country polygons
#' @param resolution Grid resolution in km
#' @return Grid sf object with cell IDs and centroids
initialize_spatial_grid <- function(countries_sf, resolution = WAVE_CONFIG$grid_resolution) {
  # Get bounding box

bbox <- st_bbox(countries_sf)

  # Create grid
  grid <- st_make_grid(
    countries_sf,
    cellsize = c(resolution * 1000, resolution * 1000),  # Convert km to meters
    what = "polygons"
  ) |>
    st_sf() |>
    mutate(cell_id = row_number())

  # Add centroids
  grid$centroid <- st_centroid(grid$geometry)
  grid$lon <- st_coordinates(grid$centroid)[, 1]
  grid$lat <- st_coordinates(grid$centroid)[, 2]

  # Intersect with country boundaries to keep only land cells
  grid <- grid |>
    st_filter(countries_sf)

  # Initialize SIR compartments
  grid$S <- 1.0  # Susceptible fraction
  grid$I <- 0.0  # Infected fraction
  grid$R <- 0.0  # Recovered fraction
  grid$N <- 1.0  # Normalized population

  grid
}

#' Create adjacency matrix for grid cells
#' @param grid sf grid object
#' @return Sparse adjacency matrix
create_adjacency_matrix <- function(grid) {
  # Find neighboring cells (Queen's case - includes diagonals)
  neighbors <- st_touches(grid)

  n_cells <- nrow(grid)
  adj_matrix <- Matrix::sparseMatrix(
    i = integer(0),
    j = integer(0),
    x = numeric(0),
    dims = c(n_cells, n_cells)
  )

  for (i in seq_len(n_cells)) {
    neighbor_ids <- neighbors[[i]]
    if (length(neighbor_ids) > 0) {
      # Equal weight to all neighbors
      weight <- 1 / length(neighbor_ids)
      adj_matrix[i, neighbor_ids] <- weight
    }
  }

  adj_matrix
}

#' Run one time step of spatial SIR with diffusion
#' @param state Current state matrix (n_cells x 3: S, I, R)
#' @param adj_matrix Adjacency matrix
#' @param params Model parameters (beta, gamma, D)
#' @param dt Time step
#' @return Updated state matrix
sir_diffusion_step <- function(state, adj_matrix, params, dt = 1) {
  S <- state[, 1]
  I <- state[, 2]
  R <- state[, 3]

  beta <- params$beta
  gamma <- params$gamma
  D <- params$diffusion

  # SIR dynamics
  new_infections <- beta * S * I * dt
  new_recoveries <- gamma * I * dt

  # Diffusion of infected (spread to neighbors)
  I_diffused <- as.vector(adj_matrix %*% I)
  diffusion_flux <- D * (I_diffused - I) * dt

  # Update compartments
  S_new <- S - new_infections
  I_new <- I + new_infections - new_recoveries + diffusion_flux
  R_new <- R + new_recoveries

  # Ensure non-negative values
  S_new <- pmax(S_new, 0)
  I_new <- pmax(I_new, 0)
  R_new <- pmax(R_new, 0)

  # Renormalize
  total <- S_new + I_new + R_new
  S_new <- S_new / total
  I_new <- I_new / total
  R_new <- R_new / total

  cbind(S_new, I_new, R_new)
}

#' Run full spatial SIR simulation
#' @param grid Initial grid with S, I, R columns
#' @param adj_matrix Adjacency matrix
#' @param params Model parameters
#' @param n_days Number of days to simulate
#' @return List of grid states by day
simulate_spatial_spread <- function(grid, adj_matrix, params, n_days = WAVE_CONFIG$forecast_horizon) {
  n_cells <- nrow(grid)

  # Initialize state matrix
  state <- cbind(grid$S, grid$I, grid$R)

  # Store results
  results <- vector("list", n_days + 1)
  results[[1]] <- list(
    day = 0,
    state = state,
    grid = grid
  )

  # Run simulation
  for (day in seq_len(n_days)) {
    state <- sir_diffusion_step(state, adj_matrix, params)

    # Update grid
    grid$S <- state[, 1]
    grid$I <- state[, 2]
    grid$R <- state[, 3]

    results[[day + 1]] <- list(
      day = day,
      state = state,
      grid = grid
    )
  }

  results
}

# =============================================================================
# WAVE VELOCITY ESTIMATION
# =============================================================================

#' Estimate wave propagation velocity from time series
#' @param surveillance_data Data with dates and geographic info
#' @param pathogen Pathogen code
#' @return List with velocity estimate and direction
estimate_wave_velocity <- function(surveillance_data, pathogen) {
  # Filter to pathogen
  data <- surveillance_data |>
    filter(pathogen_code == pathogen) |>
    arrange(observation_date)

  if (nrow(data) < 10) {
    return(list(
      status = "insufficient_data",
      velocity_km_day = NA,
      direction = NA,
      confidence = "low"
    ))
  }

  # Group by week and location to find wave front
  weekly <- data |>
    mutate(week = floor_date(observation_date, "week")) |>
    group_by(week, country_code) |>
    summarise(
      total_cases = sum(case_count, na.rm = TRUE),
      positivity = mean(positivity_rate, na.rm = TRUE),
      .groups = "drop"
    )

  # Identify week when each country crossed threshold
  onset_dates <- weekly |>
    filter(positivity > 5 | total_cases > 100) |>  # Threshold for "wave arrival"
    group_by(country_code) |>
    summarise(onset_week = min(week), .groups = "drop")

  if (nrow(onset_dates) < 3) {
    return(list(
      status = "insufficient_spread",
      velocity_km_day = NA,
      direction = NA,
      confidence = "low"
    ))
  }

  # Need geographic coordinates to calculate actual distance
  # Using placeholder estimation based on timing differences
  time_range <- as.numeric(diff(range(onset_dates$onset_week)), units = "days")
  n_countries <- nrow(onset_dates)

  # Rough estimate assuming average inter-country distance of 500km
  avg_distance_km <- 500
  velocity_km_day <- (n_countries - 1) * avg_distance_km / max(time_range, 1)

  list(
    status = "success",
    velocity_km_day = round(velocity_km_day, 1),
    direction = "multi-directional",  # Would need actual coords for direction
    n_countries_affected = n_countries,
    spread_duration_days = time_range,
    onset_dates = onset_dates,
    confidence = if (n_countries >= 5) "high" else "medium"
  )
}

#' Calculate wave front position from simulation results
#' @param sim_results Results from simulate_spatial_spread()
#' @param threshold Prevalence threshold for wave front
#' @return Data frame with wave front positions by day
extract_wave_front <- function(sim_results, threshold = WAVE_CONFIG$wave_threshold) {
  wave_fronts <- lapply(sim_results, function(result) {
    grid <- result$grid

    # Wave front is where I is just above threshold
    front_cells <- grid |>
      filter(I >= threshold, I < threshold * 10) |>
      st_drop_geometry()

    if (nrow(front_cells) > 0) {
      data.frame(
        day = result$day,
        n_front_cells = nrow(front_cells),
        mean_lon = mean(front_cells$lon),
        mean_lat = mean(front_cells$lat),
        max_lon = max(front_cells$lon),
        min_lon = min(front_cells$lon),
        max_lat = max(front_cells$lat),
        min_lat = min(front_cells$lat)
      )
    } else {
      NULL
    }
  })

  bind_rows(wave_fronts)
}

# =============================================================================
# WAVE PREDICTION
# =============================================================================

#' Predict wave front position N days ahead
#' @param surveillance_data Current surveillance data
#' @param pathogen Pathogen code
#' @param countries_sf Country boundaries (sf object)
#' @param forecast_days Days to forecast
#' @return Prediction results with wave front positions
predict_wave_front <- function(surveillance_data, pathogen, countries_sf,
                                forecast_days = WAVE_CONFIG$forecast_horizon) {

  # Get current velocity estimate
  velocity <- estimate_wave_velocity(surveillance_data, pathogen)

  if (velocity$status != "success") {
    return(list(
      status = velocity$status,
      predictions = NULL,
      velocity = velocity
    ))
  }

  # Initialize grid
  tryCatch({
    grid <- initialize_spatial_grid(countries_sf)
    adj_matrix <- create_adjacency_matrix(grid)
  }, error = function(e) {
    return(list(
      status = "grid_initialization_failed",
      error = e$message,
      predictions = NULL
    ))
  })

  # Seed infection based on current data
  current_data <- surveillance_data |>
    filter(pathogen_code == pathogen) |>
    filter(observation_date >= max(observation_date) - days(7))

  # Set initial infected fraction based on positivity rates
  # This is a simplified seeding - real implementation would use geocoding
  initial_prevalence <- mean(current_data$positivity_rate, na.rm = TRUE) / 100
  initial_prevalence <- min(max(initial_prevalence, 0.001), 0.1)

  # Seed some cells as infected (placeholder - would use actual locations)
  n_seed <- max(1, round(nrow(grid) * 0.05))
  seed_indices <- sample(seq_len(nrow(grid)), n_seed)
  grid$I[seed_indices] <- initial_prevalence
  grid$S[seed_indices] <- 1 - initial_prevalence

  # Run simulation
  params <- list(
    beta = WAVE_CONFIG$beta_default,
    gamma = WAVE_CONFIG$gamma_default,
    diffusion = WAVE_CONFIG$diffusion_coefficient / (WAVE_CONFIG$grid_resolution^2)
  )

  sim_results <- simulate_spatial_spread(grid, adj_matrix, params, forecast_days)

  # Extract wave fronts
  wave_fronts <- extract_wave_front(sim_results)

  # Generate predictions
  predictions <- lapply(c(7, 14, 21, 28), function(d) {
    if (d <= forecast_days && d <= nrow(wave_fronts)) {
      front <- wave_fronts |> filter(day == d)
      if (nrow(front) > 0) {
        list(
          day = d,
          date = Sys.Date() + days(d),
          wave_front_lon_range = c(front$min_lon, front$max_lon),
          wave_front_lat_range = c(front$min_lat, front$max_lat),
          n_affected_cells = front$n_front_cells
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  list(
    status = "success",
    pathogen = pathogen,
    velocity = velocity,
    predictions = Filter(Negate(is.null), predictions),
    simulation = sim_results,
    wave_fronts = wave_fronts
  )
}

# =============================================================================
# ANIMATION FRAME GENERATION
# =============================================================================

#' Generate animation frames for wave visualization
#' @param sim_results Results from simulate_spatial_spread()
#' @param variable Which variable to visualize (I, S, R)
#' @return List of frame data for animation
generate_wave_animation_frames <- function(sim_results, variable = "I") {
  frames <- lapply(sim_results, function(result) {
    grid <- result$grid
    values <- grid[[variable]]

    list(
      day = result$day,
      date = Sys.Date() + days(result$day),
      cells = data.frame(
        cell_id = grid$cell_id,
        lon = grid$lon,
        lat = grid$lat,
        value = values
      ),
      summary = list(
        total_infected = sum(values * grid$N),
        peak_prevalence = max(values),
        mean_prevalence = mean(values),
        n_affected_cells = sum(values > WAVE_CONFIG$wave_threshold)
      )
    )
  })

  list(
    status = "success",
    variable = variable,
    n_frames = length(frames),
    frames = frames
  )
}

#' Get color for wave intensity
#' @param value Prevalence value (0-1)
#' @return Hex color code
get_wave_color <- function(value) {
  # Color scale from white (no infection) to red (high)
  if (value < 0.001) return("#F3F4F6")
  if (value < 0.01) return("#FEE2E2")
  if (value < 0.05) return("#FECACA")
  if (value < 0.1) return("#FCA5A5")
  if (value < 0.2) return("#F87171")
  if (value < 0.3) return("#EF4444")
  return("#DC2626")
}

# =============================================================================
# SUBNATIONAL DATA INTEGRATION
# =============================================================================

#' Get subnational surveillance data (placeholder for data sources)
#' @param country_code ISO country code
#' @param pathogen Pathogen code
#' @return Data frame with subnational data
get_subnational_data <- function(country_code, pathogen) {
  # This is a placeholder - real implementation would connect to:
  # - CDC HHS Protect (US states)
  # - ECDC TESSy (EU regions)
  # - UKHSA (UK trusts)

  # Return empty structure for now
  data.frame(
    country_code = character(),
    region_code = character(),
    region_name = character(),
    observation_date = as.Date(character()),
    case_count = integer(),
    positivity_rate = numeric(),
    population = integer(),
    stringsAsFactors = FALSE
  )
}

#' Check availability of subnational data
#' @param country_code ISO country code
#' @return List with availability info
check_subnational_availability <- function(country_code) {
  # Known sources of subnational data
  subnational_sources <- list(
    USA = list(
      available = TRUE,
      source = "CDC HHS Protect",
      granularity = "state",
      n_regions = 51
    ),
    GBR = list(
      available = TRUE,
      source = "UKHSA",
      granularity = "region",
      n_regions = 9
    ),
    DEU = list(
      available = TRUE,
      source = "RKI",
      granularity = "bundesland",
      n_regions = 16
    ),
    FRA = list(
      available = TRUE,
      source = "Sante Publique France",
      granularity = "region",
      n_regions = 18
    )
  )

  if (country_code %in% names(subnational_sources)) {
    subnational_sources[[country_code]]
  } else {
    list(
      available = FALSE,
      source = NA,
      granularity = NA,
      n_regions = 0
    )
  }
}

# =============================================================================
# MAIN API FUNCTIONS
# =============================================================================

#' Get wave propagation analysis for dashboard
#' @param surveillance_data Surveillance data
#' @param pathogen Pathogen code
#' @param countries_sf Country boundaries (sf object)
#' @return Complete wave analysis results
get_wave_analysis <- function(surveillance_data, pathogen, countries_sf = NULL) {
  results <- list(
    pathogen = pathogen,
    timestamp = Sys.time(),
    status = "processing"
  )

  # Estimate current velocity
  velocity <- estimate_wave_velocity(surveillance_data, pathogen)
  results$velocity <- velocity

  # If we have country boundaries, run spatial simulation
  if (!is.null(countries_sf) && velocity$status == "success") {
    tryCatch({
      prediction <- predict_wave_front(
        surveillance_data,
        pathogen,
        countries_sf,
        forecast_days = 28
      )
      results$prediction <- prediction
      results$status <- "success"
    }, error = function(e) {
      results$prediction <- NULL
      results$prediction_error <- e$message
      results$status <- "partial"
    })
  } else {
    results$prediction <- NULL
    results$status <- if (velocity$status == "success") "velocity_only" else velocity$status
  }

  # Generate summary
  results$summary <- list(
    has_velocity = !is.na(velocity$velocity_km_day),
    velocity_km_day = velocity$velocity_km_day,
    has_predictions = !is.null(results$prediction),
    confidence = velocity$confidence
  )

  results
}

#' Format wave analysis for display
#' @param analysis Results from get_wave_analysis()
#' @return Formatted HTML content
format_wave_analysis_html <- function(analysis) {
  if (analysis$status %in% c("insufficient_data", "insufficient_spread")) {
    return('<div class="alert alert-info">
      <i class="fas fa-info-circle"></i>
      Insufficient data for wave propagation analysis.
      Need surveillance data from at least 3 countries with significant spread.
    </div>')
  }

  velocity <- analysis$velocity

  html <- sprintf('
    <div class="wave-analysis-card">
      <h5>Wave Propagation Analysis: %s</h5>
      <div class="row">
        <div class="col-md-4">
          <div class="metric-box">
            <span class="metric-value">%.1f</span>
            <span class="metric-label">km/day spread velocity</span>
          </div>
        </div>
        <div class="col-md-4">
          <div class="metric-box">
            <span class="metric-value">%d</span>
            <span class="metric-label">countries affected</span>
          </div>
        </div>
        <div class="col-md-4">
          <div class="metric-box">
            <span class="metric-value">%s</span>
            <span class="metric-label">confidence level</span>
          </div>
        </div>
      </div>
    </div>',
    analysis$pathogen,
    velocity$velocity_km_day,
    velocity$n_countries_affected,
    velocity$confidence
  )

  html
}
