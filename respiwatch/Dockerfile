# ==============================================================================
# RespiWatch Shiny Dashboard - Dockerfile for Hugging Face Spaces
# Multi-stage build for optimized image size
# ==============================================================================

# Stage 1: Build stage with all development dependencies
FROM rocker/shiny:4.3.2 AS builder

# Install system dependencies for R packages (including Stan for brms)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libsqlite3-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libv8-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*

# Install core R packages (batch 1 - foundations)
RUN R -e "install.packages(c( \
    'shiny', \
    'shinyjs', \
    'bslib', \
    'jsonlite', \
    'httr', \
    'httr2', \
    'digest', \
    'DBI', \
    'RSQLite', \
    'lubridate' \
), repos='https://cloud.r-project.org/')"

# Install visualization packages (batch 2)
RUN R -e "install.packages(c( \
    'leaflet', \
    'plotly', \
    'ggplot2', \
    'scales', \
    'DT' \
), repos='https://cloud.r-project.org/')"

# Install tidyverse and data packages (batch 3)
RUN R -e "install.packages(c( \
    'dplyr', \
    'tidyr', \
    'tidyverse', \
    'zoo' \
), repos='https://cloud.r-project.org/')"

# Install spatial packages (batch 4)
RUN R -e "install.packages(c( \
    'sf', \
    'rnaturalearth', \
    'rnaturalearthdata', \
    'geosphere' \
), repos='https://cloud.r-project.org/')"

# Install epidemiology packages (batch 5)
RUN R -e "install.packages(c( \
    'surveillance', \
    'openxlsx', \
    'rmarkdown', \
    'knitr' \
), repos='https://cloud.r-project.org/')"

# Install Bayesian packages (batch 6 - these are heavy)
RUN R -e "install.packages(c( \
    'bayesplot', \
    'posterior', \
    'tidybayes' \
), repos='https://cloud.r-project.org/')"

# Install brms (needs Stan backend - this is the heaviest)
RUN R -e "install.packages('brms', repos='https://cloud.r-project.org/')"

# Install remotes and GitHub packages
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')" && \
    R -e "remotes::install_github('mrc-ide/EpiEstim', upgrade='never')"

# ==============================================================================
# Stage 2: Production image
# ==============================================================================
FROM rocker/shiny:4.3.2

# Copy installed packages from builder
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Install only runtime system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libsqlite3-dev \
    libv8-dev \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /srv/shiny-server/respiwatch

# Copy application files
COPY app.R ./
COPY global.R ./
COPY R/ ./R/
COPY data/ ./data/
COPY www/ ./www/

# Create logs directory
RUN mkdir -p logs && chmod 777 logs

# Create cache directory for models
RUN mkdir -p data/cache/models && chmod 777 data/cache/models

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/respiwatch

# Configure Shiny Server for Hugging Face Spaces (runs as root)
# Note: HF Spaces runs containers as root, so we don't specify run_as
RUN echo 'server {\n\
  listen 7860;\n\
  location / {\n\
    site_dir /srv/shiny-server/respiwatch;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}' > /etc/shiny-server/shiny-server.conf

# Create startup validation script
RUN echo '#!/usr/bin/env Rscript\n\
required_pkgs <- c("shiny", "bslib", "lubridate", "digest", "EpiEstim", "brms", "sf", "leaflet", "plotly", "DBI", "RSQLite")\n\
missing <- c()\n\
for (pkg in required_pkgs) {\n\
  if (!requireNamespace(pkg, quietly = TRUE)) {\n\
    missing <- c(missing, pkg)\n\
  }\n\
}\n\
if (length(missing) > 0) {\n\
  stop(sprintf("Missing required packages: %s", paste(missing, collapse = ", ")))\n\
}\n\
message("All required packages validated successfully")\n\
' > /srv/shiny-server/respiwatch/test_startup.R && chmod +x /srv/shiny-server/respiwatch/test_startup.R

# Expose Hugging Face Spaces port
EXPOSE 7860

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=120s --retries=3 \
  CMD curl -f http://localhost:7860/ || exit 1

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
