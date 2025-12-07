# ==============================================================================
# RespiWatch Shiny Dashboard - Dockerfile for Hugging Face Spaces
# Multi-stage build for optimized image size
# ==============================================================================

# Stage 1: Build stage with all development dependencies
FROM rocker/shiny:4.3.2 AS builder

# Install system dependencies for R packages
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
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'bslib', \
    'jsonlite', \
    'leaflet', \
    'plotly', \
    'dplyr', \
    'tidyr', \
    'ggplot2', \
    'scales', \
    'DT', \
    'sf', \
    'rnaturalearth', \
    'rnaturalearthdata', \
    'DBI', \
    'RSQLite', \
    'lubridate', \
    'httr', \
    'openxlsx', \
    'rmarkdown', \
    'knitr' \
), repos='https://cloud.r-project.org/')"

# Install EpiEstim for Rt estimation (not on CRAN, use GitHub)
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
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /srv/shiny-server/respiwatch

# Copy application files
COPY app.R ./
COPY R/ ./R/
COPY data/ ./data/
COPY www/ ./www/

# Create logs directory
RUN mkdir -p logs && chmod 777 logs

# Create cache directory for models
RUN mkdir -p data/cache/models && chmod 777 data/cache/models

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/respiwatch

# Configure Shiny Server
RUN echo 'run_as shiny;\n\
server {\n\
  listen 7860;\n\
  location / {\n\
    site_dir /srv/shiny-server/respiwatch;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}' > /etc/shiny-server/shiny-server.conf

# Expose Hugging Face Spaces port
EXPOSE 7860

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
  CMD curl -f http://localhost:7860/ || exit 1

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
