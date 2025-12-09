# ==============================================================================
# RespiWatch Shiny Dashboard - Dockerfile for Hugging Face Spaces
# Using r2u for pre-built binary R packages (fast builds)
# ==============================================================================

FROM rocker/r2u:jammy

# Install all R packages as pre-built binaries (minutes instead of hours)
# Core packages
RUN install.r \
    shiny \
    shinyjs \
    bslib \
    jsonlite \
    httr \
    httr2 \
    digest \
    DBI \
    RSQLite \
    lubridate

# Visualization packages
RUN install.r \
    leaflet \
    plotly \
    ggplot2 \
    scales \
    DT

# Data manipulation packages
RUN install.r \
    dplyr \
    tidyr \
    zoo

# Spatial packages
RUN install.r \
    sf \
    rnaturalearth \
    rnaturalearthdata \
    geosphere

# Epidemiology and reporting packages
RUN install.r \
    surveillance \
    openxlsx \
    rmarkdown \
    knitr

# Bayesian packages (these are pre-compiled in r2u!)
RUN install.r \
    bayesplot \
    posterior \
    tidybayes \
    brms

# Install remotes and EpiEstim from GitHub
RUN install.r remotes && \
    R -e "remotes::install_github('mrc-ide/EpiEstim', upgrade='never')"

# Install Shiny Server
RUN apt-get update && apt-get install -y --no-install-recommends \
    gdebi-core \
    curl \
    && curl -O https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb \
    && gdebi -n shiny-server-1.5.22.1017-amd64.deb \
    && rm shiny-server-*.deb \
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

# Configure Shiny Server for Hugging Face Spaces (runs as root)
RUN echo 'server {\n\
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
