# RespiWatch: Global Respiratory Surveillance Platform

A real-time surveillance dashboard for tracking respiratory pathogens including H3N2 Influenza, RSV, and COVID-19. Built with R Shiny for deployment on Hugging Face Spaces.

## Features

### Core Surveillance
- **Multi-pathogen tracking**: H3N2, RSV, COVID-19
- **Real-time data**: Integrates with CDC APIs (ILINet, RSV-NET, NCIRD)
- **Global coverage**: 10+ countries with weekly surveillance data
- **Interactive maps**: Choropleth visualization of outbreak intensity

### Advanced Analytics
- **Rt Estimation**: Real-time reproduction number calculation using EpiEstim
- **Ensemble Forecasting**: Combined Rt-renewal and Bayesian model predictions
- **Outbreak Detection**: CUSUM and EARS (C1/C2/C3) algorithms
- **Scenario Modeling**: What-if analysis for intervention planning

### User Experience
- **Alert System**: Real-time notifications for outbreak signals
- **Report Generation**: PDF and Excel export capabilities
- **Mobile Responsive**: Works on desktop, tablet, and mobile devices
- **Production Logging**: Comprehensive monitoring and metrics

## Deployment

### Hugging Face Spaces

1. Fork this repository
2. Create a new Space on Hugging Face
3. Select "Docker" as the SDK
4. Connect your repository
5. The app will build and deploy automatically

### Local Development

```bash
# Using Docker Compose
docker-compose up --build

# Or run directly with R
R -e "shiny::runApp('app.R', port = 8889)"
```

### Configuration

Environment variables:
- `LOG_LEVEL`: Logging verbosity (DEBUG, INFO, WARNING, ERROR)
- `SHINY_LOG_LEVEL`: Shiny server log level

## Project Structure

```
respiwatch/
├── app.R                    # Main Shiny application
├── R/
│   ├── data_loader.R        # Database and API data loading
│   ├── db_schema.R          # SQLite schema definitions
│   ├── db_operations.R      # Database CRUD operations
│   ├── rt_estimation.R      # EpiEstim Rt calculation
│   ├── forecasting.R        # Basic forecasting functions
│   ├── bayesian_forecast.R  # brms Bayesian models
│   ├── ensemble_forecast.R  # Multi-model ensembles
│   ├── outbreak_detection.R # CUSUM/EARS algorithms
│   ├── scenario_modeling.R  # What-if analysis
│   ├── alert_system.R       # Notification generation
│   ├── report_generation.R  # PDF/Excel exports
│   └── logging.R            # Production logging
├── data/
│   ├── raw/                 # JSON surveillance data
│   └── respiwatch.sqlite    # SQLite database
├── www/
│   └── mobile-responsive.css
├── Dockerfile
├── docker-compose.yml
└── README.md
```

## Data Sources

- **CDC FluView**: Weekly influenza surveillance data
- **NREVSS**: RSV laboratory surveillance
- **COVID Data Tracker**: SARS-CoV-2 surveillance
- **WHO FluMart**: International influenza data

## License

MIT License

## Acknowledgments

Built with:
- [R Shiny](https://shiny.rstudio.com/)
- [bslib](https://rstudio.github.io/bslib/)
- [EpiEstim](https://github.com/mrc-ide/EpiEstim)
- [Plotly](https://plotly.com/r/)
- [Leaflet](https://rstudio.github.io/leaflet/)
