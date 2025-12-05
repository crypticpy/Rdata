# ============================================================================
# Shiny Application
# Description: Interactive data exploration dashboard
# Usage: R -e "shiny::runApp('app.R')"
# ============================================================================

library(shiny)
library(tidyverse)
library(plotly)
library(DT)

# Load data
iris_df <- read_csv("data/raw/iris.csv", show_col_types = FALSE)
mtcars_df <- read_csv("data/raw/mtcars.csv", show_col_types = FALSE)

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  titlePanel("Data Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      selectInput(
        "dataset",
        "Select Dataset:",
        choices = c("Iris" = "iris", "MTCars" = "mtcars")
      ),

      hr(),

      conditionalPanel(
        condition = "input.dataset == 'iris'",
        selectInput(
          "iris_x",
          "X Variable:",
          choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
          selected = "Sepal.Length"
        ),
        selectInput(
          "iris_y",
          "Y Variable:",
          choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
          selected = "Sepal.Width"
        ),
        checkboxGroupInput(
          "iris_species",
          "Species:",
          choices = unique(iris_df$Species),
          selected = unique(iris_df$Species)
        )
      ),

      conditionalPanel(
        condition = "input.dataset == 'mtcars'",
        selectInput(
          "mtcars_x",
          "X Variable:",
          choices = c("mpg", "cyl", "disp", "hp", "wt", "qsec"),
          selected = "hp"
        ),
        selectInput(
          "mtcars_y",
          "Y Variable:",
          choices = c("mpg", "cyl", "disp", "hp", "wt", "qsec"),
          selected = "mpg"
        ),
        checkboxGroupInput(
          "mtcars_cyl",
          "Cylinders:",
          choices = sort(unique(mtcars_df$cyl)),
          selected = sort(unique(mtcars_df$cyl))
        )
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Visualization",
          br(),
          plotlyOutput("scatter_plot", height = "500px")
        ),
        tabPanel(
          "Data Table",
          br(),
          DTOutput("data_table")
        ),
        tabPanel(
          "Summary",
          br(),
          verbatimTextOutput("summary_stats")
        )
      )
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  # Reactive: Get filtered data
  filtered_data <- reactive({
    if (input$dataset == "iris") {
      iris_df %>%
        filter(Species %in% input$iris_species)
    } else {
      mtcars_df %>%
        filter(cyl %in% input$mtcars_cyl)
    }
  })

  # Output: Scatter plot
  output$scatter_plot <- renderPlotly({
    df <- filtered_data()

    if (input$dataset == "iris") {
      p <- ggplot(df, aes(
        x = .data[[input$iris_x]],
        y = .data[[input$iris_y]],
        color = Species
      )) +
        geom_point(size = 3, alpha = 0.7) +
        theme_minimal() +
        scale_color_brewer(palette = "Set2") +
        labs(
          title = paste("Iris:", input$iris_x, "vs", input$iris_y),
          x = input$iris_x,
          y = input$iris_y
        )
    } else {
      p <- ggplot(df, aes(
        x = .data[[input$mtcars_x]],
        y = .data[[input$mtcars_y]],
        color = factor(cyl)
      )) +
        geom_point(size = 3, alpha = 0.7) +
        theme_minimal() +
        scale_color_brewer(palette = "Dark2") +
        labs(
          title = paste("MTCars:", input$mtcars_x, "vs", input$mtcars_y),
          x = input$mtcars_x,
          y = input$mtcars_y,
          color = "Cylinders"
        )
    }

    ggplotly(p)
  })

  # Output: Data table
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })

  # Output: Summary statistics
  output$summary_stats <- renderPrint({
    summary(filtered_data())
  })
}

# ============================================================================
# Run Application
# ============================================================================

shinyApp(ui = ui, server = server)
