
library(shiny)


# Helper to mock the app structure for testing
# We need to source app.R but prevent it from running shinyApp()
# We can do this by checking interactive() or just reading the file and extracting server

# Read app.R content
app_lines <- readLines("app.R")
# Remove the last line that runs the app
app_lines <- head(app_lines, -1)
# Write to temp file
temp_app <- tempfile(fileext = ".R")
writeLines(app_lines, temp_app)

# Source the temp app file to get 'server' and 'ui' and 'outbreak_data'
source(temp_app, local = TRUE)

# Test the server function
testServer(server, {
  session$setInputs(selected_pathogen = "h3n2")
  session$setInputs(selected_country = "USA")
  
  print("--- Testing Pathogen KPIs ---")
  tryCatch({
    res <- output$pathogen_kpis
    print(substr(res, 1, 100)) # Print start of HTML
    print("Pathogen KPIs: Success")
  }, error = function(e) {
    print(paste("Pathogen KPIs Error:", e$message))
  })
  
  print("--- Testing Wave Analysis ---")
  tryCatch({
     # Create dummy surveillance data
     dummy_surveillance <- data.frame(
       date = seq(as.Date("2023-01-01"), as.Date("2023-01-30"), by="day"),
       observation_date = seq(as.Date("2023-01-01"), as.Date("2023-01-30"), by="day"),
       pathogen = "Influenza A",
       country_code = "USA",
       case_count = 100,
       positivity_rate = 10,
       stringsAsFactors = FALSE
     )
     
     # Test get_wave_analysis
     # We expect it to handle the 'pathogen' column correctly now
     result <- get_wave_analysis(dummy_surveillance, "Influenza A", NULL)
     
     if (result$pathogen == "Influenza A") {
        print("Wave Analysis: Success (Function call worked)")
     } else {
        print("Wave Analysis: Failed (Pathogen mismatch)")
     }
  }, error = function(e) {
    print(paste("Wave Analysis Error:", e$message))
  })
  
  print("--- Testing Continental Status ---")
  tryCatch({
    res <- output$continental_status
    print(substr(res, 1, 100))
    print("Continental Status: Success")
  }, error = function(e) {
    print(paste("Continental Status Error:", e$message))
  })
  
  print("--- Testing Country KPIs ---")
  tryCatch({
    res <- output$country_kpis
    print(substr(res, 1, 100))
    print("Country KPIs: Success")
  }, error = function(e) {
    print(paste("Country KPIs Error:", e$message))
  })
  
  print("--- Testing Country Anomalies ---")
  tryCatch({
    res <- output$country_anomalies
    print(substr(res, 1, 100))
    print("Country Anomalies: Success")
  }, error = function(e) {
    print(paste("Country Anomalies Error:", e$message))
  })
  
  print("--- Testing Country Healthcare ---")
  tryCatch({
    res <- output$country_healthcare
    print(substr(res, 1, 100))
    print("Country Healthcare: Success")
  }, error = function(e) {
    print(paste("Country Healthcare Error:", e$message))
  })
})
