# ============================================================================
# Title: RespiWatch Report Generation Module
# Purpose: Generate PDF and Excel reports for download
# Input: Dashboard data, forecasts, alerts
# Output: Downloadable report files
# ============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# =============================================================================
# EXCEL REPORT GENERATION
# =============================================================================

#' Generate Excel workbook with surveillance data
#' @param surveillance_data Surveillance data frame
#' @param rt_results Rt estimation results
#' @param forecast_results Forecast results
#' @param alerts Current alerts
#' @return Path to generated Excel file
generate_excel_report <- function(surveillance_data, rt_results = NULL,
                                   forecast_results = NULL, alerts = NULL) {
  # Check for openxlsx package
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("openxlsx package required for Excel export. Install with: install.packages('openxlsx')")
    # Fall back to CSV export
    return(generate_csv_report(surveillance_data, rt_results, forecast_results))
  }

  library(openxlsx)

  # Create workbook
  wb <- createWorkbook()

  # Style definitions
  header_style <- createStyle(
    fontSize = 12,
    fontColour = "#FFFFFF",
    fgFill = "#1E3A5F",
    halign = "center",
    textDecoration = "bold"
  )

  date_style <- createStyle(numFmt = "YYYY-MM-DD")

  # Sheet 1: Executive Summary
  addWorksheet(wb, "Summary")

  summary_data <- data.frame(
    Metric = c(
      "Report Generated",
      "Data Period",
      "Pathogens Tracked",
      "Total Observations",
      "Active Alerts"
    ),
    Value = c(
      format(Sys.time(), "%Y-%m-%d %H:%M"),
      if (!is.null(surveillance_data) && nrow(surveillance_data) > 0) {
        sprintf("%s to %s",
                min(surveillance_data$observation_date, na.rm = TRUE),
                max(surveillance_data$observation_date, na.rm = TRUE))
      } else "N/A",
      if (!is.null(surveillance_data)) {
        paste(unique(surveillance_data$pathogen_code), collapse = ", ")
      } else "N/A",
      if (!is.null(surveillance_data)) nrow(surveillance_data) else 0,
      if (!is.null(alerts)) length(alerts) else 0
    ),
    stringsAsFactors = FALSE
  )

  writeData(wb, "Summary", summary_data, headerStyle = header_style)
  setColWidths(wb, "Summary", cols = 1:2, widths = c(25, 40))

  # Sheet 2: Surveillance Data
  if (!is.null(surveillance_data) && nrow(surveillance_data) > 0) {
    addWorksheet(wb, "Surveillance Data")
    writeData(wb, "Surveillance Data", surveillance_data, headerStyle = header_style)
    addStyle(wb, "Surveillance Data",
             style = date_style,
             rows = 2:(nrow(surveillance_data) + 1),
             cols = which(names(surveillance_data) == "observation_date"))
    setColWidths(wb, "Surveillance Data", cols = 1:ncol(surveillance_data), widths = "auto")
  }

  # Sheet 3: Rt Estimates
  if (!is.null(rt_results)) {
    addWorksheet(wb, "Rt Estimates")

    rt_data <- lapply(names(rt_results), function(p) {
      result <- rt_results[[p]]
      if (!is.null(result$rt_estimates)) {
        df <- result$rt_estimates
        df$pathogen <- p
        df
      } else {
        NULL
      }
    })
    rt_combined <- bind_rows(rt_data)

    if (nrow(rt_combined) > 0) {
      writeData(wb, "Rt Estimates", rt_combined, headerStyle = header_style)
      setColWidths(wb, "Rt Estimates", cols = 1:ncol(rt_combined), widths = "auto")
    }

    # Current Rt summary
    current_rt_summary <- data.frame(
      Pathogen = names(rt_results),
      Current_Rt = sapply(rt_results, function(r) {
        if (!is.null(r$current_rt$value) && !is.na(r$current_rt$value)) {
          sprintf("%.2f (%.2f-%.2f)", r$current_rt$value,
                  r$current_rt$lower, r$current_rt$upper)
        } else "N/A"
      }),
      Phase = sapply(rt_results, function(r) {
        if (!is.null(r$current_rt$phase)) r$current_rt$phase else "N/A"
      }),
      Trend = sapply(rt_results, function(r) {
        if (!is.null(r$current_rt$trend)) r$current_rt$trend else "N/A"
      }),
      stringsAsFactors = FALSE
    )

    # Add to Summary sheet
    writeData(wb, "Summary", current_rt_summary,
              startRow = nrow(summary_data) + 3,
              headerStyle = header_style)
  }

  # Sheet 4: Forecasts
  if (!is.null(forecast_results)) {
    addWorksheet(wb, "Forecasts")

    forecast_data <- lapply(names(forecast_results), function(p) {
      result <- forecast_results[[p]]
      if (!is.null(result$forecast)) {
        df <- result$forecast
        df$pathogen <- p
        df
      } else {
        NULL
      }
    })
    forecast_combined <- bind_rows(forecast_data)

    if (nrow(forecast_combined) > 0) {
      writeData(wb, "Forecasts", forecast_combined, headerStyle = header_style)
      setColWidths(wb, "Forecasts", cols = 1:ncol(forecast_combined), widths = "auto")
    }
  }

  # Sheet 5: Alerts
  if (!is.null(alerts) && length(alerts) > 0) {
    addWorksheet(wb, "Alerts")

    if (!exists("summarize_alert_table")) {
      source("R/alert_system.R")
    }
    alert_table <- summarize_alert_table(alerts)
    writeData(wb, "Alerts", alert_table, headerStyle = header_style)
    setColWidths(wb, "Alerts", cols = 1:ncol(alert_table), widths = "auto")
  }

  # Save workbook
  filename <- sprintf("respiwatch_report_%s.xlsx", format(Sys.Date(), "%Y%m%d"))
  filepath <- file.path(tempdir(), filename)
  saveWorkbook(wb, filepath, overwrite = TRUE)

  filepath
}

#' Generate CSV export (fallback when openxlsx not available)
#' @param surveillance_data Surveillance data
#' @param rt_results Rt results
#' @param forecast_results Forecast results
#' @return Path to generated CSV file
generate_csv_report <- function(surveillance_data, rt_results = NULL,
                                 forecast_results = NULL) {
  filename <- sprintf("respiwatch_data_%s.csv", format(Sys.Date(), "%Y%m%d"))
  filepath <- file.path(tempdir(), filename)

  if (!is.null(surveillance_data) && nrow(surveillance_data) > 0) {
    write.csv(surveillance_data, filepath, row.names = FALSE)
  }

  filepath
}

# =============================================================================
# PDF REPORT GENERATION
# =============================================================================

#' Generate PDF report using rmarkdown
#' @param surveillance_data Surveillance data
#' @param rt_results Rt estimation results
#' @param forecast_results Forecast results
#' @param alerts Current alerts
#' @return Path to generated PDF file
generate_pdf_report <- function(surveillance_data, rt_results = NULL,
                                 forecast_results = NULL, alerts = NULL) {
  # Check for required packages
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    warning("rmarkdown package required for PDF export")
    return(NULL)
  }

  # Create temporary Rmd file
  rmd_content <- create_report_rmd(surveillance_data, rt_results,
                                    forecast_results, alerts)

  rmd_path <- file.path(tempdir(), "respiwatch_report.Rmd")
  writeLines(rmd_content, rmd_path)

  # Render to PDF (or HTML as fallback)
  output_format <- if (rmarkdown::pandoc_available() &&
                       rmarkdown::pandoc_version() >= "2.0") {
    "pdf_document"
  } else {
    "html_document"
  }

  output_file <- tryCatch({
    rmarkdown::render(
      rmd_path,
      output_format = output_format,
      output_file = sprintf("respiwatch_report_%s.%s",
                            format(Sys.Date(), "%Y%m%d"),
                            if (output_format == "pdf_document") "pdf" else "html"),
      output_dir = tempdir(),
      quiet = TRUE
    )
  }, error = function(e) {
    warning(paste("PDF generation failed:", e$message))
    NULL
  })

  output_file
}

#' Create Rmd content for report
#' @param surveillance_data Surveillance data
#' @param rt_results Rt results
#' @param forecast_results Forecast results
#' @param alerts Alerts
#' @return Character string with Rmd content
create_report_rmd <- function(surveillance_data, rt_results = NULL,
                               forecast_results = NULL, alerts = NULL) {
  rmd <- sprintf('---
title: "RespiWatch Surveillance Report"
date: "%s"
output:
  pdf_document:
    toc: true
    number_sections: true
  html_document:
    toc: true
    theme: flatly
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(dplyr)
library(knitr)
```

# Executive Summary

This report provides an overview of respiratory pathogen surveillance data
as of %s.

**Report Highlights:**

- Pathogens tracked: %s
- Active alerts: %d
- Data period: %s

# Current Situation

## Reproduction Number (Rt) Summary

%s

## Active Alerts

%s

# Surveillance Data

%s

# Forecasts

%s

---

*Report generated automatically by RespiWatch*
',
    format(Sys.Date(), "%B %d, %Y"),
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    if (!is.null(surveillance_data)) {
      paste(unique(surveillance_data$pathogen_code), collapse = ", ")
    } else "N/A",
    if (!is.null(alerts)) length(alerts) else 0,
    if (!is.null(surveillance_data) && nrow(surveillance_data) > 0) {
      sprintf("%s to %s",
              min(surveillance_data$observation_date, na.rm = TRUE),
              max(surveillance_data$observation_date, na.rm = TRUE))
    } else "N/A",
    create_rt_section(rt_results),
    create_alerts_section(alerts),
    create_surveillance_section(surveillance_data),
    create_forecast_section(forecast_results)
  )

  rmd
}

#' Create Rt section for report
#' @param rt_results Rt results
#' @return Character string with Rmd content
create_rt_section <- function(rt_results) {
  if (is.null(rt_results)) {
    return("*Rt estimates not available*")
  }

  sections <- sapply(names(rt_results), function(p) {
    result <- rt_results[[p]]
    if (!is.null(result$current_rt$value) && !is.na(result$current_rt$value)) {
      sprintf("- **%s**: Rt = %.2f (95%% CI: %.2f-%.2f), Phase: %s, Trend: %s",
              p, result$current_rt$value,
              result$current_rt$lower, result$current_rt$upper,
              result$current_rt$phase, result$current_rt$trend)
    } else {
      sprintf("- **%s**: Rt estimates not available", p)
    }
  })

  paste(sections, collapse = "\n")
}

#' Create alerts section for report
#' @param alerts List of alerts
#' @return Character string with Rmd content
create_alerts_section <- function(alerts) {
  if (is.null(alerts) || length(alerts) == 0) {
    return("**No active alerts** - All monitored pathogens within normal parameters.")
  }

  alert_items <- sapply(alerts, function(a) {
    sprintf("- **[%s] %s**: %s",
            toupper(a$severity), a$title, a$message)
  })

  paste(alert_items, collapse = "\n")
}

#' Create surveillance section for report
#' @param surveillance_data Surveillance data
#' @return Character string with Rmd content
create_surveillance_section <- function(surveillance_data) {
  if (is.null(surveillance_data) || nrow(surveillance_data) == 0) {
    return("*Surveillance data not available*")
  }

  # Summary by pathogen
  summary_df <- surveillance_data |>
    group_by(pathogen_code) |>
    summarize(
      `Latest Date` = max(observation_date, na.rm = TRUE),
      `Total Cases` = sum(case_count, na.rm = TRUE),
      `Avg Positivity` = mean(positivity_rate, na.rm = TRUE),
      .groups = "drop"
    )

  sprintf("```{r}\nkable(data.frame(\n  Pathogen = c('%s'),\n  `Latest Date` = c('%s'),\n  `Total Cases` = c(%s),\n  `Avg Positivity` = c('%s')\n))\n```",
          paste(summary_df$pathogen_code, collapse = "', '"),
          paste(summary_df$`Latest Date`, collapse = "', '"),
          paste(summary_df$`Total Cases`, collapse = ", "),
          paste(sprintf("%.1f%%", summary_df$`Avg Positivity`), collapse = "', '")
  )
}

#' Create forecast section for report
#' @param forecast_results Forecast results
#' @return Character string with Rmd content
create_forecast_section <- function(forecast_results) {
  if (is.null(forecast_results)) {
    return("*Forecasts not available*")
  }

  sections <- sapply(names(forecast_results), function(p) {
    result <- forecast_results[[p]]
    if (!is.null(result$forecast) && nrow(result$forecast) > 0) {
      week1 <- result$forecast[1, ]
      week4 <- if (nrow(result$forecast) >= 4) result$forecast[4, ] else NULL

      sprintf("### %s\n\n- 1-week forecast: %d cases (80%% CI: %d-%d)\n%s",
              p, week1$predicted_cases, week1$lower_80, week1$upper_80,
              if (!is.null(week4)) {
                sprintf("- 4-week forecast: %d cases (80%% CI: %d-%d)",
                        week4$predicted_cases, week4$lower_80, week4$upper_80)
              } else "")
    } else {
      sprintf("### %s\n\n*Forecast not available*", p)
    }
  })

  paste(sections, collapse = "\n\n")
}

# =============================================================================
# MAIN API FUNCTIONS
# =============================================================================

#' Generate all report formats
#' @param surveillance_data Surveillance data
#' @param rt_results Rt estimation results
#' @param forecast_results Forecast results
#' @param alerts Current alerts
#' @param formats Vector of formats ("excel", "pdf", "csv")
#' @return Named list of file paths
generate_reports <- function(surveillance_data, rt_results = NULL,
                              forecast_results = NULL, alerts = NULL,
                              formats = c("excel", "csv")) {
  results <- list()

  if ("excel" %in% formats) {
    results$excel <- tryCatch({
      generate_excel_report(surveillance_data, rt_results,
                            forecast_results, alerts)
    }, error = function(e) {
      warning(paste("Excel report failed:", e$message))
      NULL
    })
  }

  if ("csv" %in% formats) {
    results$csv <- tryCatch({
      generate_csv_report(surveillance_data, rt_results, forecast_results)
    }, error = function(e) {
      warning(paste("CSV report failed:", e$message))
      NULL
    })
  }

  if ("pdf" %in% formats) {
    results$pdf <- tryCatch({
      generate_pdf_report(surveillance_data, rt_results,
                          forecast_results, alerts)
    }, error = function(e) {
      warning(paste("PDF report failed:", e$message))
      NULL
    })
  }

  results
}

#' Get download handler for Shiny
#' @param format Report format ("excel", "pdf", "csv")
#' @param surveillance_data Reactive surveillance data
#' @param rt_results Reactive Rt results
#' @param forecast_results Reactive forecast results
#' @param alerts Reactive alerts
#' @return Function suitable for downloadHandler
create_download_handler <- function(format, surveillance_data, rt_results = NULL,
                                     forecast_results = NULL, alerts = NULL) {
  function(file) {
    report_path <- switch(
      format,
      "excel" = generate_excel_report(surveillance_data, rt_results,
                                       forecast_results, alerts),
      "csv" = generate_csv_report(surveillance_data, rt_results, forecast_results),
      "pdf" = generate_pdf_report(surveillance_data, rt_results,
                                   forecast_results, alerts),
      generate_csv_report(surveillance_data, rt_results, forecast_results)
    )

    if (!is.null(report_path) && file.exists(report_path)) {
      file.copy(report_path, file)
    }
  }
}
