# ============================================================
# app.R
# Interactive Customer Feedback Intelligence and Satisfaction
# Analytics Dashboard (R Shiny)
#
# Run with:  shiny::runApp()   (from inside this project folder)
# ============================================================

# ---- 1. Packages -------------------------------------------------
required_packages <- c(
  "shiny", "shinydashboard", "shinycssloaders",
  "ggplot2", "dplyr", "tidyr", "readr", "plotly", "DT",
  "lubridate", "scales", "purrr", "tibble"
)
missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(DT)
library(lubridate)
library(scales)
library(purrr)
library(tibble)

# ---- 2. Load helper modules ---------------------------------------
source("R/data_processing.R")
source("R/statistical_analysis.R")
source("R/visualizations.R")

# ---- 3. Load data once at app start --------------------------------
feedback_data <- load_feedback_data("data/customer_feedback.csv")

product_choices <- levels(feedback_data$Product)
gender_choices <- levels(feedback_data$Gender)
min_date <- min(feedback_data$Date)
max_date <- max(feedback_data$Date)

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "Feedback Intelligence Dashboard",
    titleWidth = 320
  ),

  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("gauge-high")),
      menuItem("Customer Feedback", tabName = "feedback", icon = icon("comments")),
      menuItem("Satisfaction Analysis", tabName = "satisfaction", icon = icon("face-smile")),
      menuItem("Product Analysis", tabName = "product", icon = icon("box")),
      menuItem("Statistical Analysis", tabName = "stats", icon = icon("chart-line")),
      menuItem("Reports", tabName = "reports", icon = icon("file-lines"))
    ),
    hr(style = "border-color: rgba(255,255,255,0.15);"),
    div(
      style = "padding: 0 15px;",
      dateRangeInput(
        "date_range", "Date Range",
        start = min_date, end = max_date,
        min = min_date, max = max_date
      ),
      selectInput(
        "product_filter", "Product / Service",
        choices = product_choices,
        selected = product_choices,
        multiple = TRUE,
        selectize = TRUE
      ),
      sliderInput(
        "rating_filter", "Overall Rating",
        min = 1, max = 5, value = c(1, 5), step = 1
      ),
      selectInput(
        "gender_filter", "Gender",
        choices = gender_choices,
        selected = gender_choices,
        multiple = TRUE,
        selectize = TRUE
      ),
      actionButton("reset_filters", "Reset Filters",
        icon = icon("rotate-left"), width = "100%",
        style = "margin-top: 8px; margin-bottom: 15px;"
      )
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    tabItems(

      # ---------------- DASHBOARD (overview) ----------------
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("kpi_total", width = 3),
          valueBoxOutput("kpi_avg_rating", width = 3),
          valueBoxOutput("kpi_satisfaction", width = 3),
          valueBoxOutput("kpi_complaints", width = 3)
        ),
        fluidRow(
          box(
            title = "Customer Satisfaction Trend", status = "primary",
            solidHeader = TRUE, width = 8,
            withSpinner(plotlyOutput("plot_trend", height = 320))
          ),
          box(
            title = "Feedback Category Breakdown", status = "primary",
            solidHeader = TRUE, width = 4,
            withSpinner(plotlyOutput("plot_category_donut", height = 320))
          )
        ),
        fluidRow(
          box(
            title = "Rating Distribution", status = "primary",
            solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plot_rating_dist", height = 300))
          ),
          box(
            title = "Product Performance", status = "primary",
            solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plot_product_perf", height = 300))
          )
        )
      ),

      # ---------------- CUSTOMER FEEDBACK (raw data) ----------------
      tabItem(
        tabName = "feedback",
        fluidRow(
          box(
            title = "Filtered Customer Feedback Records", status = "primary",
            solidHeader = TRUE, width = 12,
            downloadButton("download_csv", "Download Filtered Data (CSV)",
              style = "margin-bottom: 12px;"
            ),
            withSpinner(DTOutput("feedback_table"))
          )
        )
      ),

      # ---------------- SATISFACTION ANALYSIS ----------------
      tabItem(
        tabName = "satisfaction",
        fluidRow(
          box(
            title = "Satisfaction Factor Analysis", status = "primary",
            solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plot_factors", height = 340))
          ),
          box(
            title = "Satisfaction vs Service Quality", status = "primary",
            solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plot_scatter", height = 340))
          )
        ),
        fluidRow(
          box(
            title = "Satisfaction Trend Over Time", status = "primary",
            solidHeader = TRUE, width = 12,
            withSpinner(plotlyOutput("plot_trend2", height = 320))
          )
        )
      ),

      # ---------------- PRODUCT ANALYSIS ----------------
      tabItem(
        tabName = "product",
        fluidRow(
          box(
            title = "Average Rating by Product", status = "primary",
            solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plot_product_perf2", height = 380))
          ),
          box(
            title = "Rating Distribution by Product (Box Plot)", status = "primary",
            solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plot_boxplot", height = 380))
          )
        )
      ),

      # ---------------- STATISTICAL ANALYSIS ----------------
      tabItem(
        tabName = "stats",
        fluidRow(
          box(
            title = "Descriptive Statistics", status = "primary",
            solidHeader = TRUE, width = 12,
            withSpinner(DTOutput("desc_stats_table"))
          )
        ),
        fluidRow(
          box(
            title = "Correlation Analysis", status = "primary",
            solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plot_corr", height = 360))
          ),
          box(
            title = "Histogram of Overall Ratings", status = "primary",
            solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plot_hist", height = 360))
          )
        ),
        fluidRow(
          box(
            title = "Linear Regression: Overall ~ Service Quality", status = "primary",
            solidHeader = TRUE, width = 6,
            verbatimTextOutput("regression_output")
          ),
          box(
            title = "ANOVA: Overall Rating by Product", status = "primary",
            solidHeader = TRUE, width = 6,
            verbatimTextOutput("anova_output")
          )
        ),
        fluidRow(
          box(
            title = "Confidence Interval (Mean Overall Rating)", status = "primary",
            solidHeader = TRUE, width = 6,
            verbatimTextOutput("ci_output")
          ),
          box(
            title = "Normality Check (Q-Q Plot + Shapiro-Wilk)", status = "primary",
            solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plot_qq", height = 260)),
            verbatimTextOutput("shapiro_output")
          )
        )
      ),

      # ---------------- REPORTS ----------------
      tabItem(
        tabName = "reports",
        fluidRow(
          box(
            title = "Summary Report", status = "primary",
            solidHeader = TRUE, width = 12,
            p("This report summarizes the currently filtered dataset. Use the sidebar filters to change the scope, then re-download."),
            downloadButton("download_report_csv", "Download Summary Data (CSV)"),
            br(), br(),
            withSpinner(DTOutput("report_summary_table"))
          )
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ---- Reset filters ----
  observeEvent(input$reset_filters, {
    updateDateRangeInput(session, "date_range", start = min_date, end = max_date)
    updateSelectInput(session, "product_filter", selected = product_choices)
    updateSliderInput(session, "rating_filter", value = c(1, 5))
    updateSelectInput(session, "gender_filter", selected = gender_choices)
  })

  # ---- Reactive filtered dataset ----
  filtered_data <- reactive({
    req(input$date_range, input$rating_filter)
    filter_feedback_data(
      feedback_data,
      date_range = input$date_range,
      products = if (is.null(input$product_filter)) "All" else input$product_filter,
      rating_range = input$rating_filter,
      genders = if (is.null(input$gender_filter)) "All" else input$gender_filter
    )
  })

  # Guard: never let a plot/table render on zero filtered rows
  safe_data <- reactive({
    d <- filtered_data()
    validate(need(nrow(d) > 0, "No records match the current filters. Try widening your filter selection."))
    d
  })

  # ---- KPI value boxes ----
  output$kpi_total <- renderValueBox({
    kpis <- compute_summary_kpis(filtered_data())
    valueBox(kpis$total_feedback, "Total Feedback", icon = icon("comments"), color = "blue")
  })

  output$kpi_avg_rating <- renderValueBox({
    kpis <- compute_summary_kpis(filtered_data())
    valueBox(ifelse(is.na(kpis$avg_rating), "-", kpis$avg_rating), "Average Rating",
      icon = icon("star"), color = "yellow"
    )
  })

  output$kpi_satisfaction <- renderValueBox({
    kpis <- compute_summary_kpis(filtered_data())
    valueBox(
      ifelse(is.na(kpis$satisfaction_pct), "-", paste0(kpis$satisfaction_pct, "%")),
      "Satisfaction Percentage", icon = icon("face-smile"), color = "green"
    )
  })

  output$kpi_complaints <- renderValueBox({
    kpis <- compute_summary_kpis(filtered_data())
    valueBox(kpis$complaints, "Total Complaints", icon = icon("triangle-exclamation"), color = "red")
  })

  # ---- Dashboard tab plots ----
  output$plot_trend <- renderPlotly(plot_satisfaction_trend(safe_data()))
  output$plot_category_donut <- renderPlotly(plot_feedback_category_donut(safe_data()))
  output$plot_rating_dist <- renderPlotly(plot_rating_distribution(safe_data()))
  output$plot_product_perf <- renderPlotly(plot_product_performance(safe_data()))

  # ---- Feedback table ----
  output$feedback_table <- renderDT({
    datatable(safe_data(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("filtered_feedback_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(filtered_data(), file)
  )

  # ---- Satisfaction analysis tab ----
  output$plot_factors <- renderPlotly(plot_satisfaction_factors(safe_data()))
  output$plot_scatter <- renderPlotly(plot_satisfaction_vs_service(safe_data()))
  output$plot_trend2 <- renderPlotly(plot_satisfaction_trend(safe_data()))

  # ---- Product analysis tab ----
  output$plot_product_perf2 <- renderPlotly(plot_product_performance(safe_data()))
  output$plot_boxplot <- renderPlotly(plot_rating_boxplot(safe_data()))

  # ---- Statistical analysis tab ----
  output$desc_stats_table <- renderDT({
    datatable(descriptive_stats_table(safe_data()), options = list(dom = "t"), rownames = FALSE)
  })

  output$plot_corr <- renderPlotly({
    cm <- correlation_matrix(safe_data())
    plot_correlation_heatmap(cm)
  })

  output$plot_hist <- renderPlotly(plot_rating_histogram(safe_data()))

  output$regression_output <- renderPrint({
    model <- fit_regression(safe_data())
    rs <- regression_summary(model)
    cat("Linear Regression: OverallRating ~ ServiceQualityRating\n")
    cat("--------------------------------------------------------\n")
    cat(sprintf("Intercept          : %.3f\n", rs$intercept))
    cat(sprintf("Slope (coefficient): %.3f\n", rs$slope))
    cat(sprintf("P-value (slope)    : %s\n", rs$p_value_slope))
    cat(sprintf("R-squared          : %.3f\n", rs$r_squared))
    cat(sprintf("Adjusted R-squared : %.3f\n", rs$adj_r_squared))
    cat("\nInterpretation: for every 1-point increase in Service Quality\n")
    cat(sprintf("Rating, Overall Rating changes by about %.3f points, on average.\n", rs$slope))
  })

  output$anova_output <- renderPrint({
    model <- fit_anova(safe_data())
    as_ <- anova_summary(model)
    cat("One-Way ANOVA: OverallRating ~ Product\n")
    cat("--------------------------------------------------------\n")
    cat(sprintf("df (between) : %d\n", as_$df_between))
    cat(sprintf("df (within)  : %d\n", as_$df_within))
    cat(sprintf("F value      : %.3f\n", as_$f_value))
    cat(sprintf("P-value      : %s\n", as_$p_value))
    cat("\n")
    if (!is.na(as_$p_value) && as_$p_value < 0.05) {
      cat("Result: statistically significant difference in average\n")
      cat("Overall Rating between at least two products (p < 0.05).\n")
    } else {
      cat("Result: no statistically significant difference detected\n")
      cat("in average Overall Rating between products (p >= 0.05).\n")
    }
  })

  output$ci_output <- renderPrint({
    ci <- confidence_interval(safe_data()$OverallRating)
    cat(sprintf("%.0f%% Confidence Interval for Mean Overall Rating\n", ci$conf_level * 100))
    cat("--------------------------------------------------------\n")
    cat(sprintf("Sample Mean : %.3f\n", ci$mean))
    cat(sprintf("Lower Bound : %.3f\n", ci$lower))
    cat(sprintf("Upper Bound : %.3f\n", ci$upper))
  })

  output$plot_qq <- renderPlotly(plot_qq(safe_data()))

  output$shapiro_output <- renderPrint({
    nt <- normality_test(safe_data()$OverallRating)
    cat("Shapiro-Wilk Normality Test (Overall Rating)\n")
    cat("--------------------------------------------------------\n")
    cat(sprintf("W statistic : %s\n", nt$statistic))
    cat(sprintf("P-value     : %s\n", nt$p_value))
    cat("\n")
    if (!is.na(nt$p_value) && nt$p_value < 0.05) {
      cat("Result: ratings deviate significantly from a normal\n")
      cat("distribution (p < 0.05) - expected for 1-5 ordinal data.\n")
    } else {
      cat("Result: no significant deviation from normality detected.\n")
    }
  })

  # ---- Reports tab ----
  output$report_summary_table <- renderDT({
    d <- safe_data()
    summary_df <- d %>%
      dplyr::group_by(Product) %>%
      dplyr::summarise(
        `Total Feedback` = dplyr::n(),
        `Avg Overall Rating` = round(mean(OverallRating), 2),
        `Satisfaction %` = round(100 * mean(SatisfactionLevel == "Satisfied"), 1),
        `Complaints` = sum(SatisfactionLevel == "Dissatisfied"),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(`Avg Overall Rating`))
    datatable(summary_df, options = list(dom = "t", pageLength = 20), rownames = FALSE)
  })

  output$download_report_csv <- downloadHandler(
    filename = function() paste0("summary_report_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- filtered_data()
      summary_df <- d %>%
        dplyr::group_by(Product) %>%
        dplyr::summarise(
          TotalFeedback = dplyr::n(),
          AvgOverallRating = round(mean(OverallRating), 2),
          SatisfactionPct = round(100 * mean(SatisfactionLevel == "Satisfied"), 1),
          Complaints = sum(SatisfactionLevel == "Dissatisfied"),
          .groups = "drop"
        )
      readr::write_csv(summary_df, file)
    }
  )
}

# ============================================================
# Run the app
# ============================================================
shinyApp(ui, server)
