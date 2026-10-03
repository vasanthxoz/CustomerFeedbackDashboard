# Customer Feedback Intelligence and Satisfaction Analytics Dashboard

An interactive R Shiny dashboard that analyzes customer feedback, identifies
satisfaction patterns, compares products/services, and surfaces the
statistical drivers of satisfaction (correlation, regression, ANOVA,
confidence intervals, normality checks) — all through a modern, filterable
analytics interface.

## Project structure

```
CustomerFeedbackDashboard/
├── app.R                        # Main Shiny app (UI + server)
├── data/
│   └── customer_feedback.csv    # Sample dataset (750 realistic records)
├── R/
│   ├── data_processing.R        # Load / clean / filter / KPI logic
│   ├── statistical_analysis.R   # Descriptive stats, correlation, regression,
│   │                            # ANOVA, confidence intervals, normality test
│   └── visualizations.R         # All ggplot2 + plotly chart builders
├── www/
│   └── custom.css               # Modern dashboard styling
└── README.md
```

## 1. Requirements

- R version 4.1 or later
- RStudio (recommended)

## 2. Install the required packages

Open R or RStudio and run:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinycssloaders",
  "ggplot2", "dplyr", "tidyr", "readr", "plotly", "DT",
  "lubridate", "scales", "purrr", "tibble"
))
```

`app.R` also auto-installs any of these packages that are missing the first
time it runs, so this step is optional but recommended (faster first launch).

## 3. Run the project in RStudio

1. Unzip the project and open the folder `CustomerFeedbackDashboard` in
   RStudio (**File → Open Project/Folder**, or just open `app.R`).
2. Make sure your R working directory is the project's root folder (the one
   containing `app.R`, `data/`, `R/`, and `www/`). RStudio does this
   automatically when you open `app.R` directly from that folder.
3. Click the **"Run App"** button that appears at the top of the source pane
   when `app.R` is open — or run in the console:

   ```r
   shiny::runApp()
   ```
4. The dashboard opens in a browser window / RStudio Viewer.

## 4. Using the dashboard

- **Sidebar filters**: Date range, Product/Service, Overall Rating range, and
  Gender. Every chart, KPI card, table, and statistical result updates live
  as you change filters. Click **Reset Filters** to return to the full
  dataset.
- **Dashboard**: KPI summary cards + satisfaction trend, category breakdown,
  rating distribution, and product performance.
- **Customer Feedback**: Sortable/searchable table of the filtered raw
  feedback records, with a CSV download button.
- **Satisfaction Analysis**: Satisfaction-factor comparison (Product Quality,
  Service Quality, Delivery, Price), the Service-Quality-vs-Overall-Rating
  scatter plot with a regression trend line, and the satisfaction trend.
- **Product Analysis**: Average rating per product and a box plot comparing
  rating spread across products.
- **Statistical Analysis**: Descriptive statistics table, correlation
  heatmap, histogram, linear regression output, one-way ANOVA output, a 95%
  confidence interval for the mean rating, and a Q-Q plot + Shapiro-Wilk test
  for normality.
- **Reports**: A per-product summary table with a CSV download.

## 5. Using your own data

Replace `data/customer_feedback.csv` with your own file, keeping these
columns (same names, same order is not required):

`CustomerID, Date, Age, Gender, Product, ProductQualityRating,
ServiceQualityRating, DeliveryRating, PriceRating, OverallRating, Feedback,
FeedbackCategory, SatisfactionLevel`

Ratings (`ProductQualityRating`, `ServiceQualityRating`, `DeliveryRating`,
`PriceRating`, `OverallRating`) should be integers from 1–5.
`SatisfactionLevel` is automatically re-derived from `OverallRating` on load
(1–2 → Dissatisfied, 3 → Neutral, 4–5 → Satisfied), so it's fine if your
source file doesn't include it.

## 6. Notes on the statistics used

| Analysis | Where | What it tells you |
|---|---|---|
| Mean / Median / SD / Variance | Statistical Analysis tab | Central tendency & spread of each rating dimension |
| Pearson correlation | Correlation heatmap | Strength/direction of relationships between rating dimensions |
| Linear regression | `OverallRating ~ ServiceQualityRating` | How much Overall Rating changes per unit change in Service Quality |
| One-way ANOVA | `OverallRating ~ Product` | Whether average satisfaction differs significantly across products |
| 95% Confidence Interval | Mean Overall Rating | Range likely to contain the true average satisfaction |
| Shapiro-Wilk + Q-Q plot | Overall Rating | Whether ratings are normally distributed (informs which further tests are appropriate) |

## 7. Novelty statement

This system goes beyond a simple rating-visualization tool: it combines
customer feedback analysis with statistical inference (correlation,
regression, ANOVA, confidence intervals, normality testing) and interactive
visual analytics, so a user can filter the data and *immediately* see how
both the graphs and the underlying statistical results change — helping
identify which factors are most associated with customer satisfaction and
how satisfaction compares across products/services.
