# ============================================================
# visualizations.R
# All chart-building functions. Each returns a ggplot object
# (wrapped with plotly::ggplotly where interactivity is desired).
# ============================================================

THEME_DASHBOARD <- ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14, color = "#1f2937"),
    axis.title = ggplot2::element_text(color = "#4b5563"),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom"
  )

PALETTE <- c("#4f46e5", "#06b6d4", "#f59e0b", "#ef4444", "#10b981", "#8b5cf6", "#ec4899", "#14b8a6")

#' A. Customer satisfaction / average rating trend over time
plot_satisfaction_trend <- function(df, freq = "week") {
  trend <- df %>%
    dplyr::mutate(Period = lubridate::floor_date(Date, unit = freq)) %>%
    dplyr::group_by(Period) %>%
    dplyr::summarise(
      AvgRating = mean(OverallRating, na.rm = TRUE),
      SatisfactionPct = 100 * mean(SatisfactionLevel == "Satisfied", na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(trend, ggplot2::aes(x = Period, y = AvgRating)) +
    ggplot2::geom_line(color = "#4f46e5", linewidth = 1) +
    ggplot2::geom_point(color = "#4f46e5", size = 1.5) +
    ggplot2::labs(title = "Customer Satisfaction Trend", x = "Date", y = "Average Overall Rating") +
    ggplot2::ylim(1, 5) +
    THEME_DASHBOARD

  plotly::ggplotly(p, tooltip = c("x", "y"))
}

#' B. Rating distribution (count of customers per rating 1-5)
plot_rating_distribution <- function(df) {
  dist <- df %>%
    dplyr::count(OverallRating) %>%
    dplyr::mutate(OverallRating = factor(OverallRating, levels = 1:5))

  p <- ggplot2::ggplot(dist, ggplot2::aes(x = OverallRating, y = n, fill = OverallRating)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = PALETTE) +
    ggplot2::labs(title = "Rating Distribution", x = "Overall Rating", y = "Number of Customers") +
    THEME_DASHBOARD

  plotly::ggplotly(p, tooltip = c("x", "y"))
}

#' C. Product performance: average rating per product
plot_product_performance <- function(df) {
  perf <- df %>%
    dplyr::group_by(Product) %>%
    dplyr::summarise(AvgRating = mean(OverallRating, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(AvgRating))

  p <- ggplot2::ggplot(perf, ggplot2::aes(
    x = stats::reorder(Product, AvgRating), y = AvgRating, fill = Product
  )) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = rep(PALETTE, length.out = nrow(perf))) +
    ggplot2::labs(title = "Product / Service Performance", x = NULL, y = "Average Overall Rating") +
    THEME_DASHBOARD

  plotly::ggplotly(p, tooltip = c("x", "y"))
}

#' D. Feedback category breakdown as a donut chart
#'
#' Built directly with plotly (rather than ggplot2 + coord_polar, which
#' ggplotly() cannot translate) so it renders as a proper interactive donut.
plot_feedback_category_donut <- function(df) {
  cat_counts <- df %>%
    dplyr::count(FeedbackCategory) %>%
    dplyr::arrange(dplyr::desc(n))

  plotly::plot_ly(
    cat_counts,
    labels = ~FeedbackCategory,
    values = ~n,
    type = "pie",
    hole = 0.55,
    marker = list(colors = rep(PALETTE, length.out = nrow(cat_counts)),
                  line = list(color = "white", width = 2)),
    textinfo = "label+percent",
    hoverinfo = "label+value+percent"
  ) %>%
    plotly::layout(
      title = list(text = "Feedback Category Breakdown", font = list(size = 14, color = "#1f2937")),
      showlegend = TRUE,
      legend = list(orientation = "v")
    )
}

#' E. Satisfaction factor comparison: average of each rating dimension
plot_satisfaction_factors <- function(df) {
  factors <- df %>%
    dplyr::summarise(
      `Product Quality` = mean(ProductQualityRating, na.rm = TRUE),
      `Service Quality` = mean(ServiceQualityRating, na.rm = TRUE),
      `Delivery` = mean(DeliveryRating, na.rm = TRUE),
      `Price` = mean(PriceRating, na.rm = TRUE)
    ) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "Factor", values_to = "AvgRating")

  p <- ggplot2::ggplot(factors, ggplot2::aes(x = Factor, y = AvgRating, fill = Factor)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = PALETTE) +
    ggplot2::ylim(0, 5) +
    ggplot2::labs(title = "Satisfaction Factor Analysis", x = NULL, y = "Average Rating") +
    THEME_DASHBOARD

  plotly::ggplotly(p, tooltip = c("x", "y"))
}

#' F. Scatter: Service Quality vs Overall Rating with regression line
plot_satisfaction_vs_service <- function(df) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = ServiceQualityRating, y = OverallRating)) +
    ggplot2::geom_jitter(color = "#06b6d4", alpha = 0.5, width = 0.15, height = 0.15) +
    ggplot2::geom_smooth(method = "lm", color = "#ef4444", se = TRUE) +
    ggplot2::labs(
      title = "Satisfaction vs Service Quality",
      x = "Service Quality Rating", y = "Overall Rating"
    ) +
    THEME_DASHBOARD

  plotly::ggplotly(p, tooltip = c("x", "y"))
}

#' G1. Box plot of overall rating by product
plot_rating_boxplot <- function(df) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Product, y = OverallRating, fill = Product)) +
    ggplot2::geom_boxplot(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = rep(PALETTE, length.out = dplyr::n_distinct(df$Product))) +
    ggplot2::labs(title = "Overall Rating Distribution by Product", x = NULL, y = "Overall Rating") +
    THEME_DASHBOARD

  plotly::ggplotly(p, tooltip = c("x", "y"))
}

#' G2. Histogram of overall ratings
plot_rating_histogram <- function(df) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = OverallRating)) +
    ggplot2::geom_histogram(binwidth = 1, fill = "#4f46e5", color = "white", boundary = 0.5) +
    ggplot2::labs(title = "Histogram of Overall Ratings", x = "Overall Rating", y = "Frequency") +
    THEME_DASHBOARD

  plotly::ggplotly(p, tooltip = c("x", "y"))
}

#' H. Correlation heatmap across the rating variables
plot_correlation_heatmap <- function(cor_mat) {
  cor_df <- as.data.frame(as.table(cor_mat))
  names(cor_df) <- c("Var1", "Var2", "Correlation")

  p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)), size = 3.2, color = "#1f2937") +
    ggplot2::scale_fill_gradient2(
      low = "#ef4444", mid = "white", high = "#4f46e5", midpoint = 0, limits = c(-1, 1)
    ) +
    ggplot2::labs(title = "Correlation Analysis", x = NULL, y = NULL) +
    THEME_DASHBOARD +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))

  plotly::ggplotly(p, tooltip = c("x", "y", "fill"))
}

#' Q-Q plot for normality checking of Overall Rating
plot_qq <- function(df) {
  vals <- df$OverallRating
  qq_data <- as.data.frame(stats::qqnorm(vals, plot.it = FALSE))

  p <- ggplot2::ggplot(qq_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(color = "#06b6d4", alpha = 0.6) +
    ggplot2::geom_qq_line(ggplot2::aes(sample = vals), color = "#ef4444") +
    ggplot2::labs(
      title = "Q-Q Plot: Normality Check (Overall Rating)",
      x = "Theoretical Quantiles", y = "Sample Quantiles"
    ) +
    THEME_DASHBOARD

  plotly::ggplotly(p, tooltip = c("x", "y"))
}
