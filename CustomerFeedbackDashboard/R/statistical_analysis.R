# ============================================================
# statistical_analysis.R
# All statistical computations used by the dashboard:
# descriptive stats, correlation, linear regression, ANOVA,
# confidence intervals, and normality checking.
# ============================================================

#' Descriptive statistics (mean, median, sd, variance) for a numeric vector
descriptive_stats <- function(x) {
  x <- x[!is.na(x)]
  list(
    n = length(x),
    mean = round(mean(x), 3),
    median = round(median(x), 3),
    sd = round(sd(x), 3),
    variance = round(var(x), 3),
    min = min(x),
    max = max(x)
  )
}

#' Descriptive stats table across all rating columns
descriptive_stats_table <- function(df) {
  rating_cols <- c(
    "ProductQualityRating", "ServiceQualityRating",
    "DeliveryRating", "PriceRating", "OverallRating"
  )
  purrr::map_dfr(rating_cols, function(col) {
    s <- descriptive_stats(df[[col]])
    tibble::tibble(
      Metric = col,
      N = s$n,
      Mean = s$mean,
      Median = s$median,
      SD = s$sd,
      Variance = s$variance,
      Min = s$min,
      Max = s$max
    )
  })
}

#' Correlation matrix across the rating variables
#' @return a matrix suitable for plotting or display
correlation_matrix <- function(df) {
  rating_cols <- c(
    "ProductQualityRating", "ServiceQualityRating",
    "DeliveryRating", "PriceRating", "OverallRating"
  )
  cor(df[, rating_cols], use = "pairwise.complete.obs", method = "pearson")
}

#' Simple linear regression: OverallRating ~ ServiceQualityRating
#' @return the fitted lm object
fit_regression <- function(df) {
  stats::lm(OverallRating ~ ServiceQualityRating, data = df)
}

#' Tidy summary of the regression model for on-dashboard display
regression_summary <- function(model) {
  s <- summary(model)
  coefs <- s$coefficients
  list(
    intercept = round(coefs[1, "Estimate"], 3),
    slope = round(coefs[2, "Estimate"], 3),
    p_value_slope = signif(coefs[2, "Pr(>|t|)"], 3),
    r_squared = round(s$r.squared, 3),
    adj_r_squared = round(s$adj.r.squared, 3)
  )
}

#' One-way ANOVA comparing OverallRating across Product
#' @return the aov object
fit_anova <- function(df) {
  stats::aov(OverallRating ~ Product, data = df)
}

#' Tidy summary of an ANOVA model
anova_summary <- function(model) {
  s <- summary(model)[[1]]
  list(
    df_between = s$Df[1],
    df_within = s$Df[2],
    f_value = round(s$`F value`[1], 3),
    p_value = signif(s$`Pr(>F)`[1], 4)
  )
}

#' 95% confidence interval for the mean of a numeric vector
confidence_interval <- function(x, conf_level = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) {
    return(list(mean = ifelse(n == 1, x, NA), lower = NA, upper = NA))
  }
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  alpha <- 1 - conf_level
  t_crit <- stats::qt(1 - alpha / 2, df = n - 1)
  list(
    mean = round(m, 3),
    lower = round(m - t_crit * se, 3),
    upper = round(m + t_crit * se, 3),
    conf_level = conf_level
  )
}

#' Shapiro-Wilk normality test on OverallRating (sampled if data is large,
#' since shapiro.test only supports up to 5000 observations)
normality_test <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) > 5000) x <- sample(x, 5000)
  if (length(x) < 3) {
    return(list(statistic = NA, p_value = NA))
  }
  test <- stats::shapiro.test(x)
  list(statistic = round(unname(test$statistic), 4), p_value = signif(test$p.value, 4))
}
