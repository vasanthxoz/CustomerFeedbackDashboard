# ============================================================
# data_processing.R
# Loading, cleaning, and filtering logic for the Customer
# Feedback Intelligence Dashboard.
# ============================================================

#' Load the raw customer feedback CSV and clean/derive columns
#'
#' @param path path to the CSV file
#' @return a cleaned data.frame / tibble
load_feedback_data <- function(path = "data/customer_feedback.csv") {
  df <- readr::read_csv(path, show_col_types = FALSE)

  df <- df %>%
    dplyr::mutate(
      Date = as.Date(Date),
      Gender = factor(Gender),
      Product = factor(Product),
      FeedbackCategory = factor(FeedbackCategory),
      # Re-derive SatisfactionLevel from OverallRating so the rule is
      # transparent and always consistent with the raw rating, even if
      # the source CSV is edited or replaced.
      SatisfactionLevel = dplyr::case_when(
        OverallRating <= 2 ~ "Dissatisfied",
        OverallRating == 3 ~ "Neutral",
        OverallRating >= 4 ~ "Satisfied",
        TRUE ~ NA_character_
      ),
      SatisfactionLevel = factor(
        SatisfactionLevel,
        levels = c("Dissatisfied", "Neutral", "Satisfied")
      )
    ) %>%
    tidyr::drop_na(OverallRating, Date)

  df
}

#' Apply the dashboard's interactive filters to the feedback data
#'
#' @param df cleaned feedback data.frame
#' @param date_range length-2 vector/Date of min and max date
#' @param products character vector of selected products ("All" = no filter)
#' @param rating_range length-2 numeric vector, min/max Overall Rating
#' @param genders character vector of selected genders ("All" = no filter)
#' @return filtered data.frame
filter_feedback_data <- function(df,
                                  date_range = NULL,
                                  products = "All",
                                  rating_range = c(1, 5),
                                  genders = "All") {
  out <- df

  if (!is.null(date_range) && length(date_range) == 2) {
    out <- out %>%
      dplyr::filter(Date >= date_range[1], Date <= date_range[2])
  }

  if (!is.null(products) && !("All" %in% products)) {
    out <- out %>% dplyr::filter(Product %in% products)
  }

  if (!is.null(rating_range) && length(rating_range) == 2) {
    out <- out %>%
      dplyr::filter(
        OverallRating >= rating_range[1],
        OverallRating <= rating_range[2]
      )
  }

  if (!is.null(genders) && !("All" %in% genders)) {
    out <- out %>% dplyr::filter(Gender %in% genders)
  }

  out
}

#' Compute the four KPI summary card values
#'
#' @param df (filtered) feedback data.frame
#' @return a named list: total_feedback, avg_rating, satisfaction_pct, complaints
compute_summary_kpis <- function(df) {
  total_feedback <- nrow(df)

  if (total_feedback == 0) {
    return(list(
      total_feedback = 0,
      avg_rating = NA_real_,
      satisfaction_pct = NA_real_,
      complaints = 0
    ))
  }

  avg_rating <- round(mean(df$OverallRating, na.rm = TRUE), 2)
  satisfaction_pct <- round(
    100 * mean(df$SatisfactionLevel == "Satisfied", na.rm = TRUE), 1
  )
  complaints <- sum(df$SatisfactionLevel == "Dissatisfied", na.rm = TRUE)

  list(
    total_feedback = total_feedback,
    avg_rating = avg_rating,
    satisfaction_pct = satisfaction_pct,
    complaints = complaints
  )
}
