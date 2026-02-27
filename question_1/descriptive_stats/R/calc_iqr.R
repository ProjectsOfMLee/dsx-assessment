#' Calculate Interquartile Range (IQR)
#'
#' Computes the interquartile range (Q3 - Q1) of a numeric vector after
#' removing NA values. Uses R's default quantile algorithm (type 7).
#'
#' @param x A numeric vector.
#'
#' @return A single numeric value representing the IQR.
#'
#' @details
#' - NA values are silently removed before computation.
#' - Calculated as \code{calc_q3(x) - calc_q1(x)}.
#' - An error is raised if \code{x} is not numeric, is empty, or contains
#'   only NA values.
#'
#' @examples
#' calc_iqr(c(1, 2, 3, 4, 5, 6, 7, 8))
#' # [1] 3.5
#'
#' calc_iqr(c(5, 5, 5, 5))
#' # [1] 0
#'
#' @family descriptive statistics
#' @export
calc_iqr <- function(x) {
  x_clean <- validate_numeric_input(x, "calc_iqr")
  q3 <- unname(quantile(x_clean, probs = 0.75))
  q1 <- unname(quantile(x_clean, probs = 0.25))
  q3 - q1
}
