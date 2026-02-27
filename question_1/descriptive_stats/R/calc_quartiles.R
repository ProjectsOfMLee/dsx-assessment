#' Calculate First Quartile (Q1)
#'
#' Computes the 25th percentile of a numeric vector after removing NA values.
#' Uses R's default quantile algorithm (type 7).
#'
#' @param x A numeric vector.
#'
#' @return A single numeric value representing Q1.
#'
#' @details
#' - NA values are silently removed before computation.
#' - Uses \code{\link[stats]{quantile}} with \code{type = 7} (R default).
#' - An error is raised if \code{x} is not numeric, is empty, or contains
#'   only NA values.
#'
#' @examples
#' calc_q1(c(1, 2, 3, 4, 5, 6, 7, 8))
#' # [1] 2.75
#'
#' calc_q1(c(1, NA, 3, 5, 7))
#' # [1] 2
#'
#' @family descriptive statistics
#' @export
calc_q1 <- function(x) {
  x_clean <- validate_numeric_input(x, "calc_q1")
  unname(quantile(x_clean, probs = 0.25))
}

#' Calculate Third Quartile (Q3)
#'
#' Computes the 75th percentile of a numeric vector after removing NA values.
#' Uses R's default quantile algorithm (type 7).
#'
#' @param x A numeric vector.
#'
#' @return A single numeric value representing Q3.
#'
#' @details
#' - NA values are silently removed before computation.
#' - Uses \code{\link[stats]{quantile}} with \code{type = 7} (R default).
#' - An error is raised if \code{x} is not numeric, is empty, or contains
#'   only NA values.
#'
#' @examples
#' calc_q3(c(1, 2, 3, 4, 5, 6, 7, 8))
#' # [1] 6.25
#'
#' calc_q3(c(1, NA, 3, 5, 7))
#' # [1] 6
#'
#' @family descriptive statistics
#' @export
calc_q3 <- function(x) {
  x_clean <- validate_numeric_input(x, "calc_q3")
  unname(quantile(x_clean, probs = 0.75))
}
