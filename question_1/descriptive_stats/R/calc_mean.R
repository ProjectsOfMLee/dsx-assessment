#' Calculate Arithmetic Mean
#'
#' Computes the arithmetic mean of a numeric vector after removing NA values.
#'
#' @param x A numeric vector.
#'
#' @return A single numeric value representing the arithmetic mean.
#'
#' @details
#' - NA values are silently removed before computation.
#' - An error is raised if \code{x} is not numeric, is empty, or contains
#'   only NA values.
#'
#' @examples
#' calc_mean(c(1, 2, 3, 4, 5))
#' # [1] 3
#'
#' calc_mean(c(1, NA, 3))
#' # [1] 2
#'
#' calc_mean(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10))
#' # [1] 4.3
#'
#' @family descriptive statistics
#' @export
calc_mean <- function(x) {
  x_clean <- validate_numeric_input(x, "calc_mean")
  mean(x_clean)
}
