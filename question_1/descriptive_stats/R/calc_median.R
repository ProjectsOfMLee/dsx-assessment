#' Calculate Median
#'
#' Computes the median of a numeric vector after removing NA values.
#'
#' @param x A numeric vector.
#'
#' @return A single numeric value representing the median.
#'
#' @details
#' - NA values are silently removed before computation.
#' - For even-length vectors the median is the average of the two middle values.
#' - An error is raised if \code{x} is not numeric, is empty, or contains
#'   only NA values.
#'
#' @examples
#' calc_median(c(1, 2, 3, 4, 5))
#' # [1] 3
#'
#' calc_median(c(1, 2, 3, 4))
#' # [1] 2.5
#'
#' calc_median(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10))
#' # [1] 4.5
#'
#' @family descriptive statistics
#' @export
calc_median <- function(x) {
  x_clean <- validate_numeric_input(x, "calc_median")
  median(x_clean)
}
