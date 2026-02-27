#' Calculate Mode
#'
#' Returns the most frequently occurring value(s) in a numeric vector.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector of mode value(s). Returns multiple values when
#'   there is a tie (multimodal). Returns \code{NA} with a message when
#'   multiple distinct values all occur with equal frequency (no unique mode).
#'
#' @details
#' - NA values are silently removed before computation.
#' - If all distinct values appear with equal frequency and there is more
#'   than one distinct value (e.g., \code{c(1, 2, 3)}), the function
#'   returns \code{NA} and emits a message.
#' - A single distinct value (e.g., \code{c(5, 5, 5)}) is the mode.
#' - If multiple values share the highest frequency, all are returned
#'   (multimodal case).
#' - An error is raised if \code{x} is not numeric, is empty, or contains
#'   only NA values.
#'
#' @examples
#' calc_mode(c(1, 2, 2, 3))
#' # [1] 2
#'
#' calc_mode(c(1, 1, 2, 2, 3))
#' # [1] 1 2
#'
#' calc_mode(c(1, 2, 3))
#' # [1] NA  (no unique mode)
#'
#' calc_mode(c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10))
#' # [1] 5
#'
#' @family descriptive statistics
#' @export
calc_mode <- function(x) {
  x_clean <- validate_numeric_input(x, "calc_mode")

  freq <- table(x_clean)
  max_freq <- max(freq)

  # When every distinct value appears the same number of times and there
  # is more than one distinct value, there is no unique mode.
  # A single distinct value (e.g., c(5,5,5)) is the mode by definition.
  if (length(freq) > 1L && all(freq == max_freq)) {
    message("calc_mode: no unique mode; all values occur with equal frequency.")
    return(NA_real_)
  }

  modes <- as.numeric(names(freq[freq == max_freq]))
  modes
}
