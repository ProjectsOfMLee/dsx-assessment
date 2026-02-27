#' Validate numeric input vector
#'
#' Internal helper that checks input is a non-empty numeric vector.
#' NA values are removed and the cleaned vector is returned.
#' An all-NA vector is treated the same as an empty one.
#'
#' @param x Input to validate.
#' @param fn_name Name of the calling function, used in error messages.
#'
#' @return The input vector with NA values removed.
#'
#' @noRd
validate_numeric_input <- function(x, fn_name) {
  if (!is.numeric(x)) {
    stop(
      sprintf("%s: input must be a numeric vector, got %s.", fn_name, class(x)[1]),
      call. = FALSE
    )
  }

  x_clean <- x[!is.na(x)]

  if (length(x_clean) == 0L) {
    stop(
      sprintf("%s: input has no non-NA values.", fn_name),
      call. = FALSE
    )
  }

  x_clean
}
