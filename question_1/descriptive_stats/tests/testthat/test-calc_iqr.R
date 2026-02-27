test_that("calc_iqr returns correct IQR", {
  expect_equal(calc_iqr(c(1, 2, 3, 4, 5, 6, 7, 8)), 3.5)
})

test_that("calc_iqr handles NA values", {
  expect_equal(calc_iqr(c(1, NA, 2, 3, 4, 5)), calc_iqr(c(1, 2, 3, 4, 5)))
})

test_that("calc_iqr returns 0 for single value", {
  expect_equal(calc_iqr(10), 0)
})

test_that("calc_iqr returns 0 for identical values", {
  expect_equal(calc_iqr(c(5, 5, 5, 5)), 0)
})

test_that("calc_iqr is consistent with calc_q3 - calc_q1", {
  data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
  expect_equal(calc_iqr(data), calc_q3(data) - calc_q1(data))
})

test_that("calc_iqr errors on non-numeric input", {
  expect_error(calc_iqr("a"), "must be a numeric vector")
})

test_that("calc_iqr errors on empty vector", {
  expect_error(calc_iqr(numeric(0)), "no non-NA values")
})

test_that("calc_iqr errors on all-NA vector", {
  expect_error(calc_iqr(c(NA_real_, NA_real_)), "no non-NA values")
})

test_that("calc_iqr works with two-element vector", {
  expect_equal(calc_iqr(c(1, 2)), 0.5)
})

test_that("calc_iqr works with negative values", {
  expect_equal(calc_iqr(c(-10, -5, 0, 5, 10)), 10)
})

test_that("calc_iqr handles large spread", {
  expect_equal(calc_iqr(c(0, 1000000)), 500000)
})
