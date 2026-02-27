test_that("calc_mean returns correct mean for basic input", {
  expect_equal(calc_mean(c(1, 2, 3, 4, 5)), 3)
})

test_that("calc_mean works with the assessment example data", {
  data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
  expect_equal(calc_mean(data), 4.3)
})

test_that("calc_mean handles NA values", {
  expect_equal(calc_mean(c(1, NA, 3)), 2)
  expect_equal(calc_mean(c(NA, 10, NA, 20, NA)), 15)
})

test_that("calc_mean works with single value", {
  expect_equal(calc_mean(7), 7)
})

test_that("calc_mean works with negative and decimal values", {
  expect_equal(calc_mean(c(-2, 0, 2)), 0)
  expect_equal(calc_mean(c(1.5, 2.5)), 2)
})

test_that("calc_mean errors on non-numeric input", {
  expect_error(calc_mean("a"), "must be a numeric vector")
  expect_error(calc_mean(c(TRUE, FALSE)), "must be a numeric vector")
})

test_that("calc_mean errors on empty vector", {
  expect_error(calc_mean(numeric(0)), "no non-NA values")
})

test_that("calc_mean errors on all-NA vector", {
  expect_error(calc_mean(c(NA_real_, NA_real_)), "no non-NA values")
})

test_that("calc_mean propagates Inf correctly", {
  expect_equal(calc_mean(c(1, Inf)), Inf)
  expect_true(is.nan(calc_mean(c(Inf, -Inf))))
})

test_that("calc_mean works with integer input", {
  expect_equal(calc_mean(1L:5L), 3)
})

test_that("calc_mean errors on NULL input", {
  expect_error(calc_mean(NULL), "must be a numeric vector")
})
