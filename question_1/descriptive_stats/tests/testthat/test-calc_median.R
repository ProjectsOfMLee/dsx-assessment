test_that("calc_median returns correct median for odd-length vector", {
  expect_equal(calc_median(c(1, 2, 3, 4, 5)), 3)
})

test_that("calc_median returns correct median for even-length vector", {
  expect_equal(calc_median(c(1, 2, 3, 4)), 2.5)
})

test_that("calc_median works with the assessment example data", {
  data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
  expect_equal(calc_median(data), 4.5)
})

test_that("calc_median handles NA values", {
  expect_equal(calc_median(c(1, NA, 3, NA, 5)), 3)
})

test_that("calc_median works with single value", {
  expect_equal(calc_median(42), 42)
})

test_that("calc_median works with unsorted input", {
  expect_equal(calc_median(c(5, 1, 3)), 3)
})

test_that("calc_median errors on non-numeric input", {
  expect_error(calc_median("a"), "must be a numeric vector")
})

test_that("calc_median errors on empty vector", {
  expect_error(calc_median(numeric(0)), "no non-NA values")
})

test_that("calc_median errors on all-NA vector", {
  expect_error(calc_median(c(NA_real_, NA_real_)), "no non-NA values")
})

test_that("calc_median works with two-element vector", {
  expect_equal(calc_median(c(1, 2)), 1.5)
})

test_that("calc_median works with negative values", {
  expect_equal(calc_median(c(-5, -1, 0, 3, 7)), 0)
})

test_that("calc_median works with duplicate values", {
  expect_equal(calc_median(c(3, 3, 3, 3)), 3)
})
