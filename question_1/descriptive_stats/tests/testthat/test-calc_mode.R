test_that("calc_mode returns single mode", {
  expect_equal(calc_mode(c(1, 2, 2, 3)), 2)
})

test_that("calc_mode works with the assessment example data", {
  data <- c(1, 2, 2, 3, 4, 5, 5, 5, 6, 10)
  expect_equal(calc_mode(data), 5)
})

test_that("calc_mode returns multiple modes (tie)", {
  expect_equal(calc_mode(c(1, 1, 2, 2, 3)), c(1, 2))
})

test_that("calc_mode returns NA when no unique mode (all equal frequency)", {
  expect_message(result <- calc_mode(c(1, 2, 3)), "no unique mode")
  expect_true(is.na(result))
})

test_that("calc_mode returns the value for single-element input", {
  expect_equal(calc_mode(5), 5)
})

test_that("calc_mode handles NA values in input", {
  expect_equal(calc_mode(c(1, 2, 2, NA, 3, NA)), 2)
})

test_that("calc_mode errors on non-numeric input", {
  expect_error(calc_mode("a"), "must be a numeric vector")
})

test_that("calc_mode errors on empty vector", {
  expect_error(calc_mode(numeric(0)), "no non-NA values")
})

test_that("calc_mode errors on all-NA vector", {
  expect_error(calc_mode(c(NA_real_, NA_real_)), "no non-NA values")
})

test_that("calc_mode returns the value for all-identical inputs", {
  expect_equal(calc_mode(c(5, 5, 5)), 5)
})

test_that("calc_mode handles negative values as mode", {
  expect_equal(calc_mode(c(-1, -1, 2, 3)), -1)
})

test_that("calc_mode handles three-way tie (3+ modes)", {
  expect_equal(calc_mode(c(1, 1, 2, 2, 3, 3, 4)), c(1, 2, 3))
})
