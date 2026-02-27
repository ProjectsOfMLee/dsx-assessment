test_that("calc_q1 returns correct first quartile", {
  expect_equal(calc_q1(c(1, 2, 3, 4, 5, 6, 7, 8)), 2.75)
})

test_that("calc_q3 returns correct third quartile", {
  expect_equal(calc_q3(c(1, 2, 3, 4, 5, 6, 7, 8)), 6.25)
})

test_that("calc_q1 handles NA values", {
  expect_equal(calc_q1(c(1, NA, 2, 3, 4, 5)), calc_q1(c(1, 2, 3, 4, 5)))
})

test_that("calc_q3 handles NA values", {
  expect_equal(calc_q3(c(1, NA, 2, 3, 4, 5)), calc_q3(c(1, 2, 3, 4, 5)))
})

test_that("calc_q1 works with single value", {
  expect_equal(calc_q1(10), 10)
})

test_that("calc_q3 works with single value", {
  expect_equal(calc_q3(10), 10)
})

test_that("calc_q1 errors on non-numeric input", {
  expect_error(calc_q1("a"), "must be a numeric vector")
})

test_that("calc_q3 errors on non-numeric input", {
  expect_error(calc_q3("a"), "must be a numeric vector")
})

test_that("calc_q1 errors on empty vector", {
  expect_error(calc_q1(numeric(0)), "no non-NA values")
})

test_that("calc_q3 errors on empty vector", {
  expect_error(calc_q3(numeric(0)), "no non-NA values")
})

test_that("calc_q1 errors on all-NA vector", {
  expect_error(calc_q1(c(NA_real_, NA_real_)), "no non-NA values")
})

test_that("calc_q3 errors on all-NA vector", {
  expect_error(calc_q3(c(NA_real_, NA_real_)), "no non-NA values")
})

test_that("calc_q1 works with two-element vector", {
  expect_equal(calc_q1(c(1, 2)), 1.25)
})

test_that("calc_q3 works with two-element vector", {
  expect_equal(calc_q3(c(1, 2)), 1.75)
})

test_that("calc_q1 works with negative values", {
  expect_equal(calc_q1(c(-10, -5, 0, 5, 10)), -5)
})

test_that("calc_q3 works with negative values", {
  expect_equal(calc_q3(c(-10, -5, 0, 5, 10)), 5)
})
