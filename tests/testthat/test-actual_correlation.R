########################## REGULAR TESTS #######################

test_that("Regular test - rnorm matrix case", {
  # arrange
  data <- data.frame(Distance_pixels = c(0,1), Distance_km = c(0,1), Empirical_covariance = c(0.91106702, 0.49231819), Empirical_correlation = c(1,1))
  set.seed(1)
  Z <- actual_correlation(matrix(rnorm(4, 0, 1), nrow = 2), c(0,1))
  # act
  actual <- data
  # assert
  expected <- as.data.frame(Z)
  expect_equal(actual, expected)
})


test_that("Regular test - 2*2 matrix case", {
  # arrange
  data <- data.frame(Distance_pixels = c(0,1), Distance_km = c(0,1), Empirical_covariance = c(1.25, 0.25), Empirical_correlation = c(1,1))
  Z <- actual_correlation(matrix(1:4, nrow = 2), c(0,1))
  # act
  actual <- data
  # assert
  expected <- Z
  expect_equal(actual, expected)
})

test_that("Regular test - 2*2 dataframe case", {
  # arrange
  data <- data.frame(Distance_pixels = c(0,1), Distance_km = c(0,1), Empirical_covariance = c(1.25, 0.25), Empirical_correlation = c(1,1))
  Z <- actual_correlation(matrix(1:4, nrow = 2), c(0,1))
  # act
  actual <- data
  # assert
  expected <- as.data.frame(Z)
  expect_equal(actual, expected)
})


test_that("Regular test -  number case", {
  # arrange
  data <- data.frame(Distance_pixels = c(0), Distance_km = c(0), Empirical_covariance = c(0), Empirical_correlation = c(NaN))
  Z <- actual_correlation(c(1), c(1,0))
  # act
  actual <- data
  # assert
  expected <- as.data.frame(Z)
  expect_equal(actual, expected)
})


test_that("Regular test - scale is correctly changed", {
  # arrange
  data <- data.frame(Distance_pixels = c(0,1), Distance_km = c(0,2), Empirical_covariance = c(1.25, 0.25), Empirical_correlation = c(1,1))
  Z <- actual_correlation(matrix(1:4, nrow = 2), c(0,1), scale = 2)
  # act
  actual <- data
  # assert
  expected <- as.data.frame(Z)
  expect_equal(actual, expected)
})

test_that("Regular test - direction is correctly changed", {
  # arrange
  # act
  actual <- actual_correlation(matrix(1:4, nrow = 2), c(4,6))
  # assert
  expected <- actual_correlation(matrix(1:4, nrow = 2), c(2,3))
  expect_equal(actual, expected)
})



######################## ANOMALY TESTS #####################

# On the matrix M

test_that("Anomaly test - error when M is not a vector/matrix/dataframe", {
  # arrange
  M <- "matrix(1:4, nrow = 2)"
  # act & assert
  expect_error(actual_correlation(M, c(0,1)), "^M must contain a vector, matrix or dataframe$")
})

# On the direction a_vector

test_that("Anomaly test - error when a_vector is a string", {
  # arrange
  a_vector <- "matrix(1:4, nrow = 2)"
  # act & assert
  expect_error(actual_correlation(matrix(1:4, nrow = 2), a_vector), "^a_vector must have 2 coordinates$")
})


test_that("Anomaly test - error when a_vector is not a 2D-vector", {
  # arrange
  a_vector <- c(1,2,3)
  # act & assert
  expect_error(actual_correlation(matrix(1:4, nrow = 2), a_vector), "^a_vector must have 2 coordinates$")
})

test_that("Anomaly test - error when a_vector is not a 2D-vector", {
  # arrange
  a_vector <- as.data.frame(c(1,2,3))
  # act & assert
  expect_error(actual_correlation(matrix(1:4, nrow = 2), a_vector), "^a_vector must contain a vector$")
})

# On the result

test_that("Anomaly test - warning when a_vector is not a 2D-vector", {
  # arrange
  a_vector <- c(0,1)
  # act & assert
  expect_warning(actual_correlation(matrix(1:4, nrow = 1), a_vector), "^the result contains infinity NaN$")
})

