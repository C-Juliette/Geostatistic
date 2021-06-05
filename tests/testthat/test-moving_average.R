######################### REGULAR TESTS #######################


test_that("Regular test 1", {
  # arrange
  set.seed(1)
  Z <- matrix(rbinom(5*5, 1, 0.5), nrow = 5)
  # act
  actual <- moving_average(Z,2)
  # assert
  expected <- matrix(c(13/25))
  expect_identical(actual, expected)
})



######################## ANOMALY TESTS #####################


test_that("Anomaly test - error when r parametre isn't a number", {
  # arrange
  Z <- matrix(rbinom(5*5, 1, 0.5), nrow = 5)
  # act & assert
  expect_error(moving_average(Z, "a"), "^r must be an integer$")
})

test_that("Anomaly test - error when r parametre is a floating number", {
  # arrange
  Z <- matrix(rbinom(5*5, 1, 0.5), nrow = 5)
  # act & assert
  expect_error(moving_average(Z, 0.5), "^r must be an integer$")
})

test_that("Anomaly test - error when r parametre is less than zero", {
  # arrange
  Z <- matrix(rbinom(5*5, 1, 0.5), nrow = 5)
  # act & assert
  expect_error(moving_average(Z, -2), "^r must be >= 0$")
})




