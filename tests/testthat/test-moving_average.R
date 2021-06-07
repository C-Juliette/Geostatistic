######################### REGULAR TESTS #######################
test_that("Regular test - full of zeros grid", {
  # arrange
  set.seed(1)
  Z <- matrix(rep(0, 5*5), nrow = 5)
  # act
  actual <- moving_average(Z,2)
  # assert
  expected <- matrix(c(0))
  expect_identical(actual, expected)
})


test_that("Regular test - full of ones grid (and 5*5 window)", {
  # arrange
  set.seed(1)
  Z <- matrix(rep(1, 5*5), nrow = 5)
  # act
  actual <- moving_average(Z,2)
  # assert
  expected <- matrix(c(1))
  expect_identical(actual, expected)
})

test_that("Regular test - full of ones grid (and 3*3 window)", {
  # arrange
  set.seed(1)
  Z <- matrix(rep(1, 5*5), nrow = 5)
  # act
  actual <- moving_average(Z,1)
  # assert
  expected <- matrix(rep(1,9), nrow = 3)
  expect_identical(actual, expected)
})


test_that("Regular test - Bernoulli grid", {
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

test_that("Anomaly test - error when the window is smaller than the grid", {
  # arrange
  Z <- matrix(rbinom(5*5, 1, 0.5), nrow = 5)
  # act & assert
  expect_error(moving_average(Z, 4), "^The grid must be larger than the window. Decrease r or increase the grid size$")
})

test_that("Anomaly test - error when the window lenght is smaller than the grid lenght", {
  # arrange
  Z <- matrix(rbinom(5*2, 1, 0.5), nrow = 5)
  # act & assert
  expect_error(moving_average(Z, 1), "^The grid must be larger than the window. Decrease r or increase the grid size$")
})


test_that("Anomaly test - error when the window width is smaller than the grid width", {
  # arrange
  Z <- matrix(rbinom(3*10, 1, 0.5), nrow = 3)
  # act & assert
  expect_error(moving_average(Z, 2), "^The grid must be larger than the window. Decrease r or increase the grid size$")
})

