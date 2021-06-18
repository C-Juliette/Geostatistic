########################## REGULAR TESTS #######################

test_that("Regular test - Bernoulli law", {
  # arrange
  set.seed(1)
  Y <- matrix(rbinom(6*6, 1, 0.5), nrow = 6)
  firstrows <- c(6/9,6/9,6/9, 3/9,3/9,3/9)
  lastrows <- rep(4/9, 6)
  # act
  actual <- matrix(c(rep(firstrows, 3), rep(lastrows, 3)), nrow = 6, byrow = T)
  # assert
  expected <- block_averages(Y, 1)
  expect_equal(actual, expected)
})

test_that("Regular test - Bernoulli law and grand_pixel = F", {
  # arrange
  set.seed(1)
  Y <- matrix(rbinom(6*6, 1, 0.5), nrow = 6)
  # act
  actual <- matrix(c(6/9 ,3/9, 4/9, 4/9), nrow = 2, byrow = T)
  # assert
  expected <- block_averages(Y, 1, grand_pixel = F)
  expect_equal(actual, expected)
})
