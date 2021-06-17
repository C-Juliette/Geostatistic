########################## REGULAR TESTS #######################

test_that("Regular test - Variance of a grid of 1", {
  # arrange
  Z <- matrix(rep(1, 15*20), nrow = 15)
  # act
  actual <- VW(Z, 2)
  # assert
  expected <- 0
  expect_equal(actual, expected)
})
