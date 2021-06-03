test_that("moving average works", {
  set.seed(1)
  Z <- matrix(rbinom(5*5, 1, 0.5), nrow = 5)
  expect_identical(moving_average(Z,2), matrix(c(13/25)))
})






