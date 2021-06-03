test_that("variance function works", {
  set.seed(1)
  Z <- matrix(rbinom(2*2, 1, 0.5), nrow = 2)
  expect_equal(variance(Z), 0.5**2)
})
