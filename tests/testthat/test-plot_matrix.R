######################## ANOMALY TESTS #####################

test_that("Anomaly test - error when M does not contain number", {
  # arrange
  Z <- "a"
  # act & assert
  expect_error(plot_matrix(Z), "^M must contain numbers$")
})
