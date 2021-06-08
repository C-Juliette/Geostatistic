######################## ANOMALY TESTS #####################

test_that("Anomaly test - error when M does not contain number - test 1", {
  # arrange
  Z <- "a"
  # act & assert
  expect_error(plot_matrix(Z), "^M must contain numbers$")
})


test_that("Anomaly test - error when M does not contain number - test 2", {
  # arrange
  Z <- c("a")
  # act & assert
  expect_error(plot_matrix(Z), "^M must contain numbers$")
})
