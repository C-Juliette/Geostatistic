ggplot <- ggplot2::ggplot
aes <- ggplot2::aes
geom_tile <- ggplot2::geom_tile
scale_fill_viridis_c <- ggplot2::scale_fill_viridis_c
labs <- ggplot2::labs
guides <- ggplot2::guides
guide_colorbar <- ggplot2::guide_colorbar
scale_y_continuous <- ggplot2::scale_y_continuous
scale_x_continuous <- ggplot2::scale_x_continuous

########################## REGULAR TESTS #######################
#test_that("Regular test - simple plot", {
#  # arrange
#  M <- matrix(1:25, nrow = 5)
#
#  #titre <- stringr::str_c("Var = " ,as.character(round(variance(M), 4)),  sep = "")
#  p <- ggplot(long_shaped_matrix(M), aes(x = .data$Var1 -0.5, y = .data$Var2 -0.5)) + #, z= .data$value, fill=.data$value)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis_c(option = "B", direction = -1) #+
#   #labs(title = titre,
#   #     x = "",
#   #     y = "") +
#   #guides(fill = guide_colorbar(title = "Scale"))+
#   #scale_y_continuous(breaks = scales::pretty_breaks())+
#   #scale_x_continuous(breaks = scales::pretty_breaks())
#
#  disp_hist_base <- function() hist(mtcars$disp)
#  # act
#  actual <- plot_matrix(M)
#  # assert
#  expected <- p
#  expect_doppelganger("actual", actual)
#})
#
#







######################## ANOMALY TESTS #####################

test_that("Anomaly test - error when M does not contain number - test 1", {
  # arrange
  M <- "a"
  # act & assert
  expect_error(plot_matrix(M), "^M must contain numbers$")
})


test_that("Anomaly test - error when M does not contain number - test 2", {
  # arrange
  M <- c("a")
  # act & assert
  expect_error(plot_matrix(M), "^M must contain numbers$")
})

#
#library("ggplot2")
#
#test_that("plots have known output", {
#  disp_hist_base <- function() hist(mtcars$disp)
#  expect_doppelganger("disp-histogram-base", disp_hist_base)
#
#  disp_hist_ggplot <- ggplot(mtcars, aes(disp)) + geom_histogram()
#  expect_doppelganger("disp-histogram-ggplot", disp_hist_ggplot)
#})
#


