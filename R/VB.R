#' Variance Between
#'
#' @param Y a matrix
#' @param rprime the radius of blocks. For example, rprime = 1 implies a window of 3*3.
#'
#' @return the variance between blocks
#' @export
#'
#' @examples
#' VB(matrix(rnorm(15*20, 0, 1), nrow = 15), rprime = 2)
VB <- function(Y,rprime){
  Y2 <- block_averages(Y, rprime = 2, grand_pixel = FALSE)
  sinter <- variance(Y2)
  return(sinter)
}
