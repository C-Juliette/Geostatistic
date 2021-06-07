#' Long shaped matrix
#'
#' @param Z a matrix/dataframe/vector
#'
#' @return a dataframe : first column is x coordinates, second column is y coordinate, third column is the value
#' @export
#'
#' @examples
#' Z <- matrix(1:25, nrow = 5)
#' long_shaped_matrix(Z)
long_shaped_matrix <- function(Z){
  if (!is.vector(Z) & !is.matrix(Z) & !is.data.frame(Z)){stop("Z must be a vector/matrix/dataframe")}
  Z <- as.data.frame(Z)
  nrows <- dim(Z)[1]
  ncols <- dim(Z)[2]
  Z <- Z |> as.data.frame() |> setNames(1:ncols) |> tidyr::gather() |> stats::setNames(c("Var2","value"))
  Z$Var1 <- seq(1:nrows)
  Z$Var2 <- as.integer(Z$Var2)
  Z <- Z[, c("Var1", "Var2", "value")] |> tibble::tibble()
  return (Z)
}
