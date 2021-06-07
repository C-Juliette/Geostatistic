long_shaped_matrix <- function(Z){
  nrows <- dim(Z)[1]
  ncols <- dim(Z)[2]
  Z <- Z |> as.data.frame() |> setNames(1:ncols) |> tidyr::gather() |> setNames(c("Var2","value"))
  Z$Var1 <- seq(1:nrows)
  Z$Var2 <- as.integer(Z$Var2)
  Z <- Z[, c("Var1", "Var2", "value")] |> tibble::tibble()
  return (Z)
}
