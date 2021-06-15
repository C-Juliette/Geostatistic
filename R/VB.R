VB <- function(Y,rprime){
  Y2 <- block_averages(Y, rprime = 2, grand_pixel = FALSE)
  sinter <- variance(Y2)
  return(sinter)
}
