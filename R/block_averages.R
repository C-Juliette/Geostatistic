#' Mean by blocks
#'
#' @param Y a matrix
#' @param rprime the radius of blocks. For example, rprime = 1 implies a window of 3*3.
#' @param grand_pixel return a matrix with blocks of (2rprime+1)**2 or of 1 pixels only (by default)
#'
#' @return
#' @export
#'
#' @examples
#'
block_averages <- function(Y, rprime, grand_pixel = TRUE){
  facteur_dilatation <- 2*rprime+1

  Ytemp <- moving_average(Y, rprime)
  selection_ligne <- seq(1, dim(Ytemp)[1], facteur_dilatation)
  selection_colonne <- seq(1, dim(Ytemp)[2], facteur_dilatation)
  Y2_sansNA <- Ytemp[selection_ligne,selection_colonne]

  if(grand_pixel == TRUE){
    Y2 <- matrix(rep(0, dim(Y)[1]*dim(Y)[2]), nrow = dim(Y)[1])
    for(i in 1:dim(Y2_sansNA)[1]){
      for(j in 1:dim(Y2_sansNA)[2]){
        fenetre_ligne <- (facteur_dilatation*i - (facteur_dilatation -1)):(facteur_dilatation*i)
        fenetre_colonne  <- (facteur_dilatation*j - (facteur_dilatation -1)):(facteur_dilatation*j)
        Y2[fenetre_ligne,fenetre_colonne] <- Y2_sansNA[i,j]
      }
    }
  }
  if(grand_pixel == FALSE){Y2 <- Y2_sansNA}
  return(Y2)
}
