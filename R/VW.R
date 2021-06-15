#' Variance Within
#'
#' @param Y a matrix
#' @param rprime the radius of blocks. For example, rprime = 1 implies a window of 3*3.
#'
#' @return the mean of the variance inside blocks
#' @export
#'
#' @examples
#' VW(matrix(rnorm(15*20, 0, 1), nrow = 15), rprime = 2)
#'
VW <- function(Y, rprime){
  facteur_dilatation <- 2*rprime+1
  V <- c()
  bloc_ligne <- dim(Y)[1] %/% facteur_dilatation
  bloc_colonne <- dim(Y)[2] %/% facteur_dilatation
  for( i in 1:bloc_ligne){
    for(j in 1:bloc_colonne){
      fenetre_ligne <- (facteur_dilatation*i - (facteur_dilatation -1)):(facteur_dilatation*i)
      fenetre_colonne  <- (facteur_dilatation*j - (facteur_dilatation -1)):(facteur_dilatation*j)
      fenetre <- Y[fenetre_ligne,fenetre_colonne]
      V <- append(V,variance(as.vector(fenetre)))
    }
  }
  return(mean(V))
}
