#' moving average
#'
#' Create a new map/matrix by doing a moving average on a matrix
#'
#' @param Z matrix
#' @param r integer : the radius of the window of the moving average. Ex : r = 1 implies a window of 3*3
#'
#' @return matrix (side effects imply a smaller matrix than the Z matrix passed in parametre)
#' @export
#'
#' @examples
#' moving_average(matrix(rnorm(10*10, 0,1), nrow = 10), r=2)
moving_average <- function(Z, r){
  if (r < 0 ){paste("Error : r must be positive or zero")}
  else if (isInteger(r) == F){paste("Error : r must be an integer")}
  else if (is.matrix(Z) == F & is.data.frame(Z) == F){paste("Error : Z must be a matrix/dataframe/tibble")}
  else{
  Z <- as.matrix(Z)
  nblignesZ <- dim(Z)[1]
  nbcolonnesZ <- dim(Z)[2]
  nblignesY <- nblignesZ - 2*r
  nbcolonnesY <- nbcolonnesZ - 2*r

  Y <- matrix(data = rep(0, nblignesY*nbcolonnesY), nrow = nblignesY)

  for(i in (r+1):(nblignesZ-r)){
    for(j in (r+1):(nbcolonnesZ-r)){
      fenetre <- Z[(i-r):(i+r), (j-r):(j+r)]
      Y[i-r,j-r] <- mean(fenetre)
    }
  }
  return (Y)
  }
}

