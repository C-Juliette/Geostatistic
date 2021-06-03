moy_gliss <- function(Z, r){
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
