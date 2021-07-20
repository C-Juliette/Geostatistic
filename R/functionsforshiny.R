#' charactervector_as_vector
#'
#' @param x Ex : x <- "(0, 1)" a string
#'
#' @return a vector Ex : c(0, 1)
#' @export
#'
#' @examples
#' charactervector_as_vector("(0, 1)")
#'
charactervector_as_vector <- function(x){
  chaine_splitee <- strsplit(x,"")
  return(c(as.numeric(chaine_splitee[[1]][2]),as.numeric(chaine_splitee[[1]][5])))}


#' Liste
#'
#' @param v a vector with many strings of vectors Ex :  c("(0, 1)", "(1, 1)")
#'
#' @return a list of vectors
#' @export
#'
#' @examples
#' liste(c("(0, 1)", "(1, 1)"))
liste <- function(v){
  v2 <- list()
  for (i in 1:length(v)){
    v2 <- rlist::list.append(v2, charactervector_as_vector(v[i]))
  }
  return(v2)
}
